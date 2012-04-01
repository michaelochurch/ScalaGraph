import java.lang.StringBuilder

import scala.collection.mutable

import Name.{T => Name}

trait Graph[NodeT <: Node[NodeT, _], EdgeT <: Edge] {
  def getNode(nodeId:Name):Option[NodeT]
  def getEdge(edgeId:Name):Option[EdgeT]

  def outEdges(nodeId:Name):Set[EdgeT]
  def inEdges(nodeId:Name):Set[EdgeT]
}

// NodeFilter is essentially (NodeT => Boolean) so it's contravariant.  
abstract class NodeFilter[-NodeT]
case object TrueNF extends NodeFilter[Any]
case class NodeIdIn(ids:Set[Name]) extends NodeFilter[Any]

// Note: not all Queries are supported on all graphs. 
abstract class Query[NodeT, EdgeT]
case class FindNodes[NodeT, EdgeT](nodeFilter:NodeFilter[NodeT]) extends Query[NodeT, EdgeT]
//case class FollowEdges(q:Query, edgeFilter:EdgeFilter, depth:Option[Int] = Some(1)) extends Query[NodeT, EdgeT]

// ResultGraph: small, immutable graph indexed by IDs only. 
// Used as a return type from computations and searches. 

class QueryNotSupported(query:Query[_,_], graphType:String) extends Exception

class ResultGraph[NodeT <: Node[NodeT, _], EdgeT <: Edge] (nodeColl:Iterable[NodeT], edgeColl:Iterable[EdgeT]) extends Graph[NodeT, EdgeT] {
  private val nodes = nodeColl.map(node => (node.id, node)).toMap
  private val edges = edgeColl.map(edge => (edge.id, edge)).toMap

  def validate():Boolean = {
    edges.values.forall(edge => nodes.contains(edge.source) 
			          && nodes.contains(edge.dest))
  }

  require(validate())

  def toStringLongform():String = {
    val builder = new StringBuilder()
    builder.append("%d NODES\n".format(nodes.size))
    for (node <- nodes.values) {
      builder.append(node.toString + "\n")
    }
    builder.append("%d EDGES\n".format(edges.size))
    for (edge <- edges.values) {
      builder.append(edge.toString + "\n")
    }
    builder.toString
  }

  def getNode(nodeId: Name):Option[NodeT] = {
    nodes.get(nodeId)
  }

  def getEdge(edgeId: Name):Option[EdgeT] = {
    edges.get(edgeId)
  }

  def outEdges(nodeId:Name):Set[EdgeT] = {
    // For ResultGraph, O(n) in number of edges. 
    edges.values.filter(edge => edge.source == nodeId).toSet
  }

  def inEdges(nodeId:Name):Set[EdgeT] = {
    // For ResultGraph, O(n) in number of edges.
    edges.values.filter(edge => edge.dest == nodeId).toSet
  }

  def print():Unit = {
    println(this.toStringLongform)
  }

  def search(q:Query[NodeT, EdgeT]):ResultGraph[NodeT, EdgeT] = {
    q match {
      case FindNodes(nf) => nf match {
	case TrueNF => new ResultGraph(nodes.values, Set.empty)
	case NodeIdIn(ids) => 
	  new ResultGraph(nodes.values.filter(n => ids.contains(n.id)), 
			  Set.empty)
	case _ => throw new QueryNotSupported(q, "ResultGraph")
      }
    }
  }

  private def tuple() = (nodes, edges)

  override def toString() = {
    "ResultGraph: %d nodes, %d edges".format(nodes.size, edges.size)
  }

  override def hashCode() = {
    this.tuple.hashCode
  }

  override def equals(that:Any) = {
    that match {
      // Be mindful of type erasure here. 
      case (graph:ResultGraph[_, _]) => graph.tuple == this.tuple
      case _ => false
    }
  }
}

class MutableInMemoryGraph[NodeDelta, NodeT <: Node[NodeT, NodeDelta], EdgeT <: Edge] {
  private val nodes = mutable.Map[Name, NodeT]()
  private val edges = mutable.Map[Name, EdgeT]()

  private val edgesBySource = new Index[Name, Name]()
  private val edgesByDest = new Index[Name, Name]()

  def getNode(nodeId:Name):Option[NodeT] = {
    nodes.get(nodeId)
  }

  private def nodeExists(nodeId:Name):Boolean = {
    nodes.contains(nodeId)
  }

  def getEdge(edgeId:Name):Option[EdgeT] = {
    edges.get(edgeId)
  }

  def addNode(node:NodeT):Name = {
    nodes += (node.id -> node)
    node.id
  }

  def addEdge(edge:EdgeT):Name = {
    edges += (edge.id -> edge)
    val sourceId = edge.source
    val destId = edge.dest

    require(nodeExists(sourceId) && nodeExists(destId))

    edgesBySource.add(edge.source, edge.id)
    edgesByDest.add(edge.dest, edge.id)
    edge.id
  }

  def deleteEdge(edgeId:Name):Boolean = {
    edges.get(edgeId) match {
      case Some(edge) => {
	edgesBySource.remove(edge.source, edge.id)
	edgesByDest.remove(edge.dest, edge.id)
	edges -= edgeId
	true
      }
      case None => false
    }
  }

  def deleteNode(nodeId:Name):Boolean = {
    if (edgesBySource.lookup(nodeId).isEmpty && 
	edgesByDest.lookup(nodeId).isEmpty) {
      nodes.get(nodeId) match {
	case Some(node) => {
	  nodes -= nodeId
	  true
	}
	case None => false
      }
    } else false
  }

  def updateNode(nodeId:Name, delta:NodeDelta) = {
    nodes.get(nodeId) match {
      case Some(node) => {
	nodes(nodeId) = node + delta
	true
      }
      case None => false
    }
  }

  def toResultGraph():ResultGraph[NodeT, EdgeT] = {
    new ResultGraph(nodes.values, edges.values)
  }

  def print():Unit = {
    toResultGraph.print()
  }
}
