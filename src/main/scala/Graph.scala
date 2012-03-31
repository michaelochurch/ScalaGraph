import java.lang.StringBuilder
import java.util.UUID

import scala.collection.mutable
import scala.collection.JavaConverters._

object Utils {
  // We need the curried function signature or we'll have to explicitly type f. 
  def mergeMaps[K, V1, V2, V](m1:Map[K, V1], m2:Map[K, V2])(f:((K, Option[V1], Option[V2]) => Option[V])) = {
    val allKeys = m1.keySet union m2.keySet
    allKeys.flatMap(k => f(k, m1.get(k), m2.get(k)).map(v => (k, v))).toMap
  }
}

object Name {
  type T = UUID
  
  def make() = {
    UUID.randomUUID
  }
}

import Name.{T => Name}

abstract class Node {
  val id : Name
}

abstract class Edge {
  val id : Name
  val source : Name
  val dest : Name
}

case class Payload(typ:Option[String], data:Map[String,String])

case class PayloadDelta(data:Map[String,Option[String]])

object Payload {
  def empty = new Payload(None, Map())

  def add(payload:Payload, delta:PayloadDelta) = {
    val newData = 
      Utils.mergeMaps(payload.data, delta.data)(
	(_:String, v1Opt:Option[String], v2Opt:Option[Option[String]]) => 
	  v2Opt match {
	    case None => v1Opt
	    case Some(v2) => v2
	  })
    new Payload(payload.typ, newData)
  }
}

class BaseNode(val payload: Payload) extends Node {
  val id = Name.make()

  private def tuple() = {
    (id, payload)
  }

  override def hashCode() = {
    this.tuple.hashCode()
  }

  override def equals(that:Any) = {
    that match {
      case (node:BaseNode) => this.tuple == node.tuple
      case _ => false
    }
  }

  override def toString() = {
    val typeString = this.payload.typ getOrElse "(untyped)"
    "<Node %s: %s, %s>".format(this.id, typeString, this.payload.data)
  }
}

object BaseNode {
  def apply() = new BaseNode(Payload.empty)

  def apply(payload:Payload) = new BaseNode(payload)
}

class BaseEdge(val payload: Payload, val source:Name, val dest:Name) extends Edge {
  val id = Name.make()

  private def tuple() = {
    (id, source, dest, payload)
  }

  override def hashCode() = {
    this.tuple.hashCode()
  }

  override def equals(that:Any) = {
    that match {
      case (edge:BaseEdge) => this.tuple == edge.tuple
      case _ => false
    }
  }

  override def toString() = {
    val typeString = this.payload.typ getOrElse "(untyped)"
    "<Edge %s (%s -> %s): %s, %s>".format(this.id, this.source, this.dest, 
					  typeString, this.payload.data)
  }
}

object BaseEdge {
  def apply(source:Name, dest:Name) = {
    new BaseEdge(Payload.empty, source, dest)
  }

  def apply(p:Payload, source:Name, dest:Name) = {
    new BaseEdge(p, source, dest)
  }
}

trait Graph[NodeT <: Node, EdgeT <: Edge] {
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

class ResultGraph[NodeT <: Node, EdgeT <: Edge] (nodeColl:Iterable[NodeT], edgeColl:Iterable[EdgeT]) extends Graph[NodeT, EdgeT] {
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

// Mutable in-memory index. Used for mutable graphs where we care
// about performance.
class Index[K <% Ordered[K], V] {
  // TODO(michaelochurch): replace with mutable.TreeMap when it exists
  private val data = new java.util.TreeMap[K, Set[V]]

  // Include (k, v) in the index. 
  def add(k:K, v:V):Unit = {
    if (data.containsKey(k)) {
      val vs = data.get(k)
      data.put(k, vs + v)
    } else {
      data.put(k, Set(v))
    }
  }

  // Remove (k, v) from the index. 
  def remove(k:K, v:V):Unit = {
    if (data.containsKey(k)) {
      val vs = data.get(k)
      if (vs == Set(v)) {
	data.remove(k)
      } else {
	data.put(k, vs - v)
      }
    }
  }

  // Remove (k, v) for all v from the index. 
  def removeAll(k:K):Unit = {
    data.remove(k)
  }

  // Get all v for which (k, v) is in the index. 
  def lookup(k:K):Set[V] = {
    if (data.containsKey(k)) data.get(k) else Set()
  }

  // Get all v for which there's a (k, v) in the index such the
  // range. Some(k) represents a bound and None represents no bound.
  def lookupRange(kLow:Option[K], kHigh:Option[K]):Iterable[V] = {
    val mutableSubmap = 
      (kLow, kHigh) match {
	case (None, None) => data
	case (None, Some(kH)) => data.headMap(kH)
	case (Some(kL), None) => data.tailMap(kL)
	case (Some(kL), Some(kH)) => data.subMap(kL, kH)
      }
    val submap = Map() ++ mutableSubmap.asScala 
    submap.flatMap({case (k, vs) => vs})
  }
}

class MutableInMemoryGraph[NodeT <: Node, EdgeT <: Edge] {
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

  def toResultGraph():ResultGraph[NodeT, EdgeT] = {
    new ResultGraph(nodes.values, edges.values)
  }

  def print():Unit = {
    toResultGraph.print()
  }
}
