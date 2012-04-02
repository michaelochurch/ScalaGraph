import Name.{T => Name}
import java.lang.StringBuilder

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
