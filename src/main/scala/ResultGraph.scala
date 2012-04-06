import Name.{T => Name}
import java.lang.StringBuilder

// ResultGraph: small, immutable graph indexed by IDs only. 
// Used as a return type from computations and searches. 
// No optimizations for fast Queries, since it's intended for small graphs. 

class ResultGraph[NodeT <: Node, EdgeT <: Edge] (nodeColl:Iterable[NodeT], edgeColl:Iterable[EdgeT]) extends Graph[NodeT, EdgeT] {
  private val nodes = nodeColl.map(node => (node.id, node)).toMap
  private val edges = edgeColl.map(edge => (edge.id, edge)).toMap

  private var validation = None : Option[Boolean]

  def validate():Unit = {
    val result = 
      edges.values.forall(edge => nodes.contains(edge.source) 
			  && nodes.contains(edge.dest))
    if (result) {
      validation = Some(true)
    }
    else {
      validation = Some(false)
      throw new ValidationException("ResultGraph")
    }
  }

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

  private def findNodes(nodeFilter:NodeFilter[NodeT]) = {
    new ResultGraph(nodes.values.filter(nodeFilter), Set.empty[EdgeT])
  }

  private def followEdgesFrom(g:ResultGraph[NodeT,EdgeT],
			      edgeFilter:EdgeFilter[EdgeT],
			      nodeFilter:NodeFilter[NodeT],
			      depth:Option[Int]) = {
    throw new Exception("not implemented")
  }

  private def followEdges(q:Query[NodeT, EdgeT],
			  edgeFilter:EdgeFilter[EdgeT], 
			  nodeFilter:NodeFilter[NodeT],
			  depth:Option[Int]) = {
    followEdgesFrom(search(q),
		    edgeFilter, nodeFilter, depth)
  }

  def search(q:Query[NodeT, EdgeT]):ResultGraph[NodeT, EdgeT] = {
    q match {
      case FindNodes(nodeFilter) => findNodes(nodeFilter)
      case FollowEdges(q, edgeFilter, nodeFilter, depth) => 
	followEdges(q, edgeFilter, nodeFilter, depth)
    }
  }

  private def tuple() = (nodes, edges)

  override def toString() = {
    val validationString = validation match {
      case None => "(not validated)"
      case Some(true) => "(validated)"
      case Some(false) => "(FAILED validation)"
    }
    "ResultGraph: %d nodes, %d edges %s".format(nodes.size, edges.size,
						validationString)
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
