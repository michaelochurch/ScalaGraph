import Name.{T => Name}
import java.lang.StringBuilder
import scala.collection.mutable

// ResultGraph: small, immutable graph indexed by IDs only. 
// Used as a return type from computations and searches. 
// No optimizations for fast Queries, since it's intended for small graphs. 

class ResultGraph[NodeT <: Node, EdgeT <: Edge] (nodeColl:Iterable[NodeT], edgeColl:Iterable[EdgeT]) extends Graph[NodeT, EdgeT, ResultGraph[NodeT, EdgeT]] {
  val nodes = nodeColl.map(node => (node.id, node)).toMap
  val edges = edgeColl.map(edge => (edge.id, edge)).toMap

  private var validation = None : Option[Boolean]

  def this() = {
    this(Set.empty, Set.empty)
  }

  // Returns a *new* ResultGraph. 
  def loadFromFile(filename:String) = {
    ResultGraph.loadFromFile(filename)
  }

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
    def fail() = {
      throw new Exception("inconsistent graph")
    }

    // TODO(michaelochurch): refactor the huge function. 
    def loop(unexploredNodeIds:Set[Name],
             exploredNodeIds:Set[Name],
             allNodes:Set[NodeT],
             allEdges:Set[EdgeT],
             depth:Int):ResultGraph[NodeT,EdgeT] = {
      if (depth == 0 || unexploredNodeIds.isEmpty) {
        new ResultGraph(allNodes, allEdges)
      }
      else {
        def nodeAndEdgeFilter(edge:EdgeT):Option[(EdgeT, NodeT)] = {
          if (edgeFilter(edge)) {
            val node = getNode(edge.dest).getOrElse(fail())
            if (nodeFilter(node)) {
              Some((edge, node))
            } else None          
          } else None
        }
        val outEdgeIds = unexploredNodeIds.flatMap(outEdges(_))
        val matches = outEdgeIds.flatMap(nodeAndEdgeFilter)
        val newNodes = matches.map(_._2)
        val nowExplored = exploredNodeIds ++ unexploredNodeIds
        loop(newNodes.map(_.id) -- nowExplored, 
             nowExplored,
             allNodes ++ newNodes,
             allEdges ++ matches.map(_._1),
             depth - 1)
      }
    }
    loop(unexploredNodeIds = g.nodes.keySet, 
         exploredNodeIds = Set.empty, 
         allNodes = g.nodes.values.toSet,
         allEdges = g.edges.values.toSet,
         depth = depth.getOrElse(-1))
  }

  private def followEdges(q:Query[NodeT, EdgeT],
                          edgeFilter:EdgeFilter[EdgeT], 
                          nodeFilter:NodeFilter[NodeT],
                          depth:Option[Int]) = {
    followEdgesFrom(search(q),
                    edgeFilter, nodeFilter, depth)
  }

  def toResultGraph() = this

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
      case (graph:ResultGraph[_, _]) => graph.tuple == this.tuple
      case _ => false
    }
  }

  def saveToFile(filename:String) = {
    val writer = Serialization.objectWriter(filename)
    try {
      Serialization.writeGraphSize(writer, nodes.size, edges.size)
      for (node <- nodes.values) {
        Serialization.writeNode(writer, node)
      }
      for (edge <- edges.values) {
        Serialization.writeEdge(writer, edge)
      }
    } finally {
      writer.close()
    }
  }
}

object ResultGraph {
  def loadFromFile[NodeT <: Node, EdgeT <: Edge](filename:String) = {
    val reader = Serialization.objectReader(filename) 
    try {
      val (nNodes, nEdges) = Serialization.readGraphSize(reader)
      val nodes = (1 to nNodes).map(_ => Serialization.readNode[NodeT](reader))
      val edges = (1 to nEdges).map(_ => Serialization.readEdge[EdgeT](reader))
      new ResultGraph[NodeT, EdgeT](nodes, edges)
    } finally {
      reader.close()
    }
  }

  def empty[NodeT <: Node, EdgeT <: Edge]() = 
    new ResultGraph[NodeT, EdgeT]()

  def basic = empty[BaseNode, BaseEdge]
}
