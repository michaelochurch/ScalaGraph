import Name.{T => Name}
import scala.collection.mutable

class MutableInMemoryGraph[NodeT <: Node, EdgeT <: Edge] extends Graph[NodeT, EdgeT, MutableInMemoryGraph[NodeT, EdgeT]] {
  private val nodes = mutable.Map[Name, NodeT]()
  private val edges = mutable.Map[Name, EdgeT]()

  private val edgesBySource = new Index[Name, Name]()
  private val edgesByDest = new Index[Name, Name]()

  private val typeIndex = new Index[String, Name]()

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

  def loadFromFile(filename:String) = {
    val reader = Serialization.objectReader(filename)
    try {
      val (nNodes, nEdges) = Serialization.readGraphSize(reader)
      for (i <- (1 to nNodes)) {
        addNode(Serialization.readNode[NodeT](reader))
      }
      for (i <- (1 to nEdges)) {
        addEdge(Serialization.readEdge[EdgeT](reader))
      }
      this
    } finally {
      reader.close()
    }
  }

  def getNode(nodeId:Name):Option[NodeT] = {
    nodes.get(nodeId)
  }

  private def nodeExists(nodeId:Name):Boolean = {
    nodes.contains(nodeId)
  }

  def getEdge(edgeId:Name):Option[EdgeT] = {
    edges.get(edgeId)
  }

  def inEdges(nodeId:Name) = {
    edgesByDest.lookup(nodeId).map(edges(_))
  }

  def outEdges(nodeId:Name) = {
    edgesBySource.lookup(nodeId).map(edges(_))
  }

  // TODO(michaelochurch): Field Indexes. People should be able to trim a 
  // findNodes query by a field index.  
  private def findNodes(nf:NodeFilter[NodeT]) = {
    val foundNodes = 
      nf match {        
        case TrueNF => nodes.values      // Full-table scan. Generally bad. 
        case FalseNF => Set.empty
        case NodeIdIn(ids) => 
          ids.flatMap(nodes.get)
        case nt@NodeTypeIn(_) => {
          val nodeIds = nt.types.flatMap(typ => typeIndex.lookup(typ))
          nodeIds.flatMap(nodes.get)
        }
        case _ => 
          nodes.values.filter(nf)        // Full-table scan. Generally bad.
      }
    new ResultGraph(foundNodes, Set.empty[EdgeT])
  }

  // Identical to algorithm in ResultGraph.scala (as of 13. April 2012)
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

  def search(q:Query[NodeT, EdgeT]):ResultGraph[NodeT, EdgeT] = {
    q match {
      case FindNodes(nodeFilter) => findNodes(nodeFilter)
      case FollowEdges(q, edgeFilter, nodeFilter, depth) => 
        followEdges(q, edgeFilter, nodeFilter, depth)
    }
  }

  // addNode and addEdge return the ID for convenience, because some 
  // chains of operations might build a node/edge (with unknown ID).

  def addNode(node:NodeT):Name = {
    nodes += (node.id -> node)
    node.getType match {
      case Some(typ) => 
        typeIndex.add(typ, node.id)
      case None => 
    }
    node.id
  }

  def addEdge(edge:EdgeT):Name = {
    edges += (edge.id -> edge)
    val sourceId = edge.source
    val destId = edge.dest

    // TODO(mike): return a better error.
    if (!nodeExists(sourceId) || !nodeExists(destId))
      throw new IllegalGraphOperationException("MutableInMemoryGraph.addEdge")

    edgesBySource.add(edge.source, edge.id)
    edgesByDest.add(edge.dest, edge.id)
    edge.id
  }

  def deleteEdge(edgeId:Name):Unit = {
    edges.get(edgeId) match {
      case Some(edge) => {
        edgesBySource.remove(edge.source, edge.id)
        edgesByDest.remove(edge.dest, edge.id)
        edges -= edgeId
      }
      case None =>
    }
  }

  def deleteNode(nodeId:Name):Unit = {
    if (edgesBySource.lookup(nodeId).isEmpty && 
        edgesByDest.lookup(nodeId).isEmpty) {
      nodes.get(nodeId) match {
        case Some(node) => {
          nodes -= nodeId
        }
        case None =>
      }
    } else {
      throw new IllegalGraphOperationException("MutableInMemoryGraph.deleteNode")
    }
  }

  def toResultGraph():ResultGraph[NodeT, EdgeT] = {
    if (nodes.size > 1000000 || edges.size > 1000000) {
      throw new GraphTooLargeException("MutableInMemoryGraph.toResultGraph")
    } else {
      new ResultGraph(nodes.values, edges.values)
    }
  }

  def print():Unit = {
    toResultGraph.print()
  }

  override def toString() = {
    "MutableInMemoryGraph: %d nodes, %d edges".format(nodes.size, edges.size)
  }
}

object MutableInMemoryGraph {
  def empty[NodeT <: Node, EdgeT <: Edge]() =
    new MutableInMemoryGraph[NodeT, EdgeT]()
  
  def basic = empty[BaseNode, BaseEdge] 

  def loadFromFile[NodeT <: Node, EdgeT <: Edge](filename:String) = 
    empty[NodeT, EdgeT].loadFromFile(filename)
}
