import Name.{T => Name}
import scala.collection.mutable

class MutableInMemoryGraph[NodeT <: Node, EdgeT <: Edge] extends Graph[NodeT, EdgeT, MutableInMemoryGraph[NodeT, EdgeT]] {
  private val nodes = mutable.Map[Name, NodeT]()
  private val edges = mutable.Map[Name, EdgeT]()

  private val edgesBySource = new Index[Name, Name]()
  private val edgesByDest = new Index[Name, Name]()

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

  def addNode(node:NodeT):Name = {
    nodes += (node.id -> node)
    node.id
  }

  def addEdge(edge:EdgeT):Name = {
    edges += (edge.id -> edge)
    val sourceId = edge.source
    val destId = edge.dest

    // TODO(mike): return a better error.
    require(nodeExists(sourceId) && nodeExists(destId))

    edgesBySource.add(edge.source, edge.id)
    edgesByDest.add(edge.dest, edge.id)
    edge.id
  }

  //TODO(michaelochurch): The no-op cases should throw an error. A better one. 
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
    if (nodes.size > 1000000 || edges.size > 1000000) {
      throw new GraphTooLargeException("MutableInMemoryGraph.toResultGraph")
    } else {
      new ResultGraph(nodes.values, edges.values)
    }
  }

  def print():Unit = {
    toResultGraph.print()
  }

  def search(q:Query[NodeT, EdgeT]) = {
    throw new Exception("not impl!")
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
