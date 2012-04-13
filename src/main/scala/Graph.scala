import Name.{T => Name}

class GraphTooLargeException(msg:String) extends Exception(msg)
class ValidationException(msg:String) extends Exception(msg)
class IllegalGraphOperationException(msg:String) extends Exception(msg)
class Rule34Exception(msg:String) extends Exception(msg)

trait Graph[NodeT <: Node, EdgeT <: Edge, T] {
  def getNode(nodeId:Name):Option[NodeT]
  def getEdge(edgeId:Name):Option[EdgeT]

  def outEdges(nodeId:Name):Set[EdgeT]
  def inEdges(nodeId:Name):Set[EdgeT]

  // For testing, but not to be used on large graphs. 
  def toResultGraph():ResultGraph[NodeT, EdgeT]

  def search(q:Query[NodeT, EdgeT]):ResultGraph[NodeT, EdgeT]

  // If the Graph is immutable, loadFromFile returns a new graph.
  // If it's mutable, loadFromFile adds all nodes and edges in that file, 
  // and returns the graph itself. 
  def loadFromFile(filename:String):T
  def saveToFile(filename:String):Unit
}
