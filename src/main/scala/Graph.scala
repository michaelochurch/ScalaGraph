import Name.{T => Name}

class ValidationException(msg:String) extends Exception(msg)

trait Graph[NodeT <: Node, EdgeT <: Edge, T] {
  def getNode(nodeId:Name):Option[NodeT]
  def getEdge(edgeId:Name):Option[EdgeT]

  def outEdges(nodeId:Name):Set[EdgeT]
  def inEdges(nodeId:Name):Set[EdgeT]

  // If the Graph is immutable, loadFromFile returns a new graph.
  // If it's mutable, loadFromFile adds all nodes and edges in that file, 
  // and returns the graph itself. 
  def loadFromFile(filename:String):T
  def saveToFile(filename:String):Unit
}
