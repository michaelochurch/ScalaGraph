import Name.{T => Name}

class ValidationException(msg:String) extends Exception(msg)

trait Graph[NodeT <: Node, EdgeT <: Edge] {
  def getNode(nodeId:Name):Option[NodeT]
  def getEdge(edgeId:Name):Option[EdgeT]

  def outEdges(nodeId:Name):Set[EdgeT]
  def inEdges(nodeId:Name):Set[EdgeT]
}
