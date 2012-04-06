import Name.{T => Name}

// This is the intermediate Query language for graphs. It's too
// complex to be a human interface, but it's precise and easier to
// work with than a DSL (for now). 

// NodeFilter is essentially (NodeT => Boolean) so it's contravariant.  
abstract class NodeFilter[-NodeT <: Node] extends Function1[NodeT, Boolean]

case object TrueNF extends NodeFilter[Node] {
  def apply(n:Node) = true
}

case object FalseNF extends NodeFilter[Node] {
  def apply(n:Node) = false
}

case class NodeIdIn(ids:Set[Name]) extends NodeFilter[Node] {
  def apply(n:Node) = ids.contains(n.id)
}

case class NodeTypeIn[-NodeT <: Node](types:String*) extends NodeFilter[NodeT] {
  val typeSet = types.toSet
  
  def apply(n:NodeT) = {
    n.getType() match {
      case Some(t) => typeSet.contains(t)
      case None => false
    }
  }
}

abstract class EdgeFilter[-EdgeT]
case object TrueEF extends EdgeFilter[Any]
case object FalseEF extends EdgeFilter[Any]
case class EdgeIdIn(ids:Set[Name]) extends EdgeFilter[Any]
case class EdgeTypeIn[-EdgeT](types:String*) extends EdgeFilter[EdgeT]

// Note: not all Queries are supported on all graphs. 
abstract class Query[NodeT, EdgeT]
case class FindNodes[NodeT <: Node, EdgeT <: Edge](nodeFilter:NodeFilter[NodeT]) extends Query[NodeT, EdgeT]
case class FollowEdges[NodeT <: Node, EdgeT <: Edge](
  q:Query[NodeT, EdgeT], edgeFilter:EdgeFilter[EdgeT], 
  nodeFilter:NodeFilter[NodeT] = TrueNF, 
  depth:Option[Int] = Some(1)) extends Query[NodeT, EdgeT]

class QueryNotSupported(query:Query[_,_], graphType:String) extends Exception
