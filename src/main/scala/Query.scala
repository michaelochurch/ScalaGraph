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

abstract class EdgeFilter[-EdgeT <: Edge] extends Function1[EdgeT, Boolean]
case object TrueEF extends EdgeFilter[Edge] {
  def apply(e:Edge) = true
}
case object FalseEF extends EdgeFilter[Edge] {
  def apply(e:Edge) = false
}
// case class EdgeIdIn(ids:Set[Name]) extends EdgeFilter[Any]
case class EdgeTypeIn[-EdgeT <: Edge](types:String*) extends EdgeFilter[EdgeT] {
  val typeSet = types.toSet
  
  def apply(e:EdgeT) = {
    e.getType() match {
      case Some(t) => typeSet.contains(t)
      case None => false
    }
  }
}

// Note: not all Queries are supported on all graphs. 
abstract class Query[NodeT, EdgeT]
case class FindNodes[NodeT <: Node, EdgeT <: Edge](nodeFilter:NodeFilter[NodeT]) extends Query[NodeT, EdgeT]
case class FollowEdges[NodeT <: Node, EdgeT <: Edge](
  q:Query[NodeT, EdgeT], 
  edgeFilter:EdgeFilter[EdgeT] = TrueEF, 
  nodeFilter:NodeFilter[NodeT] = TrueNF, 
  depth:Option[Int] = Some(1)) extends Query[NodeT, EdgeT]

class QueryNotSupported(query:Query[_,_], graphType:String) extends Exception
