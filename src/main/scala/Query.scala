import Name.{T => Name}

// This is the intermediate Query language for graphs. It's too
// complex to be a human interface, but it's precise and easier to
// work with than a DSL (for now). 

// NodeFilter is essentially (NodeT => Boolean) so it's contravariant.  
abstract class NodeFilter[-NodeT]
case object TrueNF extends NodeFilter[Any]
case object FalseNF extends NodeFilter[Any]
case class NodeIdIn(ids:Set[Name]) extends NodeFilter[Any]
case class NodeTypeIn[-NodeT](types:String*) extends NodeFilter[NodeT]

abstract class EdgeFilter[-EdgeT]
case object TrueEF extends EdgeFilter[Any]
case object FalseEF extends EdgeFilter[Any]
case class EdgeIdIn(ids:Set[Name]) extends EdgeFilter[Any]
case class EdgeTypeIn[-EdgeT](types:String*) extends EdgeFilter[EdgeT]

// Note: not all Queries are supported on all graphs. 
abstract class Query[NodeT, EdgeT]
case class FindNodes[NodeT, EdgeT](nodeFilter:NodeFilter[NodeT]) extends Query[NodeT, EdgeT]
//case class FollowEdges(q:Query, edgeFilter:EdgeFilter, depth:Option[Int] = Some(1)) extends Query[NodeT, EdgeT]

class QueryNotSupported(query:Query[_,_], graphType:String) extends Exception
