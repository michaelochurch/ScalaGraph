import Name.{T => Name}

// NodeFilter is essentially (NodeT => Boolean) so it's contravariant.  
abstract class NodeFilter[-NodeT]
case object TrueNF extends NodeFilter[Any]
case class NodeIdIn(ids:Set[Name]) extends NodeFilter[Any]

// Note: not all Queries are supported on all graphs. 
abstract class Query[NodeT, EdgeT]
case class FindNodes[NodeT, EdgeT](nodeFilter:NodeFilter[NodeT]) extends Query[NodeT, EdgeT]
//case class FollowEdges(q:Query, edgeFilter:EdgeFilter, depth:Option[Int] = Some(1)) extends Query[NodeT, EdgeT]

// ResultGraph: small, immutable graph indexed by IDs only. 
// Used as a return type from computations and searches. 

class QueryNotSupported(query:Query[_,_], graphType:String) extends Exception
