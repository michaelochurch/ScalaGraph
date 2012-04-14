import Name.{T => Name}

// This is the intermediate Query language for graphs. It's too complex to be a
// human interface, but it's precise and easier to work with than a DSL (for
// now).

// The purpose of the NodeFilter class is to represent the more general (NodeT
// => Boolean), but to be a restricted subset that we can analyze for, e.g.,
// optimization. We don't, in general, want arbitrary (NodeT => Boolean) to be
// used in Queries.

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

case class NodeFieldEQ[-NodeT <: Node](field:String, value:String) extends NodeFilter[NodeT] {
  def apply(n:NodeT) = {
    n.getField(field) match {
      case Some(v) => (v == value)
      case None => false
    }
  }
}

case class NodeFieldNE[-NodeT <: Node](field:String, value:String) extends NodeFilter[NodeT] {
  def apply(n:NodeT) = {
    n.getField(field) match {
      case Some(v) => (v != value)
      case None => false
    }
  }
}

case class NodeFieldLE[-NodeT <: Node](field:String, value:String) extends NodeFilter[NodeT] {
  def apply(n:NodeT) = {
    n.getField(field) match {
      case Some(v) => (v <= value)
      case None => false
    }
  }
}

case class NodeFieldLT[-NodeT <: Node](field:String, value:String) extends NodeFilter[NodeT] {
  def apply(n:NodeT) = {
    n.getField(field) match {
      case Some(v) => (v < value)
      case None => false
    }
  }
}

case class NodeFieldGE[-NodeT <: Node](field:String, value:String) extends NodeFilter[NodeT] {
  def apply(n:NodeT) = {
    n.getField(field) match {
      case Some(v) => (v >= value)
      case None => false
    }
  }
}

case class NodeFieldGT[-NodeT <: Node](field:String, value:String) extends NodeFilter[NodeT] {
  def apply(n:NodeT) = {
    n.getField(field) match {
      case Some(v) => (v > value)
      case None => false
    }
  }
}

case class NodeFieldExists[-NodeT <: Node](field:String) extends NodeFilter[NodeT] {
  def apply(n:NodeT) = {
    n.getField(field) match {
      case Some(_) => true
      case None => false
    }
  }
}

case class NodeFieldNExists[-NodeT <: Node](field:String) extends NodeFilter[NodeT] {
  def apply(n:NodeT) = {
    n.getField(field) match {
      case Some(_) => false
      case None => true
    }
  }
}

case class NFAnd[-NodeT <: Node](nfs:NodeFilter[NodeT]*) extends NodeFilter[NodeT] {
  def apply(n:NodeT) = {
    nfs.forall(nf => nf(n))
  }
}

case class NFOr[-NodeT <: Node](nfs:NodeFilter[NodeT]*) extends NodeFilter[NodeT] {
  def apply(n:NodeT) = {
    nfs.exists(nf => nf(n))
  }
}

case class NFXor[-NodeT <: Node](nfs:NodeFilter[NodeT]*) extends NodeFilter[NodeT] {
  def apply(n:NodeT) = {
    nfs.count(nf => nf(n)) % 2 == 1
  }
}

case class NFNot[-NodeT <: Node](nf:NodeFilter[NodeT]) extends NodeFilter[NodeT] {
  def apply(n:NodeT) = {
    !nf(n)
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
