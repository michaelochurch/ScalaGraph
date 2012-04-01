import Name.{T => Name}

trait Edge {
  val id : Name
  val source : Name
  val dest : Name
}

class BaseEdge private (val payload: Payload, val source:Name, val dest:Name, val id:Name) extends Edge {
  def this(payload:Payload, source:Name, dest:Name) = {
    this(payload, source, dest, Name.make())
  }

  private def tuple() = {
    (id, source, dest, payload)
  }

  override def hashCode() = {
    this.tuple.hashCode()
  }

  override def equals(that:Any) = {
    that match {
      case (edge:BaseEdge) => this.tuple == edge.tuple
      case _ => false
    }
  }

  override def toString() = {
    val typeString = this.payload.typ getOrElse "(untyped)"
    "<Edge %s (%s -> %s): %s, %s>".format(this.id, this.source, this.dest, 
					  typeString, this.payload.data)
  }
}

object BaseEdge {
  def apply(source:Name, dest:Name) = {
    new BaseEdge(Payload.empty, source, dest)
  }

  def apply(p:Payload, source:Name, dest:Name) = {
    new BaseEdge(p, source, dest)
  }
}
