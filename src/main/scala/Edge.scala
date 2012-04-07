import Name.{T => Name}

trait Edge {
  val id : Name
  val source : Name
  val dest : Name

  def getType() : Option[String]
  def getField(fieldName:String) : Option[String]
}

class BaseEdge private (val source:Name, val dest:Name, val payload: Payload, val id:Name) extends Edge {
  def this(source:Name, dest:Name, payload:Payload) = {
    this(source, dest, payload, Name.make())
  }

  def getType() = payload.typ
  def getField(fieldName:String) = payload.data.get(fieldName)

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
    new BaseEdge(source, dest, Payload.empty)
  }

  def apply(source:Name, dest:Name, payload:Payload) = {
    new BaseEdge(source, dest, payload)
  }

  def apply(source:Name, dest:Name, typ:String, data:Map[String, String] = Map()) = {
    new BaseEdge(source, dest, Payload(Option(typ), data))
  }
}
