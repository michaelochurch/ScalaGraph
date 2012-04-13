import Name.{T => Name}

trait Node extends Serializable {
  val id : Name
  def getType() : Option[String]
  def getField(fieldName:String) : Option[String]
}

// Reference implementation for Node
class BaseNode private (val payload: Payload, val id:Name) extends Node {
  def this(payload:Payload) {
    this(payload, Name.make())
  }

  def getType() = payload.typ

  def getField(fieldName:String) = payload.data.get(fieldName)

  private def tuple() = {
    (id, payload)
  }

  override def hashCode() = {
    this.tuple.hashCode()
  }

  override def equals(that:Any) = {
    that match {
      case (node:BaseNode) => this.tuple == node.tuple
      case _ => false
    }
  }

  override def toString() = {
    val typeString = this.payload.typ getOrElse "(untyped)"
    "<Node %s: %s, %s>".format(this.id, typeString, this.payload.data)
  }
}

object BaseNode {
  def apply() = new BaseNode(Payload.empty)

  def apply(payload:Payload) = new BaseNode(payload)

  def apply(typ:String, fields:Map[String, String]) =
    new BaseNode(Payload(Option(typ), fields))

  def apply(id:Name, typ:String, fields:Map[String, String]) =
    new BaseNode(Payload(Option(typ), fields), id)
}
