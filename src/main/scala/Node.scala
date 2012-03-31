import Name.{T => Name}

trait Node {
  val id : Name
}

// Reference implementation for Node
class BaseNode(val payload: Payload) extends Node {
  val id = Name.make()

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
}
