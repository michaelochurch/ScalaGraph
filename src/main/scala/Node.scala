import Name.{T => Name}

trait Node[T, Delta] {
  val id : Name
  def +(delta:Delta):T
}

// Reference implementation for Node
class BaseNode private (val payload: Payload, val id:Name) extends Node[BaseNode, PayloadDelta] {
  def this(payload:Payload) {
    this(payload, Name.make())
  }

  def +(delta:PayloadDelta):BaseNode = {
    new BaseNode(payload + delta, id)
  }

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
  type Delta = PayloadDelta

  def apply() = new BaseNode(Payload.empty)

  def apply(payload:Payload) = new BaseNode(payload)
}
