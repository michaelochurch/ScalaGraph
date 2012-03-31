import Name.{T => Name}

trait Node {
  type T
  type Delta
  
  val id : Name
  def +(delta:Delta):T
}

// Reference implementation for Node
class BaseNode private (val payload: Payload, val id:Name) extends Node {
  type T = BaseNode
  type Delta = PayloadDelta

  def this(payload:Payload) {
    this(payload, Name.make())
  }

  def +(delta:Delta):BaseNode = {
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
  def apply() = new BaseNode(Payload.empty)

  def apply(payload:Payload) = new BaseNode(payload)
}
