object Payload {
  def empty = new Payload(None, Map())
}

case class Payload(typ:Option[String], data:Map[String,String])
