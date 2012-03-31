case class PayloadDelta(data:Map[String,Option[String]])

object Payload {
  case class Delta(data:Map[String, Option[String]])

  def empty = new Payload(None, Map())
}

case class Payload(typ:Option[String], data:Map[String,String]) {
  def +(delta:PayloadDelta) = {
    val newData = 
      Utils.mergeMaps(this.data, delta.data)(
	(_:String, v1Opt:Option[String], v2Opt:Option[Option[String]]) =>
	  v2Opt match {
	    case None => v1Opt
	    case Some(v2) => v2
	  })
    new Payload(this.typ, newData)
  }
}
