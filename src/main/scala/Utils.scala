object Utils {
  // We need the curried function signature or we'll have to explicitly type f. 
  def mergeMaps[K, V1, V2, V](m1:Map[K, V1], m2:Map[K, V2])(f:((K, Option[V1], Option[V2]) => Option[V])) = {
    val allKeys = m1.keySet union m2.keySet
    allKeys.flatMap(k => f(k, m1.get(k), m2.get(k)).map(v => (k, v))).toMap
  }

  abstract sealed class MayFail[+T]
  sealed case class Failure(x:Throwable) extends MayFail
  sealed case class Success[+T](x:T) extends MayFail[T]

  def mayFail[T](body : => T) {
    try {
      Success(body)
    } catch {
      case (t:Throwable) => Failure(t)
    }
  } 
}
