// TestEase.scala
// For ease of testing.

// This is inspired by a macro-driven test framework I wrote in
// Clojure. I'm not married to it, and it may not work for
// Scala. Mostly experimental.

object TestEase {
  abstract sealed class MayFail[+T]
  sealed case class Failure(x:Throwable) extends MayFail
  sealed case class Success[+T](x:T) extends MayFail[T]

  def mayFail[T](body : => T):MayFail[T] = {
    try {
      Success(body)
    } catch {
      case (t:Throwable) => Failure(t)
    }
  } 

  def valEq[T](valIs:T)(body: => T) = {
    assert(body == valIs)
  }
  
  def valFn[T](valFn:(T => Boolean))(body: => T) = {
    assert(valFn(body))
  }
  
  def exnFn[T](exnFn:(Throwable => Boolean))(body: => T) = {
    mayFail(body) match {
      case Success(_) => assert(false)
      case Failure(exn) => assert(exnFn(exn))
    }
  }

  // Reflection hack. Will fix when 2.10 is stable. 
  def exnClass[T](exnClass:String)(body: => T) = {
    exnFn(t => t.getClass.getName.contains(exnClass))(body)
  }
}
