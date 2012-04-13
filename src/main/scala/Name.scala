import java.util.UUID

object Name {
  type T = UUID
  
  def make() = {
    UUID.randomUUID
  }

  def forString(s:String) = {
    if (TestEase.testing) {
      UUID.nameUUIDFromBytes(s.getBytes("utf-8"))
    } else {
      sys.error("Name.forString can only be called in testing.")
    }
  }
}
