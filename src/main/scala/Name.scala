import java.util.UUID

object Name {
  type T = UUID
  
  def make() = {
    UUID.randomUUID
  }
}
