import java.util.UUID

object TempFile {
  def name():String = {
    "/tmp/%s".format(UUID.randomUUID)
  }
}
