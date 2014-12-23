package zzb.rest.unmarshalling

sealed trait DeserializationError

case object ContentExpected extends DeserializationError

case class MalformedContent(errorMessage: String, cause: Option[Throwable] = None) extends DeserializationError

object MalformedContent {
  def apply(errorMessage: String, cause: Throwable): MalformedContent = new MalformedContent(errorMessage, Some(cause))
}
