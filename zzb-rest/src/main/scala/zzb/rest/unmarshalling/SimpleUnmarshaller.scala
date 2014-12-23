package zzb.rest
package unmarshalling

import scala.util.control.NonFatal
import spray.util._

abstract class SimpleUnmarshaller[T] extends Unmarshaller[T] {
  //val canUnmarshalFrom: Seq[ContentTypeRange]

  def apply(entity: RestEntity): Deserialized[T] =
    entity match {
      case RestEntity.Empty ⇒ unmarshal(entity)
      case _                ⇒ unmarshal(entity)
    }

  protected def unmarshal(entity: RestEntity): Either[DeserializationError, T]

  /**
   * Helper method for turning exceptions occuring during evaluation of the named parameter into
   * [[zzb.rest.unmarshalling.MalformedContent]] instances.
   */
  protected def protect(f: ⇒ T): Either[DeserializationError, T] =
    try Right(f)
    catch {
      case NonFatal(ex) ⇒ Left(MalformedContent(ex.getMessage.nullAsEmpty, ex))
    }
}