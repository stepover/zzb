package zzb.rest.unmarshalling
import scala.util.control.NonFatal

trait Deserializer[A, B] extends (A ⇒ Deserialized[B]) { self ⇒

  def withDefaultValue(defaultValue: B): Deserializer[A, B] =
    new Deserializer[A, B] {
      def apply(value: A) = self(value).left.flatMap {
        case ContentExpected ⇒ Right(defaultValue)
        case error           ⇒ Left(error)
      }
    }
}

object Deserializer extends DeserializerLowerPriorityImplicits
  with BasicUnmarshallers
  with UnmarshallerLifting
  with FromStringDeserializers {
  //with FormDataUnmarshallers {

  implicit def fromFunction2Converter[A, B](implicit f: A ⇒ B) =
    new Deserializer[A, B] {
      def apply(a: A) = {
        try Right(f(a))
        catch {
          case NonFatal(ex) ⇒ Left(MalformedContent(ex.toString, ex))
        }
      }
    }

  implicit def liftToTargetOption[A, B](implicit converter: Deserializer[A, B]) =
    new Deserializer[A, Option[B]] {
      def apply(value: A) = converter(value) match {
        case Right(a)              ⇒ Right(Some(a))
        case Left(ContentExpected) ⇒ Right(None)
        case Left(error)           ⇒ Left(error)
      }
    }
}

private[unmarshalling] abstract class DeserializerLowerPriorityImplicits {
  implicit def lift2SourceOption[A, B](converter: Deserializer[A, B]) = liftToSourceOption(converter)
  implicit def liftToSourceOption[A, B](implicit converter: Deserializer[A, B]) = {
    new Deserializer[Option[A], B] {
      def apply(value: Option[A]) = value match {
        case Some(a) ⇒ converter(a)
        case None    ⇒ Left(ContentExpected)
      }
    }
  }
}
