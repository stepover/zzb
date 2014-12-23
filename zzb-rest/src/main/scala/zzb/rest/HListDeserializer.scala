package zzb.rest

import shapeless._
import zzb.rest.unmarshalling._
import spray.util.pimpString_

// I don't think we can get around spelling out 22 different cases without giving up on our short
// directive.as(CaseClass) notation (since we have to provide a dedicated magnet for the proper
// apply function type (e.g. (A, B, C) => CC), but we might be able to simplify the implementation
// of the 22 cases by converting into an HList that can then be mapped/folded over

trait HListDeserializer[L <: HList, T] extends Deserializer[L, T]

object HListDeserializer extends HListDeserializerInstances {

  protected type DS[A, AA] = Deserializer[A, AA] // alias for brevity

  implicit def fromDeserializer[L <: HList, T](ds: DS[L, T]) = new HListDeserializer[L, T] {
    def apply(list: L) = ds(list)
  }

  /////////////////////////////// CASE CLASS DESERIALIZATION ////////////////////////////////

  // we use a special exception to bubble up errors rather than relying on long "right.flatMap" cascades in order to
  // save lines of code as well as excessive closure class creation in the many "hld" methods below
  private class BubbleLeftException(val left: Left[Any, Any]) extends RuntimeException

  protected def create[L <: HList, T](deserialize: L ⇒ T): HListDeserializer[L, T] =
    new HListDeserializer[L, T] {
      def apply(list: L): Deserialized[T] =
        try Right(deserialize(list))
        catch {
          case e: BubbleLeftException      ⇒ e.left.asInstanceOf[Left[DeserializationError, T]]
          case e: IllegalArgumentException ⇒ Left(MalformedContent(e.getMessage.nullAsEmpty, e))
        }
    }

  protected def get[T](either: Either[DeserializationError, T]): T = either match {
    case Right(x)         ⇒ x
    case left: Left[_, _] ⇒ throw new BubbleLeftException(left)
  }
}