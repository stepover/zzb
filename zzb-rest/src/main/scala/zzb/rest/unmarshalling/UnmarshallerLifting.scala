package zzb.rest
package unmarshalling

trait UnmarshallerLifting {

  implicit def fromRequestUnmarshaller[T](implicit um: FromMessageUnmarshaller[T]): FromRequestUnmarshaller[T] =
    new FromRequestUnmarshaller[T] {
      def apply(request: RestRequest): Deserialized[T] = um(request)
    }

  implicit def fromResponseUnmarshaller[T](implicit um: FromMessageUnmarshaller[T]): FromResponseUnmarshaller[T] =
    new FromResponseUnmarshaller[T] {
      def apply(response: RestResponse): Deserialized[T] = um(response)
    }

  implicit def fromMessageUnmarshaller[T](implicit um: Unmarshaller[T]): FromMessageUnmarshaller[T] =
    new FromMessageUnmarshaller[T] {
      def apply(msg: RestMessage): Deserialized[T] = um(msg.entity)
    }
}

object UnmarshallerLifting extends UnmarshallerLifting
