package zzb.rest
package marshalling

import scala.util.control.NonFatal
import akka.actor.ActorRef
//import spray.http._

//# source-quote
trait Marshaller[-T] {
  def apply(value: T, ctx: MarshallingContext)
}

//#

object Marshaller extends BasicMarshallers
  with MetaMarshallers {

  def apply[T](f: (T, MarshallingContext) ⇒ Unit): Marshaller[T] =
    new Marshaller[T] {
      def apply(value: T, ctx: MarshallingContext): Unit =
        try f(value, ctx)
        catch {
          case NonFatal(e) ⇒ ctx.handleError(e)
        }
    }

  def of[T]()(f: (T, MarshallingContext) ⇒ Unit): Marshaller[T] =
    new Marshaller[T] {
      def apply(value: T, ctx: MarshallingContext): Unit =
        try {
          f(value, ctx)
        } catch {
          case NonFatal(e) ⇒ ctx.handleError(e)
        }
    }

  def delegate[A, B]() = new MarshallerDelegation[A, B]()

  class MarshallerDelegation[A, B]() {
    def apply(f: (A) ⇒ B)(implicit mb: Marshaller[B]): Marshaller[A] =
      Marshaller.of[A]() {
        (value, ctx) ⇒
          mb(f(value), ctx)
      }
  }

}

trait ToResponseMarshaller[-T] {
  def apply(value: T, ctx: ToResponseMarshallingContext)

  def compose[U](f: U ⇒ T): ToResponseMarshaller[U] = ToResponseMarshaller((value, ctx) ⇒ apply(f(value), ctx))
}

object ToResponseMarshaller extends BasicToResponseMarshallers
  with MetaToResponseMarshallers
  with LowPriorityToResponseMarshallerImplicits {
  def fromMarshaller[T](status: StatusCode = StatusCodes.OK, headers: Seq[RestHeader] = Nil)(implicit m: Marshaller[T]): ToResponseMarshaller[T] =
    fromStatusCodeAndHeadersAndT.compose(t ⇒ (status, headers, t))

  def apply[T](f: (T, ToResponseMarshallingContext) ⇒ Unit): ToResponseMarshaller[T] =
    new ToResponseMarshaller[T] {
      def apply(value: T, ctx: ToResponseMarshallingContext): Unit =
        try f(value, ctx)
        catch {
          case NonFatal(e) ⇒ ctx.handleError(e)
        }
    }

  def of[T]()(f: (T, ToResponseMarshallingContext) ⇒ Unit): ToResponseMarshaller[T] =
    new ToResponseMarshaller[T] {
      def apply(value: T, ctx: ToResponseMarshallingContext): Unit =
        try {
          f(value, ctx)
        } catch {
          case NonFatal(e) ⇒ ctx.handleError(e)
        }
    }

  //  def oneOf[T]()(marshallers: ToResponseMarshaller[T]*): ToResponseMarshaller[T] =
  //    ToResponseMarshaller.of[T]() {
  //      (t,  ctx) ⇒
  //        def tryNext(marshallers: List[ToResponseMarshaller[T]], previouslySupported: Set[ContentType]): Unit = marshallers match {
  //          case head :: tail ⇒
  //            head(t, new ToResponseMarshallingContext {
  //              //def tryAccept(contentTypes: Seq[ContentType]): Option[ContentType] = ctx.tryAccept(contentTypes)
  //
  //              def rejectMarshalling(supported: Seq[ContentType]): Unit = tryNext(tail, previouslySupported ++ supported)
  //
  //              def marshalTo(response: HttpResponse): Unit = ctx.marshalTo(response)
  //
  //              def handleError(error: Throwable): Unit = ctx.handleError(error)
  //
  //              def startChunkedMessage(response: HttpResponse, ack: Option[Any])(implicit sender: ActorRef): ActorRef =
  //                ctx.startChunkedMessage(response, ack)(sender)
  //            })
  //          case Nil ⇒ ctx.rejectMarshalling(previouslySupported.toSeq)
  //        }
  //        tryNext(marshallers.toList, Set.empty)
  //    }

  def delegate[A, B]() = new MarshallerDelegation[A, B]()

  class MarshallerDelegation[A, B]() {
    //def apply(f: A ⇒ B)(implicit mb: ToResponseMarshaller[B]): ToResponseMarshaller[A] = apply((a, ct) ⇒ f(a))

    def apply(f: A ⇒ B)(implicit mb: ToResponseMarshaller[B]): ToResponseMarshaller[A] =
      ToResponseMarshaller.of[A]() {
        (value, ctx) ⇒
          mb(f(value), ctx)
      }
  }

}

sealed trait LowPriorityToResponseMarshallerImplicits {
  implicit def liftMarshallerConversion[T](m: Marshaller[T]): ToResponseMarshaller[T] = liftMarshaller(m)

  implicit def liftMarshaller[T](implicit m: Marshaller[T]): ToResponseMarshaller[T] =
    ToResponseMarshaller.fromMarshaller()
}

/** Something that can later be marshalled into a response */
trait ToResponseMarshallable {
  def marshal(ctx: ToResponseMarshallingContext): Unit
}

object ToResponseMarshallable {
  implicit def isMarshallable[T](value: T)(implicit marshaller: ToResponseMarshaller[T]): ToResponseMarshallable =
    new ToResponseMarshallable {
      def marshal(ctx: ToResponseMarshallingContext): Unit = marshaller(value, ctx)
    }

  implicit def marshallableIsMarshallable: ToResponseMarshaller[ToResponseMarshallable] =
    new ToResponseMarshaller[ToResponseMarshallable] {
      def apply(value: ToResponseMarshallable, ctx: ToResponseMarshallingContext): Unit = value.marshal(ctx)
    }
}
