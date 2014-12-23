package zzb.rest

import scala.concurrent.duration._
import scala.util.control.NonFatal
import akka.util.Timeout
import akka.actor.ActorRefFactory
import spray.util.identityFunc

package object marshalling {

  def marshal[T](value: T, ctx: CollectingMarshallingContext = new CollectingMarshallingContext)(implicit marshaller: Marshaller[T], actorRefFactory: ActorRefFactory = null,
                                                                                                 timeout: Timeout = 1.second): Either[Throwable, RestEntity] =
    marshalToEntityAndHeaders(value, ctx).right.map(_._1)

  def marshalToEntityAndHeaders[T](value: T, ctx: CollectingMarshallingContext = new CollectingMarshallingContext)(implicit marshaller: Marshaller[T], actorRefFactory: ActorRefFactory = null,
                                                                                                                   timeout: Timeout = 1.second): Either[Throwable, (RestEntity, Seq[RestHeader])] = {
    marshalCollecting(value, ctx)
    ctx.entityAndHeaders match {
      case Some(value) ⇒ Right(value)
      case None ⇒
        Left(ctx.error.getOrElse(new RuntimeException("Marshaller for %s did not produce result" format value)))
    }
  }

  def marshalCollecting[T](value: T, ctx: CollectingMarshallingContext)(implicit marshaller: Marshaller[T], actorRefFactory: ActorRefFactory = null,
                                                                        timeout: Timeout = 1.second): Unit =
    try {
      marshaller(value, ctx)
      ctx.awaitResults
    } catch {
      case NonFatal(e) ⇒ ctx.handleError(e)
    }

  def marshalUnsafe[T: Marshaller](value: T): RestEntity = marshal(value).fold(throw _, identityFunc)
}
