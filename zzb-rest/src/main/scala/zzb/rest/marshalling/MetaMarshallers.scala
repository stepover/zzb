package zzb.rest
package marshalling

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }
import akka.actor.ActorRefFactory
//import spray.http._

trait MetaMarshallers extends LowerPriorityImplicitMetaMarshallers {

  implicit def optionMarshaller[T](implicit m: Marshaller[T]) =
    Marshaller[Option[T]] { (value, ctx) ⇒
      value match {
        case Some(v) ⇒ m(v, ctx)
        case None    ⇒ ctx.marshalTo(RestEntity.Empty)
      }
    }

  implicit def eitherMarshaller[A, B](implicit ma: Marshaller[A], mb: Marshaller[B]) =
    Marshaller[Either[A, B]] { (value, ctx) ⇒
      value match {
        case Left(a)  ⇒ ma(a, ctx)
        case Right(b) ⇒ mb(b, ctx)
      }
    }

  implicit def futureMarshaller[T](implicit m: Marshaller[T], ec: ExecutionContext) =
    Marshaller[Future[T]] { (value, ctx) ⇒
      value.onComplete {
        case Success(v)     ⇒ m(v, ctx)
        case Failure(error) ⇒ ctx.handleError(error)
      }
    }

  implicit def tryMarshaller[T](implicit m: Marshaller[T]) =
    Marshaller[Try[T]] { (value, ctx) ⇒
      value match {
        case Success(v) ⇒ m(v, ctx)
        case Failure(t) ⇒ ctx.handleError(t)
      }
    }

  //  implicit def streamMarshaller[T](implicit marshaller: Marshaller[T], refFactory: ActorRefFactory) =
  //    Marshaller[Stream[T]] { (value, ctx) ⇒
  //      refFactory.actorOf(Props(new MetaMarshallers.ChunkingActor(marshaller, ctx))) ! value
  //    }
}
//
//object MetaMarshallers extends MetaMarshallers {
//  private case class SentOk(remaining: Stream[_])
//
//  class ChunkingActor[T](marshaller: Marshaller[T], ctx: MarshallingContext) extends Actor {
//    var connectionActor: ActorRef = _
//
//    def receive = {
//
//      case current #:: rest ⇒
//        val chunkingCtx = new DelegatingMarshallingContext(ctx) {
//          override def marshalTo(entity: RestEntity, headers: RestHeader*): Unit =
//            if (connectionActor == null) connectionActor = ctx.startChunkedMessage(entity, Some(SentOk(rest)), headers)
//            else connectionActor ! MessageChunk(entity.data).withAck(SentOk(rest))
//
//          override def handleError(error: Throwable): Unit = {
//            context.stop(self)
//            ctx.handleError(error)
//          }
//          override def startChunkedMessage(entity: RestEntity, sentAck: Option[Any], headers: Seq[RestHeader])(implicit sender: ActorRef) =
//            sys.error("Cannot marshal a stream of streams")
//        }
//        marshaller(current.asInstanceOf[T], chunkingCtx)
//
//      case SentOk(remaining) ⇒
//        if (remaining.isEmpty) {
//          connectionActor ! ChunkedMessageEnd
//          context.stop(self)
//        } else self ! remaining
//
//      case _: Tcp.ConnectionClosed ⇒
//        context.stop(self)
//    }
//  }
//}

trait LowerPriorityImplicitMetaMarshallers {
  implicit def mMarshaller[M[_], T](implicit mm: MarshallerM[M], mt: Marshaller[T]): Marshaller[M[T]] =
    mm.marshaller
}

trait MarshallerM[M[_]] {
  def marshaller[T: Marshaller]: Marshaller[M[T]]
}

//object MarshallerM {
//  implicit val optionMarshallerM: MarshallerM[Option] =
//    new MarshallerM[Option] { def marshaller[T: Marshaller] = implicitly[Marshaller[Option[T]]] }
//
//  implicit def futureMarshallerM(implicit ec: ExecutionContext): MarshallerM[Future] =
//    new MarshallerM[Future] { def marshaller[T: Marshaller] = implicitly[Marshaller[Future[T]]] }
//
//  implicit val tryMarshallerM: MarshallerM[Try] =
//    new MarshallerM[Try] { def marshaller[T: Marshaller] = implicitly[Marshaller[Try[T]]] }
//
//  implicit def streamMarshallerM(implicit refFactory: ActorRefFactory): MarshallerM[Stream] =
//    new MarshallerM[Stream] { def marshaller[T: Marshaller] = implicitly[Marshaller[Stream[T]]] }
//}