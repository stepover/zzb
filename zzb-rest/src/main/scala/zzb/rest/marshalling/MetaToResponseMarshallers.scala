package zzb.rest
package marshalling

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }
//import spray.http._

trait MetaToResponseMarshallers {

  implicit def optionMarshaller[T](implicit m: ToResponseMarshaller[T]) =
    ToResponseMarshaller[Option[T]] { (value, ctx) ⇒
      value match {
        case Some(v) ⇒ m(v, ctx)
        case None    ⇒ ctx.marshalTo(RestResponse(StatusCodes.NotFound))
      }
    }

  implicit def eitherMarshaller[A, B](implicit ma: ToResponseMarshaller[A], mb: ToResponseMarshaller[B]) =
    ToResponseMarshaller[Either[A, B]] { (value, ctx) ⇒
      value match {
        case Left(a)  ⇒ ma(a, ctx)
        case Right(b) ⇒ mb(b, ctx)
      }
    }

  implicit def futureMarshaller[T](implicit m: ToResponseMarshaller[T], ec: ExecutionContext) =
    ToResponseMarshaller[Future[T]] { (value, ctx) ⇒
      value.onComplete {
        case Success(v)     ⇒ m(v, ctx)
        case Failure(error) ⇒ ctx.handleError(error)
      }
    }

  implicit def tryMarshaller[T](implicit m: ToResponseMarshaller[T]) =
    ToResponseMarshaller[Try[T]] { (value, ctx) ⇒
      value match {
        case Success(v) ⇒ m(v, ctx)
        case Failure(t) ⇒ ctx.handleError(t)
      }
    }
}

object MetaToResponseMarshallers extends MetaToResponseMarshallers