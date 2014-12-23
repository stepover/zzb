package zzb.rest

import scala.util.control.NonFatal
import spray.util.LoggingContext
import spray.http.{ RequestProcessingException, IllegalRequestException }

//import spray.http._
import StatusCodes._

trait ExceptionHandler extends ExceptionHandler.PF

object ExceptionHandler {
  type PF = PartialFunction[Throwable, Route]

  implicit def apply(pf: PF): ExceptionHandler =
    new ExceptionHandler {
      def isDefinedAt(error: Throwable) = pf.isDefinedAt(error)
      def apply(error: Throwable) = pf(error)
    }

  implicit def default(implicit log: LoggingContext): ExceptionHandler =
    apply {
      case e: IllegalRequestException ⇒ ctx ⇒
        log.warning("Illegal request {}\n\t{}\n\tCompleting with '{}' response",
          ctx.request, e.getMessage, e.status)
        ctx.complete(e.status)

        case e: RequestProcessingException ⇒ ctx ⇒
        log.warning("Request {} could not be handled normally\n\t{}\n\tCompleting with '{}' response",
          ctx.request, e.getMessage, e.status)
        ctx.complete(e.status)

        case NonFatal(e) ⇒ ctx ⇒
        log.error(e, "Error during processing of request {}", ctx.request)
        ctx.complete(InternalServerError, e.getMessage)
    }
}
