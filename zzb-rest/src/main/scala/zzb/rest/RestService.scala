package zzb.rest

import akka.actor._
import StatusCodes._
import spray.http.Uri
import spray.util.LoggingContext
import scala.util.control.NonFatal

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-3-1
 * Time: 上午10:33
 * Copyright baoxian.com 2012~2020
 */
trait RestServiceBase extends Directives {

  /**
   * Supplies the actor behavior for executing the given route.
   */
  def runRoute(route: Route)(implicit eh: ExceptionHandler, rh: RejectionHandler, ac: ActorContext,
                             rs: RestSettings, log: LoggingContext): Actor.Receive = {
    val sealedExceptionHandler = eh orElse ExceptionHandler.default
    val sealedRoute = sealRoute(route)(sealedExceptionHandler, rh)
    def runSealedRoute(ctx: RestReqContext): Unit =
      try sealedRoute(ctx)
      catch {
        case NonFatal(e) ⇒
          val errorRoute = sealedExceptionHandler(e)
          errorRoute(ctx)
      }

    {
      case (request: RestRequest, unmatchedPath: String) ⇒
        val ctx = RestReqContext(request, ac.sender(), Uri.Path(unmatchedPath)).withDefaultSender(ac.self)
        runSealedRoute(ctx)
      case request: RestRequest ⇒
        val ctx = RestReqContext(request, ac.sender(), request.uri.path).withDefaultSender(ac.self)
        runSealedRoute(ctx)

      case ctx: RestReqContext ⇒ runSealedRoute(ctx)

      //      case Tcp.Connected(_, _) ⇒
      //        // by default we register ourselves as the handler for a new connection
      //        ac.sender ! Tcp.Register(ac.self)
      //
      //      case Timedout(request: RestRequest) ⇒ runRoute(timeoutRoute)(eh, rh, ac, rs, log)(request)
    }
  }

  /**
   * "Seals" a route by wrapping it with exception handling and rejection conversion.
   */
  def sealRoute(route: Route)(implicit eh: ExceptionHandler, rh: RejectionHandler): Route =
    (handleExceptions(eh) & handleRejections(sealRejectionHandler(rh)))(route)

  def sealRejectionHandler(rh: RejectionHandler): RejectionHandler =
    rh orElse RejectionHandler.Default orElse handleUnhandledRejections

  def handleUnhandledRejections: RejectionHandler.PF = {
    case x :: _ ⇒ sys.error("Unhandled rejection: " + x)
  }

  //# timeout-route
  def timeoutRoute: Route = complete(
    InternalServerError,
    "The server was not able to produce a timely response to your request.")
  //#
}

object RestService extends RestServiceBase

trait RestService extends RestServiceBase {
  /**
   * An ActorRefFactory needs to be supplied by the class mixing us in
   * (mostly either the service actor or the service test)
   */
  implicit def actorRefFactory: ActorRefFactory
}

abstract class RestServiceActor extends Actor with RestService {
  def actorRefFactory = context
}