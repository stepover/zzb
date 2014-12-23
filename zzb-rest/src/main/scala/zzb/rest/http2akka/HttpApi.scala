package zzb.rest.http2akka

import akka.event.{ BusLogging, LogSource, DiagnosticLoggingAdapter }
import spray.routing._
import akka.util.Timeout
import akka.actor.ActorSystem
import zzb.util.MdcLoggingContext
import scala.reflect.macros.Context
import language.experimental.macros

/**
 * Created by Simon on 2014/6/9
 */
trait HttpApi extends Directives with Http2AkkaDirectives {

  implicit val timeout: Timeout
  implicit val system: ActorSystem

  def logName: String

  implicit lazy val log = logFactory(logName)

  implicit lazy val mdcLogCtx = MdcLoggingContext.fromAdapter(log)

  protected def logFactory(logName: String): DiagnosticLoggingAdapter = {

    val (str, clazz) = LogSource(logName)
    new BusLogging(system.eventStream, str, clazz) with DiagnosticLoggingAdapter
  }

  def api: Route
}

object HttpApi {
  def apply[T <: HttpApi](implicit sys: ActorSystem, time: Timeout): Route = macro api_generator[T]

  def api_generator[T <: HttpApi](c: Context)(sys: c.Expr[ActorSystem], time: c.Expr[Timeout])(implicit T: c.WeakTypeTag[T]) = {
    import c.universe._

    c.Expr[T](
      q""" new $T {
          implicit val system = ${sys.tree}
          implicit val timeout = ${time.tree}
         }.api   """)
  }
}

