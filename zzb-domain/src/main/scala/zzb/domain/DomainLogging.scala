package zzb.domain

import akka.actor.{ActorLogging, Actor}
import akka.event.{BusLogging, DiagnosticLoggingAdapter, LogSource}
import zzb.domain.directive.AuthorizedOperator
import zzb.util.MdcLoggingContext

/**
 * Created by Simon on 2014/6/29
 */
trait DomainLogging  extends ActorLogging{ this: Actor ⇒
  private var _log: DiagnosticLoggingAdapter = _

  protected var mdc = Map[String,Any]()

  def logName :String

  //def withHeritLog(herit:String): ( =>Unit) => Unit = logStatement =>  withHerit(herit)(logStatement)

  case class HeritLog(herit:String){
    def apply(logStatement: ⇒ Unit):Unit = {
      log.mdc(log.mdc ++ mdc ++ Map("herit" -> herit))
      logStatement
    }
    def apply(mdcMap :Map[String,Any])(logStatement: ⇒ Unit):Unit = {
      log.mdc(log.mdc ++ mdc ++ mdcMap ++ Map("herit" -> herit))
      logStatement
    }
    def apply(opt:AuthorizedOperator)(logStatement: ⇒ Unit):Unit = {
      apply(Map("opt" -> opt))(logStatement)
    }
  }

  final def withHerit(herit :String,otherMdc:Map[String,Any]=Map())(logStatement: ⇒ Unit){
    withMdc(otherMdc ++ Map("herit" -> herit))(logStatement)
  }

  @inline
  final def withMdc(mdcMap :Map[String,Any])(logStatement: ⇒ Unit){
    log.mdc( this.mdc ++ mdcMap)
    logStatement
  }

  protected def logFactory : DiagnosticLoggingAdapter = {

    val (str, clazz) = LogSource( logName )
    new BusLogging(context.system.eventStream, str, clazz)  with DiagnosticLoggingAdapter
  }

  override def log: DiagnosticLoggingAdapter = {
    // only used in Actor, i.e. thread safe
    if (_log eq null)
      _log = logFactory
    _log
  }

  implicit lazy val mdcLogCtx = MdcLoggingContext.fromAdapter(log)

}
