package zzb.srvbox

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-7-29
 * Time: 下午3:36
 * Copyright goodsl.org 2012~2020
 */
object SrvManageProtocol {
  import spray.json._
  import spray.routing.RequestContext
  import DefaultJsonProtocol._

  case class Register(name: String, serviceClass: String, sharedActorSystem: Boolean)

  case class ServiceStatus(name: String, running: Boolean)

  case class Services(services: List[ServiceStatus])

  case class RequestStart(name: String)

  case class RequestStop(name: String)

  case object ServiceNotExist

  case object Halt

  case object RequestList

  case class RestRequest(name: String, ctx: RequestContext)

  /*过滤器注册,注销*/
  case class FilterReg(name:String,serverNames:List[String])
  case class FilterUnReg(name:String)
  case object FilterList
  /*过滤结果返回*/
  case object FilterOk
  case class FilterError(msg:String)

  object FilterError extends DefaultJsonProtocol {
    implicit val format = jsonFormat1(FilterError.apply)
  }

  object RequestStart extends DefaultJsonProtocol {
    implicit val format = jsonFormat1(RequestStart.apply)
  }
  object RequestStop extends DefaultJsonProtocol {
    implicit val format = jsonFormat1(RequestStop.apply)
  }
  object ServiceStatus extends DefaultJsonProtocol {
    implicit val format = jsonFormat2(ServiceStatus.apply)
  }

  case class ServiceReady(serviceName:String)
}
