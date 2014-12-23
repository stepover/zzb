package zzb.srvdemo

import com.typesafe.config.Config
import zzb.service.{RestSupport, ServiceFilter, BoxedService}

import akka.actor._
import zzb.srvbox.SrvManageProtocol.{FilterOk, FilterError, RestRequest}
import spray.routing.Route
import zzb.srvdemo.http_rest.UserHttpActor
import zzb.srvdemo.akka_rest.{DemoRestActor, AkkaRestHttpApi, UserRestActor}
import zzb.db.DBPools


/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-7-29
 * Time: 下午1:17
 * Copyright goodsl.org 2012~2020
 */
class DemoService(system: ActorSystem, config: Config)
  extends ServiceFilter(system, config) with RestSupport {

  override def init() {


    //这个只是让Demo服务每次运行都清空数据，从无开始，实际服务不要干这个事情哦。
    DBOperate.reCreateDb()
    super.init()

  }

  val httpActor = system.actorOf(Props[UserHttpActor], "h")
  val restActor = system.actorOf(Props[DemoRestActor], "demo")
  val akkaRestHttpApi = system.actorOf(Props[AkkaRestHttpApi], "r_api")

  override def fini() {
    DBPools.closeAllDB()
  }

  def name: String = "demo"

  def filter(req: RestRequest): Either[FilterError, FilterOk.type ] = Left(FilterError("tete"))

  def routes: Route =
    pathPrefix("r"){ctx =>
      akkaRestHttpApi ! ctx
    } ~ {
      ctx => httpActor ! ctx
    }


}

