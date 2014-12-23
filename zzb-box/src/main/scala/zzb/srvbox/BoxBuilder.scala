package zzb.srvbox

import akka.actor.{Props, ActorSystem}
import zzb.rest._
import akka.routing.{RoundRobinPool, FromConfig}
import com.typesafe.config.{ConfigFactory, Config}
import com.typesafe.scalalogging.slf4j.Logging
import com.typesafe.config.ConfigException.NotResolved
import scala.collection.mutable
import zzb.config.EnvConfigLoader
import scala.concurrent.Future
import akka.util.Timeout
import scala.concurrent.duration._
import zzb.srvbox.SrvManageProtocol._
import akka.pattern._

/**
 * Created by Simon on 2014/5/28
 */
class BoxBuilder(system: ActorSystem, config: Config) extends Logging {

  val boxActor = system.actorOf(Props[BoxActor], name = "boxActor")


  private[srvbox] def startBoxedServices(services: Seq[ServiceOption]): Future[List[ServiceStatus]] = {

    import system.dispatcher
    def start(name: String)(implicit timeout: Timeout) = (boxActor ? RequestStart(name)).mapTo[ServiceStatus]

    //顺序执行服务的启动，前一个服务启动成功后再启动下一个
    def startServices(srvList: List[ServiceOption]): Future[List[ServiceStatus]] = {
      require(srvList.length > 0)
      srvList match {
        case s :: Nil =>
          logger.info(s"Start service： ${s.name} ")
          Future.sequence(List(start(s.name)(s.startTimeout seconds)))
        case s :: tail =>
          logger.info(s"Start service： ${s.name} ")
          for {
            status0 <- start(s.name)(s.startTimeout seconds)
            statusTail <- startServices(tail)
          } yield status0 :: statusTail
      }
    }

    for (s ← services) {
      logger.info(s"Register service : ${s.name}")
      boxActor ! Register(s.name, s.className, s.shareSystem == 1)
    }
    startServices(services.toList)
  }
}

object BoxBuilder extends EnvConfigLoader with Logging {
  private[srvbox] def getSelectService(args: List[String]) = {
    var mainConfigFile = "srvbox"

    if (args != Nil) {
      mainConfigFile = args.head.replace(".conf", "")
    }

    val selectService = if (args != Nil) args.tail else Nil
    var config = loadConfig(mainConfigFile).getOrElse(
      throw new NotResolved(s" Not found config file '$mainConfigFile.conf' ")
    )
    //配置文件检查
    val servicesOpts = mutable.Map[String,ServiceOption]()

    val boxConfig = config.getConfig("services")
    //装载缺省值配置,缺省值配置文件在资源包中
    val defaultServiceConfig = ConfigFactory.load("defaultBoxedService.conf").getConfig("service")

    import scala.collection.JavaConversions._
    val configedServices = boxConfig.getStringList("names").toList

    val exeService =
      if (selectService != Nil) {
        configedServices.filter(selectService.contains(_))
      } else configedServices

    for (serviceName ← exeService) {
      val serviceConfig = boxConfig.getConfig(serviceName).withFallback(defaultServiceConfig) //提取主配置中的服务配置信息

      val serviceOwnConfig = loadConfig(serviceName,withSysConfig = false).getOrElse(serviceConfig) //装载服务单独的配置信息，没有就用主配置中的

      val mergedServiceConfig = serviceOwnConfig.withFallback(serviceConfig) //合并两个配置为最终的服务配置，服务独立配置文件的信息优先采用

      config = config.withValue(s"services.$serviceName", mergedServiceConfig.root()) //将合并后的额配置置入主配置

      servicesOpts(serviceName) = ServiceOption(serviceName,
        serviceConfig.getInt("init-start"),
        serviceConfig.getInt("share-actorSystem"),
        serviceConfig.getString("serviceClass"),
        mergedServiceConfig,
        serviceConfig.getInt("start-timeout"),
        if (serviceConfig.hasPath("depend-on")) serviceConfig.getStringList("depend-on").toList else Nil
      )
    }
    //检查子服务类型能否找到
    servicesOpts.values.foreach(opt ⇒ Class.forName(opt.className))

    (config, servicesOpts.values.toList)
  }

}

case class ServiceOption(name: String, initStart: Int, shareSystem: Int, className: String, config: Config, startTimeout: Int, dependOn: List[String] = Nil)
