package zzb.srvbox

import akka.actor._
import spray.routing._
import scala.collection.JavaConverters._
import java.util

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-7-29
 * Time: 下午3:32
 * Copyright goodsl.org 2012~2020
 */
class RestInterface extends HttpServiceActor
with RestApi {

  def receive = runRoute(routes)
}

trait RestApi extends HttpService with ActorLogging {
  actor: Actor ⇒

  import zzb.srvbox.SrvManageProtocol.RestRequest

  val serviceManagerActor = context.actorOf(Props[ServiceManagerActor], "serviceManagerActor")

  //url设置
  private val pathMatchs: collection.mutable.Map[String, List[String]] = collection.mutable.Map.empty

  override def preStart {
    if (context.system.settings.config.hasPath("services.path")) {
      context.system.settings.config.getConfig("services.path").entrySet().asScala.foreach {
        entry =>
          pathMatchs.getOrElseUpdate(entry.getKey, entry.getValue.unwrapped.asInstanceOf[util.ArrayList[String]].asScala.toList)
      }
    }
  }

  def routes: Route =
    requestUri {
      uri =>
        //查找是否有路径配置
        val founds = for {
          pathMatch <- pathMatchs
          key = pathMatch._1
          urlMap <- pathMatch._2 if uri.path.toString().matches(urlMap)
        } yield (key, urlMap.dropRight(2).drop(1))
        val (serviceName, path) =
          if (founds.isEmpty)
            (uri.path.tail.head.toString, segmentStringToPathMatcher(uri.path.tail.head.toString))
          else
            (founds.head._1, separateOnSlashes(founds.head._2))
      //去除设置地路径或者服务名
        pathPrefix(path) {
          requestContext => {
            serviceManagerActor ! RestRequest(serviceName, requestContext)
          }
        }
    }
}

