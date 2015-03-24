package zzb.domain.http

import spray.http.StatusCodes
import zzb.datatype._
import zzb.storage.TStorable
import spray.routing.{Directives, Route}
import zzb.rest.http2akka.{HttpApi, Http2AkkaDirectives}
import zzb.domain._
import spray.httpx.SprayJsonSupport._

/**
 * Created by Simon on 2014/4/23
 */
trait DomainHttpApi[K, KT <: DataType[K], T <: TStorable[K, KT]]
  extends DomainHttpDirectives with HttpApi {

  val domainType: T

  val actionBuilder: ActionBuilder = null

  def optionRoute: Route = {
    val docOptionRoute: DomainHttpRoute = {
      path =>
        (get | options) {
          complete(path.targetType.typeInfo)
        }
    }
    handDoc(domainType, docOptionRoute)
  }

  def domainApi: Route =
    pathEndOrSingleSlash {
      delete {
        akkaCompleteAs[String]
      } ~
        docRoute
    } ~
      pathPrefix("ver" / IntNumber) {
        verNum =>
          docRoute
      } ~
      pathPrefix("tag" / Segment) {
        tag =>
          docRoute
      } ~
      pathPrefix(Segment) {
        case "latest" => docRoute
        case "alter" => alterManagerRoute
        case "versions" => versionsRoute
        case "action" => actionRoute
        case _ => reject
      }

  def actionRoute: Route = if (actionBuilder == null) reject
  else
    innerActionRoute(actionBuilder.actions.toMap)

  private def innerActionRoute(actionDefs: Map[String, ActionDefine]): Route = {
    val routes = for ((actName, define) <- actionDefs) yield {
      define.entityType match {
        case None =>
          path(actName) {
            akkaCompleteAs[ActionResult]
          }
        case Some(dt) =>
          path(actName) {
            entity(unpack(dt)) {
              pk =>
                akkaWithEntityCompleteAs[ActionResult](pk)
            }
          }
      }
    }
    routes.reduce(_ ~ _) ~ path(Segment) {
      actName => complete(StatusCodes.NotFound, s"action '$actName' not exist!")
    }
  }

  def versionsRoute: Route = domainCompleteAs(VersionInfos)


  def docRoute: Route = {
    val docNodeRoute: DomainHttpRoute = {
      path =>
        get {
          domainCompleteAs(path.targetType)
        }
    }
    handDoc(domainType, docNodeRoute)
  }

  def alterManagerRoute: Route =
    pathPrefix(IntNumber) {
      //操作某一个变更会话
      alterSeq =>
        handDoc(domainType, domainAlterRoute)
    } ~ handDoc(domainType, directAlterRoute)

  def directAlterRoute(path: StructPath): Route =
    path.inStructPath.through match {
      case listType: TList[_] => put {
        //针对某一个路径直接修改数据
        entity(unpack(path.targetType)) {
          ent =>
            akkaWithEntityCompleteAs[ActionResult](ent)
        }
      } ~ post {
        hasEntity {
          case true => //向列表中新增
            val listPath = ListPath(path.inStructPath, -1)
            entity(unpack(listPath.targetType)) {
              ent =>
                akkaWithEntityCompleteAs[ActionResult](ent)
            }
          case false =>
            akkaCompleteAs[ActionResult]
        }
      } ~ delete {
        akkaCompleteAs[ActionResult]
      }
      case _ => put {
        //针对某一个路径直接修改数据
        entity(unpack(path.targetType)) {
          ent =>
            akkaWithEntityCompleteAs[ActionResult](ent)
        }
      } ~ (post | delete) {
        //针对某一个路径请求创建变更会话
        akkaCompleteAs[ActionResult]
      }
    }

  def domainAlterRoute(path: StructPath): Route =
    if (path.inStructPath.size == 1) {
      pathGet(path) ~ pathPut(path) ~ alterSubmitOrAbandon
    } else {
      path.targetType match {
        case listType: TList[_] => pathGet(path) ~ pathPut(path) ~ listPost(path) ~ pathDelete(path)
        case _ => pathGet(path) ~ pathPut(path) ~ pathDelete(path)
      }
    }

  private def pathGet(path: StructPath): Route = get {
    domainCompleteAs(path.targetType)
  }

  private def pathAlter(path: StructPath): Route =
    entity(unpack(path.targetType)) {
      ent =>
        akkaWithEntityCompleteAs[ActionResult](ent)
    }

  private def pathPut(path: StructPath): Route = put {
    pathAlter(path)
  }

  private def pathDelete(path: StructPath): Route = delete {
    akkaCompleteAs[ActionResult]
  }

  private def listPost(path: StructPath): Route = post {
    pathAlter(ListPath(path.inStructPath, -1))
  }

  private def alterSubmitOrAbandon: Route = (post | delete) {
    akkaCompleteAs[ActionResult]
  }
}
