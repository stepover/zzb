package zzb.domain.http

import zzb.datatype.meta.EnumTypeInfo
import zzb.rest.http2akka.HttpApi
import spray.routing.Route
import zzb.datatype.{TEnum, EnumRegistry, StructRegistry}
import spray.json.DefaultJsonProtocol
import DefaultJsonProtocol._
import spray.httpx.SprayJsonSupport._
import spray.httpx.TwirlSupport

/**
 * Created by Simon on 2014/6/12
 */
trait TypeViewApi extends HttpApi with TwirlSupport with DomainHttpDirectives {
  val rootPath: String

  def domainRoute: DomainHttpRoute = structPath =>
    parameter("json".as[Boolean] ?) { json: Option[Boolean] =>
      if (json.isDefined && json.get) {
        complete(structPath.targetType.typeInfo)
      } else
        structPath.targetType match {
          case eumnType: TEnum =>
            complete(html.typzenum.render(rootPath, structPath, structPath.targetType.typeInfo.asInstanceOf[EnumTypeInfo]))
          case _ =>
            complete(html.typz.render(rootPath, structPath, structPath.targetType.typeInfo))
        }
    }

  override def api: Route = getFromResourceDirectory("theme") ~
    pathEndOrSingleSlash {
      complete(html.typeList.render(rootPath))
    } ~ pathPrefix(Segment) { typeName =>
    (StructRegistry.get(typeName), EnumRegistry.get(typeName)) match {
      case (Some(theType), _) =>
        handDoc(theType, domainRoute)
      case (_, Some(theType)) =>
        parameter("json".as[Boolean] ?) { json: Option[Boolean] =>
          if (json.isDefined && json.get) {
            complete(theType.typeInfo)
          } else
            complete(html.typzenum.render(rootPath, () => theType, theType.typeInfo))
        }
      case _ => reject
    }
  }
}

