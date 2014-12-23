package zzb.domain.http

import spray.http.HttpEntity.Empty
import spray.httpx.marshalling.Marshaller
import spray.routing._
import zzb.datatype._
import akka.util.Timeout
import akka.actor.ActorSystem
import zzb.rest.http2akka.Http2AkkaDirectives
import spray.json._
import spray.httpx.SprayJsonSupport
import SprayJsonSupport._
import zzb.domain._
import spray.http._
import spray.httpx._
import spray.httpx.unmarshalling._
import spray.http.HttpRequest
import zzb.util.MdcLoggingContext


/**
 * Created by Simon on 2014/4/23
 */
trait DomainHttpDirectives extends Http2AkkaDirectives{

  import directives.PathDirectives._
  import RouteConcatenation._
  import directives.RouteDirectives._
  import directives.BasicDirectives._

  type DomainHttpRoute = StructPath => Route

  def hasEntity: Directive1[Boolean] = extract(_.request.entity).flatMap {
    case Empty ⇒
      provide(false)
    case _ =>
      provide(true)
  }

  def handDoc[T <: TStruct](dt: T, domainRoute: DomainHttpRoute): Route = {
    val rootPath = List(() => dt)
    pathEndOrSingleSlash {
      domainRoute(NodePath(rootPath))
    } ~ handSubFields(rootPath, domainRoute)
  }

  private def handSubFields(sections: NestedStructFields, domainRoute: DomainHttpRoute): Route = {
    val dt = sections.head()
    dt match {
      case st: TStruct =>
        val fieldInfos = st.fieldsInfo
        val routes = for (fieldInfo <- fieldInfos) yield {
          handField(fieldInfo.name, fieldInfo.field :: sections, domainRoute)
        }
        routes.reduce(_ ~ _)
      case lt: TList[_] =>
        path(IntNumber) {
          idx =>
            domainRoute(ListPath(sections.reverse, idx))
        } ~ path("@size") {
          domainRoute(ListPath(sections.reverse, StructPath.ListSizeIdx))
        }
      case mt: TStrKeyMap[_] =>
        path(Segment) {
          key =>
            domainRoute(MapPath(sections.reverse, key))
        }
      case _ => reject
    }
  }

  private def handField(name: String, sections: NestedStructFields, domainRoute: DomainHttpRoute): Route = {
    pathPrefix(name) {
      pathEndOrSingleSlash {
        domainRoute(NodePath(sections.reverse))
      } ~ handSubFields(sections, domainRoute)
    }
  }


}

object DomainHttpDirectives extends DomainHttpDirectives {


}


