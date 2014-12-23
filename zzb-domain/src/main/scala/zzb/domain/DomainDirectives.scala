package zzb.domain

import shapeless.HNil
import zzb.datatype._
import zzb.rest.unmarshalling._
import zzb.rest._

/**
 * Created by Simon on 2014/4/10
 */
trait DomainDirectives {

  import zzb.rest.directives.PathDirectives._
  import RouteConcatenation._
  import zzb.rest.directives.RouteDirectives._

  type DomainRoute = StructPath => Route

  def handDoc[T <: TStruct](dt: T, domainRoute: DomainRoute): Route = {
    val rootPath = List(() => dt)
    pathEndOrSingleSlash {
      domainRoute(NodePath(rootPath))
    } ~ handSubFields(rootPath, domainRoute)
  }

  private def handSubFields(sections: NestedStructFields, domainRoute: DomainRoute): Route = {
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
          domainRoute(ListPath(sections.reverse,StructPath.ListSizeIdx))
        }
      case mt: TStrKeyMap[_] =>
        path(Segment) {
          case "#size" => complete(mt.size)
          case key =>
            domainRoute(MapPath(sections.reverse, key))
        }
      case _ => reject
    }
  }

  private def handField(name: String, sections: NestedStructFields, domainRoute: DomainRoute): Route = {
    pathPrefix(name) {
      pathEndOrSingleSlash {
        domainRoute(NodePath(sections.reverse))
      } ~ handSubFields(sections, domainRoute)
    }
  }

  def pathIn(allowPath: StructPath, tryPath: StructPath): Directive0 = new Directive0 {
    override def happly(f: (HNil) => Route): Route =
      if (allowPath.contains(tryPath)) f(HNil)
      else new StandardRoute {
        def apply(ctx: RestReqContext): Unit = ctx.complete(StatusCodes.Forbidden, s"Can't alter unauthorized path,allow path = '$allowPath', request path = '$tryPath' ")
      }
  }
}

object DomainDirectives extends DomainDirectives

