package zzb.domain.plane

import spray.httpx.SprayJsonSupport._
import spray.routing._
import zzb.datatype._
import zzb.domain.ActionBuilder
import zzb.domain.http.DomainHttpApi

/**
 * Created by Simon on 2014/5/15
 */
trait PlaneHttpApi extends DomainHttpApi[String, TString,Plane.type ]{

  override val domainType = Plane

  override val actionBuilder: ActionBuilder = PlaneActionBuilder

  def logName :String = "zzb.domain.plane.http"

  //implicit def sss[T] : RootJsonWriter[List[T]] = ???


  def api: Route =
    pathEndOrSingleSlash {
      post {
        entity(as[TString.Pack]) {
          owner =>
            akkaWithEntityCompleteAs[TString.Pack](owner)
        }
      }
    } ~ pathPrefix(Segment) {
      case "list" => akkaCompleteAsList[Plane.Pack]
      case "options" => optionRoute
      case "force-gc" => java.lang.System.gc();complete("do gc now!")
      case id => //plane id
        domainApi
    }
}
