package zzb.datatype

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import spray.routing._
import zzb.datatype.testuse.HomeInfo
import spray.httpx.SprayJsonSupport._

/**
 * Created by Simon on 2014/4/4
 */
class JsonSprayTest extends WordSpec with MustMatchers {

}


trait HomeInfoHttpApi extends Directives {

  def route1 : Route =
    pathEndOrSingleSlash {
      entity(as[TString.Pack]) {
        owner =>
          complete(owner)
      }
    }

  def route2 : Route =
    pathEndOrSingleSlash {
      entity(as[TStruct.Pack]) {
        owner =>
          complete(owner)
      }
    }
  def route3 : Route =
    pathEndOrSingleSlash {
      entity(as[HomeInfo.Pack]) {
        owner =>
          complete(owner)
      }
    }

}
