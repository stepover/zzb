package zzb.rest
package directives


import zzb.rest.{RestResponse}

class BasicDirectivesSpec extends RoutingSpec {

  "The 'routeRouteResponse' directive" should {
    "in its simple String form" in {
      val addYeah = routeRouteResponse {
        case RestResponse(_, entity, _) ⇒ complete(entity.asString + "Yeah")
      }
      Get() ~> addYeah(complete("abc")) ~> check { responseAs[String] === "abcYeah" }
    }
  }

}