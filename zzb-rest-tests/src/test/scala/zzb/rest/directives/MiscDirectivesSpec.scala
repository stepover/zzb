package zzb.rest
package directives

import RestMethods._



class MiscDirectivesSpec extends RoutingSpec {

  "routes created by the concatenation operator '~'" should {
    "yield the first sub route if it succeeded" in {
      Get() ~> {
        get { complete("first") } ~ get { complete("second") }
      } ~> check { responseAs[String] === "first" }
    }
    "yield the second sub route if the first did not succeed" in {
      Get() ~> {
        post { complete("first") } ~ get { complete("second") }
      } ~> check { responseAs[String] === "second" }
    }
    "collect rejections from both sub routes" in {
      Delete() ~> {
        get { completeOk } ~ put { completeOk }
      } ~> check { rejections === Seq(MethodRejection(GET), MethodRejection(PUT)) }
    }
    "clear rejections that have already been 'overcome' by previous directives" in {
      Put() ~> {
        put { parameter('yeah) { echoComplete } } ~
          get { completeOk }
      } ~> check { rejection === MissingQueryParamRejection("yeah") }
    }
  }

//  "the jsonpWithParameter directive" should {
//    val jsonResponse = RestResponse(entity = RestEntity( "[1,2,3]"))
//    "convert JSON responses to corresponding javascript responses according to the given JSONP parameter" in {
//      Get("/?jsonp=someFunc") ~> {
//        jsonpWithParameter("jsonp") {
//          complete(jsonResponse)
//        }
//      } ~> check { body === RestEntity( "someFunc([1,2,3])") }
//    }
//    "not act on JSON responses if no jsonp parameter is present" in {
//      Get() ~> {
//        jsonpWithParameter("jsonp") {
//          complete(jsonResponse)
//        }
//      } ~> check { response.entity === jsonResponse.entity }
//    }
//    "not act on non-JSON responses even if a jsonp parameter is present" in {
//      Get("/?jsonp=someFunc") ~> {
//        jsonpWithParameter("jsonp") {
//          complete(RestResponse(entity = RestEntity( "[1,2,3]")))
//        }
//      } ~> check { body === RestEntity( "[1,2,3]") }
//    }
//    "reject invalid / insecure callback identifiers" in {
//      Get(Uri.from(path = "/", query = Query("jsonp" -> "(function xss(x){evil()})"))) ~> {
//        jsonpWithParameter("jsonp") {
//          complete(RestResponse(entity = RestEntity( "[1,2,3]")))
//        }
//      } ~> check { rejections === Seq(MalformedQueryParamRejection("jsonp", "Invalid JSONP callback identifier")) }
//    }
//  }

//  "the clientIP directive" should {
//    "extract from a X-Forwarded-For header" in {
//      Get() ~> addHeaders(`X-Forwarded-For`("2.3.4.5"), RawHeader("x-real-ip", "1.2.3.4")) ~> {
//        clientIP { echoComplete }
//      } ~> check { responseAs[String] === "2.3.4.5" }
//    }
//    "extract from a Remote-Address header" in {
//      Get() ~> addHeaders(RawHeader("x-real-ip", "1.2.3.4"), `Remote-Address`("5.6.7.8")) ~> {
//        clientIP { echoComplete }
//      } ~> check { responseAs[String] === "5.6.7.8" }
//    }
//    "extract from a X-Real-IP header" in {
//      Get() ~> addHeader(RawHeader("x-real-ip", "1.2.3.4")) ~> {
//        clientIP { echoComplete }
//      } ~> check { responseAs[String] === "1.2.3.4" }
//    }
//  }

  "The `rewriteUnmatchedPath` directive" should {
    "rewrite the unmatched path" in {
      Get("/abc") ~> {
        rewriteUnmatchedPath(_ / "def") {
          path("abc" / "def") { completeOk }
        }
      } ~> check { response === Ok }
    }
  }
}
