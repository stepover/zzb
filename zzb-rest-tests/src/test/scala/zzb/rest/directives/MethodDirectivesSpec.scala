package zzb.rest
package directives


class MethodDirectivesSpec extends RoutingSpec {

"get | put" should {
    val getOrPut = (get | put) { completeOk }

    "block POST requests" in {
      Post() ~> getOrPut ~> check { handled === false }
    }
    "let GET requests pass" in {
      Get() ~> getOrPut ~> check { response === Ok }
    }
    "let PUT requests pass" in {
      Put() ~> getOrPut ~> check { response === Ok }
    }
  }

  "two failed `get` directives" should {
    "only result in a single Rejection" in {
      Put() ~> {
        get { completeOk } ~
          get { completeOk }
      } ~> check {
        rejections === List(MethodRejection(RestMethods.GET))
      }
    }
  }
//
//  "overrideMethodWithParameter" should {
//    "change the request method" in {
//      Get("/?_method=put") ~> overrideMethodWithParameter("_method") {
//        get { complete("GET") } ~
//          put { complete("PUT") }
//      } ~> check { responseAs[String] === "PUT" }
//    }
//    "not affect the request when not specified" in {
//      Get() ~> overrideMethodWithParameter("_method") {
//        get { complete("GET") } ~
//          put { complete("PUT") }
//      } ~> check { responseAs[String] === "GET" }
//    }
//    "complete with 501 Not Implemented when not a valid method" in {
//      Get("/?_method=hallo") ~> overrideMethodWithParameter("_method") {
//        get { complete("GET") } ~
//          put { complete("PUT") }
//      } ~> check { status === StatusCodes.NotImplemented }
//    }
//  }
//
  "MethodRejections" should {
    "be cancelled by a successful match" in {
      "if the match happens after the rejection" in {
        Put() ~> {
          get { completeOk } ~
            put { reject(RequestEntityExpectedRejection) }
        } ~> check {
          rejections === List(RequestEntityExpectedRejection)
        }
      }
      "if the match happens after the rejection (example 2)" in {
        Put() ~> {
          (get & complete)(Ok) ~
            (put & reject(RequestEntityExpectedRejection))
        } ~> check {
          rejections === List(RequestEntityExpectedRejection)
        }
      }
      "if the match happens before the rejection" in {
        Put() ~> {
          put { reject(RequestEntityExpectedRejection) } ~
            get { completeOk }
        } ~> check {
          rejections === List(RequestEntityExpectedRejection)
        }
      }
      "if the match happens before the rejection (example 2)" in {
        Put() ~> {
          (put & reject(RequestEntityExpectedRejection)) ~
            (get & complete)(Ok)
        } ~> check {
          rejections === List(RequestEntityExpectedRejection)
        }
      }
    }
  }

}