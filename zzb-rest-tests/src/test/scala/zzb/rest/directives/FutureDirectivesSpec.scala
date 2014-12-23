package zzb.rest
package directives

import scala.concurrent.Future
import spray.util.SingletonException


class FutureDirectivesSpec extends RoutingSpec {

  object TestException extends SingletonException

  "The `onComplete` directive" should {
    "properly unwrap a Future in the success case" in {
      var i = 0
      def nextNumber() = { i += 1; i }
      val route = onComplete(Future.successful(nextNumber())) { echoComplete }
      Get() ~> route ~> check {
        responseAs[String] === "Success(1)"
      }
      Get() ~> route ~> check {
        responseAs[String] === "Success(2)"
      }
    }
    "properly unwrap a Future in the failure case" in {
      Get() ~> onComplete(Future.failed(new RuntimeException("no"))) { echoComplete } ~> check {
        responseAs[String] === "Failure(java.lang.RuntimeException: no)"
      }
    }
  }

  "The `onSuccess` directive" should {
    "properly unwrap a Future in the success case" in {
      Get() ~> onSuccess(Future.successful("yes")) { echoComplete } ~> check {
        responseAs[String] === "yes"
      }
    }
    "throw an exception in the failure case" in {
      Get() ~> onSuccess(Future.failed(TestException)) { echoComplete } ~> check {
        status === StatusCodes.InternalServerError
      }
    }
  }

  "The `onFailure` directive" should {
    "properly unwrap a Future in the success case" in {
      Get() ~> onFailure(Future.successful("yes")) { echoComplete } ~> check {
        responseAs[String] === "yes"
      }
    }
    "throw an exception in the failure case" in {
      Get() ~> onFailure(Future.failed[String](TestException)) { echoComplete } ~> check {
        responseAs[String] === "zzb.rest.directives.FutureDirectivesSpec$TestException$"
      }
    }
  }

}
