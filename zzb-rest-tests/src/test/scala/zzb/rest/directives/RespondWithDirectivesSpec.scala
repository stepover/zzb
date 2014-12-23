package zzb.rest
package directives

import StatusCodes._
import RestHeaders.RawHeader

class RespondWithDirectivesSpec extends RoutingSpec {

  "respondWithStatus" should {
    "set the given status on successful responses" in {
      Get() ~> {
        respondWithStatus(Created) { completeOk }
      } ~> check { response === RestResponse(Created) }
    }
    "leave rejections unaffected" in {
      Get() ~> {
        respondWithStatus(Created) { reject }
      } ~> check { rejections === Nil }
    }
  }

  "respondWithHeader" should {
    val customHeader = RawHeader("custom", "custom")
    "add the given headers to successful responses" in {
      Get() ~> {
        respondWithHeader(customHeader) { completeOk }
      } ~> check { response === RestResponse(headers = customHeader :: Nil) }
    }
    "leave rejections unaffected" in {
      Get() ~> {
        respondWithHeader(customHeader) { reject }
      } ~> check { rejections === Nil }
    }
  }

//  "The 'respondWithMediaType' directive" should {
//
//    "override the media-type of its inner route response" in {
//      Get() ~> {
//        respondWithMediaType(`text/html`) {
//          complete(<i>yeah</i>)
//        }
//      } ~> check { mediaType === `text/html` }
//    }
//
//    "disable content-negotiation for its inner marshaller" in {
//      Get() ~> addHeader(Accept(`text/css`)) ~> {
//        respondWithMediaType(`text/css`) {
//          complete(<i>yeah</i>)
//        }
//      } ~> check { mediaType === `text/css` }
//    }
//
//    "reject an unacceptable request" in {
//      Get() ~> addHeader(Accept(`text/css`)) ~> {
//        respondWithMediaType(`text/xml`) {
//          complete(<i>yeah</i>)
//        }
//      } ~> check { rejection === UnacceptedResponseContentTypeRejection(List(ContentType(`text/xml`))) }
//    }
//  }

}