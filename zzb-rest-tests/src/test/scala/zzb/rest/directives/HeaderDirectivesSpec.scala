//package zzb.rest
//package directives
//
//import RestHeaders.Connection
//
//class HeaderDirectivesSpec extends RoutingSpec {
//
//  "The headerValuePF directive" should {
//    val myHeaderValue = headerValuePF {
//      case Connection(tokens) ⇒ tokens.head
//    }
//
//    "extract the respective header value if a matching request header is present" in {
//      Get("/abc") ~>
//        addHeader(Connection("close")) ~>
//        myHeaderValue {
//          echoComplete
//        } ~>
//        check {
//          responseAs[String] === "close"
//        }
//    }
//    //
//    //    "reject with an empty rejection set if no matching request header is present" in {
//    //      Get("/abc") ~> myHeaderValue { echoComplete } ~> check { rejections === Nil }
//    //    }
//    //
//    //    "reject with a MalformedHeaderRejection if the extract function throws an exception" in {
//    //      Get("/abc") ~> addHeader(Connection("close")) ~> {
//    //        (headerValuePF { case _ ⇒ sys.error("Naah!") }) { echoComplete }
//    //      } ~> check {
//    //        rejection must beLike { case MalformedHeaderRejection("Connection", "Naah!", _) ⇒ ok }
//    //      }
//    //    }
//  }
//
//  //  "The optionalHeaderValue directive" should {
//  //    val myHeaderValue = optionalHeaderValue {
//  //      case Connection(tokens) ⇒ Some(tokens.head)
//  //      case _                  ⇒ None
//  //    }
//  //
//  //    "extract the respective header value if a matching request header is present" in {
//  //      Get("/abc") ~> addHeader(Connection("close")) ~> myHeaderValue { echoComplete } ~> check {
//  //        responseAs[String] === "Some(close)"
//  //      }
//  //    }
//  //
//  //    "extract None if no matching request header is present" in {
//  //      Get("/abc") ~> myHeaderValue { echoComplete } ~> check { responseAs[String] === "None" }
//  //    }
//  //
//  //    "reject with a MalformedHeaderRejection if the extract function throws an exception" in {
//  //      Get("/abc") ~> addHeader(Connection("close")) ~> {
//  //        val myHeaderValue = optionalHeaderValue { case _ ⇒ sys.error("Naaah!") }
//  //        myHeaderValue { echoComplete }
//  //      } ~> check {
//  //        rejection must beLike { case MalformedHeaderRejection("Connection", "Naaah!", _) ⇒ ok }
//  //      }
//  //    }
//  //  }
//
//}