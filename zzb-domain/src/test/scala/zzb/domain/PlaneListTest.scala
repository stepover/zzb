package zzb.domain

import spray.http.StatusCodes._
import spray.json.{JsString, JsArray, JsonParser}
import zzb.datatype.TString
import zzb.domain.plane.Plane

/**
 * Created by Simon on 2014/8/19
 */
class PlaneListTest extends PlaneHttpTestBase {

  "Plane Http Api  " - {

    "可以取得列表空" in {
      user(Get("/api/planes/list") ) ~> check {
        println(body.asString)
        val list = JsonParser(body.asString).asInstanceOf[JsArray]
        list.elements.size mustBe 0
        status mustBe OK
      }
    }

    "可以取得非空列表" in {
      user(Post("/api/planes", entity(TString("simon").json))) ~> check {
        status mustBe OK
        pid = JsonParser(body.asString).asInstanceOf[JsString].value
        pid.length mustBe 5
      }

      user(Post("/api/planes", entity(TString("simon").json))) ~> check {
        status mustBe OK
        pid = JsonParser(body.asString).asInstanceOf[JsString].value
        pid.length mustBe 5
      }

      user(Get("/api/planes/list") ) ~> check {
        println(body.asString)
        val list = JsonParser(body.asString).asInstanceOf[JsArray]
        list.elements.size mustBe 2
        val plane = Plane.fromJsValue(list.elements(0))
        plane.owner mustBe "simon"
        status mustBe OK
      }
    }
  }

}
