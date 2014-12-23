package zzb.domain

import spray.http.StatusCodes._
import spray.json.{JsString, JsonParser}
import zzb.datatype._
import zzb.domain.plane.{Foods, PlaneState}

/**
 * Created by Simon on 2014/7/18
 */
class ActionPostEntityTest extends PlaneHttpTestBase {

  "Plane Http Api  " - {

    "可以创建一个新的领域对象 " in {
      user(Post("/api/planes", entity(TString("simon").json))) ~> check {
        status mustBe OK
        pid = JsonParser(body.asString).asInstanceOf[JsString].value
        pid.length mustBe 5
      }

      user(Get(s"/api/planes/$pid/latest/state")) ~> check {
        status mustBe OK
        PlaneState.fromJsValue(JsonParser(body.asString)).idx mustBe 1 //Stop
      }
    }

    "可以使用Acton Post 数据" in {

      val f1 = Foods(Foods.water := 2,Foods.bread := 2)

      val f2 = Foods(Foods.water := 5,Foods.bread := 2)

      manager(Post(s"/api/planes/$pid/action/reloadFools",f1.json)) ~> check{
        status mustBe OK
      }

      user(Get(s"/api/planes/$pid/latest/foods/water")) ~> check {
        status mustBe OK
        TInt.fromJsValue(JsonParser(body.asString)).value mustBe 2
      }

      manager(Post(s"/api/planes/$pid/action/reloadFools",f2.json)) ~> check{
        status mustBe OK
      }

      user(Get(s"/api/planes/$pid/latest/foods/water")) ~> check {
        status mustBe OK
        TInt.fromJsValue(JsonParser(body.asString)).value mustBe 5
      }

      manager(Post(s"/api/planes/$pid/action/nothis",f2.json)) ~> check{
        status mustBe NotFound
      }
    }
  }
}
