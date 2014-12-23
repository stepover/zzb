package zzb.domain

import spray.http.StatusCodes._
import spray.json.{JsString, JsonParser}
import zzb.datatype._
import zzb.domain.plane.Plane._
import zzb.domain.plane.PlaneState

/**
 * Created by Simon on 2014/10/11 
 */
class AlterMonitorTest extends PlaneHttpTestBase {

  "AlterMonitor" - {
    "直接修改简单字段并触发变更监控代码执行" in {
      //创建一个新的领域对象
      user(Post("/api/planes", entity(TString("simon").json))) ~> check {
        status mustBe OK
        pid = JsonParser(body.asString).asInstanceOf[JsString].value
        pid.length mustBe 5
      }

      user(Get(s"/api/planes/$pid/latest/state")) ~> check {
        status mustBe OK
        PlaneState.fromJsValue(JsonParser(body.asString)).idx mustBe 1 //Stop
      }

      manager(Put(s"/api/planes/$pid/alter/owner", TString("God").json)) ~> check {
        status mustBe OK
      }

      Thread.sleep(1000)

      manager(Get(s"/api/planes/$pid/latest/owner")) ~> check {
        status mustBe OK
        TString.fromJsValue(JsonParser(body.asString)).value mustBe "hello God"
      }
    }

    "直接修改Map字段并触发变更监控代码执行" in {

      //创建一个新的领域对象
      user(Post("/api/planes", entity(TString("simon").json))) ~> check {
        status mustBe OK
        pid = JsonParser(body.asString).asInstanceOf[JsString].value
        pid.length mustBe 5
      }

      val cs = StopTimes(Map("001" -> 100, "002" -> 200, "003" -> 300))

      manager(Put(s"/api/planes/$pid/alter/stopTimes?merge=replace", cs.json)) ~> check {
        status mustBe OK
      }
      Thread.sleep(1000)

      manager(Get(s"/api/planes/$pid/latest/message")) ~> check {
        status mustBe OK
        TString.fromJsValue(JsonParser(body.asString)).value mustBe "all stopTimes changed"
      }

      manager(Put(s"/api/planes/$pid/alter/stopTimes/abc", "120")) ~> check {
        status mustBe OK
      }
      Thread.sleep(1000)

      manager(Get(s"/api/planes/$pid/latest/message")) ~> check {
        status mustBe OK
        TString.fromJsValue(JsonParser(body.asString)).value mustBe "stopTimes abc added as 120"
      }

      manager(Put(s"/api/planes/$pid/alter/stopTimes/abc", "518")) ~> check {
        status mustBe OK
      }
      Thread.sleep(1000)

      manager(Get(s"/api/planes/$pid/latest/message")) ~> check {
        status mustBe OK
        TString.fromJsValue(JsonParser(body.asString)).value mustBe "stopTimes abc changed to 518"
      }

      manager(Delete(s"/api/planes/$pid/alter/stopTimes/abc")) ~> check {
        status mustBe OK
      }

      Thread.sleep(1000)

      manager(Get(s"/api/planes/$pid/latest/message")) ~> check {
        status mustBe OK
        TString.fromJsValue(JsonParser(body.asString)).value mustBe "stopTimes abc removed"
      }
    }

    "直接修改List字段并触发变更监控代码执行" in {

      //val c1 = "001" -> 100

      //创建一个新的领域对象
      user(Post("/api/planes", entity(TString("simon").json))) ~> check {
        status mustBe OK
        pid = JsonParser(body.asString).asInstanceOf[JsString].value
        pid.length mustBe 5
      }

      manager(Get(s"/api/planes/$pid/latest/airStops/@size")) ~> check {
        status mustBe OK
        body.asString mustBe "0"
      }

      manager(Post(s"/api/planes/$pid/alter/airStops", TString("广州").json)) ~> check {
        status mustBe OK
      }

      manager(Get(s"/api/planes/$pid/latest/airStops/@size")) ~> check {
        status mustBe OK
        body.asString mustBe "1"
      }
    }
  }


}
