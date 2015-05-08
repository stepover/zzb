package zzb.domain

import spray.http.StatusCodes._
import spray.json.{JsString, JsonParser}
import zzb.datatype._
import zzb.domain.plane._


/**
 * Created by Simon on 2014/7/7
 */
class VersionsTest extends PlaneHttpTestBase {

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

      user(Get(s"/api/planes/$pid/versions")) ~> check {
        status mustBe OK
        VersionInfos.fromJsValue(JsonParser(body.asString)).length mustBe 0
      }
    }

    "修改过程中修订号会逐步增加" in {
      //管理员请求创建一个新的Alter会话，要求修改部分数据
      manager(Post(s"/api/planes/$pid/alter/foods")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      manager(Post(s"/api/planes/$pid/alter/$alterSeq?flush=true")) ~> check {
        status mustBe OK //提交更改
        val res = ActionResult.format.read(JsonParser(body.asString))
        res.ver.version > 0 mustBe true
        res.ver.revise mustBe 0
      }
    }

    "修改数据后版本号会增加" in {

      manager(Put(s"/api/planes/$pid/alter/foods/water", TInt(5).json)) ~> check {
        status mustBe OK
      }

      user(Get(s"/api/planes/$pid/latest/foods/water")) ~> check {
        status mustBe OK
        TInt.fromJsValue(JsonParser(body.asString)).value mustBe 5
      }

      user(Get(s"/api/planes/$pid/versions")) ~> check {
        status mustBe OK
        val vv = JsonParser(body.asString)
        VersionInfos.fromJsValue(JsonParser(body.asString)).length mustBe 1 //版本数量没有变，旧版本被覆盖
      }
    }

    "状态变更后版本号会增加" in {
      manager(Post(s"/api/planes/$pid/action/slide")) ~> check {
        status mustBe OK //停止状态可以执行滑行指令，转到滑行状态
        ActionResult.format.read(JsonParser(body.asString)).param mustBe 2 //Slide
      }

      user(Get(s"/api/planes/$pid/versions")) ~> check {
        status mustBe OK
        val vv = JsonParser(body.asString)
        VersionInfos.fromJsValue(JsonParser(body.asString)).length mustBe 1 //版本数量没有变，旧版本被覆盖
      }
    }
  }
}
