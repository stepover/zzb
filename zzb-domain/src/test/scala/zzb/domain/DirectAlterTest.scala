package zzb.domain

import spray.http.StatusCodes._
import spray.json.{JsString, JsonParser}
import spray.routing.MalformedRequestContentRejection
import zzb.datatype._
import zzb.domain.plane._

/**
 * Created by Simon on 2014/5/15
 */
class DirectAlterTest extends PlaneHttpTestBase {

  var revFirstSave = VersionRevise(0, 0)

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

    "直接Alter 动作无权限时会被拒绝 " in {

      val tempPlane = Plane(Plane.id := pid, Plane.owner := "Simon")
      //直接修改全部数据
      user(Put(s"/api/planes/$pid/alter",tempPlane.json)) ~> check {
        status mustBe Forbidden
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alter.msg mustBe "用户任何时候都无权修改乘客数据"
        alter.param mustBe -1
      }

      val tempFoods = Foods(Foods.water := 2,Foods.bread := 2)

      //直接修改部分数据
      user(Put(s"/api/planes/$pid/alter/foods",tempFoods.json)) ~> check {
        status mustBe Forbidden
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alter.msg mustBe "乘客不能修改食物数量"
        alter.param mustBe -1
      }
    }

    "可以请求修改数据结构中某一个节点以下的内容" in {

      manager(Put(s"/api/planes/$pid/alter/foods/water", TInt(5).json)) ~> check {
        status mustBe OK
      }

      user(Get(s"/api/planes/$pid/latest/foods/water")) ~> check {
        status mustBe OK
        TInt.fromJsValue(JsonParser(body.asString)).value mustBe 5
      }
    }

    "可以在修改数据的同时要求对当前版本打标记" in {

      manager(Put(s"/api/planes/$pid/alter/foods/water?tag=t1", TInt(666).json)) ~> check {
        status mustBe OK
      }

      user(Get(s"/api/planes/$pid/latest/foods/water")) ~> check {
        status mustBe OK
        TInt.fromJsValue(JsonParser(body.asString)).value mustBe 666
      }

      manager(Put(s"/api/planes/$pid/alter/foods/water", TInt(5).json)) ~> check {
        status mustBe OK
      }

      user(Get(s"/api/planes/$pid/latest/foods/water")) ~> check {
        status mustBe OK
        TInt.fromJsValue(JsonParser(body.asString)).value mustBe 5
      }

      user(Get(s"/api/planes/$pid/tag/t1/foods/water")) ~> check {
        status mustBe OK
        TInt.fromJsValue(JsonParser(body.asString)).value mustBe 666
      }
    }

    "有其他变更会话在进行时，如果修改的数据有重叠路径会报冲突(409)" in {
      //管理员请求创建一个新的Alter会话，要求修改部分数据
      manager(Post(s"/api/planes/$pid/alter/foods")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      manager(Put(s"/api/planes/$pid/alter/foods/water", TInt(5).json)) ~> check {
        status mustBe Conflict
      }

      manager(Post(s"/api/planes/$pid/alter/$alterSeq")) ~> check {
        status mustBe OK //提交更改
      }
    }

    "可以删除数据结构中的某一个节点的内容" in {

      //删除数据
      manager(Delete(s"/api/planes/$pid/alter/foods/water")) ~> check {
        status mustBe OK
      }

      user(Get(s"/api/planes/$pid/latest/foods/water")) ~> check {
        status mustBe NotFound
      }
    }

    "无法用变更API删除整个文档" in {

      manager(Delete(s"/api/planes/$pid/alter")) ~> check {
        status mustBe Forbidden
      }
    }
    "不能删除必填字段" in {
      manager(Delete(s"/api/planes/$pid/alter/id")) ~> check {
        status mustBe BadRequest
      }
    }

    val p1 = Passenger(Passenger.name := "Simon")
    val p2 = Passenger(Passenger.name := "Jack")
    val p3 = Passenger(Passenger.name := "Mike")

    import Plane.{Passengers,Cargos,Vips}

    "可以控制特定状态下拒绝数据修改" in {

      manager(Put(s"/api/planes/$pid/alter/passengers", Passengers(List(p1,p2)).json)) ~> check {
        status mustBe OK //飞机停止时，管理员可以修改乘客数据
      }


      manager(Post(s"/api/planes/$pid/action/slide")) ~> check {
        status mustBe OK //停止状态可以执行滑行指令，转到滑行状态
        ActionResult.format.read(JsonParser(body.asString)).param mustBe 2 //Slide
      }

      manager(Put(s"/api/planes/$pid/alter/passengers", Passengers(List(p1,p2)).json))  ~> check {
        status mustBe Forbidden //飞机停止时，管理员也不可以修改乘客数据
      }

      manager(Post(s"/api/planes/$pid/action/stop")) ~> check {
        status mustBe OK //停止状态可以执行滑行指令，转到滑行状态
        ActionResult.format.read(JsonParser(body.asString)).param mustBe 1 //Stop
      }
    }

    "可以在提交修改的同时执行命令" in {

      manager(Get(s"/api/planes/$pid/latest/passengers")) ~> check {
        status mustBe OK
        Passengers.fromJsValue(JsonParser(body.asString)).length mustBe 2
      }

      manager(Put(s"/api/planes/$pid/alter/passengers?action=slide", Passengers(List(p1,p2,p3)).json)) ~> check {
        status mustBe OK //飞机停止时，管理员可以修改乘客数据,提交更改同时转到滑行状态
        val ddd = body.asString
        ActionResult.format.read(JsonParser(body.asString)).param mustBe 2 //Slide
      }

      user(Get(s"/api/planes/$pid/latest/passengers")) ~> check {
        status mustBe OK
        Passengers.fromJsValue(JsonParser(body.asString)).length mustBe 3
      }

      //转换到Slide状态时速度自动会变为10
      manager(Get(s"/api/planes/$pid/latest/speed")) ~> check {
        status mustBe OK
        TInt.fromJsValue(JsonParser(body.asString)).value mustBe 10
      }


      manager(Post(s"/api/planes/$pid/action/stop")) ~> check {
        status mustBe OK
        ActionResult.format.read(JsonParser(body.asString)).param mustBe 1 //Stop
      }
      //转换到Stop状态时速度自动会变为0
      manager(Get(s"/api/planes/$pid/latest/speed")) ~> check {
        status mustBe OK
        TInt.fromJsValue(JsonParser(body.asString)).value mustBe 0
      }
    }

    "提交数据修改同时执行命令，命令如果无权执行，数据修改也会放弃" in {
      //用户无权转到 slide 状态
      user(Post(s"/api/planes/$pid/action/slide")) ~> check {
        status mustBe Forbidden
      }

      user(Put(s"/api/planes/$pid/alter/memo?action=slide", TString("还不错").json)) ~> check {
        status mustBe Forbidden //用户无权转到 slide 状态
        ActionResult.format.read(JsonParser(body.asString)).param mustBe 1 //Stop
      }
    }

    "数据修改时，之前注册的监控者会得到通知" in {
      manager(Put(s"/api/planes/$pid/alter/owner", TString("God").json)) ~> check {
        status mustBe OK
      }

      Thread.sleep(1000)

      manager(Get(s"/api/planes/$pid/latest/owner")) ~> check {
        status mustBe OK
        TString.fromJsValue(JsonParser(body.asString)).value mustBe "hello God"
      }
    }


    "提交的 json格式不正确会报400错误 " in {

      manager(Put(s"/api/planes/$pid/alter/foods", "ddd")) ~> check {
        rejection mustBe a[MalformedRequestContentRejection]
      }
    }

    "列表操作,内部数据类型为ValuePack" in {
//      manager(Post(s"/api/planes/$pid/alter")) ~> check {
//        status mustBe OK
//        val alter = ActionResult.format.read(JsonParser(body.asString))
//        alterSeq = alter.param
//        alterSeq must be > 0
//      }

      val p1 = Passenger(Passenger.name := "simon")
      val p2 = Passenger(Passenger.name := "jack")
      val ps = Passengers(List(p1, p2))

      manager(Put(s"/api/planes/$pid/alter/passengers", ps.json)) ~> check {
        status mustBe OK
      }

      manager(Get(s"/api/planes/$pid/latest/passengers")) ~> check {
        status mustBe OK
        Passengers.fromJsValue(JsonParser(body.asString)).value.size mustBe 2
      }

      manager(Get(s"/api/planes/$pid/latest/passengers/@size")) ~> check {
        status mustBe OK
        Integer.parseInt(body.asString) mustBe 2
      }

      manager(Delete(s"/api/planes/$pid/alter/passengers/0")) ~> check {
        status mustBe OK
      }
      manager(Get(s"/api/planes/$pid/latest/passengers")) ~> check {
        status mustBe OK
        Passengers.fromJsValue(JsonParser(body.asString)).value.size mustBe 1
      }
      manager(Get(s"/api/planes/$pid/latest/passengers/@size")) ~> check {
        status mustBe OK
        body.asString mustBe "1"
      }
      manager(Delete(s"/api/planes/$pid/alter/passengers/0")) ~> check {
        status mustBe OK
      }
      manager(Get(s"/api/planes/$pid/latest/passengers")) ~> check {
        status mustBe OK
        Passengers.fromJsValue(JsonParser(body.asString)).value.size mustBe 0
      }
      manager(Delete(s"/api/planes/$pid/alter/passengers/0")) ~> check {
        status mustBe BadRequest
      }

    }
    "Map 类型操作,值类型是 Boolean" in {

      val cs = Vips(Map("simon" -> true, "jack" -> false,"vivian" -> true))

      manager(Put(s"/api/planes/$pid/alter/vips?merge=replace", cs.json)) ~> check {
        status mustBe OK
      }

      manager(Get(s"/api/planes/$pid/latest/vips")) ~> check {
        status mustBe OK
        val res = Vips.fromJsValue(JsonParser(body.asString))
        res.size mustBe 3
      }

      manager(Get(s"/api/planes/$pid/latest/vips/@size")) ~> check {
        status mustBe OK
        body.asString mustBe "3"
      }

      manager(Get(s"/api/planes/$pid/latest/vips/simon")) ~> check {
        val msg = body.asString
        status mustBe OK
        msg mustBe  "true"
      }

      manager(Delete(s"/api/planes/$pid/alter/vips/simon")) ~> check {
        val msg = body.asString
        status mustBe OK
      }
      manager(Get(s"/api/planes/$pid/latest/vips")) ~> check {
        val msg = body.asString
        status mustBe OK
        val res = Vips.fromJsValue(JsonParser(msg))
        res.size mustBe 2
      }

      manager(Put(s"/api/planes/$pid/alter/vips/abc", "true")) ~> check {
        status mustBe OK
      }

      manager(Get(s"/api/planes/$pid/latest/vips/abc")) ~> check {
        val msg = body.asString
        status mustBe OK
        msg mustBe  "true"
      }
    }
  }
}



