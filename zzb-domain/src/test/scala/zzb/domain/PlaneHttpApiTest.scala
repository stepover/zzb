package zzb.domain

import spray.http.StatusCodes._
import spray.json.{JsArray, JsString, JsonParser}
import spray.routing._
import zzb.datatype._
import zzb.domain.plane._
import zzb.util.MdcLoggingContext

/**
 * Created by Simon on 2014/5/15
 */
class PlaneHttpApiTest extends PlaneHttpTestBase {

  var revFirstSave = VersionRevise(0, 0)

  "Plane Http Api  " - {

    "禁止未经认证的用户访问" in {
      (Post("/api/planes", entity(TString("simon").json)) ~> api) ~> check {
        status mustBe Forbidden
      }
    }

    "未提供所有者不能创建领域对象" in {
      user(Post("/api/planes")) ~> check {
        rejection mustBe RequestEntityExpectedRejection
      }
    }

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
      //todo:
//      //get 时 latest 去掉也可以
//      user(Get(s"/api/planes/$pid/state/")) ~> check {
//        val s = body.asString
//        status mustBe OK
//        //PlaneState.fromJsValue(JsonParser(body.asString)).idx mustBe 1 //Stop
//      }
    }

    "不存在的命令(报404)或无权执行的命令(报403）会报错" in {
      user(Post(s"/api/planes/$pid/action/swim")) ~> check {
        status mustBe NotFound //NotFound //飞机不能游泳
        body.asString mustBe "action 'swim' not exist!"
      }
      user(Post(s"/api/planes/$pid/action/slide")) ~> check {
        status mustBe Forbidden //用户不让启动飞机
      }
    }

    "状态机可以拒绝不合时宜的命令" in {
      manager(Post(s"/api/planes/$pid/action/fly")) ~> check {
        status mustBe Forbidden //停止状态不能执行起飞指令
      }
      manager(Post(s"/api/planes/$pid/action/slide")) ~> check {
        status mustBe OK //停止状态可以执行滑行指令，转到滑行状态
        ActionResult.format.read(JsonParser(body.asString)).param mustBe 2 //Slide
      }

      //Thread.sleep(100)

      user(Post(s"/api/planes/$pid/action/stop")) ~> check {
        //val msg = body.asString
        //println("---->" + msg)
        status mustBe OK //转到停止状态
        ActionResult.format.read(JsonParser(body.asString)).param mustBe 1 //Stop
      }
    }

    "新建Alter会话时会拒绝无权限的请求 " in {
      //用户请求创建一个新的Alter会话，要求修改全部数据
      user(Post(s"/api/planes/$pid/alter")) ~> check {
        status mustBe Forbidden
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alter.msg mustBe "用户任何时候都无权修改乘客数据"
        alter.param mustBe -1
      }

      //用户请求创建一个新的Alter会话，要求修改部分数据
      user(Post(s"/api/planes/$pid/alter/foods")) ~> check {
        status mustBe Forbidden
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alter.msg mustBe "乘客不能修改食物数量"
        alter.param mustBe -1
      }

    }

    "可以请求修改数据结构中某一个节点以下的内容" in {

      //管理员请求创建一个新的Alter会话，要求修改部分数据
      manager(Post(s"/api/planes/$pid/alter/foods")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      manager(Put(s"/api/planes/$pid/alter/$alterSeq/foods/water", TInt(5).json)) ~> check {
        status mustBe OK
      }

      manager(Post(s"/api/planes/$pid/alter/$alterSeq")) ~> check {
        status mustBe OK //提交更改
      }

      user(Get(s"/api/planes/$pid/latest/foods/water")) ~> check {
        status mustBe OK
        TInt.fromJsValue(JsonParser(body.asString)).value mustBe 5
      }
    }


    "可以删除数据结构中的某一个节点的内容" in {

      //管理员请求创建一个新的Alter会话，要求修改部分数据
      manager(Post(s"/api/planes/$pid/alter/foods")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }
      //删除数据
      manager(Delete(s"/api/planes/$pid/alter/$alterSeq/foods/water")) ~> check {
        status mustBe OK
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/foods/water")) ~> check {
        status mustBe NotFound
      }

      manager(Delete(s"/api/planes/$pid/alter/$alterSeq/foods")) ~> check {
        status mustBe OK
      }

      manager(Post(s"/api/planes/$pid/alter/$alterSeq")) ~> check {
        status mustBe OK //提交更改
      }

      user(Get(s"/api/planes/$pid/latest/foods/water")) ~> check {
        status mustBe NotFound
      }
    }

    "无法用变更API删除整个文档" in {
      //管理员请求创建一个新的Alter会话，要求修改部分数据
      manager(Post(s"/api/planes/$pid/alter")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }
      //这个调用会导致变更会话被放弃，而不是删除整个文档
      manager(Delete(s"/api/planes/$pid/alter/$alterSeq")) ~> check {
        status mustBe OK
      }
    }

    "要修改的数据与之前申请的路径不一致，拒绝修改" in {
      //管理员请求创建一个新的Alter会话，要求修改部分数据
      manager(Post(s"/api/planes/$pid/alter/foods")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      //要修改的数据与之前申请的路径不一致，拒绝修改"
      manager(Put(s"/api/planes/$pid/alter/$alterSeq/speed", TInt(500).json)) ~> check {
        status mustBe Forbidden
      }

      manager(Post(s"/api/planes/$pid/alter/$alterSeq")) ~> check {
        status mustBe OK //提交更改
      }
    }

    val p1 = Passenger(Passenger.name := "Simon")
    val p2 = Passenger(Passenger.name := "Jack")

    import Plane.{Passengers,Cargos,AirStops,DistanceStops,StopTimes}

    "可以控制特定状态下拒绝数据修改" in {

      //管理员请求创建一个新的Alter会话
      manager(Post(s"/api/planes/$pid/alter")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      manager(Post(s"/api/planes/$pid/alter/$alterSeq/passengers", p1.json)) ~> check {
        status mustBe OK //飞机停止时，管理员可以修改乘客数据
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers")) ~> check {
        status mustBe OK
        Passengers.fromJsValue(JsonParser(body.asString)).length mustBe 1
      }

      manager(Post(s"/api/planes/$pid/alter/$alterSeq")) ~> check {
        status mustBe OK //提交更改
      }


      manager(Post(s"/api/planes/$pid/action/slide")) ~> check {
        status mustBe OK //停止状态可以执行滑行指令，转到滑行状态
        ActionResult.format.read(JsonParser(body.asString)).param mustBe 2 //Slide
      }

      //管理员请求创建一个新的Alter会话
      manager(Post(s"/api/planes/$pid/alter")) ~> check {
        status mustBe Forbidden //飞机滑行时，管理员也不可以修改乘客数据
        ActionResult.format.read(JsonParser(body.asString)).msg mustBe "在飞机滑行和飞行的时候不能修改乘客数据"
      }

      manager(Post(s"/api/planes/$pid/action/stop")) ~> check {
        status mustBe OK //停止状态可以执行滑行指令，转到滑行状态
        ActionResult.format.read(JsonParser(body.asString)).param mustBe 1 //Stop
      }
    }

    "可以在提交修改的同时执行命令" in {
      //管理员请求创建一个新的Alter会话
      manager(Post(s"/api/planes/$pid/alter")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers")) ~> check {
        status mustBe OK
        Passengers.fromJsValue(JsonParser(body.asString)).length mustBe 1
      }

      manager(Post(s"/api/planes/$pid/alter/$alterSeq/passengers", p2.json)) ~> check {
        status mustBe OK //飞机停止时，管理员可以修改乘客数据
      }

      user(Get(s"/api/planes/$pid/latest/passengers")) ~> check {
        status mustBe OK
        Passengers.fromJsValue(JsonParser(body.asString)).length mustBe 1
      }

      manager(Post(s"/api/planes/$pid/alter/$alterSeq?action=slide")) ~> check {
        status mustBe OK //提交更改同时转到滑行状态
        ActionResult.format.read(JsonParser(body.asString)).param mustBe 2 //Slide
      }

      //转换到Slide状态时速度自动会变为10
      manager(Get(s"/api/planes/$pid/latest/speed")) ~> check {
        status mustBe OK
        TInt.fromJsValue(JsonParser(body.asString)).value mustBe 10
      }

      user(Get(s"/api/planes/$pid/latest/passengers")) ~> check {
        status mustBe OK
        Passengers.fromJsValue(JsonParser(body.asString)).length mustBe 2
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers")) ~> check {
        status mustBe NotFound
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
      user(Post(s"/api/planes/$pid/alter/memo")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      user(Post(s"/api/planes/$pid/alter/$alterSeq?action=slide")) ~> check {
        status mustBe Forbidden //提交更改同时转到滑行状态
        ActionResult.format.read(JsonParser(body.asString)).param mustBe 1 //Stop
      }
    }
    "变更会话存在会阻止action提交，强制提交会废弃所有变更会话" in {
      //管理员请求创建一个新的Alter会话
      manager(Post(s"/api/planes/$pid/alter")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      //可以获取变更会话的数据
      manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers")) ~> check {
        status mustBe OK
        Passengers.fromJsValue(JsonParser(body.asString)).length mustBe 2
      }

      //请求执行动作会冲突
      manager(Post(s"/api/planes/$pid/action/slide")) ~> check {
        status mustBe Conflict
        //获取冲突的变更会话信息(数组)
        val alterSessions = JsonParser(body.asString).asInstanceOf[JsArray].elements.map(AlterSession.format.read)
        alterSessions.length mustBe 1
      }

      //强制执行动作
      manager(Post(s"/api/planes/$pid/action/slide?force=true")) ~> check {
        status mustBe OK
        ActionResult.format.read(JsonParser(body.asString)).param mustBe 2 //Slide
      }

      //强制执行后会废弃所有的变更会话
      manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers")) ~> check {
        status mustBe NotFound
      }

      //强制执行动作会冲突
      manager(Post(s"/api/planes/$pid/action/stop")) ~> check {
        status mustBe OK
        ActionResult.format.read(JsonParser(body.asString)).param mustBe 1 //Stop
      }
    }

    "数据修改时，之前注册的监控者会得到通知" in {
      //管理员请求创建一个新的Alter会话
      manager(Post(s"/api/planes/$pid/alter")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      manager(Put(s"/api/planes/$pid/alter/$alterSeq/owner", TString("God").json)) ~> check {
        status mustBe OK
      }

      manager(Post(s"/api/planes/$pid/alter/$alterSeq")) ~> check {
        status mustBe OK //提交更改,会触发事件在 owner 的前面加上 "hello "
      }

      Thread.sleep(1000)

      manager(Get(s"/api/planes/$pid/latest/owner")) ~> check {
        status mustBe OK
        TString.fromJsValue(JsonParser(body.asString)).value mustBe "hello God"
      }
    }

    "提交的 json格式不正确会报400错误 " in {
      //管理员请求创建一个新的Alter会话，要求修改部分数据
      manager(Post(s"/api/planes/$pid/alter/foods")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      manager(Put(s"/api/planes/$pid/alter/$alterSeq/foods", "ddd")) ~> check {
        rejection mustBe a[MalformedRequestContentRejection]
      }

      manager(Post(s"/api/planes/$pid/alter/$alterSeq")) ~> check {
        status mustBe OK //提交更改
      }
    }

    "列表操作,内部数据类型为ValuePack" in {
      manager(Post(s"/api/planes/$pid/alter")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      val p1 = Passenger(Passenger.name := "simon")
      val p2 = Passenger(Passenger.name := "jack")
      val ps = Passengers(List(p1, p2))

      manager(Put(s"/api/planes/$pid/alter/$alterSeq/passengers", ps.json)) ~> check {
        status mustBe OK
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers")) ~> check {
        status mustBe OK
        Passengers.fromJsValue(JsonParser(body.asString)).value.size mustBe 2
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers/@size")) ~> check {
        status mustBe OK
        Integer.parseInt(body.asString) mustBe 2
      }

      manager(Delete(s"/api/planes/$pid/alter/$alterSeq/passengers/0")) ~> check {
        status mustBe OK
      }
      manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers")) ~> check {
        status mustBe OK
        Passengers.fromJsValue(JsonParser(body.asString)).value.size mustBe 1
      }
      manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers/@size")) ~> check {
        status mustBe OK
        body.asString mustBe "1"
      }
      manager(Delete(s"/api/planes/$pid/alter/$alterSeq/passengers/0")) ~> check {
        status mustBe OK
      }
      manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers")) ~> check {
        status mustBe OK
        Passengers.fromJsValue(JsonParser(body.asString)).value.size mustBe 0
      }
      manager(Delete(s"/api/planes/$pid/alter/$alterSeq/passengers/0")) ~> check {
        status mustBe BadRequest
      }
      manager(Post(s"/api/planes/$pid/alter/$alterSeq")) ~> check {
        status mustBe OK //提交更改
      }
    }

    "列表操作,内部数据类型为 String" in {
      manager(Post(s"/api/planes/$pid/alter")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      val p1 = "北京"
      val p2 = "上海"
      val ps = AirStops(List(p1, p2))

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/airStops/@size")) ~> check {
        status mustBe OK
        Integer.parseInt(body.asString) mustBe 0
      }

      manager(Put(s"/api/planes/$pid/alter/$alterSeq/airStops", ps.json)) ~> check {
        status mustBe OK
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/airStops")) ~> check {
        status mustBe OK
        AirStops.fromJsValue(JsonParser(body.asString)).value.size mustBe 2
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/airStops/@size")) ~> check {
        status mustBe OK
        Integer.parseInt(body.asString) mustBe 2
      }

      manager(Delete(s"/api/planes/$pid/alter/$alterSeq/airStops/0")) ~> check {
        status mustBe OK
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/airStops")) ~> check {
        status mustBe OK
        AirStops.fromJsValue(JsonParser(body.asString)).value.size mustBe 1
      }
      manager(Get(s"/api/planes/$pid/alter/$alterSeq/airStops/@size")) ~> check {
        status mustBe OK
        body.asString mustBe "1"
      }
      manager(Delete(s"/api/planes/$pid/alter/$alterSeq/airStops/0")) ~> check {
        status mustBe OK
      }
      manager(Get(s"/api/planes/$pid/alter/$alterSeq/airStops")) ~> check {
        status mustBe OK
        AirStops.fromJsValue(JsonParser(body.asString)).value.size mustBe 0
      }
      manager(Delete(s"/api/planes/$pid/alter/$alterSeq/airStops/0")) ~> check {
        status mustBe BadRequest
      }
      manager(Post(s"/api/planes/$pid/alter/$alterSeq")) ~> check {
        status mustBe OK //提交更改
      }
    }

    "列表操作,内部数据类型为 Int" in {
      manager(Post(s"/api/planes/$pid/alter")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      val p1 = 120
      val p2 = 340
      val ps = DistanceStops(List(p1, p2))

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/distanceStops/@size")) ~> check {
        status mustBe OK
        Integer.parseInt(body.asString) mustBe 0
      }

      manager(Put(s"/api/planes/$pid/alter/$alterSeq/distanceStops", ps.json)) ~> check {
        status mustBe OK
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/distanceStops")) ~> check {
        status mustBe OK
        DistanceStops.fromJsValue(JsonParser(body.asString)).value.size mustBe 2
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/distanceStops/@size")) ~> check {
        status mustBe OK
        Integer.parseInt(body.asString) mustBe 2
      }

      manager(Delete(s"/api/planes/$pid/alter/$alterSeq/distanceStops/0")) ~> check {
        status mustBe OK
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/distanceStops")) ~> check {
        status mustBe OK
        DistanceStops.fromJsValue(JsonParser(body.asString)).value.size mustBe 1
      }
      manager(Get(s"/api/planes/$pid/alter/$alterSeq/distanceStops/@size")) ~> check {
        status mustBe OK
        body.asString mustBe "1"
      }
      manager(Delete(s"/api/planes/$pid/alter/$alterSeq/distanceStops/0")) ~> check {
        status mustBe OK
      }
      manager(Get(s"/api/planes/$pid/alter/$alterSeq/distanceStops")) ~> check {
        status mustBe OK
        DistanceStops.fromJsValue(JsonParser(body.asString)).value.size mustBe 0
      }
      manager(Delete(s"/api/planes/$pid/alter/$alterSeq/distanceStops/0")) ~> check {
        status mustBe BadRequest
      }
      manager(Post(s"/api/planes/$pid/alter/$alterSeq")) ~> check {
        status mustBe OK //提交更改
      }
    }

    "Map 类型操作,值类型是 ValuePack" in {
      manager(Post(s"/api/planes/$pid/alter")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      val c0 = Cargo(Cargo.id := "abc", Cargo.owner := "simon")
      val c1 = Cargo(Cargo.id := "001", Cargo.owner := "jack")
      val c2 = Cargo(Cargo.id := "002", Cargo.owner := "mike")
      val c3 = Cargo(Cargo.id := "003", Cargo.owner := "david")

      val cs = Cargos(Map("001" -> c1, "002" -> c2, "003" -> c3))

      manager(Put(s"/api/planes/$pid/alter/$alterSeq/cargos/abc", c0.json)) ~> check {
        status mustBe OK
      }

      manager(Put(s"/api/planes/$pid/alter/$alterSeq/cargos?merge=replace", cs.json)) ~> check {
        status mustBe OK
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/cargos")) ~> check {
        status mustBe OK
        val res = Cargos.fromJsValue(JsonParser(body.asString))
        res.size mustBe 3
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/cargos/@size")) ~> check {
        status mustBe OK
        body.asString mustBe "3"
      }

      manager(Delete(s"/api/planes/$pid/alter/$alterSeq/cargos/001")) ~> check {
        val msg = body.asString
        status mustBe OK
      }
      manager(Get(s"/api/planes/$pid/alter/$alterSeq/cargos")) ~> check {
        status mustBe OK
        val res = Cargos.fromJsValue(JsonParser(body.asString))
        res.size mustBe 2
      }

      manager(Post(s"/api/planes/$pid/alter/$alterSeq")) ~> check {
        status mustBe OK //提交更改
      }
    }

    "Map 类型操作,值类型是 Int" in {
      manager(Post(s"/api/planes/$pid/alter")) ~> check {
        status mustBe OK
        val alter = ActionResult.format.read(JsonParser(body.asString))
        alterSeq = alter.param
        alterSeq must be > 0
      }

      val c0 = "abc" -> 120

      val cs = StopTimes(Map("001" -> 100, "002" -> 200, "003" -> 300))

      manager(Put(s"/api/planes/$pid/alter/$alterSeq/stopTimes?merge=replace", cs.json)) ~> check {
        status mustBe OK
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/stopTimes")) ~> check {
        status mustBe OK
        val res = StopTimes.fromJsValue(JsonParser(body.asString))
        res.size mustBe 3
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/stopTimes/@size")) ~> check {
        status mustBe OK
        body.asString mustBe "3"
      }

      manager(Get(s"/api/planes/$pid/alter/$alterSeq/stopTimes/001")) ~> check {
        val msg = body.asString
        status mustBe OK
        Integer.parseInt(msg)  mustBe  100
      }

      manager(Delete(s"/api/planes/$pid/alter/$alterSeq/stopTimes/001")) ~> check {
        val msg = body.asString
        status mustBe OK
      }
      manager(Get(s"/api/planes/$pid/alter/$alterSeq/stopTimes")) ~> check {
        val msg = body.asString
        status mustBe OK
        val res = StopTimes.fromJsValue(JsonParser(msg))
        res.size mustBe 2
      }

//      manager(Put(s"/api/planes/$pid/alter/$alterSeq/stopTimes/abc", "600")) ~> check {
//        status mustBe OK
//      }

      manager(Post(s"/api/planes/$pid/alter/$alterSeq")) ~> check {
        status mustBe OK //提交更改
      }
    }

    "可以将领域对象从缓存中清理，并销毁Actor" in {
      manager(Delete(s"/api/planes/$pid/")) ~> check{
        status mustBe OK
        println(body.asString)
      }
    }
  }
}



