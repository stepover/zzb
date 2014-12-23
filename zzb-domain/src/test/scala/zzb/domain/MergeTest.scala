package zzb.domain

import spray.json.{JsString, JsonParser}
import zzb.datatype._
import zzb.domain.plane._
import spray.http.StatusCodes._

/**
 * Created by Simon on 2014/7/10
 */
class MergeTest extends PlaneHttpTestBase{

  "创建一个新的领域对象,创建一个变更会话 " in {
    user(Post("/api/planes", entity(TString("simon").json))) ~> check {
      status mustBe OK
      pid = JsonParser(body.asString).asInstanceOf[JsString].value
      pid.length mustBe 5
    }

    //管理员请求创建一个新的Alter会话，要求修改部分数据
    manager(Post(s"/api/planes/$pid/alter")) ~> check {
      status mustBe OK
      val alter = ActionResult.format.read(JsonParser(body.asString))
      alterSeq = alter.param
      alterSeq must be > 0
    }
  }

  "基本类型的合并策略" in {

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/foods/water", TInt(5).json)) ~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/foods/water")) ~> check {
      status mustBe OK
      TInt.fromJsValue(JsonParser(body.asString)).value mustBe 5
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/foods/water?merge=useold", TInt(6).json)) ~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/foods/water")) ~> check {
      status mustBe OK
      TInt.fromJsValue(JsonParser(body.asString)).value mustBe 5
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/foods/water?merge=usenew", TInt(6).json)) ~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/foods/water")) ~> check {
      status mustBe OK
      TInt.fromJsValue(JsonParser(body.asString)).value mustBe 6
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/foods/water?merge=replace", TInt(7).json)) ~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/foods/water")) ~> check {
      status mustBe OK
      TInt.fromJsValue(JsonParser(body.asString)).value mustBe 7
    }
  }

  "结构类型的合并策略" in {
    val f0 = Foods(Foods.water := 10)
    val f1 = Foods(Foods.water := 11,Foods.bread := 21)
    val f01 = Foods(Foods.water := 10,Foods.bread := 21)
    val f2 = Foods(Foods.water := 12 ,Foods.bread := 22)

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/foods",f0.json))~> check {
      status mustBe OK
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/foods?merge=useold",f1.json))~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/foods")) ~> check {
      status mustBe OK
      val foods = Foods.fromJsValue(JsonParser(body.asString))
      foods mustBe f01
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/foods?merge=usenew",f2.json))~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/foods")) ~> check {
      status mustBe OK
      val foods = Foods.fromJsValue(JsonParser(body.asString))
      foods mustBe f2
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/foods?merge=replace",f0.json))~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/foods")) ~> check {
      status mustBe OK
      val foods = Foods.fromJsValue(JsonParser(body.asString))
      foods mustBe f0
    }
  }

  import Plane.{Passengers,Cargos}

  "列表类型的合并策略" in {
    val p01 = Passenger(Passenger.name := "simon",Passenger.age := 20)
    val p02 = Passenger(Passenger.name := "jack",Passenger.age := 21)
    val ps0 = Passengers(List(p01, p02))
    val p01a = Passenger(Passenger.name := "simon",Passenger.age := 30)

    val p11 = Passenger(Passenger.name := "tom",Passenger.age := 20)
    val p12 = Passenger(Passenger.name := "mike",Passenger.age := 21)
    val ps1 = Passengers(List(p11, p12))

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/passengers", ps0.json)) ~> check {
      status mustBe OK
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/passengers?merge=useold", ps1.json)) ~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers")) ~> check {
      status mustBe OK
      Passengers.fromJsValue(JsonParser(body.asString)) mustBe ps0
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/passengers?merge=usenew", ps1.json)) ~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers")) ~> check {
      status mustBe OK
      Passengers.fromJsValue(JsonParser(body.asString)) mustBe ps1
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/passengers?merge=replace", ps0.json)) ~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers")) ~> check {
      status mustBe OK
      Passengers.fromJsValue(JsonParser(body.asString)) mustBe ps0
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/passengers/0?merge=useold", p01a.json)) ~> check {
      status mustBe OK
    }
    manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers/0")) ~> check {
      status mustBe OK
      Passenger.fromJsValue(JsonParser(body.asString)) mustBe p01
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/passengers/0?merge=usenew", p01a.json)) ~> check {
      status mustBe OK
    }
    manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers/0")) ~> check {
      status mustBe OK
      Passenger.fromJsValue(JsonParser(body.asString)) mustBe p01a
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/passengers/0?merge=replace", p01.json)) ~> check {
      status mustBe OK
    }
    manager(Get(s"/api/planes/$pid/alter/$alterSeq/passengers/0")) ~> check {
      status mustBe OK
      Passenger.fromJsValue(JsonParser(body.asString)) mustBe p01
    }
  }

  "Map类型的合并策略" in {

    val c01 = Cargo(Cargo.id:="01",Cargo.owner := "simon")
    val c01a = Cargo(Cargo.id:="01",Cargo.owner := "vivian")
    val c02 = Cargo(Cargo.id:="02",Cargo.owner := "jack")
    val c11 = Cargo(Cargo.id:="11",Cargo.owner := "mike")
    val c12 = Cargo(Cargo.id:="12",Cargo.owner := "david")

    val cs0 = Cargos(Map("01"-> c01,"02"-> c02))
    val cs0a = Cargos(Map("01"-> c01a,"02"-> c02))
    val cs1 = Cargos(Map("01"-> c11,"12"-> c12))

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/cargos",cs0.json)) ~> check {
      status mustBe OK
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/cargos?merge=useold",cs0a.json)) ~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/cargos")) ~> check {
      status mustBe OK
      Cargos.fromJsValue(JsonParser(body.asString)) mustBe cs0
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/cargos?merge=usenew",cs0a.json)) ~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/cargos")) ~> check {
      status mustBe OK
      Cargos.fromJsValue(JsonParser(body.asString)) mustBe cs0a
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/cargos?merge=replace",cs1.json)) ~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/cargos")) ~> check {
      status mustBe OK
      Cargos.fromJsValue(JsonParser(body.asString)) mustBe cs1
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/cargos/01?merge=useold",c01a.json)) ~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/cargos/01")) ~> check {
      status mustBe OK
      Cargo.fromJsValue(JsonParser(body.asString)) mustBe c11
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/cargos/01?merge=usenew",c01a.json)) ~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/cargos/01")) ~> check {
      status mustBe OK
      Cargo.fromJsValue(JsonParser(body.asString)) mustBe c01a
    }

    manager(Put(s"/api/planes/$pid/alter/$alterSeq/cargos/01?merge=replace",c11.json)) ~> check {
      status mustBe OK
    }

    manager(Get(s"/api/planes/$pid/alter/$alterSeq/cargos/01")) ~> check {
      status mustBe OK
      Cargo.fromJsValue(JsonParser(body.asString)) mustBe c11
    }
  }


}
