package zzb.datatype

import java.io._

import org.scalatest.{MustMatchers, WordSpec}
import spray.json.DefaultJsonProtocol._
import spray.json._
import zzb.datatype.demo.BizSuite

import scala.reflect._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-1-17
 * Time: 上午10:45
 * Copyright baoxian.com 2012~2020
 */
class JsonTest extends WordSpec with MustMatchers {

  import zzb.datatype.testuse._

  def serializeTest(o: Serializable) = {
    val bs = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bs)
    out.writeObject(o)
    out.close()

    val oin = new ObjectInputStream(new ByteArrayInputStream(bs.toByteArray))
    val t = oin.readObject()
    oin.close()

    println("序列化前的对象----" + o)
    println("反序列化的对象----" + t)

    t mustBe o
  }

  "json" must {

    " support basic type" in {

      import zzb.datatype.testuse.UserInfo._
      val u1 = UserInfo(
        userName := "simon",
        userAge := 39,
        driverAge := 7,
        birthDay := "1999-11-22".toDateTime,
        male := true
      )

      val u1Json = u1.toJson
      val u2 = u1Json.convertTo[UserInfo.type#Pack]

      u1Json must equal(u2.toJson)
      println(u1Json)

      serializeTest(u1)
    }
  }

  "support List Type" in {
    import zzb.datatype.testuse.UserInfo._
    object Users extends TList[UserInfo.Pack] {

      val t_memo_ : String = "用户列表"

      override implicit val elementFormat = UserInfo.Format
      override val lm: ClassTag[_] = classTag[UserInfo.Pack]
    }
    val us1 = Users(List(
      UserInfo(userName := "Simon", userAge := 39),
      UserInfo(userName := "Jack", userAge := 10)
    ))

    val us1Json = us1.toJson

    val us2 = us1Json.toJson.convertTo[Users.type#Pack]

    val us3 = Users.fromJsValue(us1Json)
    val us4 = Users.fromJsValue(JsonParser(us1Json.prettyPrint))

    us3 mustBe us1
    us4 mustBe us1

    serializeTest(us1)

    us2.toJson must equal(us1Json)
    println(us1Json)

  }

  "support Struct Type" in {
    import zzb.datatype.demo.BizSuite._


    val suite0 = BizSuite(charge := 1000.1, discountRate := 0.9)

    val suite1 = suite0 ~ VehicleDemageIns.makeItem(1)


    val suite2 = suite1 ~ List(NcfVehicleDemageIns.makeItem(1), ThirdPartyIns.makeItem(1))


    val suite3 = BizSuite(charge := 1000.1, discountRate := 0.9, suite2.Items)
    suite3.Items.usedKeys.size must equal(3)

    serializeTest(suite3)

    val s1 = suite3.toJson

    val suite4 = s1.convertTo[BizSuite.type#Pack]

    suite4.toJson must equal(s1)

    println(s1)
  }

  "support TEnum Type" in {
    val b1 = BloodType.fromJsValue(TInt(1).toJsValue)
    b1.value.idx mustBe 1

    intercept[spray.json.DeserializationException] {
      BloodType.fromJsValue(TInt(100).toJsValue)
    }
    serializeTest(b1)
    val b2 = BloodType.fromJsValue(TString("A").toJsValue)
    b2.value.idx mustBe 1

    val b3 = BloodType.fromJsValue(TString("1").toJsValue)
    b3.value.idx mustBe 1

    intercept[spray.json.DeserializationException] {
      BloodType.fromJsValue(TString("100").toJsValue)
    }

    intercept[spray.json.DeserializationException] {
      BloodType.fromJsValue(TString("ABC").toJsValue)
    }

    //b1.idx mustBe 1
  }

}
