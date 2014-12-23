

package zzb.datatype

import java.io._

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import testuse._
import spray.json.JsonParser


class StructTypeTest extends WordSpec with MustMatchers {


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

  "DataType" must {
    "implicit can work" in {
      val plateNumber: UserName.Pack = "Simon"
      plateNumber.value must equal("Simon")
    }
  }

  "TStructType" must {
    "init ok " in {
      import UserInfo._

      val userInfo = UserInfo(UserName("Simon"), userAge()(38))

      userInfo(UserName) must equal(Some(UserName("Simon")))
      serializeTest(userInfo)

      val noTypeSafe: Option[ValuePack[Any]] = userInfo.withCode("UserName")
      val typeSafe: Option[ValuePack[String]] = userInfo(UserName)

      userInfo.withCode("UserName") must equal(Some(UserName("Simon")))

      userInfo(NoThis) must be(None)
    }

    "construct and field access" in {
      val userInfo = UserInfo(UserName("Simon"), UserAge(38))
      userInfo(UserName) must equal(Some(UserName("Simon")))

      userInfo.field(UserName) must equal(Some("Simon"))

      userInfo(NoThis) must be(None)

      userInfo.fieldCount must equal(3 + 1) //有两个缺省字段


      val bt: BloodType.Pack = userInfo.field(BloodType).get

      BloodType.enumValue2Pack(BloodType.A) must equal(bt)
    }

    "two different StructType share same field type" in {
      val userInfo = UserInfo(UserName("Simon"))

      val student = Student(userInfo(UserInfo.userName()).get)
      student(Student.userName()) must equal(Some(UserName("Simon")))
    }

    "can add and del ValuePack" in {
      import UserInfo._
      val userInfo1 = UserInfo(UserName("Simon"))

      userInfo1(userAge) must be(None)

      val userInfo2 = userInfo1 <~ userAge()(38)

      userInfo2(userAge) must equal(Some(UserAge(38)))

      (userInfo2.revise - userInfo1.revise) must equal(1)

      val userInfo3 = userInfo2 - userAge()

      userInfo3(userAge) must be(None)

      val userInfo4 = userInfo2 <~ driverAge()(12)

      userInfo4(userAge) must equal(Some(userAge()(38)))

      userInfo1.userName mustBe "Simon"

      HomeInfo.userInfo()

      val userInfo1_1 = UserInfo.fromJsValue(JsonParser(userInfo1.toJsValue.toString()))
      UserInfo._debug = true
      UserName._debug = true
      userInfo1_1.userName mustBe "Simon"

    }

    "can add and del List[ValuePack]" in {
      val userInfo1 = UserInfo(UserName("Simon"))

      userInfo1(UserAge) must be(None)

      val userInfo2 = userInfo1 <~~ List(UserAge(38), DriverAge(8))
      userInfo2(UserAge) must equal(Some(UserAge(38)))
      userInfo2(NoThis) must be(None)

      val userInfo3 = userInfo1 - List(UserAge, DriverAge)

      userInfo3(DriverAge) must be(None)
      userInfo3(UserAge) must be(None)
    }

    "two TStructType instance override field values" in {
      val userInfo1 = UserInfo(UserName("Simon"), UserAge(38))

      val userInfo2 = UserInfo(UserName("Jack"))

      val userInfo3 = userInfo1 ->> userInfo2
      val userInfo4 = userInfo2 ->> userInfo1

      userInfo3(UserAge) must equal(Some(UserAge(38)))
      userInfo3(UserName) must equal(Some(UserName("Simon")))

      userInfo4(UserAge) must equal(Some(UserAge(38)))
      userInfo4(UserName) must equal(Some(UserName("Jack")))

    }
    "construct from other" in {
      val userInfo1 = UserInfo(UserName("Simon"), UserAge(38))
      val userInfo2 = UserInfo(userInfo1.value)
      val userInfo3 = UserInfo(userInfo2)

      userInfo2(UserAge) must equal(Some(UserAge(38)))
      userInfo2(UserName) must equal(Some(UserName("Simon")))

      userInfo3(UserAge) must equal(Some(UserAge(38)))
      userInfo3(UserName) must equal(Some(UserName("Simon")))

      userInfo3 eq userInfo2 mustBe false
    }

    "nest TStructType" in {

      import UserInfo._

      def thorough(fieldPath: List[() => DataType[Any]]): DataType[Any] = {
        fieldPath match {
          case fieldFunc :: Nil => fieldFunc()
          case fieldFunc :: tail =>
            fieldFunc()
            thorough(tail)
        }
      }
      val home = HomeInfo(
        UserInfo(UserName("Simon"), UserAge(38)),
        CarInfo(CarLicense("京GNR110"), CarVin("1234567"))
      )

      serializeTest(home)


      home(UserInfo)(UserName) must equal(Some(UserName("Simon")))
      home(UserInfo)(UserAge) must equal(Some(UserAge(38)))

      home(UserInfo.userAge) must equal(Some(UserAge(38)))
      home(HomeInfo.userInfo().userAge()) must equal(Some(UserAge(38)))
      home(HomeInfo.userInfo().userAge) must equal(Some(UserAge(38)))
      home(HomeInfo.userInfo.userAge) must equal(Some(UserAge(38)))
      home(UserAge) mustBe None

      home.subStruct(UserInfo).get.userName mustBe "Simon"

      val uInfo: Option[UserInfo.Pack] = home.field(UserInfo)

      uInfo.get(userAge())  must equal(Some(UserAge(38)))

      val fieldPath: List[() => DataType[Any]] =
        List(HomeInfo.userInfo, HomeInfo.userInfo().userAge)

      home(thorough(fieldPath)) must equal(Some(UserAge(38)))

      val uInfoValue0 = uInfo.get

      val uInfoValue1 = uInfoValue0 <~:  UserAge(40)

      uInfoValue1(UserAge).get.value mustBe 40


      home(CarInfo)(CarLicense) must equal(Some(CarLicense("京GNR110")))

      val home2 = home <~: home(UserInfo) <~: UserAge(39)
      home2(UserInfo)(UserAge) must equal(Some(UserAge(39)))
      home2(UserInfo.userAge()) must equal(Some(UserAge(39)))

      val home3 = home <~: home(NoThis) <~: UserAge(39)
      home3(UserInfo)(UserAge) must equal(Some(UserAge(38)))
      home3(UserInfo.userAge()) must equal(Some(UserAge(38)))
      home3(UserInfo)(UserHeight) must be(None)

      val home4 = HomeInfo(
        UserInfo(UserName("Simon"), UserAge(38)))
      home4(CarInfo)(CarLicense) must be(None)
      home4(CarInfo)(CarVin) must be(None)

      home4(HomeInfo.carInfo().carVin())(CarLicense) must be(None)

    }

    "通过字段路径进行赋值动作" in {

      def setDocField(oldDoc:  ValuePack[Any],
                      fieldPath: List[() => DataType[Any]],
                      fieldValue: ValuePack[Any]): ValuePack[Any] = {
        fieldPath match {
          case head :: Nil => oldDoc <~: fieldValue
          case head :: tail =>
            oldDoc(head()) match {
              case None => oldDoc
              case Some(d) => oldDoc <~: setDocField( d, tail, fieldValue)
            }

        }
      }

      val home0 = HomeInfo(
        UserInfo(UserName("Simon"), UserAge(38)),
        CarInfo(CarLicense("京GNR110"), CarVin("1234567"))
      )

      val home1 = setDocField( home0,
        List(HomeInfo.userInfo, HomeInfo.userInfo().userAge),UserAge(39))

      home1(UserInfo)(UserAge) must equal(Some(UserAge(39)))

      val pathStr :String = HomeInfo.me.userInfo().userAge().path
      pathStr mustBe "/homeInfo/userInfo/userAge"
      val pf = HomeInfo.me.userInfo().userAge().path

      home1(pf.map(p => p()).reverse.head) mustBe Some(UserAge(39))
    }

    ":= style value set " in {
      import UserInfo._
      val userInfo1 = UserInfo(
        userName := "Simon",
        userAge := 38
      )
      userInfo1(UserName) must equal(Some(UserName("Simon")))

      //      val userInfo2 = userInfo1 <~ (blood := 1)
      //
      //      userInfo2(blood) must equal(Some(BloodType(1)))

      val userInfo3 = userInfo1(userName := "wolfgang", userAge := 7)

      userInfo3(UserName) must equal(Some(UserName("wolfgang")))
      userInfo3(userAge) must equal(Some(UserAge(7)))

    }

    "requited field" in {
      object Work extends TStruct {
        val t_memo_ : String = "work"

        val jobName = Field(TString("jobName", "名称"), isRequired = true)
        val hours = Field(TInt("hours", "工时"))
      }

      import Work._
      val w1 = Work(hours := 5, jobName := "杀 10 匹野狼")

      w1.isOnlyRequireFields mustBe false

      w1(hours).get.value must equal(5)

//      intercept[RequiredFieldNotSetException] {
//        val w2 = Work(hours := 5)
//      }

      val w2 = Work(hours := 5)
      w2.validate.head mustBe "fields [jobName] is required."

      w2.isOnlyRequireFields mustBe false

      val w3 = Work( jobName := "杀 10 匹野狼")

      w3.isOnlyRequireFields mustBe true
    }

    "update can change one field value" in {

      def inc(count: Int) = (ori: Int) => ori + count

      val userInfo1 = UserInfo(UserName("Simon"))

      //userInfo1.version must equal(0)

      val userInfo2 = userInfo1.update(Ver, inc(1))
      //userInfo2.version must equal(1)

    }
  }

  "null value" must {
    "can be constructed by <~ with exception" in {
      import CarInfo._
      val car = CarInfo() <~ CarVin("".split(",")(2)) <~ CarLicense("京GNR110")
      car(carLicense) mustBe Some(CarLicense("京GNR110"))
      car(carVin) mustBe None
      serializeTest(car)

    }
    "can be constructed with Type" in {
      val car = CarInfo(CarLicense("京GNR110"))

    }
  }

  "类型识别" must {
    "TStructType的实例能够在运行时识别出自己是TStructType" in {
      HomeInfo.isInstanceOf[TStruct] mustBe true
      //HomeInfo.isInstanceOf[TString] mustBe false
      HomeInfo.isInstanceOf[TMono[_]] mustBe false
      UserName.isInstanceOf[TMono[_]] mustBe true
      UserName.isInstanceOf[TString] mustBe true
    }
  }

}


