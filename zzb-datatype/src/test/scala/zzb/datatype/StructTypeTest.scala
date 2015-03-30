

package zzb.datatype

import java.io._

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import testuse._
import spray.json.JsonParser
import zzb.datatype


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

    t.toString mustBe o.toString
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

      userInfo.fieldCount must equal(6) //有两个缺省字段


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
      import Address._
      import UserInfo._

      val add1 = Address(country := "china",province := "aaa")
      val add2 = Address(country := "china",province := "bbb",city := "guangzhou")

      val userInfo1 = UserInfo(UserName("Simon"), UserAge(38),add1,misc := Map("water" -> 100, "price" -> 23.1, "date" -> "1978-12-03 06:32:33"))

      val userInfo2 = UserInfo(UserName("Jack"),add2,misc := Map("food" -> 500))

      val userInfo3 = userInfo1 ->> userInfo2
      val userInfo4 = userInfo2 ->> userInfo1

      userInfo3(UserAge) must equal(Some(UserAge(38)))
      userInfo3(UserName) must equal(Some(UserName("Simon")))
      val add3 = userInfo3.address
      add3(Address.province()).get.value mustBe "aaa"
      add3(Address.city()).get.value mustBe "guangzhou"
      userInfo3.misc("water").get.toInt mustBe 100
      userInfo3.misc("food").get.toInt mustBe 500

      userInfo4(UserAge) must equal(Some(UserAge(38)))
      userInfo4(UserName) must equal(Some(UserName("Jack")))
      val add4 = userInfo4.address
      add4(Address.province()).get.value mustBe "bbb"
      add4(Address.city()).get.value mustBe "guangzhou"
      userInfo4.misc("water").get.toInt mustBe 100
      userInfo4.misc("food").get.toInt mustBe 500

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

      val nullString :String = null
      val userInfo1 = UserInfo(
        userName := Some("Simon"),
        userAge := Some(38),
        driverAge := 10,
        memo := nullString
      )

      userInfo1(UserName) must equal(Some(UserName("Simon")))

      //      val userInfo2 = userInfo1 <~ (blood := 1)
      //
      //      userInfo2(blood) must equal(Some(BloodType(1)))

      val userInfo3 = userInfo1.make(userName := "wolfgang", userAge := 7)

      userInfo3(UserName) must equal(Some(UserName("wolfgang")))
      userInfo3(userAge) must equal(Some(UserAge(7)))
      serializeTest(userInfo1)

      val userInfoA = UserInfo(
        userName := Some("Simon"),
        userAge := TInt(30),
        memo := nullString
      )

      userInfoA(userAge).get.value mustBe 30

      val userInfoB = UserInfo(
        userName := Some("Simon"),
        driverAge := userInfoA(userAge),
        memo := nullString
      )

      userInfoB(driverAge).get.value mustBe 30

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

    "can be constructed by <~ with null" in {
      import CarInfo._
      val vin:String  = null
      val car = CarInfo() <~ CarVin(vin) <~ CarLicense("京GNR110")
      car(carLicense) mustBe Some(CarLicense("京GNR110"))
      car(carVin) mustBe None
      serializeTest(car)

    }

    "类型转换，两个类型共有字段同步复制" in {
      import zzb.datatype._

      val Height = TFloat("height","高度")
      val Weight = TFloat("weight","重量")
      val Years = TInt("years","年龄")
      val Name = TString("name","名称")

      object Man extends TStruct{
        override val t_memo_ : String = "人类"

        val height = Field(Height)
        val weight = Field(Weight)
        val name = Field(Name)
        val years = Field(Years)
        val address = Field(TString("address","地址"))

        val memo = Field(TString("memo","备注"))
      }

      object Car extends TStruct{
        override val t_memo_ : String = "人类"

        val height = Field(Height)
        val weight = Field(Weight)
        val name = Field(Name)
        val brand = Field(TString("brand","品牌"))
        val memo = Field(TString("memo","备注"))
      }

      val m1: Man.Pack = Man(Man.years := 40, Man.height := 1.75f,Man.name := "Simon",Man.memo := "欢天喜地" )

      val c1 = m1.to(Car)

      c1(Car.height()).get.value mustBe 1.75f
      c1(Car.name()).get.value mustBe "Simon"
      c1(Years) mustBe None
      c1(Car.brand()) mustBe None

      //同名字段只要基础类型一样也会被复制
      c1(Car.memo()).get.value mustBe "欢天喜地"
      c1(Car.memo()).get.dataType mustBe Car.memo()
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


