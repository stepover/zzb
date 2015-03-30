package zzb.datatype

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import spray.json.JsonFormat
import scala.reflect._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-18
 * Time: 下午4:37
 * Copyright baoxian.com 2012~2020
 */
class MacroTypeTest extends WordSpec with MustMatchers {

//  object NoThis extends TMonoType[Null] {
//
//    //val code: String = "user.noThis"
//
//    val t_name_ = "头发数量"
//    def parse(str: String) = ???
//
//    implicit val valueFormat: JsonFormat[Null] = null
//  }

  object UserInfo extends TStruct {

    val userName = Field(TString("user.name", "用户名"))
    val userAge = Field(TInt("user.age", "用户年龄"))
    val userHeight = Field(TInt("user.height", "用户身高"))
    val userAddress = Field(TString("user.address", "地址"))

    val birthDay = Field(TDateTime("user.birthday", "生日"))

    val isMale =   Field(TBoolean("user.isMale", "是好汉举手"))

    val isMarried =   Field(TBoolean("user.isMarried", "婚否","已婚","未婚"))

    val houseCount = FieldInt(TInt("houseCount","几间房"),default = ()=> 1)

    import BasicFormats._

    val addresses = Field(TList[String]("addresses","地址"))

    //val code: String = "the.user.info"

    val t_memo_ : String = "用户信息"
  }

  object CarInfo extends TStruct {

    val carLicense = Field(TString("car.license", "车牌号"))
    val carVin = Field(TString("car.vin", "车架号"))

    //val code: String = "the.car.info"

    val t_memo_ : String = "车辆信息"
  }

  object HomeInfo extends TStruct {

    val userInfo = Field(UserInfo)
    val carInfo = Field(CarInfo)

    //val friends = Field(tList[String]("friends","朋友"))

    //val ff = tList[TString]("friends","朋友")

    //val ii = classTag[TListType[_]].runtimeClass.isInstance(ff)

    //val code: String = "the.home.info"

    val t_memo_ : String = "家庭信息"
  }

  "MacroType" must {


    "init ok " in {

      val userInfo = UserInfo(UserInfo.userName()("Simon"), UserInfo.userAge()(38))

      userInfo(UserInfo.userName()) must equal(Some(UserInfo.userName()("Simon")))

      userInfo(UserInfo.userName()).get.value  must equal("Simon")

      //userInfo.apply("user.name") must equal(Some(UserInfo.userName()("Simon")))

//      userInfo(NoThis) must be(None)
    }
    "init ok when import for short  " in {
      import UserInfo._

      val userInfo = UserInfo(userName()("Simon"), userAge()(38),birthDay().parse("1999-11-22"),isMale()(true),isMarried()(true))

      userInfo(userName()) must equal(Some(userName()("Simon")))

      userInfo(userName()).get.value  must equal("Simon")

      userInfo(birthDay()).get.toString  must equal("1999-11-22 00:00:00")

      //userInfo("user.name") must equal(Some(userName()("Simon")))

      userInfo(isMale()).get.value must equal(true)
      userInfo(isMale()).get.toString must equal("true")
      userInfo(isMarried()).get.value must equal(true)
      userInfo(isMarried()).get.toString must equal("已婚")

//      userInfo(NoThis) must be(None)

    }
    "not accept invalid field" in {
      import UserInfo._
      val userInfo = UserInfo(userName()("Simon"), userAge()(38))
      userInfo(userName()) must equal(Some(userName()("Simon")))

//      userInfo(NoThis) must be(None)

      userInfo.fieldCount must equal(3) // 2 + 1 个缺省的houseCount ，一个缺省的 version_
    }
    "can add and del ValuePack" in {
      import UserInfo._
      val userInfo1 = UserInfo(userName()("Simon"))

      userInfo1(userAge()) must be(None)

      val userInfo2 = userInfo1 <~ userAge()(38)

      userInfo2(userAge()) must equal(Some(userAge()(38)))

      val userInfo3 = userInfo2 - userAge()

      userInfo3(userAge()) must be(None)
    }

    "can add and del List[ValuePack]" in {
      import UserInfo._

      val userInfo1 = UserInfo(userName()("Simon"))

      userInfo1(userAge()) must be(None)

      val userInfo2 = userInfo1 <~: List(userAge()(38))
      userInfo2(userAge()) must equal(Some(userAge()(38)))
//      userInfo2(NoThis) must be(None)

      val userInfo3 = userInfo1 - List(userAge(), userName())

      userInfo3(userName()) must be(None)
      userInfo3(userAge()) must be(None)
    }


    "two CompoundType instance override field values" in {
      import UserInfo._

      val userInfo1 = UserInfo(userName()("Simon"), userAge()(38))

      val userInfo2 = UserInfo(userName()("Jack"))

      val userInfo3 = userInfo1.value ->> userInfo2.value
      val userInfo4 = userInfo2.value ->> userInfo1.value

      userInfo3(userAge()) must equal(Some(userAge()(38)))
      userInfo3(userName()) must equal(Some(userName()("Simon")))

      userInfo4(userAge()) must equal(Some(userAge()(38)))
      userInfo4(userName()) must equal(Some(userName()("Jack")))

    }

    "construct from other" in {
      import UserInfo._
      val userInfo1 = UserInfo(userName()("Simon"), userAge()(38))
      val userInfo2 = UserInfo(userInfo1.value)

      userInfo2(userAge()) must equal(Some(userAge()(38)))
      userInfo2(userName()) must equal(Some(userName()("Simon")))
    }
    
    "nest CompoundType" in {
      import UserInfo._
      import CarInfo._
      val home = HomeInfo(
        UserInfo(userName()("Simon"), userAge()(38)),
        CarInfo(carLicense()("京GNR110"), carVin()("1234567"))
      )

      //home(UserInfo)(userName()) must equal(Some(userName()("Simon")))
      //home(UserInfo)(userAge()) must equal(Some(userAge()(38)))

      home(UserInfo.userAge()) must equal(Some(userAge()(38)))
      home(HomeInfo.userInfo().userAge()) must equal(Some(userAge()(38)))
      home(userAge()) must equal(Some(userAge()(38)))

      //home(CarInfo)(carLicense()) must equal(Some(carLicense()("京GNR110")))

      val home2 = home <~: home(UserInfo) <~: userAge()(39)
      //home2(UserInfo)(userAge()) must equal(Some(userAge()(39)))
      home2(UserInfo.userAge()) must equal(Some(userAge()(39)))

//      val home3 = home <~: home(NoThis) <~: userAge()(39)
      //home3(UserInfo)(userAge()) must equal(Some(userAge()(38)))
//      home3(UserInfo.userAge()) must equal(Some(userAge()(38)))
      //home3(UserInfo)(userHeight()) must be(None)

      val home4 = HomeInfo(
        UserInfo(userName()("Simon"), userAge()(38)))
      //home4(CarInfo)(carLicense()) must be(None)
      //home4(CarInfo)(carVin()) must be(None)

      //home4(HomeInfo.carInfo().carVin())(carLicense()) must be(None)
    }
    " := style value set " in {
      import UserInfo._

      val userInfo = UserInfo(
        userName := "Simon",
        userAge := 38
      )
      userInfo(userName()) must equal(Some(userName()("Simon")))
    }

    "tList can generator TList[T]" in {
      UserInfo.addresses().lm.toString() mustBe "java.lang.String"

    }

  }


}
