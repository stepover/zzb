package zzb.datatype

import org.scalatest.WordSpec
import org.scalatest.MustMatchers

/**
 * Created by Simon on 2014/4/10
 */
class AnyToPackTest extends WordSpec with MustMatchers {

  import testuse._

  import UserInfo._

  "AnyToPack " must {
    val np1 = UserName("Simon")
    val userInfo = UserInfo(userName := "Simon", memo := "hello")
    "支持TMonoType的值直接转换为对应的Pack" in {
      val np2 = UserName.AnyToPack("Simon")
      np2.get mustBe np1

      val np4 = UserName.AnyToPack(123)
      np4 mustBe None
    }

    "支持 Pack to Pack " in {
      val np3 = UserName.AnyToPack(np1)
      np1 mustBe np3.get

      userInfo.field(userName()) mustBe Some(np1)
      userName().AnyToPack(np1) mustBe Some(np1)

      val mm = userInfo.field(memo())
      memo().AnyToPack(mm.get) mustBe mm
    }

    "如果两个TMonoType 的值类型是同样的，允许互相转换" in {
      memo().AnyToPack(TString("hello")) mustBe userInfo.field(memo())
    }

    "支持枚举类型从数字或名称转换成Pack" in {
      BloodType.AnyToPack(1) mustBe Some(BloodType.int2EnumPack(1))
      BloodType.AnyToPack(4) mustBe Some(BloodType.int2EnumPack(4))
      BloodType.AnyToPack(5) mustBe None
      BloodType.AnyToPack("A") mustBe Some(BloodType.int2EnumPack(1))
      BloodType.AnyToPack("G") mustBe None
    }
    "支持列表类型从列表数据转换成Pack" in {
      import UserInfo._
      val users = List(
        UserInfo(userName := "Simon", userAge := 39),
        UserInfo(userName := "jack", userAge := 9)
      )
      val usersPack = Users(users)

      Users.AnyToPack(usersPack) mustBe Some(usersPack)

      Users.AnyToPack(users) mustBe Some(usersPack)

      Users.AnyToPack(List(1,2,3)) mustBe None
      Users.AnyToPack(List()) mustBe Some(Users(List()))
    }
  }

}
