package zzb.datatype

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import testuse._
import zzb.datatype.meta.{EnumTypeInfo, TypeInfo}

/**
 * Created by Simon on 2014/4/17
 */
class MetaInfoTest extends WordSpec with MustMatchers {

  "基本类型元信息" in {

    val m0 = BloodType.typeInfo
    val m1 = UserInfo.birthDay().typeInfo
    val mm = UserInfo.typeInfo
    UserName.typeInfo.valueCode mustBe "String"
    UserAge.typeInfo.valueCode mustBe "int"
    UserInfo.birthDay().typeInfo.valueCode mustBe "DateTime"
    UserInfo.male().typeInfo.valueCode mustBe "boolean"

    val j0 = TypeInfo.format.write(m1)

    println(j0)

    val j1 = TypeInfo.format.read(j0)

    j1.valueCode mustBe m1.valueCode

    val bt = UserInfo.blood().typeInfo


    val n = UserInfo.getClass.getName
    mm.valueCode mustBe UserInfo.structName
    mm.simpleCode mustBe "UserInfo"

    val userInfoJson = TypeInfo.format.write(UserInfo.typeInfo)

    println(userInfoJson)

    //TypeInfo.format.read(userInfoJson) mustBe UserInfo.typeInfo

    val jm0=EnumTypeInfo.format.write(m0)
    //EnumTypeInfo.format.read(jm0) mustBe m0


  }
}
