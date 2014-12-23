package zzb.datatype

import org.scalatest.{MustMatchers, WordSpec}

/**
 * Created by Simon on 2014/6/19
 */
class StructXorTest extends WordSpec with MustMatchers {

  import testuse.path._
  import User._

  val u1 = User(
    name := "Simon",
    age := 39,
    height := 1.75F,
    blood := BloodType.AB,
    male := true
  ) <~ Car(
    Car.license := "京GNR110",
    Car.vin := "123456789"
  )

  val u2 = User(
    name := "Simon",
    age := 40,
    height := 1.75F,
    blood := BloodType.AB,
    male := true
  ) <~ Car(
    Car.license := "京GNR110",
    Car.vin := "abcefg"
  )

  "异或操作" must {
    "保留差异字段，剔除相同字段，保留必填字段" in {

      val r1 = u1 xor u2
      r1(name).get.value mustBe "Simon" //必填项即使相等也保留
      r1(age).get.value mustBe 39  //不同项目保留左边的
      r1(height) mustBe None // 相同项目被剔除
      r1(car().vin()).get.value mustBe "123456789" //异或操作可以对 TStruct 类型的字段嵌套生效
      r1(car().license()) mustBe None

      val r2 = u2 xor u1
      r2(name).get.value mustBe "Simon" //必填项即使相等也保留
      r2(age).get.value mustBe 40  //不同项目保留左边的
      r2(height) mustBe None // 相同项目被剔除

      r2(car().vin()).get.value mustBe "abcefg"
      r2(car().license()) mustBe None

    }

    "一个Struct包含另一个Struct"  in {
      val u3 = User(
        name := "Simon",
        age := 39,
        height := 1.75F
      )

      val u4 = User(
        name := "Simon",
        age := 39,
        height := 1.75F,
        blood := BloodType.AB,
        male := true
      ) <~ Car(
        Car.license := "京GNR110",
        Car.vin := "123456789"
      )

      val diff = u3.xor(u3).isOnlyRequireFields

      diff mustBe true

      val diff2 = u3.xor(u4).isOnlyRequireFields

      diff2 mustBe false
    }
  }

}
