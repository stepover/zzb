package zzb.datatype

import org.scalatest.{MustMatchers, WordSpec}
import testuse.path._

/**
 * Created by Simon on 2014/6/17
 */

/**
 * 测试异种数据结构之间的数据拷贝
 */
class DataCopyTest extends WordSpec with MustMatchers {

  import TStruct._
  import TStruct.{CopyField => ci}
  import User._

  val user = User(
    name := "Simon",
    age := 39,
    height := 1.75F,
    blood := BloodType.AB,
    male := true
  ) <~ Car(
    Car.license := "京GNR110",
    Car.vin := "123456789"
  )

  "异种数据类型直接" must {

    "可以拷贝第一级数据字段" in {
      val s0: Student.Pack = Student()
      val um = User.me
      val sm = Student.me
      val s1 = copy(user, s0, List(
        ci(um.name, sm.sname),
        ci(um.age, sm.sage),
        ci(um.car, sm.car),
        ci(um.male, sm.sex),
        ci(um.blood, sm.blood)
      ))

      s1(sm.sname()).get.value mustBe "Simon"
      s1(sm.sage()).get.value mustBe 39
      s1(sm.sex()).get.value mustBe true
      s1(sm.height()) mustBe None
      s1(sm.car().license()).get.value mustBe "京GNR110"
      s1(sm.car().vin()).get.value mustBe "123456789"
    }

    "可以拷贝多级嵌套的数据字段" in {
      val s0: Student.Pack = Student()
      val um = User.me
      val sm = Student.me
      val s1 = copy(user, s0, List(
        ci(um.name, sm.sname),
        ci(um.age, sm.sage),
        ci(um.car().license(), sm.car().license())
      ))

      s1(sm.sname()).get.value mustBe "Simon"
      s1(sm.sage()).get.value mustBe 39
      s1(sm.height()) mustBe None
      s1(sm.car().license()).get.value mustBe "京GNR110"
      s1(sm.car().vin()) mustBe None
    }

    "Replace 模式的copy动作也可以用来删除数据" in {
      val s0: Student.Pack = Student()
      val um = User.me
      val sm = Student.me
      val u1 = copy(s0, user,List(
        ci(sm.sage, um.age) // useNew 合并模式不会删除数据
      ))
      u1(um.age()).get.value mustBe 39

      val u2 = copy(s0, user,List(
        ci(sm.sage, um.age)
      ),MergeManner.Replace)  // Replace 模式下新数据如果不存在旧数据会被删除
      u2(um.age()) mustBe None
    }
  }
}

