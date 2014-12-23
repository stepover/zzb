package zzb.datatype

import org.scalatest.{MustMatchers, WordSpec}
import zzb.datatype.testuse.path._


/**
 * Created by Simon on 2014/6/17
 */
class StructPathTest extends WordSpec with MustMatchers {

  import User._

  val u0 = User(
    name := "Simon",
    age := 39,
    height := 1.75F,
    blood := BloodType.AB,
    male := true,
    phones := List(123,456),
    misc := Map("water" -> 100, "nick" -> "hello", "date" -> "1978-12-03 06:32:33")
  ) <~ Car(
    Car.license := "京GNR110",
    Car.vin := "123456789"
  )

  val u1 = User(
    name := "jack",
    age := 40
  )

  "TStruct 数据修改" must {

    "可以直接修改结构的根路径" in {
      val path : NodePath = User.me

      val a1 = path.alterDomainData(u0,Some(u1),MergeManner.UseNew)
      a1(age).get.value mustBe 40
      a1(male).get.value mustBe true

      val a2 = path.alterDomainData(u0,Some(u1),MergeManner.UseOld)
      a2(age).get.value mustBe 39
      a2(male).get.value mustBe true

      val a3 = path.alterDomainData(u0,Some(u1),MergeManner.Replace)
      a3(age).get.value mustBe 40
      a3(male) mustBe None

      intercept[NullStructNodeDataException] {
        path.alterDomainData(u0,None,MergeManner.Replace)
      }
    }

    "可以直接修改结构直接子路径的数据" in {

      val path : NodePath = User.me.age

      val a1 = path.alterDomainData(u0,Some(TInt(40)),MergeManner.UseNew)
      a1(age).get.value mustBe 40
      a1(male).get.value mustBe true

      val a2 = path.alterDomainData(u0,None,MergeManner.UseNew)
      a2(age).get.value mustBe 39
      a2(male).get.value mustBe true

      val a3 = path.alterDomainData(u0,None,MergeManner.Replace)
      a3(age) mustBe None
      a3(male).get.value mustBe true
    }

    "可以修改结构嵌套子路径的数据" in {

      val path : NodePath = User.me.car().license()

      val a1 = path.alterDomainData(u0,Some(TString("new-car")),MergeManner.UseNew)
      a1(car().license()).get.value mustBe "new-car"
      a1(male).get.value mustBe true

      val a2 = path.alterDomainData(u0,None,MergeManner.UseNew)
      a2(car().license()).get.value mustBe "京GNR110"
      a2(male).get.value mustBe true

      val a3 = path.alterDomainData(u0,None,MergeManner.Replace)
      a3(car().license()) mustBe None
      a3(male).get.value mustBe true
    }

    "可以通过字段code获取嵌套字段的路径" in {
      val path1 : NodePath = User.me.car().license().path

      val path2  = User.me.getPathByCode(List("user","car","license")).get

      val path3  = User.me.getPathByCode("/user/car/license").get

      path1 mustBe path2

      path1 mustBe path3

      val subPath1 = User.me.getPathByCode(List("car","license")).get

      val subPath2 = User.me.getPathByCode("/car/license").get

      subPath1.toString() mustBe "/user/car/license"

      subPath1 mustBe subPath2

      subPath1 mustBe path1

      u0.valueByPath("/car/license").get mustBe "京GNR110"

    }

    "与字符串转换" in {
      val path : NodePath = User.me
      path.toString mustBe "/user"
      path.relativeStr mustBe "/"

      val path2 :NodePath = path.inStructPath.tail
      path2.toString mustBe "/"
    }

    "支持列表类型路径" in {

      val path1  =  User.me.getPathByCode("/phones").get
      val path2 =  User.me.getPathByCode("/user/phones").get

      path1 mustBe path2

      val pathListItem1 = User.me.getPathByCode("/phones/0").get
      val pathListItem2 = User.me.getPathByCode("/user/phones/0").get

      pathListItem1 mustBe pathListItem2

      u0.valueByPath("/user/phones/0").get mustBe 123

      u0.valueByPath("/phones/0").get mustBe 123

      intercept[StructPathException] {
        val pathListItem3 = User.me.getPathByCode("/user/phones/abc")
      }
    }

    "支持Map类型路径" in {

      val path1  =  User.me.getPathByCode("/misc").get
      val path2 =  User.me.getPathByCode("/user/misc").get

      path1 mustBe path2

      val pathListItem1 = User.me.getPathByCode("/misc/water").get
      val pathListItem2 = User.me.getPathByCode("/user/misc/water").get

      pathListItem1 mustBe pathListItem2

      u0.valueByPath("/user/misc/water").get mustBe 100

      u0.valueByPath("/misc/water").get mustBe 100

      u0.valueByPath("/user/misc/nick").get mustBe "hello"

      u0.valueByPath("/misc/nick").get mustBe "hello"

      intercept[StructPathException] {
        val pathListItem3 = User.me.getPathByCode("/misc/nick/seefe")
      }

      u0.valueByPath("/misc/nothis") mustBe None
    }

  }

}
