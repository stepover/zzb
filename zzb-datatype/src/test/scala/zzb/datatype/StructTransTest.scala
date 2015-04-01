package zzb.datatype

import org.scalatest.{MustMatchers, WordSpec}

/**
 * Created by Simon on 2015/4/1 
 */
class StructTransTest extends WordSpec with MustMatchers {

  "TStructType" must {
    "类型转换，两个类型共有字段同步复制" in {
      import zzb.datatype._

      val Height = TFloat("height","高度")
      val Weight = TFloat("weight","重量")
      val Years = TInt("years","年龄")
      val Name = TString("name","名称")

      object Color extends Enumeration with TEnum {
        val t_memo_ = "颜色"
        val W = Value(1, "白色")
        val B = Value(2, "黑色")
        val Y = Value(3, "黄色")
      }

      val dd: Color.Value = Color.W

      object Man extends TStruct{
        override val t_memo_ : String = "人类"

        val height = Field(Height)
        val weight = Field(Weight)
        val name = Field(Name)
        val years = Field(Years)
        val address = Field(TString("address","地址"))
        val color = Field(Color)

        val memo = Field(TString("memo","备注"))
      }

      object Car extends TStruct{
        override val t_memo_ : String = "人类"

        val height = Field(Height)
        val weight = Field(Weight)
        val name = Field(Name)
        val brand = Field(TString("brand","品牌"))
        val color = Field(Color)
        val memo = Field(TString("memo","备注"))
      }

      val m1: Man.Pack = Man(Man.years := 40, Man.height := 1.75f,Man.name := "Simon",Man.memo := "欢天喜地" ) <~~ List(Some(Color.W))

      val c1 = m1.to(Car)

      c1(Car.height()).get.value mustBe 1.75f
      c1(Car.name()).get.value mustBe "Simon"
      c1(Years) mustBe None
      c1(Car.brand()) mustBe None
      c1(Car.color()).get.value.idx mustBe 1

      //同名字段只要基础类型一样也会被复制
      c1(Car.memo()).get.value mustBe "欢天喜地"
      c1(Car.memo()).get.dataType mustBe Car.memo()
    }
  }

}
