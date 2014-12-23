package zzb.datatype

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import spray.json.JsonFormat

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-23
 * Time: 下午4:05
 * Copyright baoxian.com 2012~2020
 */
class FuncTypeTest extends WordSpec with MustMatchers {


  "Func0Type" must {
    def hello: String = "hello world"

    object StrFunc extends TFunc0[String] {
      override def parse(str: String) = () => str

      val t_memo_ : String = "helloFunc"
      implicit val valueFormat: JsonFormat[() => String] = null
    }

    "init and exe ok" in {
      val fPack1 = StrFunc(hello _)
      fPack1.value() must equal("hello world")

      val fPack2 :StrFunc.Pack = hello _
      fPack2.value() must equal("hello world")
    }
  }

  "Func1Type" must {

    def plusN(step:Int)(in :Int) = in + step

    object PlusNFunc extends TFunc1[Int,Int]{
      val t_memo_ : String = "plusN"
      override def parse(str: String) = plusN(str.toInt) _

      implicit val valueFormat: JsonFormat[(Int) => Int] = null
    }

    "init and exe ok" in {

      val fPack1 = PlusNFunc(plusN(3) _)
      fPack1.value(1) must equal(4)

      val fPack2 :PlusNFunc.Pack = plusN(3) _
      fPack2.value(1) must equal(4)

      val fPack3 = PlusNFunc.parse("5")
      fPack3.value(5) must equal(10)
    }
  }

  "Func2Type" must {

    def plusN(ext:Int)(in1 :Int,in2:Int) = ext + in1 + in2

    object PlusNFunc extends TFunc2[Int,Int,Int]{
      val t_memo_ : String = "plusN"
      override def parse(str: String) = plusN(str.toInt) _

      implicit val valueFormat: JsonFormat[(Int, Int) => Int] = null
    }

    "init and exe ok" in {

      val fPack1 = PlusNFunc(plusN(3) _)
      fPack1.value(1,1) must equal(5)

      val fPack2 :PlusNFunc.Pack = plusN(3) _
      fPack2.value(1,1) must equal(5)

      val fPack3 = PlusNFunc.parse("5")
      fPack3.value(5,1) must equal(11)
    }
  }

}
