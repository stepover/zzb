package zzb.datatype

import java.io._

import org.scalatest.{MustMatchers, WordSpec}
import spray.json.JsonParser

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-30
 * Time: 下午7:35
 * Copyright baoxian.com 2012~2020
 */
class BasicTypeTest extends WordSpec with MustMatchers {

  object Name extends TString {
    val t_memo_ : String = "姓名"
  }

  object Age extends TInt {
    val t_memo_ : String = "年龄"
  }

  object Height extends TFloat {
    val t_memo_ : String = "身高"
  }

  object BirthDay extends TDateTime {
    //val code: String = "birthday"

    val t_memo_ : String = "birthday"
  }

  object IsNewCar extends TBoolean {

    val t_memo_ : String = "是否新车"

    override val YesName = "是的"
    override val NoName = "不是"
  }

  object Color extends Enumeration with TEnum {

    val t_memo_ : String = "Color"

    val White = Value(1, "white")
    val Black = Value(2, "black")
    val Blue = Value(3, "blue")
  }

  //object Color extends ColorBase with TEnum[ColorBase#Value]{

  //    val t_name_ : String = "Color"
  //
  //
  //    implicit def int2EnumValue(id: Int) = this(id)
  //
  //    implicit def nameEnumValue(name: String) = withName(name)
  //
  //    implicit def int2EnumPack(id: Int) = Pack(this(id))
  //
  //    implicit def name2EnumPack(name: String) = Pack(withName(name))
  //  }

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

  "Basic types " must {
    "can be set and read" in {
      val s = TString("zhangsan")
      serializeTest(s)


      val np = Name("Simon")
      np.value must equal("Simon")
      serializeTest(np)

      val age = Age(38)
      age.value must equal(38)
      serializeTest(age)

      val height = Height(1.75F)
      height must equal(1.75F)
      serializeTest(height)
    }
  }

  "DateTimeType" must {
    " implicitly support multi-format " in {

      val bd1: BirthDay.Pack = "1999-11-22"
      bd1.value.getYear must equal(1999)
      bd1.value.getMonthOfYear must equal(11)

      bd1.toString must equal("1999-11-22 00:00:00")

      val bd2: BirthDay.Pack = "1999-11-22 20:22:13"
      bd2.value.getYear must equal(1999)

      val bd3: BirthDay.Pack = "21:22:44"
      bd3.value.getMonthOfYear must equal(1)

      val json1 = "\"1999-11-22 20:22:13.001\""

      val fromJson1 = TDateTime.fromJsValue(JsonParser(json1))

      val json2 = "\"1999-11-22 20:22:13\""

      val fromJson2 = TDateTime.fromJsValue(JsonParser(json2))

      intercept[IllegalArgumentException] {
        val bd4: BirthDay.Pack = "1999-11"
        bd4.value.getMonthOfYear must equal(1999)
      }

      //      val c: Color.Value = 1
      //      val bd5: BirthDay.Item  = c     //compile failed
    }

    "implicitly format output 1" in {
      val bd: BirthDay.Pack = "1999-11-22 20:22:13"

      BirthDay.format(bd) must equal("1999-11-22 20:22:13")
    }

    "implicitly format output 2" in {
      val bd: BirthDay.Pack = "1999-11-22 20:22:13"

      implicit val pattern = "yyyy-MM-dd"

      BirthDay.format(bd) must equal("1999-11-22")
    }
  }

  "BooleanType" must {
    " implicitly support multi-text " in {
      val is: IsNewCar.Pack = "是的"
      is.value must equal(true)
      val no: IsNewCar.Pack = "不是"

      val y: IsNewCar.Pack = "y"
      val n: IsNewCar.Pack = ""

      val yText: String = y
      val nText: String = n
      yText must equal("是的")
      nText must equal("不是")

      n.toString must equal("不是")
      y.toString must equal("是的")


      y.value must equal(true)
      n.value must equal(false)

      no.value must equal(false)
      val nv: Boolean = no

      nv == false must equal(true)

      intercept[Exception] {
        if (!no) {
          throw new Exception("test")
        }
      }

      intercept[Exception] {
        if (is) {
          throw new Exception("test")
        }
      }

      intercept[IllegalArgumentException] {
        val nothis: IsNewCar.Pack = "没有这个"
      }

      val yInt: IsNewCar.Pack = 1
      val nInt: IsNewCar.Pack = 0
      yInt.value must equal(true)
      nInt.value must equal(false)
    }
  }

  "Enum " must {
    "toText output id" in {
      val w: Color.Pack = Color.White
      w.toString must equal("1")
    }

    " implicitly from id/name to emum value/item" in {
      val c: Color.Value = 1
      c must equal(Color.White)

      val c1: Color.Pack = c
      c1.value must equal(EnumIdx(1))

      val c2: Color.Pack = 1
      c2.value must equal(EnumIdx(1))

      val c3: Color.Value = "white"
      //c must equal(Color.White)
    }

    " implicitly from  emum value/item to id/name " in {
      val id: Int = Color.White
      id must equal(1)

      val name: String = Color.White
      name must equal("white")

      val c: Color.Pack = 1

      val id2: Int = c
      id2 must equal(1)

      val name2: String = c
      name2 must equal("white")

    }
  }
}
