package zzb.datatype

import java.io._

import org.scalatest.{MustMatchers, WordSpec}


/**
 * Created by Simon on 2014/4/29
 */
class MapTypeTest extends WordSpec with MustMatchers {

  import zzb.datatype.BasicFormats._

  val Colors = TMap[String, Int]("colors", "colors")

  object House extends TStruct {
    val colors = Field(Colors)
    override val t_memo_ : String = "多彩房屋"
  }

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


  "TMap" must {

    "支持覆盖操作" in {
      val c1 = Colors(Map("white" -> 1, "red" -> 2, "black" -> 5))
      val c2 = Colors(Map("white" -> 1, "red" -> 3, "blue" -> 4))

      serializeTest(c1)

      val c1to2 = c1 ->> c2
      val c2to1 = c2 ->> c1

      c1to2.size mustBe 4
      c2to1.size mustBe 4

      c1to2("red").get mustBe 2
      c2to1("red").get mustBe 3
      c1to2("black").get mustBe c2to1("black").get
    }

    "集合字段赋值" in {
      import House._
      val h0 = House(colors := Map("White" -> 1, "Red" -> 2))

      val cc = h0(colors).get.value
      cc("White") mustBe 1

      intercept[IllegalArgumentException] {
        House(colors := Map("White" -> "1", "Red" -> "2"))

      }
    }
  }


}
