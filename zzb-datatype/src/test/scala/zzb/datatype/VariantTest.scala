package zzb.datatype

import org.joda.time.DateTime
import org.scalatest.{MustMatchers, WordSpec}

/**
 * Created by Simon on 2014/6/27
 */
class VariantTest extends WordSpec with MustMatchers {

  "Variant Type " must {
    "与数字类型之间进行转换" in {

      val b0 :Byte = 123
      val bv = TVariant(b0)
      val b1 :Byte = bv
      b0 mustBe b1
      bv mustBe b1
      bv.isBoolean mustBe false
      bv.isDateTime mustBe false
      bv.isNumber mustBe true
      val v : TVariant.Pack = b0
      v mustBe bv

      val bv2 = TVariant("234")

      val b2:Int = bv2
      b2 mustBe 234

      val s0 :Short = 1234
      val sv = TVariant(s0)
      val s1 :Short = sv
      s0 mustBe s1
      sv mustBe s1

      val i0 :Int = 12345
      val iv = TVariant(i0)
      val i1 :Int = iv
      i0 mustBe i1
      iv mustBe i1

      val d0 :Double = 12345.54321
      val dv = TVariant(d0)
      val d1 :Double = dv
      d0 mustBe d1
      dv mustBe d1
    }

    "与Bool类型之间进行转换" in {
      val bv = TVariant(true)
      val b1 :Boolean = bv
      b1 mustBe true
      bv mustBe true
    }

    "与 DateTime 之间的转换" in {
      val dv = TVariant("1978-12-03 06:32:33")
      dv.isBoolean mustBe false
      dv.isDateTime mustBe true

      val d1 :DateTime = dv
      TDateTime.date2String(d1) mustBe "1978-12-03 06:32:33"
    }
  }
}
