package zzb.datatype

import org.scalatest.{MustMatchers, WordSpec}
import zzb.datatype.testuse.UserInfo


/**
 * Created by Simon on 2014/6/27
 */
class TPropertyTest extends WordSpec with MustMatchers {

  "Property " must {

    import UserInfo._
    val u0: UserInfo.Pack = UserInfo(userName := "simon",
      misc := Map("water" -> 100, "price" -> 23.1, "date" -> "1978-12-03 06:32:33"))

    u0.misc("water") mustBe Some(100)
    u0.misc("water") mustBe Some("100")


    u0.misc("water").get.isNumber mustBe true

    u0.misc("water").get.toInt mustBe 100

    u0.misc("water999").getOrElse("33") mustBe "33"

    val p0 = u0.misc
    val p1 = p0 + ("age" -> 40)

    val u1 = u0 <~ (u0.misc + ("age" -> 40))
    u1.misc("age") mustBe Some(40)
    u1.misc("age") mustBe Some("40")

    val u2 = u1 <~ (u1.misc - "water")
    u2.misc("water") mustBe None
    u2.misc.getOrElse("water", "nothis") mustBe "nothis"
    u2.misc("age") mustBe Some(40)

  }

}

