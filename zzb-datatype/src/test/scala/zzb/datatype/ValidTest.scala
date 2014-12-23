package zzb.datatype

import org.scalatest.WordSpec
import org.scalatest.MustMatchers

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-24
 * Time: 下午7:39
 * Copyright baoxian.com 2012~2020
 */
class ValidTest extends WordSpec with MustMatchers {

  object UserName extends TString {
    val t_memo_ : String = "username"

    override val doPreValidate = true

    override def validators  = List(minLength(3))

    private def minLength(minLen :Int) = (s:UserName.Pack) => if(s.length >= minLen) None else Some(s"length must >= $minLen")
  }

  "Data valid" must {
    "failed on invalid init value" in {

      val n1 = UserName("simon")
      n1.value must equal("simon")

      intercept[DataInitValidException]{
        val n2 = UserName("Hi")
      }



    }
  }

}
