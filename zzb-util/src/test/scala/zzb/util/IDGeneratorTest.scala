package zzb.util

import org.scalatest.WordSpec
import org.scalatest.MustMatchers

/**
 * Created by Simon on 2014/3/26
 */
class IDGeneratorTest extends WordSpec with MustMatchers {

  "IDGenerator" must {
    "generate 30000 id not duplicate,all id length is 19" in {
      import scala.collection.mutable
      val usedIds = mutable.Set[String]()

      var bitLengthIs19 = true

      val startTime = System.currentTimeMillis()

      val g = new IDGenerator(2,2)
      val total = 30000
      var idx = 0
      while (idx < total){
        idx += 1
        val id = g.next
        usedIds.add(id)
        if(bitLengthIs19) bitLengthIs19 = id.toString.length == 19
      }

      val endTime = System.currentTimeMillis()

      val usedSeconds = (endTime - startTime)/1000
      val usedMilliSeconds = (endTime - startTime)%1000

      println(s"used $usedSeconds seconds , $usedMilliSeconds milliseconds ")

      usedIds.size mustBe total
      bitLengthIs19 mustBe true
    }
  }

}
