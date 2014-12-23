package zzb.util

import com.github.nscala_time.time.Imports._
import scala.util.Random
import scala.annotation.tailrec

/**
 * Created by Simon on 2014/3/26
 */

/*
ID 生成规则

规则：
1. ID用正长整数表示，最长为19位，
2. 第一位为ID类型，每个类型一个固定值，因为正长整数最大值为
   9223372036854775807 ，所以第一位的取值范围为 1 ~ 8 ，共8个可能(0保留不用)
3. 末尾四位4为随机码，一秒钟可有10000个

多方报价：

固定值(5)  区   日   月   年   时   分  秒   随机码
1         2    2    2    2    2   2   2     4

单方报价：

固定值(6)  区   日   月   年   时   分  秒   随机码
1         2    2    2    2    2   2   2     4

订单：

固定值(8)  区   日   月   年   时   分  秒   随机码
1         2    2    2    2    2   2   2     4

*/

class IDGenerator(fixValue: Byte, district: Byte) {

  require(fixValue >= 1 && fixValue <= 8)
  require(district >= 0 && district <= 99)

  import scala.collection.mutable

  //日月年时分秒组两位毫秒成的14位字符串
  var currentBase = timeBase
  val usedIds = mutable.Set[Int]()

  val random = new Random(System.currentTimeMillis)

  val districtStr = if (district < 10) "0" + district else district.toString


  @tailrec final def next: String = {
    def genRandomId: Long = {
      //调用随机生成器生成100以内的随机数，如果生成的随机数已经使用就再试
      //共试10次，10次都重复就返回-1
      for (i <- 1 to 20) {
        val randomId = random.nextInt().abs % 100
        if (!usedIds.contains(randomId)) return {
          usedIds.add(randomId)
          randomId
        }
      }
      -1
    }
    val newTimeBase = timeBase
    if (newTimeBase != currentBase) reTimeBase(newTimeBase)

    val randomId = genRandomId
    if (randomId > 0) {
      val randomIdStr = if (randomId < 10) "0" + randomId else randomId.toString
      s"$fixValue$districtStr$currentBase$randomIdStr"
    }
    else {
      // 5毫秒后再重试，这时已经使用的ID记录可能已经被清空，不会再有重复
      Thread.sleep(5)
      next
    }
  }

  /**
   * 根据当前时间生成“日月年时分秒两位毫秒”组成的14位字符串
   * @return
   */
  private def timeBase: String = {
    DateTime.now.toString("ddMMYYHHmmssSS")
  }

  private def reTimeBase(newTimeBase: String) = {
    currentBase = newTimeBase
    usedIds.clear()

  }


}
