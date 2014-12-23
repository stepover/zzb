package zzb.storage

import org.scalatest.FlatSpec
import zzb.datatype.StructPath
import zzb.storage.data.HomeInfo
import zzb.storage.data.HomeInfo._

/**
 * todo:
 * @author 何伟波
 * @since  0.1.0
 */
class DBObjectHelperTest extends FlatSpec with DBObjectHelper{

  val structPath = StructPath(HomeInfo.carInfo().carLicense().path)

   it should "structPathToString" in {

        val result = structPathToString(structPath)
        assert(result === "carInfo.carLicense")
    }

  it should "makeFilterByConditionName" in {
    val result = makeFilterByConditionName(structPath,123,"eq")
    println (result)
    val result2 = makeFilterByConditionName(structPath,123,"lt")
    println (result2)
    val list = List(result,result2).flatten
    val result3=makeAndFilter(list)
    println (result3)
    val result4= List((StructPath(carInfo().createDate().path),"2014-08-30","lt"),(StructPath(carInfo().createDate().path),"2014-08-01","gt"))
    println(result4)
  }
}
