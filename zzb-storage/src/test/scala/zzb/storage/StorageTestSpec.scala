package zzb.storage

import org.scalatest.{BeforeAndAfterAll, FlatSpec}
import zzb.storage.data._
import zzb.storage.dirvers.{MongoDriver, MemoryDriver}
import zzb.storage.data.HomeInfo._
import scala.concurrent.ExecutionContext
import spray.util._
import zzb.datatype.{StructPath, VersionInfo}
import org.joda.time.DateTime
import com.mongodb.casbah.Imports._
import zzb.db.Mongos
import com.typesafe.config.ConfigFactory


/**
 * Created by Simon on 2014/4/1
 */
class StorageTestSpec extends FlatSpec with StorageBehaviors with BeforeAndAfterAll{

  import HomeInfo._

  def testConfigSource: String =
    """
      mongo{
      |usedb= ["mydb"]
      |mydb{
      | uri="mongodb://10.68.3.157:27017/"
      | db ="mydb"
      |}
      }
    """.stripMargin

  override protected def beforeAll() = {
    Mongos.openDB("mydb",ConfigFactory.parseString(testConfigSource).getConfig("mongo.mydb"))
  }
  override protected def afterAll() = {
//    Mongos.closeAllDB()
  }
  "TStorageType no require id field " should "throw exception" in {
    intercept[IllegalArgumentException] {
      HomeInfoError(userId := "100")
    }
  }
  "TStorageType with require id field " should "can be instance" in {
    val h = HomeInfo(userId := "100")
    assert(h(ID).get.value === "100")
  }

  "MemoryDriver " should behave like storageWork(new MemoryDriver[String, ID.type, HomeInfo.type](delay = 100) {
    override val docType = HomeInfo
  })

//  "MongoDriver " should behave like storageWork(new MongoDriver[String, ID.type, HomeInfo.type](delay = 100) {
//    override val docType = HomeInfo
//    override val dbname="mydb"
//    MongoClient("10.68.3.157", 27017)("mydb")
//  })
}

trait StorageBehaviors {
  this: FlatSpec =>

  def storageWork(makeDriver: => Driver[String, ID.type, HomeInfo.type]) {

    import ExecutionContext.Implicits.global

    val k1 = "100"

    val storage = new Storage(makeDriver)

    it should "可以保存和装载数据" in {

      assert(storage.cacheSize === 0)
      val u1 = HomeInfo(userId := k1)
      assert(u1.version === 0)

      val u1Saved = storage.save(u1).await

      assert(u1Saved.version === 1)
      assert(storage.cacheSize === 1)
      val u1Loaded = storage.load(k1).await.get
      assert(u1Loaded === u1Saved)
    }
    it should "利用缓存加速数据的装载" in {
      storage.cacheClear() //清除缓存
      assert(storage.cacheSize === 0)
      val firstLoadStart = System.currentTimeMillis()
      val firstLoad = storage.load(k1).await.get
      val firstLoadTime = System.currentTimeMillis() - firstLoadStart
      val secondLoadStart = System.currentTimeMillis()
      val secondLoad = storage(k1).await.get
      val secondLoadTime = System.currentTimeMillis() - secondLoadStart
      assert(firstLoad === secondLoad)
      assert(firstLoadTime > secondLoadTime) //第二次加载的时间小于第一次，说明缓存在起作用
      assert(storage.cacheSize === 1)
    }

    it should "装载不存在的数据得到 None" in {
      val noThisDoc = storage.load("101").await
      assert(noThisDoc === None)
    }

    it should "可以装载数据的不同版本" in {
      val v1 = storage(k1).await.get
      val vlist1 = storage.versions(k1).await
      assert(vlist1.size === 1)
      assert(vlist1.head.version === 1)

      import UserInfo._
      val v1Changed = v1 <~ UserInfo(userName := "Simon", userAge := 39 ,male := true)
      val v2 = storage.save(v1Changed).await
      assert(v2.version === 2)

      val vlist2 = storage.versions(k1).await
      assert(vlist2.size === 2)
      assert(vlist2.head.version === 2)

      val v2Loaded = storage(k1).await.get
      assert(v2Loaded === v2)

      val v2LoadedAgain = storage.load(k1, 2).await.get
      assert(v2LoadedAgain === v2)
    }

    it should "保存数据时指出操作者" in {
      val v2 = storage(k1).await.get
      import CarInfo._
      val v2Changed = v2 <~ CarInfo(carLicense := "京GNR110")
      storage.save(v2Changed, "simon", isOwnerOperate = false).await
      val vlist = storage.versions(k1).await
      val verInfo: VersionInfo.Pack = vlist.head
      assert(verInfo.version === 3)
      assert(verInfo(VersionInfo.opt).get === "simon")

    }
    it should "查询出名字为Simon地数据，车牌号为京GNR110" in{
      val result=storage.find((StructPath(userInfo().userName().path),"Simon")).await
      assert(result.size ==1)
      assert(result.head(carInfo().carLicense()).get.value=="京GNR110")
    }
    it should "查询不出名字为test地数据" in{
      val result=storage.find((StructPath(userInfo().userName().path),"test")).await
      assert(result.size ==0)
    }
    it should "查询出年龄有等于39地数据，车牌号为京GNR110" in{
      val result=storage.find((StructPath(userInfo().userAge().path),39)).await
      assert(result.size ==1)
      assert(result.head(carInfo().carLicense()).get.value=="京GNR110")
    }
    it should "查询不出年龄有等于40地数据" in{
      val result=storage.find((StructPath(userInfo().userAge().path),40)).await
      assert(result.size ==0)
    }
    it should "查询出性别为男地数据，车牌号为京GNR110" in{
      val result=storage.find((StructPath(userInfo().male().path),true)).await
      assert(result.size ==1)
      assert(result.head(carInfo().carLicense()).get.value=="京GNR110")
    }
    it should "查询不出性别为女地数据" in{
      val result=storage.find((StructPath(userInfo().male().path),false)).await
      assert(result.size ==0)
    }

//    it should "查询Query" in {
////      val result = storage.query(List((StructPath(verInfo().time().path),"2014-08-30","lt"),(StructPath(verInfo().time().path),"2014-08-01","gt"))).await
//      val result = storage.query(List((StructPath(carInfo().createDate().path),"2014-08-30","lt"),(StructPath(carInfo().createDate().path),"2014-08-01","gt"))).await
//      println(result)
//      assert(result.size ==1)
//      assert(result.head(carInfo().carLicense()).get.value=="京GNR110")
//    }

    it should "可以恢复数据的旧版本" in {
      val v2Reverted = storage.revert(k1, 2).await.get
      assert(v2Reverted.version === 4)

      val v4 = storage.load(k1).await.get
      assert(v2Reverted === v4)

      assert(v4(HomeInfo.carInfo().carLicense()) === None) //恢复的版本中没有这个数据

      val v4Reverted = storage.revert(k1, 4).await.get
      assert(v4Reverted.version === 4) //最新版本 revert 没动作

      assert(storage.revert(k1, 0).await === None)
    }

    it should "可以标记删除数据 " in {

      assert(storage.delete("nothis").await === 0)
      assert(storage.delete(k1).await === 1)

      assert(storage.load(k1).await === None)

      assert(storage.load(k1,2).await.get.version === 2) //标记删除后，指定版本还是可以装载的

      assert(storage.delete(k1,justMarkDelete = false).await === 1) //真的删掉了

      assert(storage.load(k1,2).await === None) //这回真没了
    }
  }
}
