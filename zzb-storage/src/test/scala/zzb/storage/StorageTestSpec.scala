package zzb.storage

import org.scalatest.{BeforeAndAfterEach, BeforeAndAfterAll, FlatSpec}
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

  "MemoryDriver " should behave like storageWork(new MemoryDriver[String, ID.type, HomeInfo.type](delay = 200) {
    override val docType = HomeInfo
  })

//  "MongoDriver " should behave like storageWork(new MongoDriver[String, ID.type, HomeInfo.type](delay = 100) {
//    override val docType = HomeInfo
//    override val dbname="mydb"
//    MongoClient("10.68.3.157", 27017)("mydb")
//  })
}

trait StorageBehaviors  {
  this: FlatSpec  =>

  def storageWork(makeDriver: => Driver[String, ID.type, HomeInfo.type]) {

    import ExecutionContext.Implicits.global

    val k1 = "100"
    val t1 = "tag1"
    val t2 = "tag2"
    val t3 = "tag3"

    val storage = new Storage(makeDriver)

    it should "可以保存和装载数据" in {

      assert(storage.cacheSize === 0)
      val u1 = HomeInfo(userId := k1)
      assert(u1.version === 0)

      val u1Saved = storage.save(u1).await

      assert(u1Saved.revise === 0) //保存后的数据修订号都是0

      assert(u1Saved.version === 1)
      assert(storage.cacheSize === 1)
      val u1Loaded = storage.load(k1).await.get
      assert(u1Loaded === u1Saved)
      assert(u1Loaded.revise === 0) //新装载的数据修订号都是0
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
      val v2 = storage.save(v1Changed).await //保存动作会增加版本号，但是会把旧版本实例覆盖（只有打了tag的版本才会保留副本）
      assert(v2.version === 2)
      assert(v2.revise === 0)

      val vlist2 = storage.versions(k1).await
      assert(vlist2.size === 1)   //版本数量不变
      assert(vlist2.head.version === 2) //版本号+1

      val v2Loaded = storage(k1).await.get
      assert(v2Loaded === v2)
      assert(v2Loaded.revise ===0)

      val v2LoadedAgain = storage.load(k1, 2).await.get
      assert(v2LoadedAgain === v2)
      assert(v2LoadedAgain.revise ===0)

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

    it should "可以给数据增加 tag" in {
      import UserInfo._
      val vlt1Before = storage.versions(k1).await
      val t1Before = storage.load(k1).await.get
      assert(t1Before.revise ===0)

      val t1Done = storage.tag(k1,t1).await
      assert(t1Done.revise ===0)

      val vlt1Done = storage.versions(k1).await

      assert( t1Done.version - t1Before.version === 2) //打tag 增加一个版本号，返回无tag版本再增加一个版本号
      assert(t1Done.tag === "") // 打tag 动作返回的最新版本没有tag
      assert(t1Done.eqtag === t1)  // 打tag 动作返回的最新版本的eqtag等于t1
      assert(vlt1Done.size - vlt1Before.size === 1  ) //打 tag 版本数量增加 1

      val t2Before = storage.save(t1Done).await //修改数据后再次保存
      val vlt2Before = storage.versions(k1).await

      assert( t2Before.version - t1Done.version === 1) //版本号加1
      assert( t2Before.eqtag === "") //etag 会被清理掉
      assert(vlt2Before.size - vlt1Done.size === 0  ) //版本数量不变

      val t2Changed = t2Before <~ UserInfo(userName := "jack", userAge := 40 ,male := false)
      val t2Done = storage.save(t2Changed,"",isOwnerOperate = true,t2).await //保存新数据同时打标签
      assert(t2Done.revise ===0)
      val vlt2Done = storage.versions(k1).await

      assert(t2Done.version - t2Before.version === 2  ) //保存并打 tag 版本号增加 2
      assert(vlt2Done.size - vlt2Before.size === 1  ) //保存并打 tag 版本数量增加1
      assert(t2Done.tag === "") // 打tag 动作返回的最新版本没有tag
      assert(t2Done.eqtag === t2)  // 打tag 动作返回的最新版本的eqtag等于t2

      assert(t2Done(userInfo().userName()).get.value=="jack")

      val t3Changed = t2Done <~  UserInfo(userName := "vivian", userAge := 40 ,male := false)

      assert(t3Changed.eqtag === "")  // 任何数据改变都会把 eqtag 清空


    }

    it should "可以恢复数据的旧版本号" in {

      val verCount1 = storage.versions(k1).await.size
      val v4Reverted = storage.revert(k1, 4).await.get
      val verCount2 = storage.versions(k1).await.size

      assert(v4Reverted.version === 9)
      assert(verCount1 === verCount2) //版本数量不变

      val v9 = storage.load(k1).await.get
      assert(v4Reverted === v9)
      assert(v9.tag === "")
      assert(v9.eqtag === "")

      assert(v9(userInfo().userName()).get.value === "Simon") //数据已经恢复

      val v9Reverted = storage.revert(k1, 9).await.get
      assert(v9Reverted.version === 9) //最新版本 revert 没动作

      assert(storage.revert(k1, 0).await === None)
    }

    it should "可以恢复数据的旧tag" in {
      val verCount1 = storage.versions(k1).await.size
      val t2Reverted = storage.revert(k1, t2).await.get
      val verCount2 = storage.versions(k1).await.size

      assert(t2Reverted.version === 10)
      assert(verCount1 === verCount2) //版本数量不变

      val vt2 = storage.load(k1).await.get
      assert(vt2.revise === 0)

      assert(t2Reverted === vt2)
      assert(vt2.tag === "")
      assert(vt2.eqtag === t2)

      assert(vt2(userInfo().userName()).get.value === "jack") //数据已经恢复
      val t2RevertedAgain = storage.revert(k1, t2).await.get
      assert(t2RevertedAgain.version === 10) //最新版本 revert 没动作
      assert(storage.revert(k1, "aaa").await === None)

    }

    it should "可以标记删除数据 " in {

      assert(storage.delete("nothis").await === 0)
      assert(storage.delete(k1).await === 1)

      assert(storage.load(k1).await === None)

      assert(storage.load(k1,4).await.get.version === 4) //标记删除后，指定版本还是可以装载的

      assert(storage.load(k1,t1).await.get.version !== 0) //标记删除后，指定tag还是可以装载的

      assert(storage.delete(k1,justMarkDelete = false).await === 1) //真的删掉了

      assert(storage.load(k1,4).await === None) //这回真没了

      assert(storage.load(k1,t1).await === None) //这回真没了
    }
  }
}
