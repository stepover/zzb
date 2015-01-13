package zzb.storage

import com.github.nscala_time.time.Imports._
import org.scalatest.{BeforeAndAfterAll, FlatSpec}
import zzb.datatype.{StructPath, VersionInfo}
import zzb.storage.data.UserInfo._
import zzb.storage.data.{CarInfo, UserInfo, ID, HomeInfo}
import org.scalatest.{BeforeAndAfterAll, FlatSpec}
import zzb.storage.data._
import zzb.storage.dirvers.{MongoDriver, MemoryDriver}
import zzb.storage.data.HomeInfo._
import spray.util._
import zzb.datatype.{StructPath, VersionInfo}
import com.mongodb.casbah.Imports._
import zzb.db.Mongos
import com.typesafe.config.ConfigFactory

import scala.concurrent.ExecutionContext

/**
 * todo:
 * @author 何伟波
 * @since  0.1.0
 */
class MongoStorageQueryTestSpec extends FlatSpec with MongoStorageBehaviors with BeforeAndAfterAll {

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
    Mongos.openDB("mydb", ConfigFactory.parseString(testConfigSource).getConfig("mongo.mydb"))
  }

  override protected def afterAll() = {
//    Mongos.closeAllDB()
  }

//  "MongoDriver " should behave like mongoStorageQuery(new MongoDriver[String, ID.type, HomeInfo.type](delay = 100) {
//    override val docType = HomeInfo
//    override val dbname = "mydb"
//    MongoClient("10.68.3.157", 27017)("mydb")
//  })


}

trait MongoStorageBehaviors extends DBObjectHelper {
  this: FlatSpec =>

  def mongoStorageQuery(makeDriver: => Driver[String, ID.type, HomeInfo.type]) {

    import ExecutionContext.Implicits.global

    val k1 = "100"
    val k2 = "200"
    val k3 = "300"

    val storage = new Storage(makeDriver)

    it should "可以保存和装载数据" in {

      import CarInfo._
      assert(storage.cacheSize === 0)

      val addTrackInfo1 = TrackInfo(TrackInfo.actionName := "测试1" , TrackInfo.operator := "op1" ,TrackInfo.remark := "very good1")
      val addTrackInfo2 = TrackInfo(TrackInfo.actionName := "测试2" , TrackInfo.operator := "op2" ,TrackInfo.remark := "very good2")
      val addTrackInfo3 = TrackInfo(TrackInfo.actionName := "测试3" , TrackInfo.operator := "op3" ,TrackInfo.remark := "very good3")
      val trackInfos1=TrackInfos(List(addTrackInfo1,addTrackInfo2,addTrackInfo3))
      val trackInfos2=TrackInfos(List(addTrackInfo2,addTrackInfo3))
      val trackInfos3=TrackInfos(List(addTrackInfo3))

      val userInfo1 = UserInfo(userName := "Simon1", userAge := 40, driverAge := 21, male := true, birthDay := ("2014-01-05T00:01:02").toDateTime ,track := trackInfos1.value)
      val carInfo1 = CarInfo(carLicense := "1粤A123456", carVin := "1carVin")
      val userInfo2 = UserInfo(userName := "Simon2", userAge := 40, driverAge := 22, male := false, birthDay := ("2014-01-06T00:02:03").toDateTime ,track := trackInfos2.value)
      val carInfo2 = CarInfo(carLicense := "2粤A123456", carVin := "2carVin")
      val userInfo3 = UserInfo(userName := "Simon3", userAge := 41, driverAge := 22, male := true, birthDay := ("2014-01-07T03:00:00").toDateTime ,track := trackInfos3.value)
      val carInfo3 = CarInfo(carLicense := "3粤A123456", carVin := "3carVin")
      val u1 = HomeInfo(userId := k1) <~ userInfo1 <~ carInfo1
      val u2 = HomeInfo(userId := k2) <~ userInfo2 <~ carInfo2
      val u3 = HomeInfo(userId := k3) <~ userInfo3 <~ carInfo3
      assert(u1.version === 0)
      assert(u2.version === 0)
      assert(u3.version === 0)

      val u1Saved = storage.save(u1).await
      val u2Saved = storage.save(u2).await
      val u3Saved = storage.save(u3).await

      val u1Loaded = storage.load(k1).await.get
      val u1Loaded2 = storage.load(k2).await.get
      val u1Loaded3 = storage.load(k3).await.get
      assert(u1Loaded === u1Saved)
      assert(u1Loaded2 === u2Saved)
    }




    it should "查询Query" in {
      val result = storage.query(List((StructPath(userInfo().birthDay().path), "2014-08-30", "lt"), (StructPath(userInfo().birthDay().path), "2013-08-01", "gt"))).await

      println(result)
      assert(result.size == 3)
    }

    it should "查询数据库，带上排序" in {

//      40 21
//      40 22
//      41 22
//      u d
      val filter1 = makeGteFilter(StructPath(userInfo().birthDay().path), "2014-01-05").get
      val filter2 = makeLtFilter(StructPath(userInfo().birthDay().path), "2014-01-31").get
      val filter3 = makeAndFilter(filter1, filter2)
      val userAgeFilter= structPathToString(StructPath(userInfo().userAge().path))
      val driverAgeFilter= structPathToString(StructPath(userInfo().driverAge().path))

      val sortFilter1 = MongoDBObject(driverAgeFilter->1) ++ (userAgeFilter -> 1)  //driverAgeFilter 升序 ，userAge 升序
      val result1 = storage.queryWithLimitSort(dbObjcet = filter3,sort=sortFilter1).await
      val resultList=result1.asInstanceOf[List[HomeInfo.Pack]]
      val userAge1 = resultList.head(HomeInfo.userInfo().userAge()).get.value
      val driverAge1 = resultList.head(HomeInfo.userInfo().driverAge()).get.value
      assert(userAge1 == 40 ,driverAge1 == 21)

      val sortFilter2 = MongoDBObject(driverAgeFilter-> -1) ++ (userAgeFilter -> 1)  //driverAgeFilter 降序 ，userAge 升序
      val result2 = storage.queryWithLimitSort(dbObjcet = filter3,sort=sortFilter2).await
      val resultList2=result2.asInstanceOf[List[HomeInfo.Pack]]
      val userAge2 = resultList2.head(HomeInfo.userInfo().userAge()).get.value
      val driverAge2 = resultList2.head(HomeInfo.userInfo().driverAge()).get.value
      assert(userAge2 == 40 ,driverAge2 == 22)

      val sortFilter3 = MongoDBObject (userAgeFilter -> -1)++  (driverAgeFilter-> 1)  //userAge 降序  ，driverAgeFilter 升序
      val result3 = storage.queryWithLimitSort(dbObjcet = filter3,sort=sortFilter3).await
      val resultList3=result3.asInstanceOf[List[HomeInfo.Pack]]
      val userAge3 = resultList3.head(HomeInfo.userInfo().userAge()).get.value
      val driverAge3 = resultList3.head(HomeInfo.userInfo().driverAge()).get.value
      assert(userAge3 == 41 ,driverAge3 == 22)

      val sortFilter4 = MongoDBObject (userAgeFilter -> -1)++  (driverAgeFilter-> -1)  //userAge 降序  ，driverAgeFilter 降序
      val result4 = storage.queryWithLimitSort(dbObjcet = filter3,sort=sortFilter4).await
      val resultList4=result4.asInstanceOf[List[HomeInfo.Pack]]
      val userAge4 = resultList4.head(HomeInfo.userInfo().userAge()).get.value
      val driverAge4 = resultList4.head(HomeInfo.userInfo().driverAge()).get.value
      assert(userAge4 == 41 ,driverAge4 == 22)

    }

    it should "大于等于，小于" in {
      val filter1 = makeGteFilter(StructPath(userInfo().birthDay().path), "2014-01-05").get
      val filter2 = makeLtFilter(StructPath(userInfo().birthDay().path), "2014-01-31").get
      val filter3 = makeAndFilter(filter1, filter2)
      val result = storage.query(filter3).await
      assert(result.size == 3)
      val count = storage.count(filter3).await
      assert(count == 3)
      val result1 = storage.queryWithLimit(filter3).await
      assert(result1.size == 3)
      val result2 = storage.queryWithLimit(filter3, 2).await
      assert(result2.size == 2)
      val result4 = storage.queryWithLimit(filter3, 2, 2).await
      assert(result4.size == 1)
    }

    it should "求总数" in {
      val filter1 = makeGteFilter(StructPath(userInfo().birthDay().path), "2014-01-05").get
      val filter2 = makeLtFilter(StructPath(userInfo().birthDay().path), "2014-01-31").get

      val filter3 = makeAndFilter(filter1, filter2)
      val count = storage.count(filter3).await
      println(count)
      assert(count == 3)
    }

    it should "求总数 --创建一个无条件的查询" in {

      /********创建无条件的condition************/
      val noCondition = MongoDBObject()
      /********测试count方法************/
      val count = storage.count(noCondition).await
      println(count)
      assert(count == 3)
      /*********测试queryWithLimit方法***********/
      val result1 = storage.queryWithLimit(noCondition).await
      assert(result1.size == 3)
      /**********测试queryWithLimitSort方法**********/

      /*************创建没有排序的条件******************/
      val sortFilter1 = MongoDBObject()  //driverAgeFilter 升序 ，userAge 降序

      val result2 = storage.queryWithLimitSort(dbObjcet = noCondition,sort=sortFilter1).await
      assert(result2.size == 3)
    }

    it should "列表查询" in {

      val filter3 = makeEqFilter("userInfo.trackInfos.actionName","测试3").get
      val result3 =storage.query(filter3).await
      assert(result3.size == 3)

      val filter2 = makeEqFilter("userInfo.trackInfos.actionName","测试2").get
      val result2 =storage.query(filter2).await
      assert(result2.size == 2)

      val filter1 = makeEqFilter("userInfo.trackInfos.actionName","测试1").get
      val result1 =storage.query(filter1).await
      assert(result1.size == 1)
    }


    it should "过滤条数" in {
      val filter1 = makeGteFilter(StructPath(userInfo().birthDay().path), "2014-01-05").get
      val filter2 = makeLtFilter(StructPath(userInfo().birthDay().path), "2014-01-31").get
      val filter3 = makeAndFilter(filter1, filter2)
      val result1 = storage.queryWithLimit(filter3).await
      assert(result1.size == 3)
      val result2 = storage.queryWithLimit(filter3, 2).await
      assert(result2.size == 2)
      val result4 = storage.queryWithLimit(filter3, 2, 2).await
      assert(result4.size == 1)
    }

    it should "大于，小于" in {
      val filter1 = makeGtFilter(StructPath(userInfo().birthDay().path), "2014-01-05").get
      val filter2 = makeLtFilter(StructPath(userInfo().birthDay().path), "2014-01-31").get

      val filter3 = makeAndFilter(filter1, filter2)
      val result = storage.query(filter3).await
      println(result)
      assert(result.size == 3)
      val count = storage.count(filter3).await
      println(count)
      assert(count == 3)

    }

    it should "大于，小于等于" in {
      val filter1 = makeGtFilter(StructPath(userInfo().driverAge().path), 21).get
      val filter2 = makeLteFilter(StructPath(userInfo().driverAge().path), 23).get

      val filter3 = makeAndFilter(filter1, filter2)
      val result = storage.query(filter3).await
      println(result)
      assert(result.size == 2)
    }


    it should "等于，且" in {
      val filter1 = makeEqFilter(StructPath(userInfo().driverAge().path), 21).get
      val filter2 = makeEqFilter(StructPath(userInfo().driverAge().path), 23).get

      val filter3 = makeAndFilter(filter1, filter2)
      val result = storage.query(filter3).await
      println(result)
      assert(result.size == 0)
    }

    it should "等于，或" in {
      val filter1 = makeEqFilter(StructPath(userInfo().driverAge().path), 20).get
      val filter2 = makeEqFilter(StructPath(userInfo().driverAge().path), 23).get

      val filter3 = makeOrFilter(filter1, filter2)
      val result = storage.query(filter3).await
      println(result)
      assert(result.size == 0)
    }

    it should "等于" in {
      val filter1 = makeEqFilter(StructPath(carInfo().carVin().path), "car").get
      println(filter1)
      val result = storage.query(filter1).await
      println(result)
      assert(result.size == 0)
    }

    it should "可以标记删除数据 " in {

      assert(storage.delete(k1, justMarkDelete = false).await === 1) //真的删掉了
      assert(storage.delete(k2, justMarkDelete = false).await === 1) //真的删掉了
      assert(storage.delete(k3, justMarkDelete = false).await === 1) //真的删掉了

      assert(storage.load(k1, 2).await === None) //这回真没了
      assert(storage.load(k2, 2).await === None) //这回真没了
      assert(storage.load(k3, 2).await === None) //这回真没了
    }
  }
}
