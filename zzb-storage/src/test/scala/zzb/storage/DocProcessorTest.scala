package zzb.storage

import org.scalatest.FlatSpec
import zzb.storage.data._
import zzb.storage.dirvers.MemoryDriver
import scala.concurrent.{Future, ExecutionContext}
import spray.util._
import zzb.storage.data.UserInfo._
import zzb.storage.data.CarInfo._

/**
 * Created by Simon on 2014/4/3
 */
class DocProcessorTest extends FlatSpec {

  import HomeInfo._
  //import ExecutionContext.Implicits.global
  val storage = new Storage(new MemoryDriver[String, ID.type, HomeInfo.type](delay = 200) {
    override val docType = HomeInfo
  })

  val hip = new HomeInfoProcessor() {
    override val specStorage = storage.specific("100")
    override val isNewCreate: Boolean = true
    def docExecutionContext: ExecutionContext = ExecutionContext.Implicits.global
    override def createDoc: Future[Option[HomeInfo.Pack]] = Future(None)

  }

  "DocProcessor" should "can be create with a new create data" in {

    val v0 = HomeInfo(userId := "100")
    assert(hip.latest.await === None)
    hip.save(v0)
    assert(hip.latest.await.get.version === 1)
    assert(hip.version(1).await.get.version === 1)
  }

  it should  "can be create with saved data" in {


    val v1 = hip.latest.await.get
    assert(v1.version === 1)
    val v1Changed = v1 <~ UserInfo(userName:="Simon",userAge := 39)
    hip.save(v1Changed)
    assert(hip.latest.await.get.version === 2)

  }
  it should "can load latest version" in {
    val v2 = hip.latest.await.get
    assert(v2.version === 2)
  }

  it should "can load spec version" in {

    val v2 = hip.version(2).await.get
    assert(v2.version === 2)
  }

  it should "hand no exist version" in {

    assert(hip.version(3).await === None)
    val v2 = hip.version(2).await.get
    assert(v2.version === 2)
    val v2Changed = v2 <~ CarInfo(carLicense:="京GNR110")
    hip.save(v2Changed)
    assert(hip.latest.await.get.version === 3)
    assert(hip.version(3).await.get.version === 3)
    assert(hip.latestReload.await.get.version === 3)
  }

  it should "hand wrong key" in {
    val hip1 = new HomeInfoProcessor() {
      override val specStorage = storage.specific("101")
      override val isNewCreate: Boolean = false
      def docExecutionContext: ExecutionContext = ExecutionContext.Implicits.global
      override def createDoc: Future[Option[HomeInfo.Pack]] = Future(None)

    }
    assert(hip1.latest.await === None)
    assert(hip1.latestReload.await === None)

  }

  it should "can revert to old version" in {
    val versions = hip.versions.await
    assert(versions.size === 1)
    //assert(hip.revert(2).await.get.version == 4)
  }

//  it should "can remove latest version" in {
//
//    assert(hip.latest.await.get.version === 4)
//    hip.delete().await
//    assert(hip.latest.await === None)
//
//    assert(hip.versions.await.size == 4) //只是做了标记删除
//  }
}

abstract class HomeInfoProcessor extends DocProcessor[String, ID.type, HomeInfo.type] {}
