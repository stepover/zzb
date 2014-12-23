package zzb.srvdemo.test

import akka.actor.ActorSystem
import akka.util.Timeout
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import org.scalatest.{BeforeAndAfterAll, MustMatchers, WordSpec}
import spray.util._
import zzb.db.DBPools
import zzb.rest._
import zzb.srvdemo.DemoService
import zzb.srvdemo.entites.{ChangeCount, User}

import scala.concurrent.duration._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-3-13
 * Time: 下午1:22
 * Copyright baoxian.com 2012~2020
 */
class AkkaRestTest extends WordSpec with MustMatchers with Requester with BeforeAndAfterAll{

  val config = ConfigFactory.load("demo").withValue("box.service-name", ConfigValueFactory.fromAnyRef("demo"))
  implicit val system = ActorSystem("demoApp", config)
  val service = new DemoService(system,config)

  override protected def beforeAll() {


    DBPools.openDB("userdb", config.getConfig("db.userdb"))
    DBPools.openDB("infodb", config.getConfig("db.infodb"))

    service.init()

  }

  override protected def afterAll() {
    super.afterAll()
    service.fini()
    system.shutdown()
  }

  implicit val timeout = Timeout(500000 milliseconds)

  "Akka Rest Api  " must {
    "can add user"  in {

      val u1 = new User("simon@qq.com","123456")
      val u2 = new User("jack@qq.com","abcedfg")

      //测试一个简单的请求
      val res = (Get("/demo/r/hello") ~> doRequest).await
      res.status must equal(StatusCodes.OK)
      res.entity.data must equal("ok")

      //获取列表，应该为空列表
      val res1 = (Get("/demo/r/user") ~> doRequest).await
      res1.entity.data must equal(List[User]())

      //增加一个user,返回的是user本身
      val res2 = (Post("/demo/r/user",u1) ~> doRequest).await
      res2.status must equal(StatusCodes.OK)
      res2.entity.data must equal(u1)

      //再次获取列表，应该有一个user
      val res3 = (Get("/demo/r/user") ~> doRequest).await
      val u1_saved = res3.entity.data.asInstanceOf[List[User]].head
      u1_saved must equal(u1)

      //更新u1
      u1.password = "qwert"
      val res4 = (Put("/demo/r/user/1",u1) ~> doRequest).await
      res4.entity.data must equal(u1)

      //Get u1，
      val res5 = (Get("/demo/r/user/1") ~> doRequest).await
      res5.entity.data must equal(u1)

      //Get a no exist user ，
      val res6 = (Get("/demo/r/user/100") ~> doRequest).await
      res6.status must equal(StatusCodes.NotFound)

      //删除u1
      val res7 = (Delete("/demo/r/user/1") ~> doRequest).await
      res7.status must equal(StatusCodes.OK)
      res7.entity.data must equal(ChangeCount(1))  //删除了一个

      //删除不存在的用户
      val res8 = (Delete("/demo/r/user/100") ~> doRequest).await
      res8.status must equal(StatusCodes.OK)
      res8.entity.data must equal(ChangeCount(0))  //删除了0个
    }
  }



}
