package zzb.mvm.rest

import akka.actor.{Actor, ActorSystem, Props}
import akka.event.{BusLogging, DiagnosticLoggingAdapter, LogSource}
import akka.testkit.{ImplicitSender, TestKit}
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.{BeforeAndAfterAll, MustMatchers, WordSpecLike}
import spray.util._
import zzb.datatype._
import zzb.mvm.rest.RemoteConfig._
import zzb.rest._
import zzb.rest.testuse._
import zzb.util.MdcLoggingContext

import scala.concurrent.duration._
import scala.util.{Failure, Success}


/**
 * Created by Simon on 2014/5/26
 */
abstract class DataEntityMvmTest(val nodeName: String, config: Config) extends TestKit(ActorSystem(nodeName, config))
with WordSpecLike
with MustMatchers with ImplicitSender
with Caller with BeforeAndAfterAll {

  def logName: String

  implicit val log = logFactory(logName)

  implicit val mdcLogCtx = MdcLoggingContext.fromAdapter(log)


  protected def logFactory(logName: String): DiagnosticLoggingAdapter = {

    val (str, clazz) = LogSource(logName)
    new BusLogging(system.eventStream, str, clazz) with DiagnosticLoggingAdapter
  }

  override protected def beforeAll() {

    println(s"$nodeName system started!")
  }

  override protected def afterAll() {
    super.afterAll()
    println(s"$nodeName system shutdown!")
    system.shutdown()
  }
}

class DataEntityMultiJvmNode1 extends DataEntityMvmTest("Server", RemoteConfig.serverConfig) {

  def logName = "zzb.multi-jvm.server"

  override protected def beforeAll() {
    super.beforeAll()
    system.actorOf(Props[AccountActor], "account")
    system.actorOf(Props[AccountActor2], "account2")
  }

  val receiver = system.actorOf(Props(new Actor {
    def receive: Receive = {
      case m =>
        testActor forward m
    }
  }), "testReceiver")


  "Server" must {
    "shutdown" in {
      expectMsg(4.seconds, "stopServer")
      Thread.sleep(100)
    }
  }
}

class DataEntityMultiJvmNode2 extends DataEntityMvmTest("Client", clientConfig) {

  // Get an ActorRef for the remote system's testActor
  def remoteTestReceiver() =
    system.actorSelection(s"akka.tcp://Server@127.0.0.1:$serverPort/user/testReceiver")

  def logName = "zzb.multi-jvm.client"

  val rootUri = s"akka.tcp://Server@127.0.0.1:$serverPort"

  log.info("*************start test")

  "跨虚拟机客户端" must {

    "由向远程主机请求数据,使用绝对路径" in {

      implicit val timeout = Timeout(1600 milliseconds)
      val res1 = doGet(s"$rootUri/account/ok").await
      res1.status mustBe StatusCodes.OK
      res1.entity.asString mustBe "ok"
    }

    "由向远程主机请求数据,使用绝对路径,超时设置太短时会收到超时错误" in {

      implicit val timeout = Timeout(50 milliseconds) //服务器那边会sleep 500 毫秒

      val res1 = doGet(s"$rootUri/account/ok")
      import system.dispatcher

      res1.onComplete {
        case Success(v) => assert(false)
        case Failure(e: RestTimeoutException) =>
          println("==========> success get RestTimeoutException")
          assert(true)
        case Failure(e) => assert(false)
      }
      intercept[RestTimeoutException] {
        res1.await(Timeout(500 milliseconds)) //使用显式的根地址
      }
    }


    "case class 可以远程传递" in {
      implicit val timeout = Timeout(500 milliseconds)
      val account = Account("simon", 39)

      val res0 = doPost(s"$rootUri/account/name", account).await
      res0.status must equal(StatusCodes.OK)
      res0.entity.data must equal(account.name)

      val res01 = doGet(s"$rootUri/account/age", account).await
      res01.status must equal(StatusCodes.OK)
      res01.entity.data must equal(account.age)

      val res1 = doPost(s"$rootUri/account", account).await
      res1.status must equal(StatusCodes.OK)
      res1.entity.data must equal(account)

      val res2 = doPost(s"$rootUri/account2", account).await
      res2.status must equal(StatusCodes.OK)
      res2.entity.data must equal(account)

      val res3 = doGet(s"$rootUri/account2").await
      res3.status must equal(StatusCodes.OK)
      res3.entity.data must equal(account)

      //故意给出错误的类型
      val res4 = doPost(s"$rootUri/account2", new Info("home")).await
      res4.status must equal(StatusCodes.BadRequest)

      val res5 = doPut(s"$rootUri/account2", account).await
      res5.status must equal(StatusCodes.OK)

      val s1 = User("jack", 3)

      val res6 = doPost(s"$rootUri/account2/user", s1).await
      res6.status must equal(StatusCodes.OK)
      res6.entity.data mustBe s1
    }
    "zzb DataType 可以远程传递" in {
      implicit val timeout = Timeout(1500 milliseconds)
      import zzb.rest.testuse.Student
      import zzb.rest.testuse.Student._

      val student: Student.Pack = Student(name := "Simon" /*, phones := List(123, 345)*/)

      val res0 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/student", student).await(Timeout(2500 milliseconds))
      res0.status must equal(StatusCodes.OK)
      res0.entity.data mustBe student

      import Task._
      val tt =Task(track:=List(TrackInfo(TrackInfo.actionName:="zhangsan",TrackInfo.operator:="122322"),TrackInfo(TrackInfo.actionName:="lisi",TrackInfo.operator:="cx")))
      println("task>>>>"+tt)
      val res00 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/task", tt).await(Timeout(2500 milliseconds))
      res00.status must equal(StatusCodes.OK)
      res00.entity.data mustBe tt
      println("taskk>>"+res00.entity.data)

      val res1 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/TString", TString("张三")).await(Timeout(2500 milliseconds))
      res1.status must equal(StatusCodes.OK)
      res1.entity.data mustBe TString("张三")

      val res2 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/TBoolean", TBoolean(true)).await(Timeout(2500 milliseconds))
      res2.status must equal(StatusCodes.OK)
      res2.entity.data mustBe TBoolean(true)

      val res3 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/TDateTime", TDateTime("2014-10-01")).await(Timeout(2500 milliseconds))
      res3.status must equal(StatusCodes.OK)
      res3.entity.data mustBe TDateTime("2014-10-01")

      val res4 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/TBigDecimal", TBigDecimal.parse("20.30")).await(Timeout(2500 milliseconds))
      res4.status must equal(StatusCodes.OK)
      res4.entity.data mustBe TBigDecimal.parse("20.30")

      val res51 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/TShort", TShort.parse("26")).await(Timeout(2500 milliseconds))
      res51.status must equal(StatusCodes.OK)
      res51.entity.data mustBe TShort.parse("26")

      val res5 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/TInt", TInt(26)).await(Timeout(2500 milliseconds))
      res5.status must equal(StatusCodes.OK)
      res5.entity.data mustBe TInt(26)

      val res6 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/TLong", TLong.parse("26")).await(Timeout(2500 milliseconds))
      res6.status must equal(StatusCodes.OK)
      res6.entity.data mustBe TLong.parse("26") //未注册类型错误

      val res7 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/String", "9").await(Timeout(2500 milliseconds))
      res7.status must equal(StatusCodes.OK)
      res7.entity.data mustBe "9"

      /* val res8 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/Int", 9).await(Timeout(2500 milliseconds))
       res8.status must equal(StatusCodes.OK)
       res8.entity.data mustBe 9*/
      //400错误

      val res9 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/Sex", Sex(EnumIdx(2))).await(Timeout(2500 milliseconds))
      res9.status must equal(StatusCodes.OK)
      res9.entity.data mustBe Sex(EnumIdx(2))

      val res10 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/TFloat", 0.2f).await(Timeout(2500 milliseconds))
      res10.status must equal(StatusCodes.OK)
      res10.entity.data mustBe TFloat(0.2f)


      val cc = CLists(List(CList(CList.lcase := "1f")))
      val res11 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/TList", cc).await(Timeout(2500 milliseconds))
      res11.status must equal(StatusCodes.OK)
      res11.entity.data mustBe cc

      val c1 = TMaps(Map("white" -> 1))
      val res12 = doPost(s"akka.tcp://Server@127.0.0.1:$serverPort/account2/TMap", c1).await(Timeout(2500 milliseconds))
      res12.status must equal(StatusCodes.OK)
      res12.entity.data mustBe c1
    }
    "stop server" in {
      val sc = remoteTestReceiver()
      sc ! "stopServer"

      Thread.sleep(100)
    }
  }
}

object RemoteConfig {
  val clientPort = 3552
  val serverPort = 3553
  val serverConfig = ConfigFactory.parseString( s"""
    akka {
      loggers = ["akka.event.slf4j.Slf4jLogger"]
      loglevel = "debug"
      actor {
        provider = "akka.remote.RemoteActorRefProvider"
      }
      remote {
        enabled-transports = ["akka.remote.netty.tcp"]
        netty {
          tcp{
            hostname = "127.0.0.1"
            port = $serverPort
          }
        }
      }
    }""")

  val clientConfig = ConfigFactory.parseString( s"""
    akka {
      loggers = ["akka.event.slf4j.Slf4jLogger"]
      loglevel = "debug"
      actor {
        provider = "akka.remote.RemoteActorRefProvider"
      }
      remote {
        enabled-transports = ["akka.remote.netty.tcp"]
        netty {
          tcp{
            hostname = "127.0.0.1"
            port = $clientPort
          }
        }
      }
    }""")
}



