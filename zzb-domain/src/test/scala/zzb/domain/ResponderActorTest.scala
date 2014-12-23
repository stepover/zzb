package zzb.domain

import akka.actor.{ActorSystem, Props}
import akka.event.{BusLogging, DiagnosticLoggingAdapter, LogSource}
import akka.testkit.{ImplicitSender, TestKit}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, MustMatchers, WordSpecLike}
import spray.http.HttpHeaders.RawHeader
import zzb.datatype._
import zzb.domain.plane.{PlaneState, PlaneSvcActor}
import zzb.rest.util.StatableActor
import zzb.rest.{Caller, EntityResponder}
import zzb.util.MdcLoggingContext

import scala.concurrent.duration._

/**
 * Created by Simon on 2014/7/26
 */
class ResponderActorTest extends TestKit(ActorSystem("testSystem",
  ConfigFactory.parseString( """
                               |akka {
                               |  loggers = ["akka.event.slf4j.Slf4jLogger"]
                               |  loglevel = "debug"
                               |}
                             """.stripMargin)
))
with WordSpecLike with MustMatchers with ImplicitSender with BeforeAndAfterAll with Caller {

  override protected def beforeAll() {
    system.actorOf(Props(new PlaneSvcActor with StatableActor), "api")
  }

  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

  val manager = List(RawHeader("X-Manager-Id", "system"))
  val user = List(RawHeader("X-User-Id", "Simon"))
  val nobody = List[RawHeader]()

  implicit lazy val log = logFactory(logName)

  implicit lazy val mdcLogCtx = MdcLoggingContext.fromAdapter(log)

  protected def logFactory(logName: String): DiagnosticLoggingAdapter = {

    val (str, clazz) = LogSource(logName)
    new BusLogging(system.eventStream, str, clazz) with DiagnosticLoggingAdapter
  }

  def logName = "zzb.plane.test"

  log.info("start test ")

  import spray.http.StatusCodes._

  implicit val timeout = Timeout(950 milliseconds)

  "aaa " must {
    "禁止未经认证的用户访问" in {
      doPost("/api/planes", TString("simon"),system.actorOf(Props(new EntityResponder[String]() {
        override def onSuccess(v:String): Unit = {}
        override def requestId = "Post /api/planes"
        status(Forbidden) { res =>
          testActor ! Forbidden
        }
      })))
      expectMsg(Forbidden)
      //      val res = doPost("/api/planes", TString("simon")).await
      //      res.status mustBe Forbidden
    }
    "未提供所有者不能创建领域对象" in {
      doPost("/api/planes",system.actorOf(Props(new EntityResponder[String]() {
        override def requestId = "Post /api/planes"
        override def onSuccess(v: String): Unit = {}

        status(Forbidden) { res =>
          testActor ! Forbidden
        }
      })))
      expectMsg(Forbidden)
    }

    var pid = ""

    "可以创建一个新的领域对象 " in {
      doPost("/api/planes", TString("simon"), user,system.actorOf(Props(new EntityResponder[String]() {
        override def requestId = "Post /api/planes"

        override def onSuccess(value: String): Unit = {
          pid = value
          testActor ! "GotID"
        }
      })))
      expectMsg("GotID")

      doGet(s"/api/planes/$pid/latest/state", user,system.actorOf(Props(new EntityResponder[PlaneState.Pack]() {
        override def requestId = "Get " +  s"/api/planes/$pid/latest/state"

        override def onSuccess(v: PlaneState.Pack): Unit = {
          testActor ! v.id
        }
      })))
      expectMsg(PlaneState.Stopped.id)
    }

    "不存在的命令(报404)或无权执行的命令(报403）会报错" in {
      doPost(s"/api/planes/$pid/action/swim",user,system.actorOf(Props(new EntityResponder[ActionResult]() {
        override def requestId = "Post " +  s"/api/planes/$pid/action/swim"
        override def onSuccess(v: ActionResult): Unit = {}

        status(NotFound) { res =>
          testActor ! NotFound
        }
      })))
      expectMsg(NotFound)

      doPost(s"/api/planes/$pid/action/slide",user,system.actorOf(Props(new EntityResponder[ActionResult]() {
        override def requestId = "Post " +  s"/api/planes/$pid/action/slide"
        override def onSuccess(v: ActionResult): Unit = {}

        status(Forbidden) { res =>
          testActor ! Forbidden
        }
      })))
      expectMsg(Forbidden)
    }

    "状态机可以拒绝不合时宜的命令" in {
      doPost(s"/api/planes/$pid/action/fly",manager,system.actorOf(Props(new EntityResponder[ActionResult]() {
        override def requestId = "Post " +  s"/api/planes/$pid/action/fly"
        override def onSuccess(v: ActionResult): Unit = {}

        status(Forbidden) { res =>
          testActor ! Forbidden
        }
      })))
      expectMsg(Forbidden) //停止状态不能执行起飞指令

      doPost(s"/api/planes/$pid/action/slide",manager,system.actorOf(Props(new EntityResponder[ActionResult]() {
        override def requestId = "Post " +  s"/api/planes/$pid/action/slide"
        override def onSuccess(v: ActionResult): Unit = {
          testActor ! v.param
        }
      })))
      expectMsg(PlaneState.Sliding.id) //停止状态可以执行滑行指令，转到滑行状态

      doPost(s"/api/planes/$pid/action/stop",manager,system.actorOf(Props(new EntityResponder[ActionResult]() {
        override def requestId = "Post " +  s"/api/planes/$pid/action/stop"
        override def onSuccess(v: ActionResult): Unit = {
          testActor ! v.param
        }
      })))
      expectMsg(PlaneState.Stopped.id) //转到停止状态
    }

    "可以请求修改数据结构中某一个节点以下的内容" in {
      doPut(s"/api/planes/$pid/alter/foods/water", TInt(5),manager,system.actorOf(Props(new EntityResponder[ActionResult]() {
        override def requestId = "Put " +  s"/api/planes/$pid/alter/foods/water"
        override def onSuccess(v: ActionResult): Unit = {
          testActor ! "ok"
        }
      })))
      expectMsg("ok")

      doGet(s"/api/planes/$pid/latest/foods/water",user,system.actorOf(Props(new EntityResponder[Int]() {
        override def requestId = "Get " +  s"/api/planes/$pid/alter/foods/water"
        override def onSuccess(v: Int): Unit = {
          testActor ! v
        }
      })))
      expectMsg(5)

    }
  }
}
