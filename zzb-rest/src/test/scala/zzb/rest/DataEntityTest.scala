package zzb.rest

import akka.actor.{ Actor, ActorSystem, Props }
import akka.pattern._
import akka.util.Timeout
import org.scalatest.{ BeforeAndAfterAll, MustMatchers, WordSpec }
import spray.http.{ DateTime, Uri }
import spray.json.JsonFormat
import spray.util._
import zzb.datatype._
import zzb.rest.RestEntity.NonEmpty
import zzb.rest.util.{ MessageStat, RequestStat }

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.reflect._
import scala.util.{ Failure, Success }

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-3-12
 * Time: 上午9:00
 * Copyright baoxian.com 2012~2020
 */
class DataEntityTest extends WordSpec
  with MustMatchers
  with Caller with BeforeAndAfterAll {

  implicit val system = ActorSystem()

  override protected def beforeAll() {
    system.actorOf(Props[AccountActor], "account")
    system.actorOf(Props[AccountActor2], "account2")

  }

  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

  implicit val timeout = Timeout(500 milliseconds)
  implicit val rootUri = Uri("")

  def checkRes(res: RestResponse) = if (res.status != StatusCodes.OK) println(res.entity.data.toString)

  "case class " must {
    "can be send and receive" in {

      val account = Account("simon", 39)

      val res0 = doPost("/account/name", account).await
      //checkRes(res0)
      res0.status must equal(StatusCodes.OK)
      res0.entity.data must equal(account.name)

      val res01 = doGet("/account/age", account).await
      checkRes(res01)
      res01.status must equal(StatusCodes.OK)
      res01.entity.data must equal(account.age)

      val res1 = doPost("/account", account).await
      checkRes(res1)
      res1.status must equal(StatusCodes.OK)
      res1.entity.data must equal(account)

      val res2 = doPost("/account2", account).await
      checkRes(res2)
      res2.status must equal(StatusCodes.OK)
      res2.entity.data must equal(account)

      val res3 = doGet("/account2").await
      checkRes(res3)
      res3.status must equal(StatusCodes.OK)
      res3.entity.data must equal(account)

      //故意给出错误的类型
      val res4 = doPost("/account2", Info("home")).await
      checkRes(res4)
      res4.status must equal(StatusCodes.BadRequest)

      val res5 = doPut("/account2", account).await
      checkRes(res5)
      res5.status must equal(StatusCodes.OK)

      val s1 = User("jack", 3)

      val res6 = doPost("/account2/user", s1).await
      checkRes(res6)
      res6.status must equal(StatusCodes.OK)
      res6.entity.data mustBe s1
    }
  }
}

class AccountActor extends RestServiceActor {

  def receive: Actor.Receive = runRoute(route)

  def route: Route = {

    pathPrefix("ok") { ctx ⇒
      println("response ok in actorActor Route =========>")
      Thread.sleep(500)
      ctx.complete("ok")
    } ~
      pathPrefix("name") {
        ctx ⇒
          ctx.request.entity match {
            case NonEmpty(account: Account) ⇒ ctx.complete(account.name) //字符串可以正常返回
            case _                          ⇒ sender ! RestResponse(StatusCodes.InternalServerError)
          }
      } ~
      pathPrefix("tname") {
        ctx ⇒
          ctx.request.entity match {
            case NonEmpty(account: Account) ⇒ ctx.complete(TString(account.name)) //字符串可以正常返回
            case _                          ⇒ sender ! RestResponse(StatusCodes.InternalServerError)
          }
      } ~
      pathPrefix("age") {
        ctx ⇒
          ctx.request.entity match {
            case NonEmpty(account: Account) ⇒ ctx.complete(account.age) //值类型无法直接返回
            case _                          ⇒ sender ! RestResponse(StatusCodes.InternalServerError)
          }
      } ~
      pathEndOrSingleSlash {
        ctx ⇒
          ctx.request.entity match {
            case NonEmpty(account: Account) ⇒ ctx.complete(account) //case class 可以直接返回
            case _                          ⇒ sender ! RestResponse(StatusCodes.InternalServerError)
          }
      }
  }
}

class AccountActor2 extends RestServiceActor {

  def receive: Actor.Receive = runRoute(route)

  implicit val timeout = Timeout(500 milliseconds)

  import context.dispatcher

  def divide(a: Int, b: Int): Future[Int] = Future {
    a / b
  }

  import zzb.rest.testuse.Student

  def route: Route = {

    pathEndOrSingleSlash {
      post {
        entity(as[Account]) {
          //解析出 Account 类型的对象 ，与AccountActor 不同的地方是使用了 entity
          account ⇒
            complete(account) //case class 可以直接返回
        }
      } ~
        get {
          respondWithHeaders(RestHeaders.`Last-Modified`(DateTime.now)) {
            complete(Account("simon", 39))
          }
        } ~
        put {
          entity(as[Account]) {
            account ⇒
              ctx ⇒
                ctx.complete(Account("simon", 39))
          }
        }
    } ~ path("stat") {
      get {
        onComplete((self ? RequestStat).asInstanceOf[Future[MessageStat]]) {
          case Success(stat: MessageStat) ⇒ complete(stat)
          case Failure(e)                 ⇒ reject
        }
      }
    } ~ path("user") {
      post {
        entity(as[User]) { user ⇒
          complete(user) //case class 可以直接返回
        }
      }
    } ~ path("student") {
      post {
        entity(unpack(Student)) { s ⇒
          ctx ⇒
            ctx.complete(s) //TStruct.Pack 可以直接返回
        }
      }
    } ~
      path("task") {
        post {
          entity(unpack(zzb.rest.testuse.Task)) { s ⇒
            println("task in " + s)
            ctx ⇒ ctx.complete(s) //Task.Pack 可以直接返回
          }
        }
      } ~ path(Segment) { dataType ⇒
        post {
          dataType match {

            case "TString" ⇒
              entity(unpack(TString)) { s ⇒
                ctx ⇒ ctx.complete(s) //TString.Pack 可以直接返回
              }
            case "TBoolean" ⇒
              entity(unpack(TBoolean)) { s ⇒
                ctx ⇒ ctx.complete(s) //TBoolean.Pack 可以直接返回
              }
            case "TDateTime" ⇒
              entity(unpack(TDateTime)) { s ⇒
                ctx ⇒ ctx.complete(s) //TDateTime.Pack 可以直接返回
              }
            case "TBigDecimal" ⇒
              entity(unpack(TBigDecimal)) { s ⇒
                ctx ⇒ ctx.complete(s) //TBigDecimal.Pack 可以直接返回
              }
            case "TInt" ⇒
              entity(unpack(TInt)) { s ⇒
                ctx ⇒ ctx.complete(s) //TInt.Pack 可以直接返回
              }
            case "TLong" ⇒
              entity(unpack(TLong)) { s ⇒
                val a = TLong(14L)
                ctx ⇒ ctx.complete(s) //TLong.Pack 可以直接返回
              }
            case "TShort" ⇒
              entity(unpack(TShort)) { s ⇒
                ctx ⇒ ctx.complete(s) //TShort.Pack 可以直接返回
              }
            case "String" ⇒
              entity(as[String]) { s ⇒
                ctx ⇒ ctx.complete(s) //String可以直接返回
              }
            /*case "Int" ⇒
            entity(as[Int]) { s ⇒
              ctx ⇒ ctx.complete(s) //Int可以直接返回
            }*/
            case "TFloat" ⇒
              entity(unpack(TFloat)) { s ⇒
                ctx ⇒ ctx.complete(s) //TFloat.Pack可以直接返回
              }
            case "Sex" ⇒
              entity(unpack(Sex)) { s ⇒
                ctx ⇒ ctx.complete(s) //Sex.Pack可以直接返回
              }
            case "TList" ⇒
              entity(unpack(CLists)) { s ⇒
                ctx ⇒ ctx.complete(s) //TList.Pack可以直接返回
              }
            case "TMap" ⇒
              entity(unpack(TMaps)) { s ⇒
                ctx ⇒ ctx.complete(s) //TMap.Pack可以直接返回
              }

            case other ⇒ complete(StatusCodes.BadRequest, s"参数不匹配：$other")
          }
        }
      }
  }
}

case class Account(name: String, age: Int)

case class User(name: String, age: Int)

case class Info(address: String)

object Sex extends Enumeration with TEnum {
  val t_memo_ = "性别"
  val Man = Value(1, "男")
  val Woman = Value(2, "女")
}

object TMaps extends TStrKeyMap[Int] {
  val t_memo_ = "测试Map"

  import zzb.datatype.BasicFormats._

  override val km: ClassTag[_] = classTag[String]
  override val vm: ClassTag[_] = classTag[Int]
  override implicit val keyFormat: JsonFormat[String] = StringJsonFormat
  override implicit val valueFormat: JsonFormat[Int] = IntJsonFormat
}

object CList extends TStruct {
  val t_memo_ = "机构信息"
  val lcase = FieldString(TString("lcase", "例子"))
}

object CLists extends TPackList[CList.Pack] {
  val t_memo_ = "机构列表"
  override val lm: ClassTag[_] = classTag[CList.Pack]
  implicit val elementFormat: JsonFormat[CList.Pack] = CList.Format

  override def itemDataType: DataType[Any] = CList

}

