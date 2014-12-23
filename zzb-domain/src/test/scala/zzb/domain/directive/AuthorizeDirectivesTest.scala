package zzb.domain.directive

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import org.scalatest.{BeforeAndAfterAll, MustMatchers, WordSpec}
import spray.util._
import zzb.rest._

import scala.concurrent.duration._

/**
 * Created by Simon on 2014/3/25
 */
class AuthorizeDirectivesTest extends WordSpec with MustMatchers
with Requester with BeforeAndAfterAll {

  implicit val system = ActorSystem()

  override protected def beforeAll() {

    system.actorOf(Props[WorkActor], "work")
  }

  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

  implicit val timeout = Timeout(5000000 milliseconds)

  "WorkActor " must {
    import zzb.rest.RestHeaders._
    import zzb.rest.StatusCodes._

    "check opetator before create new MultiQuote" in {

      val res1 = (Post("/work") ~> RawHeader("X-User-Id", "user-Joe42") ~> doRequest).await
      res1.status mustBe OK
      res1.entity.data.asInstanceOf[AuthorizedOperator].isManager mustBe false


      val res2 = (Post("/work")  ~> doRequest).await
      res2.status mustBe Forbidden

      val res3 = (Post("/work")  ~> RawHeader("X-Manager-Id", "manager-simon")~> doRequest).await
      res3.status mustBe OK
      res3.entity.data.asInstanceOf[AuthorizedOperator].isManager mustBe true

    }

  }

}

class WorkActor extends RestServiceActor with AuthorizeDirectives{
  override def receive: Receive = runRoute(route)

  def route: Route =
    operator {
      operator =>
        complete(operator)
    }
}
