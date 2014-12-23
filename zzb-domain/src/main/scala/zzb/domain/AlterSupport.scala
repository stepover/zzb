package zzb.domain

import akka.actor.ActorSystem
import akka.util.Timeout
import spray.http.HttpHeaders.RawHeader
import spray.http.{StatusCode, Uri}
import zzb.domain.directive.AuthorizedOperator
import zzb.rest.{RestHeader, RestResponse, Caller}
import zzb.rest.RestResponse._
import scala.concurrent.Future
import scala.reflect._


/**
 * Created by Simon on 2014/6/19
 */
object AlterSupport extends Caller {

  private def genPhaseError(phase: AlterPhase.Value): RestResponse => Throwable = r =>
    new AlterFailed(phase, r.status, if (r.entity.nonEmpty) r.entity.asString else "")


  def requestPhase(r: RestResponse): ActionResult =
    to[ActionResult](r)(null, genPhaseError(AlterPhase.Request), classTag[ActionResult])

  def executePhase(r: RestResponse): ActionResult =
    to[ActionResult](r)(null, genPhaseError(AlterPhase.Execute), classTag[ActionResult])

  def submitPhase(r: RestResponse): ActionResult =
    to[ActionResult](r)(null, genPhaseError(AlterPhase.Submit), classTag[ActionResult])


  def doAlter[T](uri: Uri,alterPath:String, data: T)(implicit rootUri: Uri, timeout: Timeout, system: ActorSystem): Future[ActionResult] = doAlter(uri,alterPath, data, null)

  def doAlter[T](docUri: Uri,alterPath:String, data: T, opt: AuthorizedOperator)(implicit rootUri: Uri, timeout: Timeout, system: ActorSystem): Future[ActionResult] = {

    val uriStr = docUri.toString()
    val docUriStr = if (uriStr.endsWith("/")) uriStr.substring(0, uriStr.length - 1) else uriStr
    require(!docUriStr.endsWith("alter"), "doAlter url can't end with alter!")
    require(uriStr != "/", "no support alter on root path!")

    import system.dispatcher

    val alterDocUri = Uri(docUriStr + "/alter")

    val auth: List[RestHeader] =
      if (opt != null)
        List(RawHeader(if (opt.isManager) "X-Manager-Id" else "X-User-Id", opt.id))
      else
        Nil

    for {
      requestRes <- doPost(alterDocUri, auth).map(requestPhase)
      sessionUri = alterDocUri.withPath(alterDocUri.path.+(s"/${requestRes.param}"))
      putUri = Uri(sessionUri.toString() + s"$alterPath?errorThenAbandon=true")
      vr <- doPut(putUri, data, auth).map(executePhase)
      submitRes <- doPost(sessionUri, auth).map(submitPhase)
    } yield submitRes
  }

  object AlterPhase extends Enumeration {
    val Request = Value(0, "Request")
    val Execute = Value(1, "Execute")
    val Submit = Value(2, "Submit")
  }

  case class AlterFailed(phase: AlterPhase.Value, status: StatusCode, msg: String) extends
  Exception(s"alter failed on phase [Alter ${phase.toString}],status code : $status,more msg:$msg}")


}
