package zzb.domain.plane

import zzb.datatype._
import zzb.domain._
import zzb.domain.directive.AuthorizedOperator
import zzb.rest.Caller
import zzb.storage.SpecificStorage

import scala.concurrent.Future


/**
 * Created by Simon on 2014/5/14
 */
class PlaneActor(val domId: String,
                 val specStorage: SpecificStorage[String, TString, Plane.type],
                 val createOwner: Option[String],
                 val isNewCreate: Boolean = true)
  extends DomainFSM[String, TString, Plane.type, PlaneState.Value] with Caller{

  import zzb.domain.plane.PlaneState._

  def logName: String = "zzb.domain.Plane"

  mdc +=("pid" -> domId, "owner" -> createOwner.getOrElse(""))

  private val hlog = HeritLog("pln")

  hlog(log.info(s"planeActor creating..."))

  def executingState = Executing

  implicit val mdcLog = log

  override implicit def takeDocState(doc: Plane.Pack): PlaneState.Value = doc.state

  override def getOwnerFromDoc(doc: Plane.Pack): String = doc.owner

  override val domainType: Plane.type = Plane

  val config = context.system.settings.config
  val maxIdle: Int = if (config.hasPath("zzb.plane.maxIdle")) config.getInt("zzb.plane.maxIdle") else 30 * 60

  override val actionBuilder = PlaneActionBuilder

  startInit()

  override def createDoc: Future[Option[Plane.Pack]] =
    Future {
      createOwner match {
        case None =>
          None
        case Some(own) =>
          Some(Plane(Plane.id := domId, Plane.owner := own))
      }
    }


  def preIdleRelease: Boolean = true

  //  def actionHandler(opt: AuthorizedOperator, params: Map[String, String]): PartialFunction[(Operator, String), Future[Any]] = {
  //    case (_, "stop") => self ? Stop(opt)
  //    case (Manager, "slide") => self ? Slide(opt)
  //    case (Manager, "fly") => self ? Fly(opt)
  //  }

  blockAlter("User", Plane.me.passengers, "用户任何时候都无权修改乘客数据", Stopped, Sliding, Flying)

  blockAlter("Manager", Plane.me.passengers, "在飞机滑行和飞行的时候不能修改乘客数据", Sliding, Flying)

  blockAlter("User", Plane.me.foods().water, "乘客不能修改食物数量", Stopped, Sliding, Flying)

  def ownerChanged(monitorPath: StructPath, newData: Option[Any], newVer: Int, oldData: Option[Any], oldVer: Int) = {
    self ! "OwnerChanged"
  }

  def allChanged(monitorPath: StructPath, newData: Option[Any], newVer: Int, oldData: Option[Any], oldVer: Int) = {
    val a = 0
  }

  def stopTimesChanged(changedPath: StructPath, newData: Option[Any], newVer: Int, oldData: Option[Any], oldVer: Int) = {
    val monitorPath : StructPath = Plane.me.stopTimes
    if(changedPath == monitorPath)
      self ! Message("all stopTimes changed") 
    else{
      val key = changedPath.asInstanceOf[MapPath].key
      (newData,oldData) match {
        case (Some(newMsg:Int),Some(oldMsg:Int)) => self ! Message(s"stopTimes $key changed to $newMsg")
        case (Some(newMsg:Int),None) => self ! Message(s"stopTimes $key added as $newMsg")
        case (None,Some(oldMsg:Int)) => self ! Message(s"stopTimes $key removed")
        case _ => ()
      }
    }
  }

  val  sysUser = AuthorizedOperator("system", isManager = true)

  def generalHandler: StateFunction = {
    case Event("OwnerChanged", doc) =>
      val owner = doc.owner
      if (!owner.startsWith("hello ")) {
        doSave(doc <~ Plane.owner()("hello " + owner), sysUser,null)
      }
      stay()

    case Event(Message(msg),doc) =>
      stay() usingSaved(doc <~  Plane.message()(msg),sysUser)

    case Event(Speed(s), doc) =>
      stay() usingSaved( doc <~ Plane.speed()(s),sysUser)
  }

  monitorAlter(Plane.me.owner, ownerChanged)

  monitorAlter(Plane.me, allChanged)

  monitorAlter(Plane.me.stopTimes ,stopTimesChanged,notifyRealChangedPath = true)

  when(Stopped) {
    case Event(Action("slide", opt, _, _), doc) =>
      goto(Sliding) usingSaved( doc <~ Sliding,opt)
    case Event(A2("slide", opt), doc) =>
      stay()
    case Event(A4("reloadFools", opt, params, Some(Foods(foods))), doc) =>
      stay() usingSaved( doc <~ foods,opt)
  }
  when(Sliding) {
    case Event(Action("stop", opt, _, _), doc) =>
      goto(Stopped) usingSaved( doc <~ Stopped,opt)
    case Event(Action("fly", opt, _, _), doc) =>
      goto(Flying) usingSaved( doc <~ Flying,opt)
    case Event(Action("slide", opt, _, _), doc) =>
      stay()
  }
  when(Flying) {
    case Event(Action("slide", opt, _, _), doc) =>
      goto(Sliding) usingSaved( doc <~ Sliding,opt)
    case Event(Action("fly", opt, _, _), doc) =>
      stay()
  }

  onTransition {
    case Stopped -> Sliding =>
      self.tell(Speed(10), null)
    case Sliding -> Stopped =>
      self.tell(Speed(0), null)
    case Flying -> Sliding =>
      self.tell(Speed(10), null)
    case Sliding -> Flying =>
      self.tell(Speed(100), null)
  }

  when(Executing) {
    restAccessHandler
  }

  when(Stopped, Sliding, Flying) {
    submitAlterHandler orElse longTimeExecHandler orElse restAccessHandler orElse generalHandler
  }
}

case class Speed(s: Int) extends AllowDelay

case class Message(msg:String)

object PlaneActionBuilder extends ActionBuilder {

  action("stop", roleAny)

  action("slide", roleManager)

  action("fly", roleManager)

  action("reloadFools", roleManager, Foods)

}


