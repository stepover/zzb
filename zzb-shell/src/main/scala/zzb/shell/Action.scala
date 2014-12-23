package zzb.shell

import scala.concurrent.{ Promise, CanAwait, Future, ExecutionContext }
import scala.util.{ Failure, Success, Try }
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeoutException
import scala.util.control.NonFatal


/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-12
 * Time: 上午11:27
 * Copyright baoxian.com 2012~2020
 */

trait Action[T] {

  val system = Shell.system
  import system.dispatcher

  private val stat_p = Promise[ActionStat]()

  val future = Future(actionExec)

  private val startTime = System.currentTimeMillis



  def isCompleted: Boolean = future.isCompleted

  def func(): T

  private def actionExec: T = {
    try {
      func()
    } catch {
      case ex: Throwable ⇒ throw ActionException(this.getClass.getSimpleName, stat, ex)
    }
  }

  def map[S](f: T ⇒ S)(implicit executor: ExecutionContext, res: StatResult = null): Future[S] = {
    if (res != null) {
      res.addStat(stat)
    }
    future.map(f)(executor)

  }

  def flatMap[S](f: T ⇒ Future[S])(implicit executor: ExecutionContext, res: StatResult = null): Future[S] = {
    if (res != null) {
      res.addStat(stat)
    }
    future.flatMap(f)(executor)

  }

  def filter(pred: T ⇒ Boolean)(implicit executor: ExecutionContext, res: StatResult = null): Future[T] = {
    future.filter(pred)(executor)
  }

  def ready(atMost: Duration)(implicit permit: CanAwait): this.type =
    if (future.ready(atMost)(permit) eq future) this
    else throw new TimeoutException("Task timed out after [" + atMost + "]")

  def result(atMost: Duration)(implicit permit: CanAwait): T = future.result(atMost)(permit)

  def onComplete[U](func: (Try[T]) ⇒ U)(implicit executor: ExecutionContext) = future.onComplete(func)(executor)

  def onSuccess[U](pf: PartialFunction[T, U])(implicit executor: ExecutionContext): Unit = onComplete {
    case Success(v) if pf isDefinedAt v ⇒ pf(v)
    case _                              ⇒
  }(executor)

  def onFailure[U](callback: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): Unit = onComplete {
    case Failure(t) if NonFatal(t) && callback.isDefinedAt(t) ⇒ callback(t)
    case _ ⇒
  }(executor)

  def andThen[U](pf: PartialFunction[Try[T], U])(implicit executor: ExecutionContext): Future[T] = {
    future.andThen(pf)(executor)
  }

  def zip[U](that: Action[U]): Future[(T, U)] = {
    future.zip(that.future)
  }

  this.onComplete {
    case Success(v) ⇒ {
      stat_p.success(ActionStat(this.getClass.getSimpleName, startTime,
        System.currentTimeMillis() - startTime, msg, None))

    }
    case Failure(ex: ActionException) ⇒
      stat_p.success(ActionStat(this.getClass.getSimpleName, startTime,
        System.currentTimeMillis() - startTime, msg, Some(ex.getCause)))

    case Failure(ex) ⇒
      stat_p.success(ActionStat(this.getClass.getSimpleName, startTime,
        System.currentTimeMillis() - startTime, msg, Some(ex)))
  }

  def stat = stat_p.future

  private var _msg = ""

  protected def msg(content: String) = {
    _msg = content
  }

  protected def msg = _msg
}

case class ActionException(actionName: String, stat: Future[ActionStat], cause: Throwable) extends RuntimeException(cause)

case class ActionStat(actionName: String, startTime: Long, timeSpan: Long, msg: String, ex: Option[Throwable]) {
  def isSucceed = !ex.isDefined
}

class StatResult {
  val p = Promise[List[Future[ActionStat]]]()
  var stats = List[Future[ActionStat]]()

  def addStat(stat: Future[ActionStat]) = stats ::= stat

  def end = p.success(stats.toList)

  def future = p.future
}
object StatResult {
  def apply() = new StatResult
}

object Action {

  def stat[T](body: ⇒ Future[T])(implicit executor: ExecutionContext, res: StatResult) = {

    val f = body
    f.onComplete {
      case _ ⇒ res.end
    }
    f
  }
}
