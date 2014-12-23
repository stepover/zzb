package zzb.shell

import scala.collection.mutable.ListBuffer
import java.io.{ByteArrayOutputStream, PrintStream}
import scala.concurrent.{ExecutionContext, Future, Promise, Await}
import scala.concurrent.duration.Duration
import com.typesafe.config.Config
import wash.shell.ConsolePrintStream

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-24
 * Time: 下午9:00
 * Copyright baoxian.com 2012~2020
 */
trait Task extends Action[Unit] with DelayedInit {

  private[shell] var inited = Promise[Boolean]()

  private val initCode = new ListBuffer[() ⇒ Unit]
  private var _shell: Shell = _

  private[shell] var _err: PrintStream = _
  private[shell] var _out: PrintStream = _

  /** 啰嗦模式是否开启，任务实现可以根据这个设置决定输出信息的多少 */
  private[shell] var _verbose  = true

  //必须 task 和 shell 同时开启verbose 模式才有效
  def verbose = _verbose && shell.verbose

  private var _taskInfo: TaskInfo = _

  private[shell] var _objParam :Option[Any] = None

  def objectParam = _objParam

  protected def args: List[String] = _args

  private var _args: List[String] = _

  private var _params  : Map[String,String] = _

  def params ={
    if(_params != null ) _params
    else {
      _params = Map()
      args.foreach{ arg =>
        val pv  = arg.split("=",2)
        if(pv.length == 2){
          _params = _params + (pv(0) -> pv(1))
        }
       }
     }
    _params

  }

  final def err = _err

  final def out = _out

  def usage = ""

  def checkArgs = true

  def syncMode = TaskSyncMode.Free

  def taskInfo = _taskInfo

  def shell = {
    _shell
  }

  override def delayedInit(body: ⇒ Unit) {
    initCode += (() ⇒ body)
  }

  def func(): Unit = {
    while (inited == null) Thread.sleep(5)
    Await.result(inited.future, Duration.Inf)
    Console.withOut(out) {
      Console.withErr(err) {
        if (!checkArgs) println(usage)
        else for (proc ← initCode) proc()
      }
    }
  }

  def fullName = taskInfo.fullName

  def execOther(line: String) {
    if (_shell != null)
      _shell.requestExeCmd(line)
  }

  def fromOther[T](line: String, forceUpdate: Boolean = false,objParam:Option[Any] = None): Future[T] =
    _shell.fromOther[T](line,forceUpdate,objParam)

}


trait Pipable[T] {
  t: Task =>
  import system.dispatcher
  private val p = Promise[T]()

  def pipeToNext(result: T) = p.success(result)

  def errorToNext(e:Throwable) = p.failure(e)

  def resultFuture = p.future

  t.onFailure{
    case e:Throwable => p.failure(e.getCause)
  }
}

object TaskSyncMode extends Enumeration {
  val MustSync = Value
  val MustAsync = Value
  val Free = Value
}

class TaskNotFoundException(val taskName: String, cause: Option[Exception])
  extends RuntimeException(s"task $taskName not found", cause.orNull) {
  // Overridden to provide covariant return type as a convenience
  override def getCause: Exception = cause.orNull
}

class TaskDuplicateException(val taskName: String, val multiTask: List[TaskInfo])
  extends RuntimeException() {

  override def getMessage = {
    var msg = s"found more than task named [$taskName]:\n"
    for (info ← multiTask) {
      msg = msg + s"\t\t ${info.space}.${info.shortName} [${info.className}] \n"
    }
    msg
  }

}

case class TaskInfo(space: String, shortName: String, className: String, desc: String) {
  def fullName = space + "." + shortName
}

object Task {

  val defaultSpace = "_" //缺省名字空间

  //保存一个 space 中的所有 task
  private[shell] val spacesIndex = scala.collection.mutable.HashMap[String, List[TaskInfo]]()

  //保存一个 short name 下的所有任务
  private[shell] val shortNameIndex = scala.collection.mutable.HashMap[String, List[TaskInfo]]()

  private[shell] val taskIndex = scala.collection.mutable.HashMap[String, TaskInfo]()

  parseOne("tasks", "zzb.shell.task.Tasks")
  parseOne("*", "zzb.shell.task.WildCard")
  parseOne("remote-login", "zzb.shell.remote.RemoteLogin")
  parseOne("verbose", "zzb.shell.task.Verbose")

  def apply(key: String, out: PrintStream, err: PrintStream, shell: Shell, args: List[String]): Task = {
    val info = taskInfo(key)
    try {
      val task = Class.forName(info.className).newInstance().asInstanceOf[Task]
      task._err = err
      task._out = out
      task._shell = shell
      task._taskInfo = info
      task._args = args
      task.inited.success(true)
      task

    } catch {
      case ex: Exception ⇒
        throw new TaskNotFoundException(info.fullName, Some(ex))
    }
  }

  def apply(key: String, shell: Shell, args: List[String], isAsync: Boolean = false,verbose:Boolean = true,objParam :Option[Any] = None): Task = {

    val info = taskInfo(key)
    try {
      val task = Class.forName(info.className).newInstance().asInstanceOf[Task]
      val syncMode = task.syncMode match {
        case TaskSyncMode.Free => if (isAsync) TaskSyncMode.MustAsync else TaskSyncMode.MustSync
        case _ => task.syncMode
      }
      task._err = if (syncMode == TaskSyncMode.MustAsync) new ConsolePrintStream(new ByteArrayOutputStream) else shell.output.out
      task._out = if (syncMode == TaskSyncMode.MustAsync) new ConsolePrintStream(new ByteArrayOutputStream) else shell.output.out
      task._taskInfo = info
      task._shell = shell
      task._args = args
      task._verbose = verbose
      task._objParam = objParam
      task.inited.success(true)
      task

    } catch {
      case ex: Exception ⇒
        throw new TaskNotFoundException(key, Some(ex))
    }
  }


  def taskInfo(key: String): TaskInfo = {
    if (taskIndex.contains(key)) taskIndex(key)
    else if (shortNameIndex.contains(key)) {
      val namedTasks = shortNameIndex(key)
      if (namedTasks.size == 1) namedTasks.head
      else throw new TaskDuplicateException(key, namedTasks)
    } else throw new TaskNotFoundException(key, None)
  }

  def parseConfig(config: Config) = {

    import scala.collection.JavaConversions._
    config.entrySet().foreach {
      entry ⇒
        parseOne(entry.getKey, config.getString(entry.getKey))
    }
  }

  def parseOne(key: String, value: String) = {

    val taskId = key.trim

    if (taskIndex.contains(taskId))
      throw new TaskDuplicateException(key, Nil)
    val idParts = taskId.split('.').toList.reverse
    val shortName = idParts.head
    val space = if (idParts.tail == Nil) defaultSpace else idParts.tail.reverse.mkString(".")
    val values = value.split(";").toList
    val className = values.head.trim
    val desc = if (values.tail == Nil) "" else values.tail.mkString

    val info = TaskInfo(space, shortName, className, desc.trim)

    taskIndex += ((space + "." + shortName) -> info)

    if (spacesIndex.contains(info.space))
      spacesIndex(info.space) = info :: spacesIndex(info.space)
    else
      spacesIndex(info.space) = info :: Nil

    if (shortNameIndex.contains(info.shortName))
      shortNameIndex(info.shortName) = info :: shortNameIndex(info.shortName)
    else
      shortNameIndex(info.shortName) = info :: Nil
  }

}