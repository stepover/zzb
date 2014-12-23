package zzb.shell

import org.slf4j.LoggerFactory
import spray.caching.ExpiringLruCache
import wash.shell.ConsolePrintStream
import java.io._
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import zzb.shell.remote.ShellDaemon
import scala.util.Failure
import scala.Some
import scala.util.Success
import java.util.concurrent.TimeUnit
import akka.actor.ActorSystem
import com.typesafe.config.Config
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.Logging


/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-13
 * Time: 下午3:36
 * Copyright baoxian.com 2012~2020
 */
class Shell(val appName: String, val setting: Setting, val pipeToIn: Option[OutputStream],
            val path: List[String],notUseIoIn :Boolean = false) {

  implicit val system = Shell.system

  import system.dispatcher

  val outputConverter = new OutputConversionEngine

  private val pipeToInPrinter: Option[PrintStream] = pipeToIn match {
    case None => None
    case Some(outToIn) => Some(new PrintStream(outToIn))
  }

  def pipeToIn(text: String) = {
    if (pipeToInPrinter.isDefined)
      pipeToInPrinter.get.println(text)
  }

  def output = setting.output

  def input = setting.input

  private var lastException: Throwable = null

  //var remote: RemoteAccess = _

  private[this] val results = scala.collection.mutable.HashMap[String, Any]()

  private val todoCommands = new scala.collection.mutable.SynchronizedQueue[String]()

  /** 是否开启详细输出，各 Task 可以检索这个变量，决定是否输出某些信息  */
  private[shell] var verbose_ = true

  def verbose = verbose_


  def requestExeCmd(line: String) {
    todoCommands.enqueue(line)
  }


  /**
   * Runs the command session.
   * Create the Shell, then run this method to listen to the user,
   * and the Shell will invoke Handler's methods.
   * @throws java.io.IOException when can't readLine() from input.
   */
  private def commandLoop() = {
    output.output(appName, outputConverter)
    var command: String = if (todoCommands.size != 0 || notUseIoIn) todoCommands.dequeue() else input.readCommand(path).trim
    while (!(command == "exit")) {
      try {
        val isAsyn = command.endsWith("&")
        val lineText = if (isAsyn) command.substring(0, command.length - 1) else command

        lineText match {
          case "" => ()
          case "daemon-install" => ShellDaemon.install(this.appName)
          case "daemon-remove" => ShellDaemon.remove()
          case _ =>
            val task = processLine(lineText, isAsyn)
            checkTaskResult(task, isAsyn)

        }

      } catch {
        case te: TokenException ⇒
          lastException = te
          output.outputException(command, te)

        case clie: CLIException ⇒
          lastException = clie
          if (!(command.trim == "exit")) {
            output.outputException(clie)
          }


        case ex: TaskNotFoundException ⇒
          output.outputException(s"error: ${ex.getMessage}", ex)
        case ex: TaskDuplicateException ⇒
          output.outputException(s"error: ${ex.getMessage}", ex)
        case ex: Throwable ⇒
          ex.printStackTrace(output.err)
      }
      Thread.sleep(100) //防止输出串乱
      command = if (todoCommands.size != 0 || notUseIoIn) todoCommands.dequeue() else input.readCommand(path).trim
    }

    output.output(s"$appName shell exit!", outputConverter)
  }

  /**
   * You can operate Shell linewise, without entering the command loop.
   * All output is directed to shell's Output.
   *
   * @see asg.cliche.Output
   *
   * @param line Full command line
   * @throws asg.cliche.CLIException This may be TokenException
   */
  private[shell] def processLine(line: String, isAsync: Boolean,verbose:Boolean = true,objParam:Option[Any] = None): Task = {


    val tokens: List[Token] = Token.tokenize(line)

    val cmdTxt: String = tokens.head.string

    val task = if (cmdTxt.endsWith("*"))
      Task("*", this, List(cmdTxt.replace("*", "")), isAsync = false)
    else {
      Task(tokens.head.string, this, tokens.tail.map(_.string), isAsync,verbose,objParam)
    }
    task
  }

  val inCache = new ExpiringLruCache[Any](1000, 20, Duration(240, TimeUnit.SECONDS), Duration(120, TimeUnit.SECONDS))

  /**
   * 获取其他命令的执行结果
   * @param line 命令执行语句
   * @tparam T   返回类型
   * @return
   */
  private[shell] def fromOther[T](line: String, forceUpdate: Boolean = false,objParam:Option[Any] = None): Future[T] = {

    if (forceUpdate) inCache.remove(line)
    inCache.apply(line, () => {
      val preTask = processLine(line, isAsync = true,verbose = false,objParam)  //调用其他任务，让其他任务进入非 verbose 模式
      preTask.asInstanceOf[Pipable[T]].resultFuture

    }).asInstanceOf[Future[T]]
  }

  def taskResult(key: String, value: Any) {
    results += key -> value
  }

  def taskResult(key: String) = {
    results(key)
  }

  def taskResultClear(key: String) {
    results.remove(key)
  }

  private def checkTaskResult(task: Task, isAsyn: Boolean = false) = {

    if (isAsyn)
      output.output(s"[${task.fullName} running background]", outputConverter)
    task.onComplete {
      case _ ⇒
        if (isAsyn)
          output.output(s"[${task.fullName} over]", outputConverter)
        (task.out, task.err) match {
          case (out: ConsolePrintStream, err: ConsolePrintStream) ⇒
            output.output(out.content(), outputConverter)
            output.output(err.content(), outputConverter)
          case _ ⇒ ()
        }
    }
    sumaryTask(task)

    if (!isAsyn)
      Await.result(task.future, Duration.Inf)
  }

  private def sumaryTask(task: Task) = {
    //监控日志
    val log = LoggerFactory.getLogger("shellTask." + task.fullName)
    task.stat.andThen {
      case Success(s) ⇒
        if (s.ex.isDefined) {
          log.error("[ TimeSpan:" + s.timeSpan + " ms]")
        } else {
          log.info("[ TimeSpan:" + s.timeSpan + " ms]")
        }
      case Failure(ex: ActionException) ⇒ log.info("[ TimeSpan:" + ex.getCause.toString + " ms]")
      case Failure(ex) ⇒ log.info("[ TimeSpan:" + ex.toString + " ms]")
    }
  }

}

case class Setting(input: Input, output: Output, displayTime: Boolean)

object Shell extends Logging{

  def apply(prompt: String, appName: String, initCmds:Seq[String] = Nil,sync: Boolean = true,notUseIoIn :Boolean = false) = {

    require(system_ != null, "Shell must be init .")
    val io = new ConsoleIO()
    val theShell = new Shell(appName, Setting(io, io, displayTime = false), None, List(prompt),notUseIoIn)
    initCmds.foreach(cmd => theShell.requestExeCmd(cmd))
    if (sync) new ShellRunner(appName,theShell).run()
    else new Thread(new ShellRunner(appName,theShell)).start()

    theShell
  }

  def apply(prompt: String, appName: String,
            in: InputStream, out: PrintStream, err: PrintStream, pipeToIn: Option[OutputStream],
            sync: Boolean ,notUseIoIn :Boolean ) = {
    require(system_ != null, "Shell must be init .")

    val io = new ConsoleIO(in, out, err)
    val theShell = new Shell(appName, Setting(io, io, displayTime = false), pipeToIn, List(prompt),notUseIoIn)
    if (sync) new ShellRunner(appName,theShell).run()
    else new Thread(new ShellRunner(appName,theShell)).start()

    theShell
  }

  def system: ActorSystem = system_

  def config: Config = config_

  def init(cfg: Config, sys: ActorSystem, sysOnlyForShell: Boolean = true) = {
    system_ = sys
    config_ = cfg
    sysOnlyForShell_ = sysOnlyForShell
  }

  private var system_ : ActorSystem = _

  private var config_ : Config = _

  private var sysOnlyForShell_ = true

  val counter = new AtomicInteger(0)


  private class ShellRunner(val name:String ,val shell: Shell) extends Runnable {
    def run() {
      Thread.sleep(200)

      counter.incrementAndGet()

      logger.info(s" shell [$name] started .")
      shell.commandLoop()
      logger.info(s" shell [$name] exited .")

      val runningCount = counter.decrementAndGet()
      if (runningCount == 0 && sysOnlyForShell_) system_.shutdown()
    }
  }

}
