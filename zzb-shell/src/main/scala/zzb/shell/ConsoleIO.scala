package zzb.shell

import java.io._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-13
 * Time: 下午5:36
 * Copyright baoxian.com 2012~2020
 */
class ConsoleIO(val ins: InputStream,
                val out: PrintStream,
                val err: PrintStream)
    extends Input with Output with ShellManageable {

  def this() = {
    this(System.in, System.out, System.err)
  }

  val in =  new BufferedReader(new InputStreamReader(ins))

  var lastCommandOffset = 0

  private final val USER_PROMPT_SUFFIX: String = "> "
  private final val FILE_PROMPT_SUFFIX: String = "$ "
  private var log: PrintStream = null

  private final object InputState extends Enumeration {
    final val USER, SCRIPT = Value
  }

  private var inputState = InputState.USER

  def inStream : InputStream = ins

  def readCommand(path: List[String]): String = {
    try {
      val prompt: String = path.mkString("/")
      inputState match {
        case InputState.USER ⇒
          return readUsersCommand(prompt)
        case InputState.SCRIPT ⇒
          val command: String = readCommandFromScript(prompt)
          if (command != null) {
            return command
          } else {
            closeScript
            return readUsersCommand(prompt)
          }
      }
      return readUsersCommand(prompt)
    } catch {
      case ex: IOException ⇒ {
        throw new Error(ex)
      }
    }
  }
  def readClearCommand : String = {
    in.readLine
  }

  private def readUsersCommand(prompt: String): String = {
    val completePrompt: String = prompt + USER_PROMPT_SUFFIX
    print(completePrompt)
    lastCommandOffset = completePrompt.length
    val command: String = in.readLine
    if (log != null) {
      log.println(command)
    }
    command
  }

  private var scriptReader: BufferedReader = _

  private def readCommandFromScript(prompt: String): String = {
    val command: String = scriptReader.readLine
    if (command != null) {
      val completePrompt: String = prompt + FILE_PROMPT_SUFFIX
      print(completePrompt)
      lastCommandOffset = completePrompt.length
    }
    command
  }

  private def closeScript {
    if (scriptReader != null) {
      scriptReader.close
      scriptReader = null
    }
    inputState = InputState.USER
  }

  def runScript(filename: String) {
    scriptReader = new BufferedReader(new InputStreamReader(new FileInputStream(filename)))
    inputState = InputState.SCRIPT
  }

  def outputHeader(text: String) {
    if (text != null) {
      println(text)
    }
  }

  def output(obj: AnyRef, oce: OutputConversionEngine) {
    if (obj != null) {

      val outResult = oce.convertOutput(obj)

      outResult match {
        case collection: Traversable[_] ⇒
          collection.foreach(ent ⇒ output(ent.asInstanceOf[AnyRef], 0, oce))
        case _ ⇒
          output(outResult, 0, oce)
      }
    }
  }

  private def output(obj: AnyRef, indent: Int, oce: OutputConversionEngine): Unit = {
    if (obj != null) {

      val outResult = oce.convertOutput(obj)

      (1 until indent).foreach(_ ⇒ print("\t"))

      outResult match {
        case null        ⇒ println("(null)")

        //case value: AnyVal => println(value)

        case str: String ⇒ println(str)

        case collection: Traversable[_] ⇒
          println("Collection")
          collection.foreach(ent ⇒ output(ent.asInstanceOf[AnyRef], indent + 1, oce))

        case ex: Throwable ⇒ ex.printStackTrace(out)

        case _             ⇒ println(outResult)

      }
    }
  }

  private def print(x: AnyRef) {
    out.print(x) ;out.flush()
    if (log != null) {
      log.print(x)
    }
  }

  private def println(x: AnyRef) {
    out.println(x)
    if (log != null) {
      log.println(x)
    }
  }

  def printErr(x: AnyRef) {
    err.print(x) ;err.flush()
    if (log != null) {
      log.print(x)
    }
  }

  private def printlnErr(x: AnyRef) {
    err.println(x)
    if (log != null) {
      log.println(x)
    }
  }

  def outputException(input: String, error: TokenException) {
    val errIndex: Int = error.token.index + lastCommandOffset

    (1 until errIndex).foreach(_ ⇒ printErr("-"))
    (1 until error.token.string.length).foreach(_ ⇒ printErr("-"))

    printlnErr("")
    printlnErr(error)
    println("")
  }

  def outputException(e: Throwable) {
    printlnErr(e)
    if (e.getCause != null) {
      printlnErr(e.getCause)
    }
    println("")
  }

  def outputException(input: String, e: Throwable) {
    printlnErr(input)
    if (e.getCause != null) {
      printlnErr(e.getCause)
    }
    println("")
  }

  private def isLoggingEnabled: Boolean = {
    log != null
  }

  private var loopCounter: Int = 0

  /**
   * This method is called when it is about to enter the command loop.
   */
  def cliEnterLoop {
    if (isLoggingEnabled) {
      loopCounter += 1
    }
  }

  /**
   * This method is called when Shell is leaving the command loop.
   */
  def cliLeaveLoop {
    if (isLoggingEnabled) {
      loopCounter -= 1
    }

    if (loopCounter < 0) {
      disableLogging
    }
  }

  def enableLogging(filename: String) {
    log = new PrintStream(filename)
    loopCounter = 0
  }

  def disableLogging: String = {
    if (log != null) {
      log.close
      log = null
      return "Logging disabled"
    } else return "Logging is already disabled"
  }

}
