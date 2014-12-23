package zzb.shell

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-13
 * Time: 下午5:14
 * Copyright baoxian.com 2012~2020
 */
/**
 *
 * Root exception for Cliche.
 *
 * @author ASG
 */
object CLIException {
  def createCommandNotFound(commandName: String): CLIException = {
    return new CLIException("Unknown command: " + Token.escapeString(commandName))
  }

  def createCommandNotFoundForArgNum(commandName: String, argCount: Int): CLIException = {
    return new CLIException("There's no command " + Token.escapeString(commandName) + " taking " + argCount + " arguments")
  }

  def createAmbiguousCommandExc(commandName: String, argCount: Int): CLIException = {
    return new CLIException("Ambiguous command " + Token.escapeString(commandName) + " taking " + argCount + " arguments")
  }
}

class CLIException(message: String, cause: Option[Exception]) extends Exception(message, cause.orNull) {

  def this(message: String) = {
    this(message, null)
  }

}