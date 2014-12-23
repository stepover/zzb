package zzb.shell

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-13
 * Time: 下午5:22
 * Copyright baoxian.com 2012~2020
 */
/**
 * Exception pointing at the token which caused it.
 * Used to report invalid parameter types.
 *
 * @author ASG
 */
class TokenException(val token: Token, message: String, cause: Option[Exception]) extends CLIException(message, cause) {

  def this(token: Token) {
    this(token, "", None)
  }

  def this(token: Token, message: String) {
    this(token, message, None)
  }

  def this(token: Token, cause: Option[Exception]) {
    this(token, "", cause)
  }

}