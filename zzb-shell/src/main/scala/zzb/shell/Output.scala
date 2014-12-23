package zzb.shell

import java.io.PrintStream

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-13
 * Time: 下午4:01
 * Copyright baoxian.com 2012~2020
 */
trait Output {

  def output(obj: AnyRef, oce: OutputConversionEngine)

  def outputException(input: String, error: TokenException)

  def outputException(e: Throwable)

  def outputException(input: String, e: Throwable)

  def outputHeader(text: String)

  def out: PrintStream
  def err: PrintStream

}
