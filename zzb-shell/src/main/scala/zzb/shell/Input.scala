package zzb.shell

import java.io.InputStream

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-13
 * Time: 下午4:00
 * Copyright baoxian.com 2012~2020
 */
trait Input {

  def readCommand(path: List[String]): String

  def readClearCommand : String

  def inStream : InputStream
}
