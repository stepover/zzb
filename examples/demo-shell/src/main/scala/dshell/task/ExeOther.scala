package dshell.task

import zzb.shell.Task

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-10-28
 * Time: 下午4:02
 * Copyright baoxian.com 2012~2020
 */
class ExeOther extends Task {

  override def usage = "exeother 'other-command-line'"
  override def checkArgs = args != Nil

  execOther(args.head)

}
