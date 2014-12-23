package zzb.shell.task

import zzb.shell.Task

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-12-10
 * Time: 上午9:00
 * Copyright baoxian.com 2012~2020
 */
class Verbose extends Task {

  if(args.length == 0)
    shell.verbose_ = !shell.verbose_
  else args(0).toUpperCase match{
    case "ON" => shell.verbose_ = true
    case "OFF" => shell.verbose_ = false
  }

  println("verbose " + (if(shell.verbose_) "ON" else "OFF"))

}
