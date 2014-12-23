package dshell.task

import zzb.shell.Task

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-25
 * Time: 下午2:05
 * Copyright baoxian.com 2012~2020
 */
class Hello extends Task{

  val title = if(args.size>0) args.head else ""
  println("hello " + title)

}
class ThinkHello extends Task{

  val title = if(args.size>0) args.head else ""
  println("let me think a while")
  Thread.sleep(5000)
  println("ok,hello " + title+ ", after think 5 seconds")

}
