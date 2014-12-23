package zzb.shell.task

import zzb.shell.Task

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-17
 * Time: 上午9:32
 * Copyright baoxian.com 2012~2020
 */
class Tasks extends Task {

  val sortedSpaces = collection.SortedSet(Task.spacesIndex.keySet.toList: _*)

  sortedSpaces.foreach { space ⇒

    println(s"\t$space:")

    Task.spacesIndex(space).foreach { info ⇒
      println(f"\t  ${info.shortName}%-25s   -----    ${info.desc}%-35s")
    }
  }

}
