package zzb.shell.task

import zzb.shell.{ TaskInfo, Task }

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-17
 * Time: 上午11:26
 * Copyright baoxian.com 2012~2020
 */
class WildCard() extends Task {

  val wide = args.head
  var count = 0
  if (wide.length > 0) {
    for (shortName ← Task.shortNameIndex.keySet if shortName.startsWith(wide)) {
      print(f"$shortName%-15s")
      count += 1
      if (count == 6) { count = 0; println() }
    }
  }
  println()

}
