package zzb.shell.remote

import java.io.OutputStream
import akka.actor.ActorRef
import zzb.shell.remote.ShellProtocol.Data

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-10-18
 * Time: 上午10:49
 * Copyright baoxian.com 2012~2020
 */
class ActorOutputStream(val actor:ActorRef) extends OutputStream{
  def write(b: Int) = {
    actor ! Data(b)
  }
}
