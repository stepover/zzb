package zzb.shell.remote

import zzb.shell.ActionStat
import akka.actor.ActorRef

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-10-16
 * Time: 下午2:07
 * Copyright baoxian.com 2012~2020
 */
object ShellProtocol {

  case class Login(userName: String, passWord: String,peer:ActorRef)

  case class Logout(sid:String)

  case class LoginSuccess(sid: String,peer:ActorRef)

  case object LoginFailed

  case class SessionLost(sid: String,reason:String)

  case class Data(d:Int)

  case class Ping(sid:String)

  case object Pong

  case object KickAll
}