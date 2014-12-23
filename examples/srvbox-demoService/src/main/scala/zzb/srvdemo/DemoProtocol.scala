package zzb.srvdemo

import zzb.srvdemo.entites._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-21
 * Time: 上午10:58
 * Copyright baoxian.com 2012~2020
 */
object DemoProtocol {
  case class AddUser(user:User)
  case class UpdateUser(user:User)
  case class DelUser(id:Long)
  case class GetUser(id:Long)
  case class ListUser()
}
