package zzb.srvdemo.schema


import org.squeryl._
import zzb.srvdemo.entites.{Company, User}

object Util extends PrimitiveTypeMode
import Util._
/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-20
 * Time: 下午5:00
 * Copyright baoxian.com 2012~2020
 */
object
UserDBSchema extends Schema {

  val users = table[User]

  on(users)(user => declare(
    user.id is autoIncremented //,
//    user.email is (unique)
  ))

}

object
InfoDBSchema extends Schema {

  val company = table[Company]

  on(company)(company => declare(
    company.id is autoIncremented
  ))

}