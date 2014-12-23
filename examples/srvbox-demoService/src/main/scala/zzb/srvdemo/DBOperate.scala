package zzb.srvdemo

import zzb.db.DBAccess
import zzb.srvdemo.schema._
import zzb.srvdemo.entites._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-21
 * Time: 上午11:00
 * Copyright baoxian.com 2012~2020
 */
object DBOperate extends DBAccess {

  //这个只是让Demo服务每次运行都是从头开始，实际服务不要干这个事情哦。
  //实际生产服务不要做 schema create 的操作，表结构手动创建好。
  def reCreateDb() = {
    transaction("userdb") {
      UserDBSchema.drop //先清掉所有表
      UserDBSchema.create //创建所有表
    }
    transaction("infodb") {
      InfoDBSchema.drop //先清掉所有表
      InfoDBSchema.create //创建所有表
    }
  }

  def addUser(user: User) = {
    transaction("userdb") {
      UserDBSchema.users.insert(user)
    }
    user
  }

  def updateUser(user: User) = {
    transaction("userdb") {
      UserDBSchema.users.update(user)
    }
    user
  }

  def getUser(id: Long) = {
    var foundUser: Option[User] = None
    transaction("userdb") {
      foundUser = UserDBSchema.users.lookup(id)
    }
    foundUser
  }

  def listUsers(): List[User] = {

    var users: List[User] = Nil

    transaction("userdb") {
      users = from(UserDBSchema.users)(s => where(s.id gt 0) select (s)).toList
    }
    users
  }

  def delUser(id: Long) = {
    var deletedCount = 0
    transaction("userdb") {
      deletedCount = UserDBSchema.users.deleteWhere(user => user.id === id)
    }
    deletedCount
  }

  def addCompany(comp: Company) = {
    transaction("infodb") {
      InfoDBSchema.company.insert(comp)
    }
  }

  def listCompany() = {
    var comps: List[Company] = Nil
    transaction("infodb") {
      comps = from(InfoDBSchema.company)(s => where(s.id gt 0) select (s)).toList
    }
    comps
  }

  def delCompany(id: Long) = {
    transaction("infodb") {
      InfoDBSchema.company.deleteWhere(comp => comp.id === id)
    }
  }

}
