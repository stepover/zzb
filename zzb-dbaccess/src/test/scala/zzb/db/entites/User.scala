package zzb.db.entites

import java.sql.Timestamp
import org.squeryl.KeyedEntity

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-20
 * Time: 下午4:58
 * Copyright baoxian.com 2012~2020
 */

////case class 版本
//case class User(var email: String, var password: String) extends  KeyedEntity[Long] {
//
//  val id:Long = 0
//  var lastModified = new Timestamp(System.currentTimeMillis)
//  // Zero argument constructor required
//  // Squeryl Roadmap says 0.9.5 will not need them :-)
//  def this() = this("", "")
//
//}

// 非 case class 版本

class User(var email: String, var password: String) extends BaseEntity {

  // Zero argument constructor required
  // Squeryl Roadmap says 0.9.5 will not need them :-)
  def this() = this("", "")

}

