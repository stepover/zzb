package zzb.db.entites

import org.squeryl.KeyedEntity
import java.sql.Timestamp

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-20
 * Time: 下午6:18
 * Copyright baoxian.com 2012~2020
 */
//
//case class Company (var name: String, var address: String) extends KeyedEntity[Long]  {
//
//  val id:Long = 0
//  var lastModified = new Timestamp(System.currentTimeMillis)
//
//  def this() = this("", "")
//
//}

class Company(var name: String, var address: String) extends BaseEntity {

  def this() = this("", "")

}
