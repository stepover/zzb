package zzb.db.entites

import org.squeryl.KeyedEntity
import java.sql.Timestamp

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-20
 * Time: 下午4:58
 * Copyright baoxian.com 2012~2020
 */
class BaseEntity extends KeyedEntity[Long] {

  val id: Long = 0
  var lastModified = new Timestamp(System.currentTimeMillis)

}
