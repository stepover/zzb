package zzb.db

import org.squeryl.PrimitiveTypeMode
import zzb.db.DBPools._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-19
 * Time: 下午8:25
 * Copyright baoxian.com 2012~2020
 */
trait DBAccess extends PrimitiveTypeMode {

  def transaction[A](name: String)(a: ⇒ A): Unit =
    transaction(sessionFactory(name))(a)

  def inTransaction[A](name: String)(a: ⇒ A): Unit =
    inTransaction(sessionFactory(name))(a)

  def trans[A](name: String)(a: ⇒ A): A =
    transaction(sessionFactory(name))(a)

  def inTrans[A](name: String)(a: ⇒ A): A =
    inTransaction(sessionFactory(name))(a)
}
