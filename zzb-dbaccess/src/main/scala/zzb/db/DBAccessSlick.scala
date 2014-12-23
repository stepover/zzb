package zzb.db

import zzb.db.DBPools._
import scala.slick.jdbc.JdbcBackend
/**
 *
 * Created by blackangel on 2014/8/21
 */
trait DBAccessSlick {
  def transaction[A](name: String)(a: JdbcBackend#Session ⇒ A): Unit ={
    databaseSlick(name).withTransaction{
      implicit session=>
        a(session)
    }
  }

  def trans[A](name: String)(a: JdbcBackend#Session ⇒ A): A =
    databaseSlick(name).withTransaction{
      implicit session=>
        a(session)
    }

  def withSession[A](name: String)(a: JdbcBackend#Session ⇒ A): Unit ={
    databaseSlick(name).withSession{
      implicit session=>
        a(session)
    }
  }

  def withSessionR[A](name: String)(a: JdbcBackend#Session ⇒ A): A ={
    databaseSlick(name).withSession{
      implicit session=>
        a(session)
    }
  }
}
