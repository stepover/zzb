package zzb.db

import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterEach, MustMatchers, WordSpec}
import zzb.db.entites.Entites._
import zzb.db.schema.SchemasSlick._

import scala.slick.driver.H2Driver.simple._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-20
 * Time: 上午11:30
 * Copyright baoxian.com 2012~2020
 */
class DBSlickTest extends WordSpec with MustMatchers with BeforeAndAfterEach with DBAccessSlick  {
  val config = ConfigFactory.load("DBSupport")
  //DBPools.openDB("firstdb", config.getConfig("db.firstdb"))


  "db config file " must {

    "contans db block" in {

      assert(config.hasPath("db"))

      assert(config.hasPath("db.firstdb"))
    }

    "init db " must {
      "create connection pool" in {
        assert(DBPools.hasDB("firstdb"))
      }
    }
  }

  "data operator" must {
    "work" in {
      transaction("firstdb") { implicit session =>
        val user: User = new User(None, "user1@domain.com", "oldPassword")
        users += user
        val result = users.filter(_.password === "oldPassword").map(_.email).list
        assert(result.size == 1)
        assert(result(0) == "user1@domain.com")
      }
    }
  }

  override def beforeEach() = {
    DBPools.openDB("firstdb", config.getConfig("db.firstdb"))
    DBPools.openDB("seconddb", config.getConfig("db.seconddb"))
    transaction("firstdb"){implicit session =>
      (users.ddl ++ companys.ddl).drop
      (users.ddl ++ companys.ddl).create
    }
  }

  override def afterEach() = {
    DBPools.closeAllDB()
  }

}