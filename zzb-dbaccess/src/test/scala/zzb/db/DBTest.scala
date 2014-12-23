package zzb.db

import org.scalatest.{ BeforeAndAfterEach, WordSpec }
import org.scalatest.MustMatchers
import com.typesafe.config.ConfigFactory
import zzb.db.schema._
import zzb.db.entites.{ Company, User }

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-20
 * Time: 上午11:30
 * Copyright baoxian.com 2012~2020
 */
class DBTest extends WordSpec with MustMatchers
    with BeforeAndAfterEach with DBAccess {

  val config = ConfigFactory.load("DBSupport")
  "db config file " must {

    "contans db block" in {

      assert(config.hasPath("db"))

      assert(config.hasPath("db.firstdb"))
      assert(config.hasPath("db.seconddb"))

    }

    "init db " must {
      "create connection pool" in {
        assert(DBPools.hasDB("firstdb"))
        assert(DBPools.hasDB("seconddb"))
      }
    }
  }

  "data operator" must {

    "can create tables by shcema,insert data and query out" in {
      transaction("firstdb") {
        FirstSchema.drop //先清掉所有表
        FirstSchema.create //创建所有表

      }

      transaction("firstdb") {
        val user1: User = new User("user1@domain.com", "oldPassword")
        FirstSchema.users.insert(user1)

        user1.password = "newPassword"
        FirstSchema.users.update(user1)

        val user2: User = new User("user2@domain.com", "password")
        FirstSchema.users.insert(user2)

      }
      transaction("firstdb") {
        val queriedUser: User = FirstSchema.users.where(user ⇒ user.id === 2L).single
        queriedUser.email must equal("user2@domain.com")
      }

      transaction("seconddb") {
        SecondSchema.drop //先清掉所有表
        SecondSchema.create //创建所有表

      }
      transaction("seconddb") {
        val company = new Company("保网", "水荫路")
        SecondSchema.company.insert(company)
      }
      transaction("seconddb") {
        val company: Company = SecondSchema.company.where(com ⇒ com.id === 1L).single
        company.name must equal("保网")
      }
    }

    "can access different database" in {
      transaction("firstdb") {
        FirstSchema.drop //先清掉所有表
        FirstSchema.create //创建所有表
      }
      transaction("firstdb") {
        val user1: User = new User("user1@domain.com", "oldPassword")
        FirstSchema.users.insert(user1)
      }

      val thrown = intercept[RuntimeException] {
        transaction("seconddb") {
          val queriedUser: User = FirstSchema.users.where(user ⇒ user.id === 1L).single
          queriedUser.email must equal("user1@domain.com")
        }
      }

    }

  }

  override def beforeEach() = {
    DBPools.openDB("firstdb", config.getConfig("db.firstdb"))
    DBPools.openDB("seconddb", config.getConfig("db.seconddb"))
  }

  override def afterEach() = {
    DBPools.closeAllDB()
  }

  //DBPools.closeAllDB()

}
