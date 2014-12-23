package zzb.srvdemo.test

import org.scalatest.{BeforeAndAfterAll, BeforeAndAfter, BeforeAndAfterEach, WordSpec}
import org.scalatest.MustMatchers
import com.typesafe.config.ConfigFactory

import zzb.srvdemo.schema._
import zzb.srvdemo.entites._
import zzb.db.{DBPools, DBAccess}
import zzb.srvdemo.DBOperate
import spray.json._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-21
 * Time: 下午1:48
 * Copyright baoxian.com 2012~2020
 */
class DBOperatorTest extends WordSpec with MustMatchers
with BeforeAndAfterAll with DBAccess with DefaultJsonProtocol{

  val config = ConfigFactory.load("demo")

  "db config file " must {
    "has db config block" in {
      config.hasPath("db") must equal(true)
      config.hasPath("db.userdb") must equal(true)
      config.hasPath("db.infodb") must equal(true)
    }

  }

  "db operate" must {
    var firstid = 0L
    var user:User=null
    "can add user" in {
      DBOperate.addUser(new User("user1@demo.com", "p1"))
      val users = DBOperate.listUsers()
      users.head.email must equal("user1@demo.com")
      firstid = users.head.id
      user= users.head
    }

    "can json user" in {
      import User._
      val json =user.toJson
      json.asInstanceOf[JsObject].getFields("email").head must equal(JsString("user1@demo.com"))
      val user_ = json.convertTo[User]
      user_.email must equal("user1@demo.com")
    }
    "can del user" in {
      DBOperate.delUser(firstid)
      val users = DBOperate.listUsers()
      users must equal(Nil)
    }
    "can add company" in {
      DBOperate.addCompany(new Company("保网", "水荫路"))
      val comps = DBOperate.listCompany()
      comps.head.name must equal("保网")
      firstid = comps.head.id
    }
    "can del company" in {
      DBOperate.delCompany(firstid)
      val comps = DBOperate.listCompany()
      comps must equal(Nil)

    }
  }

  override def beforeAll = {

    DBPools.openDB("userdb", config.getConfig("db.userdb"))
    DBPools.openDB("infodb", config.getConfig("db.infodb"))
    DBOperate.reCreateDb()
  }

  override def afterAll() = {
    DBPools.closeAllDB()
  }
}
