package zzb.db.schema

import zzb.db.entites.Entites._

/**
 *
 * Created by blackangel on 2014/8/21
 */
object SchemasSlick {
  import scala.slick.driver.H2Driver.simple._
  class Companys(tag: Tag) extends Table[Company](tag,"Company"){

    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def address = column[String]("address")
    def * = (id.?,name,address) <> (Company.tupled,Company.unapply)
  }

  class Users(tag: Tag) extends Table[User](tag,"User"){

    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def email = column[String]("email")
    def password = column[String]("password")
    def * = (id.?,email,password) <> (User.tupled,User.unapply)
  }

  val companys =TableQuery[Companys]
  val users =TableQuery[Users]
}
