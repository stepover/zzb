package zzb.srvdemo.entites

import org.squeryl.KeyedEntity
import java.sql.Timestamp
import spray.json._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-20
 * Time: 下午4:58
 * Copyright baoxian.com 2012~2020
 */

class BaseEntity extends KeyedEntity[Long] {

  val id:Long = 0
  var lastModified = new Timestamp(System.currentTimeMillis)

}

case class User(var email: String, var password: String) extends  BaseEntity {
  def this() = this("", "")
}

case class ChangeCount(count:Int)

class Company (var name: String, var address: String) extends BaseEntity  {
  def this() = this("", "")
}

object ChangeCount extends DefaultJsonProtocol {
  implicit val format = jsonFormat1(ChangeCount.apply)
}

/*{"email":"user1@demo.com","password":"p1","id":1,"lastModified":"2013-09-17 17:45:41.644"}*/
object User extends DefaultJsonProtocol {
  implicit val userJsonFormat =new RootJsonFormat[User] {
    def write(c: User) =
      JsObject(Map("email"->JsString(c.email),"password"->JsString(c.password), "id"->JsNumber(c.id), "lastModified"->JsString(c.lastModified.toString)))

    def read(value: JsValue) = value match {
      case JsObject(fields) =>
        new User(
          fields("email") match {
            case JsString(email)=>email
            case _ => deserializationError("String expected")
          },
          fields("password") match {
            case JsString(password)=>password
            case _ => deserializationError("String expected")
          }
        )
      case _ => deserializationError("User expected")
    }
  }
}



