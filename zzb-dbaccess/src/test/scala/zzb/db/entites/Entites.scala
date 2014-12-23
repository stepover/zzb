package zzb.db.entites

import scala.slick.direct._
import scala.slick.direct.AnnotationMapper._
/**
 *
 * Created by blackangel on 2014/8/21
 */
object Entites {
  case class Company(id:Option[Int],var name: String,var address: String)

  case class User(id:Option[Int],var email: String, var password: String)
}
