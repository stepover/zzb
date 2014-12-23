package zzb.datatype

/**
 * Created by Simon on 2014/7/2
 */
package object testuse {

  import BasicFormats._
  val Phones = TList[Int]("phones","电话号码")

  import UserInfo.Format
  val Users = TList[UserInfo.Pack]("users","用户列表")
  val UsersP = TPackList(UserInfo,"users","用户列表")


}
