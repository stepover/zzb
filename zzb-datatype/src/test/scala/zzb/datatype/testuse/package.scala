package zzb.datatype

/**
 * Created by Simon on 2014/7/2
 */
package object testuse {

  import UserInfo.Format
  val Users = TList[UserInfo.Pack]("users","用户列表")
  val UsersP = TPackList(UserInfo,"users","用户列表")


}
