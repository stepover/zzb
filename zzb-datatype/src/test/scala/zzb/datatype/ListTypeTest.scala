package zzb.datatype

import java.io._

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import spray.json.JsonParser
import testuse._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-19
 * Time: 下午1:48
 * Copyright baoxian.com 2012~2020
 */
class ListTypeTest extends WordSpec with MustMatchers {

  def serializeTest(o: Serializable) = {
    val bs = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bs)
    out.writeObject(o)
    out.close()

    val oin = new ObjectInputStream(new ByteArrayInputStream(bs.toByteArray))
    val t = oin.readObject()
    oin.close()

    println("序列化前的对象----" + o)
    println("反序列化的对象----" + t)

    t mustBe o
  }

  "ListType " must {

    "construct and equals " in {
      val users = Users(List(UserInfo(UserName("Simon")), UserInfo(UserName("Jack"))))
     // val usersP = UsersP(List(UserInfo(UserName("Simon")), UserInfo(UserName("Jack"))))
     // val home = HomeInfo(UserInfo(UserName("Simon")))

//      users.length must equal(2)
//
//      UserName("Simon") must equal(UserName("Simon"))
//
//      UserName("Simon") must equal(UserName("Simon").value)
//
//      UserName("Simon") must equal(home(HomeInfo.userInfo().userName()))
//
//      UserName("Simon") must not be CarLicense("Simon")
//
//      val json = users.json
//
//      val nu = Users.fromJsValue(JsonParser(json))
//
//      nu mustBe users

      println(users.json)
      serializeTest(users)
     // serializeTest(usersP)

    }

    " List :: , ::: Operate " in {
      val users1 = Users(List(UserInfo(UserName("Simon")), UserInfo(UserName("Jack"))))

      val users2 = UserInfo(UserName("mike")) :: users1

      users2.length must equal(3)

      val users3 = List(UserInfo(UserName("vivian")), UserInfo(UserName("wolfgang"))) ::: users2
      users3.length must equal(5)
    }
    " List ++ , ::: Operate " in {
      val users1 = Users(List(UserInfo(UserName("Simon")), UserInfo(UserName("Jack"))))
      val users2 = Users(List(UserInfo(UserName("vivian")), UserInfo(UserName("wolfgang"))))

      val users3 = users1 ++ users2

      users3.length must equal(4)

      val users4 = users3 ++ List(UserInfo(UserName("tom")), UserInfo(UserName("mike")))

      users4.length must equal(6)

      val dd = users4.map(u => u(UserName).getOrElse(""))

      dd.length must equal(6)
    }
    " List map forall flatMap filter " in {
      val users1 = Users(List(UserInfo(UserName("Simon")), UserInfo(UserName("Jack"))))
      val users2 = Users(List(UserInfo(UserName("vivian")), UserInfo(UserName("wolfgang"))))

      val users3 = users1 ++ users2

      val names = for (u <- users3 if u(UserName).get.value.toString.length > 4)
      yield u(UserName).get.value

      names.length must equal(3)
    }

    "TStruct 中的列表字段的赋值" in {
      import UserInfo._
      val u0 = UserInfo(userName := "Simon", phones := List(123, 345))

      val thePhones = u0(phones).get.value
      thePhones.size mustBe 2
      thePhones(0) mustBe 123

      intercept[IllegalArgumentException] {
        UserInfo(userName := "Simon", phones := List("123", "345"))
      }
    }
  }

}
