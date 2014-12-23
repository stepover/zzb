package zzb.domain

import org.scalatest.{MustMatchers, WordSpec}
import zzb.datatype._
import zzb.domain.directive.AuthorizedOperator
import zzb.domain.plane.Plane

/**
 * Created by Simon on 2014/7/16
 */
class ActionExtractorTest extends WordSpec with MustMatchers {

  "Action " must {

    val manager = AuthorizedOperator("admin",isManager = true)

    val user = AuthorizedOperator("jack",isManager = false)

    val params = Map("width" -> "100","height" -> "300")

    "正常解析出各项元素" in {
      val a = Action("fly",manager,params,Some(TString("hello")))
      val res = a match {
        case Action(name,opt,p,Some(TString(h))) =>
          name + "-" + opt.id + "-" + p("width") + "-" + h
        case _ => "no match"
      }
      res mustBe "fly-admin-100-hello"
    }

    "匹配指定名称,大小写无关未实现" in {

      val a = Action("fly",manager,params,Some(TString("hello")))
      val res0 = a match {
        case Action("fly" ,opt,p,Some(TString(h))) =>
          "fly" + "-" + opt.id + "-" + p("width") + "-" + h
        case _ => "no match"
      }
      res0 mustBe "fly-admin-100-hello"
    }
    "ddd" in {
      val plane = Plane(Plane.id := "1000",Plane.owner := "simon")
      val a = Action("fly",manager,params,Some(plane))

      val res0 = a match {
        case Action(name,opt,p,Some(TString(h))) =>
          name + "-" + opt.id + "-" + p("width") + "-" + h
        case _ => "no match"
      }
      res0 mustBe "no match"

      val res1 = a match {
        case Action(name,opt,p,Some(Plane(h))) =>
          name + "-" + opt.id + "-" + p("width") + "-" + h.id
        case _ => "no match"
      }
      res1 mustBe "fly-admin-100-1000"
    }
  }

}
