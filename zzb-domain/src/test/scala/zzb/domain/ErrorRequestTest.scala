package zzb.domain

import spray.http.StatusCodes._

/**
 * Created by Simon on 2014/7/17
 */
class ErrorRequestTest extends PlaneHttpTestBase {

  "访问不存在的文档会报 404 " in {

    //管理员请求创建一个新的Alter会话
    manager(Post(s"/api/planes/nothis/alter")) ~> check {
      status mustBe NotFound
    }

    manager(Get(s"/api/planes/nothis/latest")) ~> check {
      val msg = body.asString
      status mustBe NotFound
    }

    manager(Post(s"/api/planes/nothis/alter")) ~> check {
      status mustBe NotFound
    }
  }

}
