package zzb

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-7-26
 * Time: 上午9:53
 * Copyright goodsl.org 2012~2020
 */
import org.scalatest.{ Suite, BeforeAndAfterAll }
import akka.testkit.TestKit

trait StopSystemAfterAll extends BeforeAndAfterAll {
  this: TestKit with Suite ⇒
  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }


}
