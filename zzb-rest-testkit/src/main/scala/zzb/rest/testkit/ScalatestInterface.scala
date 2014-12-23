package zzb.rest.testkit

import org.scalatest.exceptions.TestFailedException
import org.scalatest.{ Suite, BeforeAndAfterAll }

trait ScalatestInterface extends TestFrameworkInterface with BeforeAndAfterAll {
  this: Suite ⇒

  def failTest(msg: String) = throw new TestFailedException(msg, 11)

  abstract override protected def afterAll(): Unit = {
    cleanUp()
    super.afterAll()
  }
}
