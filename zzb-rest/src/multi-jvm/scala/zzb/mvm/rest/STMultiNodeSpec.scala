package zzb.rest.mvm

/**
 * Created by Simon on 2014/5/26
 */
import org.scalatest.{ BeforeAndAfterAll, WordSpec }
import org.scalatest.MustMatchers
import akka.remote.testkit.MultiNodeSpecCallbacks
import org.scalatest.Tag
//#imports

//#trait
/**
 * Hooks up MultiNodeSpec with ScalaTest
 */
trait STMultiNodeSpec extends WordSpec
with MultiNodeSpecCallbacks with MustMatchers with BeforeAndAfterAll {

  override def beforeAll() = multiNodeSpecBeforeAll()

  override def afterAll() = multiNodeSpecAfterAll()
}



object TimingTest extends Tag("timing")
object LongRunningTest extends Tag("long-running")
