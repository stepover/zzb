package zzb.rest.testkit

trait TestFrameworkInterface {

  def cleanUp()

  def failTest(msg: String): Nothing
}
