import sbt._

object Dependencies {

  val resolutionRepos = Seq(
    "BaoXian Repository" at "http://maven.baoxan.org:8083/nexus/content/groups/public/",
    "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
    "Spray Repository" at "http://repo.spray.io/",
    "Spray Nightlies" at "http://nightlies.spray.io/"
  )

  val akkaVersion = "2.2.0"
  val sprayVersion = "1.2-20130727"

  def compile(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile")
  def provided(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "provided")
  def test(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "test")
  def runtime(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "runtime")
  def container(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "container")

  val scalaloggingSlf4j = "com.typesafe" %% "scalalogging-slf4j" % "1.0.1"
  val scalatest = "org.scalatest" %% "scalatest" % "1.9.1"
  val logback = "ch.qos.logback" % "logback-classic" % "1.0.10"
  val specs2 = "org.specs2" %% "specs2" % "1.14"
  val srvBox = "com.baoxian.zzb" % "srv-box" % "0.1-SNAPSHOT"


}
