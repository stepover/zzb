resolvers += Classpaths.typesafeResolver

resolvers += "sbt-idea" at "http://mpeltonen.github.com/maven/"

resolvers += "BaoXian Repository" at "http://maven.baoxan.org:8083/nexus/content/groups/public/"

resolvers += "Typesafe Repository" at "http://repo.akka.io/releases/"

resolvers += "Spray Repository" at "http://repo.spray.io/"

libraryDependencies ++= Seq(
  "com.decodified" % "scala-ssh" % "0.6.2",
  "com.jcraft" % "jzlib" % "1.1.1",
  "org.kamranzafar" % "jtar" % "2.2",
  "org.slf4j" % "slf4j-nop" % "1.7.5"
  //"org.fusesource.scalate" % "scalate-core_2.9" % "1.6.1"
)


addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.0.1")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.0")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.5.1")

//addSbtPlugin("com.mojolly.scalate" % "xsbt-scalate-generator" % "0.4.2")

addSbtPlugin("com.baoxian.sbt" %% "sbt-box" % "0.1-RC4")