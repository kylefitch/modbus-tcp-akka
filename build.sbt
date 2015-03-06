name := "modbus-tcp-akka"

version := "0.1"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-unchecked", "-deprecation")

resolvers ++= Seq("Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= {
  val akkaVersion = "2.3.9"
  Seq(
    "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.1"
  )
}
    