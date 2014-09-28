name := "functional-programming-in-scala"

version := "1.0"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.4.1" % "test",
  "org.scalatest" %% "scalatest" % "2.1.5" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")
