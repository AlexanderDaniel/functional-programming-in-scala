name := "functional-programming-in-scala"

version := "1.0"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.7" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos", "-feature", "-language:postfixOps")
