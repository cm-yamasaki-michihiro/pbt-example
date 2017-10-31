name := "pbt-example"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
  "org.scalatest" % "scalatest_2.12" % "3.0.4" % Test,
  "org.mockito" % "mockito-core" % "2.7.0" % Test
)

testOptions in Test += Tests.Argument("-oS")
