import xerial.sbt.Pack._

scalaVersion := "2.11.5"

packAutoSettings

libraryDependencies ++= Seq(
  "org.scalatra.scalate" % "scalate-core_2.11" % "1.7.0",
  "org.rogach" %% "scallop" % "0.9.5"
)

name := "difr"

version := "0.1-SNAPSHOT"
