name := "MyMacrosProject"

version := "1.0"

scalaVersion := "2.12.1"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
   "org.scalameta" %% "scalameta" % "1.8.0",
   "org.scalatest" %% "scalatest" % "3.0.1" % Test
)

addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M8" cross CrossVersion.full)
