ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.1"

lazy val root = (project in file("."))
  .settings(
    name := "code"
  )

libraryDependencies += "org.bitbucket.vahidi" % "jdd" % "108"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"