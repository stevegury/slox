ThisBuild / scalaVersion := "3.0.1"
ThisBuild / organization := "io.stevegury.slox"

lazy val hello = (project in file("."))
  .settings(
    name := "Slox",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test,
  )