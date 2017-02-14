scalaVersion := "2.12.1"

val scalatestVersion = "3.0.1"

val paradiseVersion = "2.1.0"  // check here: https://github.com/scalamacros/paradise/releases



name := "virtualized"

organization := "org.virtualized"

version := "0.1"

scalaSource in Compile <<= baseDirectory(_/ "src")

scalaSource in Test <<= baseDirectory(_/"test")

libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test"

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

