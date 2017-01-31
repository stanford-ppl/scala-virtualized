name := "virtualized"

organization := "org.virtualized"

version := "0.1"

scalaVersion := "2.11.2"

scalaSource in Compile <<= baseDirectory(_/ "src")
scalaSource in Test <<= baseDirectory(_/"test")

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.2" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test"

addCompilerPlugin("org.scalamacros" % "paradise_2.11.2" % "2.1.0")

