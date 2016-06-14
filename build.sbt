name := "scala-virtualized"

organization := "org.scala-lang.virtualized"

version := "1.0.0-macrovirt"

scalaVersion := "2.11.2"

val paradiseVersion = "2.0.1"

isSnapshot := true //allows to overwrites old local published version

crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.11.0", "2.11.1")

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.0" % "test"
)

libraryDependencies ++= (
  if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
  else Nil
)

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test"

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
