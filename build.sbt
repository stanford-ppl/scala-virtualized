name := "virtualized"

organization := "org.virtualized"

scalaVersion in ThisBuild := "2.12.1"

//publish
val suffix = ""
val versionNumber = "0.2"
version := versionNumber + suffix
isSnapshot := true

//dependencies versions
val paradiseVersion = "2.1.0"
val scalatestVersion = "3.0.1"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test"

//paths
scalaSource in Compile := baseDirectory(_/ "src").value
scalaSource in Test := baseDirectory(_/"test").value

//paradise
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
