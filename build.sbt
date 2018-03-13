scalaVersion in ThisBuild := "2.12.1"

organization in ThisBuild := "edu.stanford.cs.dawn"

name := "virtualized"

//publish
val suffix = "-SNAPSHOT"
val versionNumber = "0.1"
version in ThisBuild := versionNumber + suffix
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
