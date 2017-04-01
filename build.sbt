name := "virtualized"

organization := "org.virtualized"

//publish
//branch dependant
val suffix = ""
val versionNumber = "0.2"
version := "0.2" + suffix
isSnapshot := true

//dependencies versions
scalaVersion in ThisBuild := "2.12.1"
val paradiseVersion = "2.1.0"
val scalatestVersion = "3.0.1"

//dependencies
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test"

//paths
scalaSource in Compile := baseDirectory(_/ "src").value
scalaSource in Test := baseDirectory(_/"test").value

//paradise
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
