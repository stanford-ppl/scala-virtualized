name := "virtualized"

scalaVersion := "2.12.1"

organization := "org.virtualized"

version := "0.6"

val scalatestVersion = "3.0.1"

val paradiseVersion = "3.0.0-M7"  // check here: https://github.com/scalamacros/paradise/releases

val metaVersion = "1.6.0"

scalaSource in Compile := baseDirectory(_/ "src").value

scalaSource in Test := baseDirectory(_/"test").value

// New-style macro annotations are under active development.  As a result, in
// this build we'll be referring to snapshot versions of both scala.meta and
// macro paradise.
resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.bintrayIvyRepo("scalameta", "maven")

libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test"

// A dependency on scala.meta is required to write new-style macros, but not
// to expand such macros.  This is similar to how it works for old-style
// macros and a dependency on scala.reflect.
libraryDependencies += "org.scalameta" %% "scalameta" % metaVersion

libraryDependencies += "org.scalameta" %% "contrib" % metaVersion

addCompilerPlugin("org.scalameta" % "paradise" % paradiseVersion cross CrossVersion.full)

scalacOptions += "-Xplugin-require:macroparadise"
// temporary workaround for https://github.com/scalameta/paradise/issues/10
scalacOptions in (Compile, console) := Seq() // macroparadise plugin doesn't work in repl yet.
// temporary workaround for https://github.com/scalameta/paradise/issues/55
sources in (Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.
