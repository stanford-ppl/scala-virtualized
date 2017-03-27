import sbt.Keys.resolvers
import sbt.{Compile, _}

object VirtualizedBuild extends Build {

  val compilerVersion = "2.12.1"
  val scalatestVersion = "3.0.1"
  val paradiseVersion = "3.0.0-M7"  // check here: https://github.com/scalamacros/paradise/releases
  val metaVersion = "1.6.0"

  lazy val virtBuildSettings = Defaults.coreDefaultSettings ++ Seq(
    organization := "org.virtualized",
    scalaVersion := compilerVersion,

    resolvers += Resolver.sonatypeRepo("releases"),
    resolvers += Resolver.bintrayIvyRepo("scalameta", "maven"),

    publishArtifact in (Compile, packageDoc) := false,
    libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile",
    libraryDependencies += "org.scalameta" %% "scalameta" % metaVersion,
    libraryDependencies += "org.scalameta" %% "contrib" % metaVersion,

    retrieveManaged := true,
    scalacOptions += "-Yno-generic-signatures",

    excludeFilter in unmanagedSources := "*template-level*" || "*app-level*" || "*resources*",

    // More strict error/warning checking
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings"),
    // It would be very annoying to have to import these everywhere in this project
    scalacOptions ++= Seq("-language:higherKinds", "-language:implicitConversions"),

    scalacOptions in (Compile, doc) ++= Seq(
      "-doc-root-content",
      baseDirectory.value+"/root-doc.txt",
      "-diagrams",
      "-diagrams-debug",
      //"-diagrams-dot-timeout", "20", "-diagrams-debug",
      "-doc-title", name.value
    ),

    addCompilerPlugin("org.scalameta" % "paradise" % paradiseVersion cross CrossVersion.full),

    scalacOptions += "-Xplugin-require:macroparadise",
    // temporary workaround for https://github.com/scalameta/paradise/issues/10
    scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
    // temporary workaround for https://github.com/scalameta/paradise/issues/55
    sources in (Compile, doc) := Nil, // macroparadise doesn't work with scaladoc yet.

    scalaSource in Compile := baseDirectory(_/ "src").value,
    scalaSource in Test := baseDirectory(_/"test").value,

    parallelExecution in Test := false,
    concurrentRestrictions in Global += Tags.limitAll(1) // we need tests to run in isolation across all projects
  )
  lazy val virtualizedSettings = virtBuildSettings ++ Seq(
    name := "virtualized",
    version := "0.2",
    isSnapshot := true 
  )
}
