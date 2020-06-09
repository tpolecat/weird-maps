
// Global Settings
lazy val commonSettings = Seq(

  // Publishing
  organization := "org.tpolecat",
  licenses    ++= Seq(("MIT", url("http://opensource.org/licenses/MIT"))),
  homepage     := Some(url("https://github.com/tpolecat/maps")),
  developers   := List(
    Developer("tpolecat", "Rob Norris", "rob_norris@mac.com", url("http://www.tpolecat.org"))
  ),

  // Headers
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2020 by Rob Norris
       |This software is licensed under the MIT License (MIT).
       |For more information see LICENSE or https://opensource.org/licenses/MIT
       |""".stripMargin
    )
  ),

  // Compilation
  scalaVersion := "2.13.1",
  Compile / doc / scalacOptions --= Seq("-Xfatal-warnings"),
  Compile / doc / scalacOptions ++= Seq(
    "-groups",
    "-implicits",
    "-sourcepath", (baseDirectory in LocalRootProject).value.getAbsolutePath,
    "-doc-source-url", "https://github.com/tpolecat/maps/blob/v" + version.value + "€{FILE_PATH}.scala",
  ),

  // Let's use MUnit for all tests, and allow Cats in test as well.
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit"            % "0.7.6" % Test,
    "org.scalameta" %% "munit-scalacheck" % "0.7.6" % Test,
  ),
  testFrameworks += new TestFramework("munit.Framework"),

  // don't publish by default
  publish / skip := true,

)

lazy val maps = project
  .in(file("."))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .dependsOn(core)
  .aggregate(core)

lazy val core = project
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(
    name := "maps-core",
    description := "Weird maps.",
    publish / skip := false,
  )

