val awsRegionsVersion = "1.0.1"
val caseInsensitiveVersion = "1.4.2"
val catsEffectVersion = "3.5.7"
val catsParseVersion = "1.1.0"
val catsVersion = "2.13.0"
val circeVersion = "0.14.10"
val fs2Version = "3.11.0"
val http4sVersion = "0.23.30"
val scala213Version = "2.13.16"
val scala3Version = "3.3.5"

inThisBuild(
  Seq(
    crossScalaVersions := Seq(scala213Version, scala3Version),
    developers := List(
      tlGitHubDev("janina9395", "Janina Komarova"),
      tlGitHubDev("vlovgr", "Viktor Rudebeck"),
    ),
    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17")),
    licenses := Seq(License.Apache2),
    organization := "com.magine",
    organizationName := "Magine Pro",
    scalaVersion := scala3Version,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    startYear := Some(2025),
    tlBaseVersion := "6.0",
    tlCiHeaderCheck := true,
    tlCiScalafixCheck := true,
    tlCiScalafmtCheck := true,
    tlFatalWarnings := true,
    tlJdkRelease := Some(8),
    tlUntaggedAreSnapshots := false,
    versionScheme := Some("early-semver")
  )
)

lazy val root = tlCrossRootProject
  .aggregate(core)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("modules/core"))
  .settings(
    name := "http4s-aws",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % fs2Version,
      "com.magine" %% "aws-regions" % awsRegionsVersion,
      "io.circe" %% "circe-core" % circeVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "org.http4s" %% "http4s-client" % http4sVersion,
      "org.http4s" %% "http4s-core" % http4sVersion,
      "org.typelevel" %% "case-insensitive" % caseInsensitiveVersion,
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-effect-kernel" % catsEffectVersion,
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "org.typelevel" %% "cats-parse" % catsParseVersion
    )
  )
