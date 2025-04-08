import com.typesafe.tools.mima.core._

val awsRegionsVersion = "1.0.1"
val caseInsensitiveVersion = "1.5.0"
val catsEffectVersion = "3.6.1"
val catsParseVersion = "1.1.0"
val catsVersion = "2.13.0"
val circeVersion = "0.14.12"
val fs2Version = "3.12.0"
val http4sVersion = "0.23.30"
val munitCatsEffectVersion = "2.1.0"
val scala213Version = "2.13.16"
val scala3Version = "3.3.5"
val scalaCheckEffectMunitVersion = "2.0.0-M2"

inThisBuild(
  Seq(
    crossScalaVersions := Seq(scala213Version, scala3Version),
    developers := List(
      tlGitHubDev("janina9395", "Janina Komarova"),
      tlGitHubDev("jesperoman", "Jesper Öman"),
      tlGitHubDev("vlovgr", "Viktor Rudebeck")
    ),
    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17")),
    githubWorkflowTargetBranches := Seq("**"),
    licenses := Seq(License.Apache2),
    mimaBinaryIssueFilters ++= Seq(
      ProblemFilters.exclude[Problem]("com.magine.http4s.aws.internal.*"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
        "com.magine.http4s.aws.CredentialsProvider.securityTokenService"
      ),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
        "com.magine.http4s.aws.CredentialsProvider.securityTokenService"
      )
    ),
    organization := "com.magine",
    organizationName := "Magine Pro",
    scalaVersion := scala3Version,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    startYear := Some(2025),
    tlBaseVersion := "6.2",
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

lazy val core = crossProject(JVMPlatform)
  .in(file("modules/core"))
  .settings(name := "http4s-aws")
  .jvmSettings(
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % fs2Version,
      "com.magine" %% "aws-regions" % awsRegionsVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "org.http4s" %% "http4s-client" % http4sVersion,
      "org.http4s" %% "http4s-core" % http4sVersion,
      "org.typelevel" %% "case-insensitive" % caseInsensitiveVersion,
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-effect-kernel" % catsEffectVersion,
      "org.typelevel" %% "cats-effect-testkit" % catsEffectVersion % Test,
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "org.typelevel" %% "cats-parse" % catsParseVersion,
      "org.typelevel" %% "munit-cats-effect" % munitCatsEffectVersion % Test,
      "org.typelevel" %% "scalacheck-effect-munit" % scalaCheckEffectMunitVersion % Test
    )
  )
