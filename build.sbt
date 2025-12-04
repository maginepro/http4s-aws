val awsRegionsVersion = "1.0.1"
val caseInsensitiveVersion = "1.5.0"
val catsEffectVersion = "3.6.3"
val catsParseVersion = "1.1.0"
val catsVersion = "2.13.0"
val circeVersion = "0.14.15"
val fs2DataVersion = "1.12.0"
val fs2Version = "3.12.2"
val http4sVersion = "0.23.33"
val literallyVersion = "1.2.0"
val munitCatsEffectVersion = "2.1.0"
val scala213Version = "2.13.18"
val scala3Version = "3.3.7"
val scalaCheckEffectMunitVersion = "2.1.0-RC1"
val scodecVersion = "1.2.4"
val slf4jVersion = "2.0.17"
val testcontainersVersion = "2.0.2"
val vaultVersion = "3.6.0"

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
    mimaBinaryIssueFilters ++= {
      import com.typesafe.tools.mima.core._
      Seq(
        // format: off
        ProblemFilters.exclude[Problem]("com.magine.http4s.aws.internal.*"),
        /* TODO: Remove below filters for 7.0 release. */
        ProblemFilters.exclude[DirectMissingMethodProblem]("com.magine.http4s.aws.CredentialsProvider.securityTokenService"),
        ProblemFilters.exclude[IncompatibleMethTypeProblem]("com.magine.http4s.aws.CredentialsProvider.securityTokenService"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("com.magine.http4s.aws.AwsPresigning.presignRequest"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("com.magine.http4s.aws.AwsPresigning.apply"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("com.magine.http4s.aws.AwsSigning.signRequest"),
        ProblemFilters.exclude[IncompatibleMethTypeProblem]("com.magine.http4s.aws.CredentialsProvider.credentialsFile"),
        ProblemFilters.exclude[IncompatibleMethTypeProblem]("com.magine.http4s.aws.TokenCodeProvider.default")
        // format: on
      )
    },
    organization := "com.magine",
    organizationName := "Magine Pro",
    scalaVersion := scala3Version,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    startYear := Some(2025),
    tlBaseVersion := "6.3",
    tlCiHeaderCheck := true,
    tlCiScalafixCheck := true,
    tlCiScalafmtCheck := true,
    tlFatalWarnings := true,
    tlJdkRelease := Some(8),
    versionScheme := Some("early-semver")
  )
)

lazy val root = tlCrossRootProject
  .aggregate(core, s3)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/core"))
  .settings(
    name := "http4s-aws",
    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-core" % fs2Version,
      "co.fs2" %%% "fs2-io" % fs2Version,
      "com.magine" %%% "aws-regions" % awsRegionsVersion,
      "io.circe" %%% "circe-core" % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion,
      "org.http4s" %%% "http4s-circe" % http4sVersion,
      "org.http4s" %%% "http4s-client" % http4sVersion,
      "org.http4s" %%% "http4s-core" % http4sVersion,
      "org.scodec" %%% "scodec-bits" % scodecVersion,
      "org.typelevel" %%% "case-insensitive" % caseInsensitiveVersion,
      "org.typelevel" %%% "cats-core" % catsVersion,
      "org.typelevel" %%% "cats-effect-kernel" % catsEffectVersion,
      "org.typelevel" %%% "cats-effect-std" % catsEffectVersion,
      "org.typelevel" %%% "cats-effect-testkit" % catsEffectVersion % Test,
      "org.typelevel" %%% "cats-effect" % catsEffectVersion,
      "org.typelevel" %%% "cats-kernel" % catsVersion,
      "org.typelevel" %%% "cats-parse" % catsParseVersion,
      "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectVersion % Test,
      "org.typelevel" %%% "scalacheck-effect-munit" % scalaCheckEffectMunitVersion % Test,
      "org.typelevel" %%% "vault" % vaultVersion
    )
  )
  .jsSettings(
    tlVersionIntroduced := List("2.13", "3").map(_ -> "6.2.0").toMap,
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )

lazy val s3 = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/s3"))
  .dependsOn(core)
  .settings(
    name := "http4s-aws-s3",
    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-core" % fs2Version,
      "com.magine" %%% "aws-regions" % awsRegionsVersion,
      "io.circe" %%% "circe-core" % circeVersion,
      "org.gnieh" %%% "fs2-data-text" % fs2DataVersion,
      "org.gnieh" %%% "fs2-data-xml" % fs2DataVersion,
      "org.http4s" %%% "http4s-client" % http4sVersion,
      "org.http4s" %%% "http4s-core" % http4sVersion,
      "org.http4s" %%% "http4s-ember-client" % http4sVersion % Test,
      "org.scodec" %%% "scodec-bits" % scodecVersion,
      "org.typelevel" %%% "cats-core" % catsVersion,
      "org.typelevel" %%% "cats-effect-kernel" % catsEffectVersion,
      "org.typelevel" %%% "cats-effect" % catsEffectVersion,
      "org.typelevel" %%% "cats-kernel" % catsVersion,
      "org.typelevel" %%% "literally" % literallyVersion,
      "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectVersion % Test,
      "org.typelevel" %%% "scalacheck-effect-munit" % scalaCheckEffectMunitVersion % Test,
      "org.typelevel" %%% "vault" % vaultVersion
    ),
    tlVersionIntroduced := List("2.13", "3").map(_ -> "6.3.0").toMap
  )
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )
  .jvmSettings(
    Test / fork := true,
    libraryDependencies ++= Seq(
      "org.slf4j" % "slf4j-nop" % slf4jVersion % Test,
      "org.testcontainers" % "testcontainers-localstack" % testcontainersVersion % Test
    )
  )
