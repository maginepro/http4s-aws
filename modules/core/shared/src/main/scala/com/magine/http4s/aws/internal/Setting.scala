/*
 * Copyright 2025 Magine Pro
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.magine.http4s.aws.internal

import cats.effect.Sync
import cats.syntax.all.*
import com.magine.http4s.aws.AwsProfileName
import com.magine.http4s.aws.Credentials
import java.nio.file.Path
import java.nio.file.Paths
import org.http4s.Header
import org.http4s.Uri
import org.typelevel.ci.*

/**
  * Describes settings (environment variables and system
  * properties) specific to AWS and loading credentials.
  *
  * For most settings, [[read]] can be used to read both
  * the system property and the environment variable. An
  * optional [[fallback]] value can also be defined when
  * neither the system property or environment variable
  * has been set. The [[env]] and [[prop]] functions can
  * be used to only read the environment variable or the
  * system property, respectively.
  *
  * The [[parse]] function must be implemented to parse
  * the `String` value from the system property and the
  * environment variable.
  */
private[aws] sealed abstract class Setting[A](envName: String, propName: String) {
  final def env[F[_]: Sync]: F[Option[A]] =
    Sync[F]
      .delay(Option(System.getenv(envName)))
      .flatMap(_.traverse(parse[F]))

  def fallback[F[_]: Sync]: F[Option[A]] =
    Sync[F].pure(None)

  def parse[F[_]: Sync](value: String): F[A]

  final def prop[F[_]: Sync]: F[Option[A]] =
    Sync[F]
      .delay(Option(System.getProperty(propName)))
      .flatMap(_.traverse(parse[F]))

  final def read[F[_]: Sync]: F[Option[A]] =
    prop
      .flatMap(_.map(_.some.pure).getOrElse(env))
      .flatMap(_.map(_.some.pure).getOrElse(fallback))
}

private[aws] object Setting {

  /**
    * Describes a [[Setting]] with a default value, which
    * can be used via [[readOrDefault]], using the default
    * value when neither the system property, environment
    * variable, or fallback value is available.
    */
  sealed abstract class Default[A](
    envName: String,
    propName: String,
    default: A
  ) extends Setting[A](envName, propName) {
    final def readOrDefault[F[_]: Sync]: F[A] =
      read.map(_.getOrElse(default))
  }

  object AccessKeyId
    extends Setting[Credentials.AccessKeyId](
      envName = "AWS_ACCESS_KEY_ID",
      propName = "aws.accessKeyId"
    ) {
    override def parse[F[_]: Sync](value: String): F[Credentials.AccessKeyId] =
      Sync[F].pure(Credentials.AccessKeyId(value))
  }

  object ConfigFile
    extends Setting[Path](
      envName = "AWS_CONFIG_FILE",
      propName = "aws.configFile",
    ) {
    override def fallback[F[_]: Sync]: F[Option[Path]] =
      Sync[F].delay(Option(System.getProperty("user.home")).map(Paths.get(_, ".aws/config")))

    override def parse[F[_]: Sync](value: String): F[Path] =
      Sync[F].delay(Paths.get(value))
  }

  object ContainerAuthorizationToken
    extends Setting[Header.Raw](
      envName = "AWS_CONTAINER_AUTHORIZATION_TOKEN",
      propName = "aws.containerAuthorizationToken"
    ) {
    override def parse[F[_]: Sync](value: String): F[Header.Raw] =
      Sync[F].pure(Header.Raw(ci"Authorization", value))
  }

  object ContainerCredentialsRelativeUri
    extends Setting[String](
      envName = "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI",
      propName = "aws.containerCredentialsPath",
    ) {
    override def parse[F[_]: Sync](value: String): F[String] =
      Sync[F].pure(value)
  }

  object ContainerCredentialsFullUri
    extends Setting[Uri](
      envName = "AWS_CONTAINER_CREDENTIALS_FULL_URI",
      propName = "aws.containerCredentialsFullUri"
    ) {
    override def parse[F[_]: Sync](value: String): F[Uri] =
      Uri.fromString(value).liftTo[F]
  }

  object ContainerServiceEndpoint
    extends Setting.Default[String](
      envName = "AWS_CONTAINER_SERVICE_ENDPOINT",
      propName = "aws.containerServiceEndpoint",
      default = "http://169.254.170.2"
    ) {
    override def parse[F[_]: Sync](value: String): F[String] =
      Sync[F].pure(value)
  }

  object Profile
    extends Setting.Default[AwsProfileName](
      envName = "AWS_PROFILE",
      propName = "aws.profile",
      default = AwsProfileName.default
    ) {
    override def parse[F[_]: Sync](value: String): F[AwsProfileName] =
      Sync[F].pure(AwsProfileName(value))
  }

  object SecretAccessKey
    extends Setting[Credentials.SecretAccessKey](
      envName = "AWS_SECRET_ACCESS_KEY",
      propName = "aws.secretAccessKey"
    ) {
    override def parse[F[_]: Sync](value: String): F[Credentials.SecretAccessKey] =
      Sync[F].pure(Credentials.SecretAccessKey(value))
  }

  object SessionToken
    extends Setting[Credentials.SessionToken](
      envName = "AWS_SESSION_TOKEN",
      propName = "aws.sessionToken"
    ) {
    override def parse[F[_]: Sync](value: String): F[Credentials.SessionToken] =
      Sync[F].pure(Credentials.SessionToken(value))
  }

  object SharedCredentialsFile
    extends Setting[Path](
      envName = "AWS_SHARED_CREDENTIALS_FILE",
      propName = "aws.sharedCredentialsFile",
    ) {
    override def fallback[F[_]: Sync]: F[Option[Path]] =
      Sync[F].delay(Option(System.getProperty("user.home")).map(Paths.get(_, ".aws/credentials")))

    override def parse[F[_]: Sync](value: String): F[Path] =
      Sync[F].delay(Paths.get(value))
  }
}
