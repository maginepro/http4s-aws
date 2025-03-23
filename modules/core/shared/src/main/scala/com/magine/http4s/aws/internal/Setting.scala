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
import com.magine.aws.Region
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
  */
private[aws] trait Setting[F[_], A] {
  def env: F[Option[A]]

  def fallback: F[Option[A]]

  def prop: F[Option[A]]

  def read: F[Option[A]]
}

private[aws] object Setting {
  sealed abstract class Standard[F[_]: Sync, A](
    envName: String,
    propName: String
  ) extends Setting[F, A] {
    final def env: F[Option[A]] =
      Sync[F]
        .delay(Option(System.getenv(envName)))
        .flatMap(_.traverse(parse))

    def fallback: F[Option[A]] =
      Sync[F].pure(None)

    def parse(value: String): F[A]

    final def prop: F[Option[A]] =
      Sync[F]
        .delay(Option(System.getProperty(propName)))
        .flatMap(_.traverse(parse))

    final def read: F[Option[A]] =
      prop
        .flatMap(_.map(_.some.pure).getOrElse(env))
        .flatMap(_.map(_.some.pure).getOrElse(fallback))
  }

  /**
    * Describes a [[Setting]] with a default value, which
    * can be used via [[readOrDefault]], using the default
    * value when neither the system property, environment
    * variable, or fallback value is available.
    */
  sealed abstract class Default[F[_]: Sync, A](
    envName: String,
    propName: String,
    default: A
  ) extends Standard[F, A](envName, propName) {
    final def readOrDefault: F[A] =
      read.map(_.getOrElse(default))
  }

  def AccessKeyId[F[_]: Sync]: Setting[F, Credentials.AccessKeyId] =
    new Setting.Standard[F, Credentials.AccessKeyId](
      envName = "AWS_ACCESS_KEY_ID",
      propName = "aws.accessKeyId"
    ) {
      override def parse(value: String): F[Credentials.AccessKeyId] =
        Credentials.AccessKeyId(value).pure
    }

  def ConfigFile[F[_]: Sync]: Setting[F, Path] =
    new Setting.Standard[F, Path](
      envName = "AWS_CONFIG_FILE",
      propName = "aws.configFile",
    ) {
      override def fallback: F[Option[Path]] =
        Sync[F].delay(Option(System.getProperty("user.home")).map(Paths.get(_, ".aws/config")))

      override def parse(value: String): F[Path] =
        Sync[F].delay(Paths.get(value))
    }

  def ContainerAuthorizationToken[F[_]: Sync]: Setting[F, Header.Raw] =
    new Setting.Standard[F, Header.Raw](
      envName = "AWS_CONTAINER_AUTHORIZATION_TOKEN",
      propName = "aws.containerAuthorizationToken"
    ) {
      override def parse(value: String): F[Header.Raw] =
        Header.Raw(ci"Authorization", value).pure
    }

  def ContainerCredentialsRelativeUri[F[_]: Sync]: Setting[F, String] =
    new Setting.Standard[F, String](
      envName = "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI",
      propName = "aws.containerCredentialsPath",
    ) {
      override def parse(value: String): F[String] =
        value.pure
    }

  def ContainerCredentialsFullUri[F[_]: Sync]: Setting[F, Uri] =
    new Setting.Standard[F, Uri](
      envName = "AWS_CONTAINER_CREDENTIALS_FULL_URI",
      propName = "aws.containerCredentialsFullUri"
    ) {
      override def parse(value: String): F[Uri] =
        Uri.fromString(value).liftTo[F]
    }

  def ContainerServiceEndpoint[F[_]: Sync]: Setting.Default[F, String] =
    new Setting.Default[F, String](
      envName = "AWS_CONTAINER_SERVICE_ENDPOINT",
      propName = "aws.containerServiceEndpoint",
      default = "http://169.254.170.2"
    ) {
      override def parse(value: String): F[String] =
        value.pure
    }

  def DefaultRegion[F[_]: Sync]: Setting[F, com.magine.aws.Region] =
    new Setting.Standard[F, Region](
      envName = "AWS_DEFAULT_REGION",
      propName = "aws.defaultRegion"
    ) {
      override def parse(value: String): F[com.magine.aws.Region] =
        com.magine.aws.Region.valid(value).liftTo[F]
    }

  def Profile[F[_]: Sync]: Setting.Default[F, AwsProfileName] =
    new Setting.Default[F, AwsProfileName](
      envName = "AWS_PROFILE",
      propName = "aws.profile",
      default = AwsProfileName.default
    ) {
      override def parse(value: String): F[AwsProfileName] =
        AwsProfileName(value).pure
    }

  def Region[F[_]: Sync]: Setting[F, com.magine.aws.Region] =
    new Setting.Standard[F, Region](
      envName = "AWS_REGION",
      propName = "aws.region"
    ) {
      override def parse(value: String): F[com.magine.aws.Region] =
        com.magine.aws.Region.valid(value).liftTo[F]
    }

  def RoleArn[F[_]: Sync]: Setting[F, AwsProfile.RoleArn] =
    new Setting.Standard[F, AwsProfile.RoleArn](
      envName = "AWS_ROLE_ARN",
      propName = "aws.roleArn"
    ) {
      override def parse(value: String): F[AwsProfile.RoleArn] =
        AwsProfile.RoleArn(value).pure
    }

  def RoleSessionName[F[_]: Sync]: Setting[F, AwsProfile.RoleSessionName] =
    new Setting.Standard[F, AwsProfile.RoleSessionName](
      envName = "AWS_ROLE_SESSION_NAME",
      propName = "aws.roleSessionName"
    ) {
      override def parse(value: String): F[AwsProfile.RoleSessionName] =
        AwsProfile.RoleSessionName(value).pure
    }

  def SecretAccessKey[F[_]: Sync]: Setting[F, Credentials.SecretAccessKey] =
    new Setting.Standard[F, Credentials.SecretAccessKey](
      envName = "AWS_SECRET_ACCESS_KEY",
      propName = "aws.secretAccessKey"
    ) {
      override def parse(value: String): F[Credentials.SecretAccessKey] =
        Credentials.SecretAccessKey(value).pure
    }

  def SessionToken[F[_]: Sync]: Setting[F, Credentials.SessionToken] =
    new Setting.Standard[F, Credentials.SessionToken](
      envName = "AWS_SESSION_TOKEN",
      propName = "aws.sessionToken"
    ) {
      override def parse(value: String): F[Credentials.SessionToken] =
        Credentials.SessionToken(value).pure
    }

  def SharedCredentialsFile[F[_]: Sync]: Setting[F, Path] =
    new Setting.Standard[F, Path](
      envName = "AWS_SHARED_CREDENTIALS_FILE",
      propName = "aws.sharedCredentialsFile",
    ) {
      override def fallback: F[Option[Path]] =
        Sync[F].delay(Option(System.getProperty("user.home")).map(Paths.get(_, ".aws/credentials")))

      override def parse(value: String): F[Path] =
        Sync[F].delay(Paths.get(value))
    }
}
