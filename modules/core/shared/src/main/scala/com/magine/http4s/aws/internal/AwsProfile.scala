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

import cats.data.NonEmptyChain
import cats.data.ValidatedNec
import cats.effect.Sync
import cats.syntax.all.*
import com.magine.aws.Region
import com.magine.http4s.aws.AwsProfileName
import com.magine.http4s.aws.MfaSerial

/**
  * Represents a profile in the `~/.aws/config` file.
  */
private[aws] final case class AwsProfile(
  profileName: AwsProfileName,
  roleArn: AwsProfile.RoleArn,
  roleSessionName: AwsProfile.RoleSessionName,
  durationSeconds: Option[AwsProfile.DurationSeconds],
  sourceProfile: AwsProfileName,
  mfaSerial: MfaSerial,
  region: Option[Region]
) {
  def resolveRegion[F[_]: Sync]: F[Region] =
    Setting.Region.read
      .flatMap(_.map(_.some.pure).getOrElse(Setting.DefaultRegion.read))
      .flatMap(_.orElse(region).toRight(missingRegion).liftTo[F])

  private def missingRegion: Throwable =
    new RuntimeException(s"Missing region for profile ${profileName.value}")
}

private[aws] object AwsProfile {
  final case class RoleArn(value: String)

  final case class RoleSessionName(value: String)

  final case class DurationSeconds(value: Int)

  object DurationSeconds {
    val default: DurationSeconds =
      DurationSeconds(3600)

    def parse(value: String): Either[String, DurationSeconds] =
      value.toIntOption.map(apply).toRight(invalidDurationSeconds(value))

    private def invalidDurationSeconds(value: String): String =
      s"Invalid duration_seconds: $value"
  }

  object Region {
    def parse(value: String): Either[String, Region] =
      com.magine.aws.Region.valid(value).leftMap(_.getMessage)
  }

  def parse(
    section: IniFile.Section,
    profileName: AwsProfileName
  ): Either[Throwable, AwsProfile] = {
    val decode = Decode(section, profileName)

    (
      profileName.validNec,
      decode.required("role_arn", RoleArn(_).asRight),
      decode.required("role_session_name", RoleSessionName(_).asRight),
      decode.optional("duration_seconds", DurationSeconds.parse(_)),
      decode.required("source_profile", AwsProfileName(_).asRight),
      decode.required("mfa_serial", MfaSerial(_).asRight),
      decode.optional("region", Region.parse(_))
    ).mapN(apply).toEither.leftMap(parseError(profileName))
  }

  private def parseError(profileName: AwsProfileName)(errors: NonEmptyChain[String]): Throwable =
    if (errors.tail.isEmpty)
      new RuntimeException(s"${errors.head} for profile ${profileName.value}")
    else {
      val messages = errors.toList.map(error => s"- $error").mkString("\n")
      val message = s"Issues for profile ${profileName.value}:\n$messages"
      new RuntimeException(message)
    }

  private final case class Decode(section: IniFile.Section, profileName: AwsProfileName) {
    def optional[A](key: String, decode: String => Either[String, A]): ValidatedNec[String, Option[A]] =
      section.keys
        .find(_.name == key)
        .map(section => decode(section.value).map(_.some))
        .getOrElse(Right(None))
        .toValidatedNec

    def required[A](key: String, decode: String => Either[String, A]): ValidatedNec[String, A] =
      section.keys
        .find(_.name == key)
        .map(section => decode(section.value))
        .getOrElse(Left(missingRequired(key)))
        .toValidatedNec

    private def missingRequired(key: String): String =
      s"Missing required key $key"
  }
}
