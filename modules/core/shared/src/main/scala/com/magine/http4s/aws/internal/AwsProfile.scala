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

    def parse(value: String, profileName: AwsProfileName): Either[Throwable, DurationSeconds] =
      value.toIntOption.map(apply).toRight(invalidDurationSeconds(value, profileName))

    private def invalidDurationSeconds(value: String, profileName: AwsProfileName): Throwable =
      new RuntimeException(s"Invalid duration_seconds $value for profile ${profileName.value}")
  }

  object Region {
    def parse(value: String, profileName: AwsProfileName): Either[Throwable, Region] =
      com.magine.aws.Region.valid(value).leftMap(_ => invalidRegion(value, profileName))

    private def invalidRegion(value: String, profileName: AwsProfileName): Throwable =
      new RuntimeException(s"Invalid region $value for profile ${profileName.value}")
  }

  def parse(
    section: IniFile.Section,
    profileName: AwsProfileName
  ): Either[Throwable, AwsProfile] = {
    val decode = Decode(section, profileName)

    (
      profileName.asRight,
      decode.required("role_arn", RoleArn(_).asRight),
      decode.required("role_session_name", RoleSessionName(_).asRight),
      decode.optional("duration_seconds", DurationSeconds.parse(_, profileName)),
      decode.required("source_profile", AwsProfileName(_).asRight),
      decode.required("mfa_serial", MfaSerial(_).asRight),
      decode.optional("region", Region.parse(_, profileName))
    ).mapN(apply)
  }

  private final case class Decode(section: IniFile.Section, profileName: AwsProfileName) {
    def optional[A](key: String, decode: String => Either[Throwable, A]): Either[Throwable, Option[A]] =
      section.keys
        .find(_.name == key)
        .map(section => decode(section.value).map(_.some))
        .getOrElse(Right(None))

    def required[A](key: String, decode: String => Either[Throwable, A]): Either[Throwable, A] =
      section.keys
        .find(_.name == key)
        .map(section => decode(section.value))
        .getOrElse(Left(missingRequired(key)))

    private def missingRequired(key: String): Throwable =
      new RuntimeException(s"Missing required key $key for profile ${profileName.value}")
  }
}
