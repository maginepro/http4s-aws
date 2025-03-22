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

import cats.syntax.all.*
import com.magine.aws.Region
import com.magine.http4s.aws.AwsProfileName
import com.magine.http4s.aws.MfaSerial

/**
  * Represents a profile in the `~/.aws/config` file.
  */
private[aws] final case class AwsProfile(
  roleArn: AwsProfile.RoleArn,
  roleSessionName: AwsProfile.RoleSessionName,
  durationSeconds: Option[AwsProfile.DurationSeconds],
  sourceProfile: AwsProfileName,
  mfaSerial: MfaSerial,
  region: Option[Region]
)

private[aws] object AwsProfile {
  final case class RoleArn(value: String)

  final case class RoleSessionName(value: String)

  final case class DurationSeconds(value: Int)

  object DurationSeconds {
    val default: DurationSeconds =
      DurationSeconds(3600)
  }

  def fromSection(
    section: IniFile.Section,
    profileName: AwsProfileName
  ): Either[Throwable, AwsProfile] =
    (
      decodeAs[RoleArn](profileName, section, "role_arn")(RoleArn(_).asRight),
      decodeAs[RoleSessionName](profileName, section, "role_session_name")(RoleSessionName(_).asRight),
      decodeAs[Option[DurationSeconds]](profileName, section, "duration_seconds")(
        decode = value =>
          value.toIntOption
            .map(DurationSeconds(_).some)
            .toRight[Throwable](
              new RuntimeException(s"Invalid duration_seconds $value for profile ${profileName.value}")
            ),
        missing = Right(None)
      ),
      decodeAs[AwsProfileName](profileName, section, "source_profile")(AwsProfileName(_).asRight),
      decodeAs[MfaSerial](profileName, section, "mfa_serial")(MfaSerial(_).asRight),
      decodeAs[Option[Region]](profileName, section, "region")(
        decode = value =>
          Region
            .valid(value)
            .map(_.some)
            .leftMap(_ => new RuntimeException(s"Invalid region $value for profile ${profileName.value}")),
        missing = Right(None)
      )
    ).mapN(apply)

  private def decodeAs[A](
    profileName: AwsProfileName,
    section: IniFile.Section,
    key: String
  )(
    decode: String => Either[Throwable, A],
    missing: => Either[Throwable, A] = Left(
      new RuntimeException(s"Missing key $key for profile ${profileName.value}")
    )
  ): Either[Throwable, A] =
    section.keys
      .find(_.name == key)
      .map(section => decode(section.value))
      .getOrElse(missing)
}
