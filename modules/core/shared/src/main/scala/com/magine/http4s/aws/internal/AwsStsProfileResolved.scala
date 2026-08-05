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
  * Represents a resolved profile in the `~/.aws/config` file.
  *
  * A resolved profile is an [[AwsProfile]] where the details
  * required by [[AwsSts]] has been resolved. This is done by
  * combining the configuration file, some system properties,
  * some environment variables, and some default values.
  */
private[aws] final case class AwsStsProfileResolved(
  profileName: AwsProfileName,
  roleArn: AwsProfile.RoleArn,
  roleSessionName: AwsProfile.RoleSessionName,
  durationSeconds: Option[AwsProfile.DurationSeconds],
  sourceProfile: AwsProfileName,
  mfaSerial: MfaSerial,
  region: Region
)

private[aws] object AwsStsProfileResolved {
  def fromProfile[F[_]: Sync](profile: AwsProfile): F[AwsStsProfileResolved] =
    for {
      mfaSerial <- profile.mfaSerial.liftTo[F](missing("mfa_serial", profile))
      sourceProfile <- profile.sourceProfile.liftTo[F](missing("source_profile", profile))
      roleArn <- resolveRoleArn(profile)
      roleSessionName <- resolveRoleSessionName(profile)
      region <- resolveRegion(profile)
    } yield AwsStsProfileResolved(
      profileName = profile.profileName,
      roleArn = roleArn,
      roleSessionName = roleSessionName,
      durationSeconds = profile.durationSeconds,
      sourceProfile = sourceProfile,
      mfaSerial = mfaSerial,
      region = region
    )

  private def resolveRoleArn[F[_]: Sync](profile: AwsProfile): F[AwsProfile.RoleArn] =
    Setting.RoleArn.read.flatMap(_.orElse(profile.roleArn).toRight(missing("role_arn", profile)).liftTo[F])

  private def resolveRoleSessionName[F[_]: Sync](profile: AwsProfile): F[AwsProfile.RoleSessionName] =
    Setting.RoleSessionName.read
      .map(_.orElse(profile.roleSessionName))
      .flatMap(_.map(_.pure).getOrElse(AwsProfile.RoleSessionName.default))

  private def resolveRegion[F[_]: Sync](profile: AwsProfile): F[Region] =
    Setting.Region.read
      .flatMap(_.map(_.some.pure).getOrElse(Setting.DefaultRegion.read))
      .flatMap(_.orElse(profile.region).toRight(missing("region", profile)).liftTo[F])

  private def missing(key: String, profile: AwsProfile): Throwable =
    new RuntimeException(s"Missing $key for profile ${profile.profileName.value}")
}
