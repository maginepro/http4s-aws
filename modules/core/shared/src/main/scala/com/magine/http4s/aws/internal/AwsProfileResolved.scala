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

private[aws] final case class AwsProfileResolved(
  profileName: AwsProfileName,
  roleArn: AwsProfile.RoleArn,
  roleSessionName: AwsProfile.RoleSessionName,
  durationSeconds: Option[AwsProfile.DurationSeconds],
  sourceProfile: AwsProfileName,
  mfaSerial: MfaSerial,
  region: Region
)

private[aws] object AwsProfileResolved {
  def fromProfile[F[_]: Sync](profile: AwsProfile): F[AwsProfileResolved] =
    for {
      roleArn <- resolveRoleArn(profile)
      roleSessionName <- resolveRoleSessionName(profile)
      region <- resolveRegion(profile)
    } yield AwsProfileResolved(
      profileName = profile.profileName,
      roleArn = roleArn,
      roleSessionName = roleSessionName,
      durationSeconds = profile.durationSeconds,
      sourceProfile = profile.sourceProfile,
      mfaSerial = profile.mfaSerial,
      region = region
    )

  private def resolveRoleArn[F[_]: Sync](profile: AwsProfile): F[AwsProfile.RoleArn] =
    Setting.RoleArn.read.flatMap(_.orElse(profile.roleArn).toRight(missingRoleArn(profile)).liftTo[F])

  private def resolveRoleSessionName[F[_]: Sync](profile: AwsProfile): F[AwsProfile.RoleSessionName] =
    Setting.RoleSessionName.read
      .map(_.orElse(profile.roleSessionName))
      .flatMap(_.map(_.pure).getOrElse(AwsProfile.RoleSessionName.default))

  private def resolveRegion[F[_]: Sync](profile: AwsProfile): F[Region] =
    Setting.Region.read
      .flatMap(_.map(_.some.pure).getOrElse(Setting.DefaultRegion.read))
      .flatMap(_.orElse(profile.region).toRight(missingRegion(profile)).liftTo[F])

  private def missingRoleArn(profile: AwsProfile): Throwable =
    new RuntimeException(s"Missing role_arn for profile ${profile.profileName.value}")

  private def missingRegion(profile: AwsProfile): Throwable =
    new RuntimeException(s"Missing region for profile ${profile.profileName.value}")
}
