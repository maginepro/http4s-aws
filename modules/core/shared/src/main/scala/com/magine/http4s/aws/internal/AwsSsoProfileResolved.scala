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

private[aws] final case class AwsSsoProfileResolved(
  profileName: AwsProfileName,
  ssoAccountId: AwsSsoAccountId,
  ssoRoleName: AwsSsoRoleName,
  ssoSessionName: AwsSsoSessionName
)

private[aws] object AwsSsoProfileResolved {
  def fromProfile[F[_]: Sync](profile: AwsProfile): F[AwsSsoProfileResolved] =
    for {
      ssoAccountId <- profile.ssoAccountId.liftTo[F](missing("sso_account_id", profile))
      ssoRoleName <- profile.ssoRoleName.liftTo[F](missing("sso_role_name", profile))
      ssoSessionName <- profile.ssoSessionName.liftTo[F](missing("sso_session", profile))
    } yield AwsSsoProfileResolved(
      profileName = profile.profileName,
      ssoAccountId = ssoAccountId,
      ssoRoleName = ssoRoleName,
      ssoSessionName = ssoSessionName,
    )

  private def missing(key: String, profile: AwsProfile): Throwable =
    new RuntimeException(s"Missing $key for profile ${profile.profileName.value}")
}
