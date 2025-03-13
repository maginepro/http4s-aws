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

import cats.effect.Concurrent
import com.magine.http4s.aws.Credentials
import io.circe.Decoder
import java.time.Instant
import java.time.temporal.ChronoUnit
import org.http4s.EntityDecoder
import org.http4s.circe.CirceEntityDecoder

/**
  * Credentials together with a timestamp indicating
  * when the credentials are no longer valid for use.
  *
  * Used by `CredentialsLoader.containerEndpoint` to
  * internally represent the response received from
  * the container endpoint.
  */
private[aws] final case class ExpiringCredentials(credentials: Credentials, expiresAt: Instant) {

  /**
    * Returns `true` if the credentials are active with at
    * least 1 minute until they expire; `false` otherwise.
    */
  def isFresh(now: Instant): Boolean =
    now.isBefore(expiresAt.minus(1, ChronoUnit.MINUTES))
}

private[aws] object ExpiringCredentials {
  implicit val expiringCredentialsDecoder: Decoder[ExpiringCredentials] =
    Decoder.instance { cursor =>
      for {
        accessKeyId <- cursor.get[Credentials.AccessKeyId]("AccessKeyId")
        secretAccessKey <- cursor.get[Credentials.SecretAccessKey]("SecretAccessKey")
        sessionToken <- cursor.get[Option[Credentials.SessionToken]]("Token")
        credentials = Credentials(accessKeyId, secretAccessKey, sessionToken)
        expiresAt <- cursor.get[Instant]("Expiration")(Iso8601.decoder)
      } yield ExpiringCredentials(credentials, expiresAt)
    }

  implicit def expiringCredentialsEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, ExpiringCredentials] =
    CirceEntityDecoder.circeEntityDecoder
}
