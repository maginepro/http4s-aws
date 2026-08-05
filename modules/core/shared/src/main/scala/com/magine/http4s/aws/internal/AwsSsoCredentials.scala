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

import com.magine.http4s.aws.Credentials
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import java.time.Instant
import java.time.temporal.ChronoUnit

private[aws] final case class AwsSsoCredentials(
  accountId: AwsAccountId,
  credentials: Credentials,
  expiration: Instant,
  cacheFileName: AwsCredentialsCache.FileName
) {
  def isFresh(now: Instant): Boolean =
    now.plus(1, ChronoUnit.MINUTES).isBefore(expiration)
}

private[aws] object AwsSsoCredentials {
  def decoder(cacheFileName: AwsCredentialsCache.FileName): Decoder[AwsSsoCredentials] =
    Decoder.instance { c =>
      for {
        accountId <- c.downField("Credentials").get[AwsAccountId]("AccountId")
        accessKeyId <- c.downField("Credentials").get[Credentials.AccessKeyId]("AccessKeyId")
        secretAccessKey <- c.downField("Credentials").get[Credentials.SecretAccessKey]("SecretAccessKey")
        sessionToken <- c.downField("Credentials").get[Credentials.SessionToken]("SessionToken")
        expirationField = c.downField("Credentials").downField("Expiration")
        expirationEpochSecond = expirationField.as[Long].map(Instant.ofEpochSecond)
        expirationIso8601 = expirationField.as(Iso8601.decoder)
        expiration <- expirationEpochSecond.orElse(expirationIso8601)
      } yield AwsSsoCredentials(
        accountId = accountId,
        credentials = Credentials(
          accessKeyId = accessKeyId,
          secretAccessKey = secretAccessKey,
          sessionToken = Some(sessionToken)
        ),
        expiration = expiration,
        cacheFileName = cacheFileName
      )
    }

  implicit val awsSsoCredentialsEncoder: Encoder[AwsSsoCredentials] =
    Encoder.instance { sso =>
      Json.obj(
        "ProviderType" -> "sso".asJson,
        "Credentials" -> Json.obj(
          "AccessKeyId" -> sso.credentials.accessKeyId.asJson,
          "SecretAccessKey" -> sso.credentials.secretAccessKey.asJson,
          "SessionToken" -> sso.credentials.sessionToken.asJson,
          "Expiration" -> sso.expiration.asJson,
          "AccountId" -> sso.accountId.asJson
        ),
      )
    }
}
