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
import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import java.time.Instant
import java.time.temporal.ChronoUnit

/**
  * Temporary credentials for a [[AwsProfile.RoleArn]].
  */
private[aws] final case class AwsAssumedRole(
  credentials: Credentials,
  expiration: Instant,
  cacheFileName: AwsCredentialsCache.FileName,
  assumedRoleId: AwsAssumedRole.AssumedRoleId,
  assumedRoleArn: AwsAssumedRole.AssumedRoleArn
) {
  def isFresh(now: Instant): Boolean =
    now.isBefore(expiration.minus(1, ChronoUnit.MINUTES))
}

private[aws] object AwsAssumedRole {
  final case class AssumedRoleId(value: String)
  object AssumedRoleId {
    implicit val assumedRoleIdCodec: Codec[AssumedRoleId] =
      Codec.from(
        Decoder[String].map(apply),
        Encoder[String].contramap(_.value)
      )
  }

  final case class AssumedRoleArn(value: String)
  object AssumedRoleArn {
    implicit val assumedRoleArnCodec: Codec[AssumedRoleArn] =
      Codec.from(
        Decoder[String].map(apply),
        Encoder[String].contramap(_.value)
      )
  }

  def decoder(cacheFileName: AwsCredentialsCache.FileName): Decoder[AwsAssumedRole] =
    Decoder.instance { c =>
      for {
        accessKeyId <- c.downField("Credentials").get[Credentials.AccessKeyId]("AccessKeyId")
        secretAccessKey <- c.downField("Credentials").get[Credentials.SecretAccessKey]("SecretAccessKey")
        sessionToken <- c.downField("Credentials").get[Credentials.SessionToken]("SessionToken")
        expirationField = c.downField("Credentials").downField("Expiration")
        expirationEpochSecond = expirationField.as[Long].map(Instant.ofEpochSecond)
        expiration <- expirationEpochSecond.orElse(expirationField.as[Instant])
        assumedRoleId <- c.downField("AssumedRoleUser").get[AssumedRoleId]("AssumedRoleId")
        assumedRoleArn <- c.downField("AssumedRoleUser").get[AssumedRoleArn]("Arn")
      } yield AwsAssumedRole(
        credentials = Credentials(
          accessKeyId = accessKeyId,
          secretAccessKey = secretAccessKey,
          sessionToken = Some(sessionToken)
        ),
        expiration = expiration,
        cacheFileName = cacheFileName,
        assumedRoleId = assumedRoleId,
        assumedRoleArn = assumedRoleArn
      )
    }

  implicit val awsAssumedRoleEncoder: Encoder[AwsAssumedRole] =
    Encoder.instance { assumedRole =>
      Json.obj(
        "Credentials" -> Json.obj(
          "AccessKeyId" -> assumedRole.credentials.accessKeyId.asJson,
          "SecretAccessKey" -> assumedRole.credentials.secretAccessKey.asJson,
          "SessionToken" -> assumedRole.credentials.sessionToken.asJson,
          "Expiration" -> assumedRole.expiration.asJson,
        ),
        "AssumedRoleUser" -> Json.obj(
          "AssumedRoleId" -> assumedRole.assumedRoleId.asJson,
          "Arn" -> assumedRole.assumedRoleArn.asJson
        )
      )
    }
}
