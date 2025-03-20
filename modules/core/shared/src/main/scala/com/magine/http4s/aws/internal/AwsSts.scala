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

import cats.effect.Async
import cats.effect.Concurrent
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.magine.aws.Region
import com.magine.http4s.aws.AwsServiceName
import com.magine.http4s.aws.AwsSigningClient
import com.magine.http4s.aws.CredentialsProvider
import com.magine.http4s.aws.MfaSerial
import com.magine.http4s.aws.TokenCode
import fs2.hashing.Hashing
import io.circe.Decoder
import org.http4s.EntityDecoder
import org.http4s.EntityEncoder
import org.http4s.MediaType
import org.http4s.Method
import org.http4s.Request
import org.http4s.Status
import org.http4s.Status.Successful
import org.http4s.Uri
import org.http4s.UrlForm
import org.http4s.circe.CirceEntityDecoder
import org.http4s.client.Client
import org.http4s.headers.Accept
import org.http4s.headers.MediaRangeAndQValue
import scala.util.control.NoStackTrace

/**
  * Capability to request temporary credentials for a [[AwsProfile.RoleArn]].
  */
private[aws] trait AwsSts[F[_]] {
  def assumeRole(
    roleArn: AwsProfile.RoleArn,
    roleSessionName: AwsProfile.RoleSessionName,
    durationSeconds: Option[AwsProfile.DurationSeconds],
    mfaSerial: MfaSerial,
    tokenCode: TokenCode
  ): F[AwsAssumedRole]
}

private[aws] object AwsSts {
  def fromClient[F[_]: Async: Hashing](
    client: Client[F],
    provider: CredentialsProvider[F],
    region: Region
  ): AwsSts[F] =
    fromSigningClient(
      AwsSigningClient(provider, region, AwsServiceName.SecurityTokenService)(client),
      region
    )

  def fromSigningClient[F[_]: Async](client: Client[F], region: Region): AwsSts[F] =
    new AwsSts[F] {
      override def assumeRole(
        roleArn: AwsProfile.RoleArn,
        roleSessionName: AwsProfile.RoleSessionName,
        durationSeconds: Option[AwsProfile.DurationSeconds],
        mfaSerial: MfaSerial,
        tokenCode: TokenCode
      ): F[AwsAssumedRole] = {
        val request =
          Uri.fromString(s"https://sts.$region.amazonaws.com").liftTo[F].map { uri =>
            Request[F](Method.POST, uri)
              .withEntity(
                AssumeRoleRequest(
                  roleArn = roleArn,
                  roleSessionName = roleSessionName,
                  durationSeconds = durationSeconds.getOrElse(AwsProfile.DurationSeconds.default),
                  mfaSerial = mfaSerial,
                  tokenCode = tokenCode
                )
              )
              .putHeaders(Accept(MediaRangeAndQValue(MediaType.application.json)))
          }

        request.toResource.flatMap(client.run).use {
          case Successful(response) =>
            AwsCredentialsCache.FileName[F](roleArn, roleSessionName, durationSeconds, mfaSerial).flatMap {
              cacheFileName =>
                AssumeRoleResponse
                  .entityDecoder(cacheFileName)
                  .decode(response, strict = false)
                  .leftWiden[Throwable]
                  .rethrowT
                  .map(_.value)
            }

          case response =>
            AssumeRoleErrorResponse.entityDecoder
              .decode(response, strict = false)
              .leftWiden[Throwable]
              .rethrowT
              .flatMap(unexpected =>
                Concurrent[F].raiseError(
                  UnexpectedError(
                    unexpected.code.value,
                    unexpected.message,
                    response.status
                  )
                )
              )
        }
      }
    }

  final case class UnexpectedError(
    errorCode: String,
    message: Option[String],
    status: Status
  ) extends RuntimeException
    with NoStackTrace {
    override def getMessage: String = {
      val trailing = message.foldMap(message => s": $message")
      s"unexpected HTTP status $status with error code $errorCode$trailing"
    }
  }

  private final case class AssumeRoleRequest(
    roleArn: AwsProfile.RoleArn,
    roleSessionName: AwsProfile.RoleSessionName,
    durationSeconds: AwsProfile.DurationSeconds,
    mfaSerial: MfaSerial,
    tokenCode: TokenCode
  )
  private object AssumeRoleRequest {
    implicit def entityEncoder[F[_]]: EntityEncoder[F, AssumeRoleRequest] =
      EntityEncoder[F, UrlForm].contramap(request =>
        UrlForm(
          "Action" -> "AssumeRole",
          "Version" -> "2011-06-15",
          "RoleArn" -> request.roleArn.value,
          "RoleSessionName" -> request.roleSessionName.value,
          "DurationSeconds" -> request.durationSeconds.value.show,
          "SerialNumber" -> request.mfaSerial.value,
          "TokenCode" -> request.tokenCode.value
        )
      )
  }

  private final case class AssumeRoleResponse(value: AwsAssumedRole)
  private object AssumeRoleResponse {
    def decoder(cacheFileName: AwsCredentialsCache.FileName): Decoder[AssumeRoleResponse] = {
      implicit val decoder: Decoder[AwsAssumedRole] =
        AwsAssumedRole.decoder(cacheFileName)

      Decoder.instance(cursor =>
        cursor
          .downField("AssumeRoleResponse")
          .downField("AssumeRoleResult")
          .as[AwsAssumedRole]
          .map(apply)
      )
    }

    def entityDecoder[F[_]: Concurrent](
      cacheFileName: AwsCredentialsCache.FileName
    ): EntityDecoder[F, AssumeRoleResponse] = {
      implicit val decoder: Decoder[AssumeRoleResponse] =
        AssumeRoleResponse.decoder(cacheFileName)

      CirceEntityDecoder.circeEntityDecoder
    }
  }

  private sealed abstract class AssumeRoleErrorResponseCode(val value: String)
  private object AssumeRoleErrorResponseCode {
    case object ExpiredToken extends AssumeRoleErrorResponseCode("ExpiredToken")
    case object MalformedPolicyDocument extends AssumeRoleErrorResponseCode("MalformedPolicyDocument")
    case object PackedPolicyTooLarge extends AssumeRoleErrorResponseCode("PackedPolicyTooLarge")
    case object RegionDisabled extends AssumeRoleErrorResponseCode("RegionDisabled")
    final case class Other(override val value: String) extends AssumeRoleErrorResponseCode(value)

    implicit val decoder: Decoder[AssumeRoleErrorResponseCode] =
      Decoder[String].map {
        case ExpiredToken.value => ExpiredToken
        case MalformedPolicyDocument.value => MalformedPolicyDocument
        case PackedPolicyTooLarge.value => PackedPolicyTooLarge
        case RegionDisabled.value => RegionDisabled
        case code => Other(code)
      }
  }

  private final case class AssumeRoleErrorResponse(
    code: AssumeRoleErrorResponseCode,
    message: Option[String]
  )
  private object AssumeRoleErrorResponse {
    implicit val decoder: Decoder[AssumeRoleErrorResponse] =
      Decoder.instance { cursor =>
        for {
          code <- cursor.downField("Error").downField("Code").as[AssumeRoleErrorResponseCode]
          message <- cursor.downField("Error").downField("Message").as[Option[String]]
        } yield apply(code, message)
      }

    implicit def entityDecoder[F[_]: Concurrent]: EntityDecoder[F, AssumeRoleErrorResponse] =
      CirceEntityDecoder.circeEntityDecoder
  }
}
