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

package com.magine.http4s.aws

import cats.Applicative
import cats.ApplicativeThrow
import cats.effect.MonadCancelThrow
import cats.effect.Temporal
import cats.syntax.all.*
import com.magine.aws.Region
import com.magine.http4s.aws.headers.*
import com.magine.http4s.aws.internal.*
import fs2.hashing.Hashing
import org.http4s.Request

trait AwsSigning[F[_]] {

  /**
    * Returns a signed version of the specified request.
    */
  def sign(request: Request[F]): F[Request[F]]
}

object AwsSigning {

  /**
    * Returns an [[AwsSigning]] instance which signs
    * requests using the specified credentials.
    *
    * The signing will target the specified region
    * and service.
    */
  def apply[F[_]: Hashing: Temporal](
    provider: CredentialsProvider[F],
    region: Region,
    serviceName: AwsServiceName
  ): AwsSigning[F] =
    new AwsSigning[F] {
      override def sign(request: Request[F]): F[Request[F]] =
        for {
          credentials <- provider.credentials
          prepared <- prepareRequest(
            request = request,
            sessionToken = credentials.sessionToken
          )
          signed <- signRequestHashing(
            request = prepared,
            accessKeyId = credentials.accessKeyId,
            secretAccessKey = credentials.secretAccessKey,
            region = region,
            serviceName = serviceName
          )
        } yield signed
    }

  /**
    * Returns an [[AwsSigning]] instance which returns
    * requests unmodified, without any signing.
    */
  def identity[F[_]: Applicative]: AwsSigning[F] =
    new AwsSigning[F] {
      override def sign(request: Request[F]): F[Request[F]] =
        request.pure
    }

  /**
    * Returns a prepared request by adding required headers
    * if they are not already present.
    *
    * The headers are required in order to be able to sign
    * requests with [[signRequest]].
    */
  def prepareRequest[F[_]: Hashing: Temporal](
    request: Request[F],
    sessionToken: Option[Credentials.SessionToken]
  ): F[Request[F]] =
    Host
      .putIfAbsent(request)
      .flatMap(`X-Amz-Content-SHA256`.putIfAbsent[F])
      .flatMap(`X-Amz-Date`.putIfAbsent[F])
      .map(`X-Amz-Security-Token`.putIfAbsent[F](sessionToken))

  /**
    * Returns a signed request by adding an `Authorization`
    * header if one is not already present.
    *
    * The specified request should have required headers, as
    * added by [[prepareRequest]].
    */
  def signRequest[F[_]: Hashing: MonadCancelThrow](
    request: Request[F],
    accessKeyId: Credentials.AccessKeyId,
    secretAccessKey: Credentials.SecretAccessKey,
    region: Region,
    serviceName: AwsServiceName
  ): F[Request[F]] =
    signRequestHashing(request, accessKeyId, secretAccessKey, region, serviceName)

  /* TODO: Inline for 7.0 release. */
  private def signRequestHashing[F[_]: Hashing: MonadCancelThrow](
    request: Request[F],
    accessKeyId: Credentials.AccessKeyId,
    secretAccessKey: Credentials.SecretAccessKey,
    region: Region,
    serviceName: AwsServiceName
  ): F[Request[F]] =
    for {
      canonicalRequest <- CanonicalRequest.fromRequest(request, serviceName)
      requestDateTime <- RequestDateTime.fromRequest(request)
      credentialScope = CredentialScope(region, requestDateTime.date, serviceName)
      signingContent = Signature.signingContent(canonicalRequest, credentialScope, requestDateTime)
      signingKey <- Signature.signingKey(region, requestDateTime.date, secretAccessKey, serviceName)
      signature <- Signature.sign(signingKey, signingContent)
    } yield Authorization.putIfAbsent(request, accessKeyId, canonicalRequest, credentialScope, signature)

  /* TODO: Remove for 7.0 release. */
  private[aws] def signRequest[F[_]: ApplicativeThrow](
    request: Request[F],
    accessKeyId: Credentials.AccessKeyId,
    secretAccessKey: Credentials.SecretAccessKey,
    region: Region,
    serviceName: AwsServiceName
  ): F[Request[F]] =
    (
      CanonicalRequest.fromRequest(request, serviceName),
      RequestDateTime.fromRequest(request)
    ).mapN { (canonicalRequest, requestDateTime) =>
      val credentialScope = CredentialScope(region, requestDateTime.date, serviceName)
      val signingContent = Signature.Legacy.signingContent(canonicalRequest, credentialScope, requestDateTime)
      val signingKey = Signature.Legacy.signingKey(region, requestDateTime.date, secretAccessKey, serviceName)
      val signature = Signature.Legacy.sign(signingKey, signingContent)
      Authorization.putIfAbsent(request, accessKeyId, canonicalRequest, credentialScope, signature)
    }
}
