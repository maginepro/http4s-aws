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
import cats.effect.Temporal
import cats.syntax.all.*
import com.magine.aws.Region
import com.magine.http4s.aws.headers.*
import com.magine.http4s.aws.headers.`X-Amz-Algorithm`.`AWS4-HMAC-SHA256`
import com.magine.http4s.aws.internal.*
import org.http4s.Request
import scala.concurrent.duration.FiniteDuration

trait AwsPresigning[F[_]] {

  /**
    * Returns a presigned version of the specified request.
    */
  def presign(request: Request[F]): F[Request[F]]
}

object AwsPresigning {

  /**
    * Returns an [[AwsPresigning]] instance which presigns
    * requests using the specified credentials.
    *
    * The presigning will target the specified region and
    * service, and use the specified expiry time.
    */
  def apply[F[_]: Temporal](
    provider: CredentialsProvider[F],
    region: Region,
    serviceName: AwsServiceName,
    expiry: FiniteDuration
  ): AwsPresigning[F] =
    new AwsPresigning[F] {
      override def presign(request: Request[F]): F[Request[F]] =
        for {
          credentials <- provider.credentials
          prepared <- prepareRequest(
            request = request,
            expiry = expiry,
            accessKeyId = credentials.accessKeyId,
            region = region,
            serviceName = serviceName,
            sessionToken = credentials.sessionToken
          )
          presigned <- presignRequest(
            request = prepared,
            secretAccessKey = credentials.secretAccessKey,
            region = region,
            serviceName = serviceName
          )
        } yield presigned
    }

  /**
    * Returns an [[AwsPresigning]] instance which returns
    * requests unmodified, without any presigning.
    */
  def identity[F[_]: Applicative]: AwsPresigning[F] =
    new AwsPresigning[F] {
      override def presign(request: Request[F]): F[Request[F]] =
        request.pure
    }

  /**
    * Returns a prepared request by adding required headers
    * and query parameters.
    *
    * The headers and query parameters are required in order
    * to be able to presign requests with [[presignRequest]].
    */
  def prepareRequest[F[_]: Temporal](
    request: Request[F],
    expiry: FiniteDuration,
    accessKeyId: Credentials.AccessKeyId,
    region: Region,
    serviceName: AwsServiceName,
    sessionToken: Option[Credentials.SessionToken]
  ): F[Request[F]] =
    for {
      now <- Temporal[F].realTimeInstant
      credential = `X-Amz-Credential`(accessKeyId, now, region, serviceName)
      prepared <- Host
        .putIfAbsent(request)
        .map(`X-Amz-Date`.putQueryParam[F](now))
        .map(`X-Amz-Security-Token`.putQueryParam[F](sessionToken))
        .map(`X-Amz-Algorithm`.putQueryParam[F](`AWS4-HMAC-SHA256`))
        .map(`X-Amz-Expires`.putQueryParam[F](expiry))
        .map(`X-Amz-Credential`.putQueryParam(credential))
        .map(`X-Amz-SignedHeaders`.putQueryParam[F])
    } yield prepared

  /**
    * Returns a presigned request by adding a `X-Amz-Signature`
    * query parameter with a signature.
    *
    * The specified request should have required headers and
    * query parameters, as added by [[prepareRequest]].
    */
  def presignRequest[F[_]: ApplicativeThrow](
    request: Request[F],
    secretAccessKey: Credentials.SecretAccessKey,
    region: Region,
    serviceName: AwsServiceName,
  ): F[Request[F]] =
    RequestDateTime.fromRequestQueryParam(request).map { requestDateTime =>
      val canonicalRequest = CanonicalRequest.fromRequestUnsignedPayload(request, serviceName)
      val credentialScope = CredentialScope(region, requestDateTime.date, serviceName)
      val signingContent = Signature.Legacy.signingContent(canonicalRequest, credentialScope, requestDateTime)
      val signingKey = Signature.Legacy.signingKey(region, requestDateTime.date, secretAccessKey, serviceName)
      val signature = Signature.Legacy.sign(signingKey, signingContent).value
      `X-Amz-Signature`.putQueryParam(signature)(request)
    }
}
