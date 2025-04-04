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

import cats.effect.MonadCancelThrow
import cats.effect.Temporal
import cats.effect.syntax.all.*
import com.magine.aws.Region
import fs2.hashing.Hashing
import org.http4s.client.Client

object AwsSigningClient {

  /**
    * Returns a client which signs all requests using the
    * specified credentials, region and service name.
    *
    * Signed requests will be passed to the specified client,
    * which is then responsible for running requests.
    */
  def apply[F[_]: Hashing: Temporal](
    provider: CredentialsProvider[F],
    region: Region,
    serviceName: AwsServiceName
  )(
    client: Client[F]
  ): Client[F] =
    fromSigning(AwsSigning(provider, region, serviceName))(client)

  /**
    * Returns a client which signs all requests using the
    * specified [[AwsSigning]] instance.
    *
    * Signed requests will be passed to the specified client,
    * which is then responsible for running requests.
    */
  def fromSigning[F[_]: MonadCancelThrow](signing: AwsSigning[F])(client: Client[F]): Client[F] =
    Client(signing.sign(_).toResource.flatMap(client.run))
}
