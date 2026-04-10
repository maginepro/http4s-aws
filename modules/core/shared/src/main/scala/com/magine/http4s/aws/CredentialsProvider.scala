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
import cats.syntax.all.*

/**
  * Capability to return [[Credentials]] from one or multiple sources.
  *
  * Following are some use cases along with appropriate sources.
  *
  * - When running a service locally, one typically sets environment
  *   variables read by [[CredentialsProvider.environmentVariables]].
  * - When running command-line applications, one typically requests
  *   temporary security credentials from the Security Token Service
  *   (STS) using `CredentialsProvider.securityTokenService`.
  * - When using long-term credentials stored in `~/.aws/credentials`,
  *   one can use `CredentialsProvider.credentialsFile` to read those.
  * - When running a service on Elastic Container Service (ECS), or on
  *   Elastic Kubernetes Service (EKS), or serverless on Fargate, one
  *   can use [[CredentialsProvider.containerEndpoint]] to retrieve
  *   credentials from a container endpoint.
  *
  * In most cases, [[CredentialsProvider.default]] will be the right
  * choice, unless there's a reason to exclude particular credential
  * sources. The default provider will ensure the application can be
  * run in most environments.
  */
trait CredentialsProvider[F[_]] {

  /**
    * Returns a set of [[Credentials]] from one or multiple sources.
    *
    * If the provider is unable to return credentials because there are
    * no credentials available, a [[MissingCredentials]] exception must
    * be raised to indicate other sources could instead be tried.
    */
  def credentials: F[Credentials]
}

object CredentialsProvider extends CredentialsProviderPlatform {

  /**
    * Returns a new [[CredentialsProvider]] which always returns
    * the specified static [[Credentials]].
    */
  def static[F[_]: Applicative](credentials: Credentials): CredentialsProvider[F] = {
    val _credentials = credentials
    new CredentialsProvider[F] {
      override def credentials: F[Credentials] =
        _credentials.pure
    }
  }
}
