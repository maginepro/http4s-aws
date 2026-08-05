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
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Temporal
import cats.syntax.all.*
import com.magine.http4s.aws.internal.AwsConfig
import com.magine.http4s.aws.internal.AwsCredentialsCache
import com.magine.http4s.aws.internal.AwsSsoCredentials
import com.magine.http4s.aws.internal.AwsSsoProfileResolved
import com.magine.http4s.aws.internal.Setting.Profile
import java.time.Instant

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
  def singleSignOn[F[_]: Async]: F[CredentialsProvider[F]] =
    for {
      profileName <- Profile.readOrDefault
      singleSignOn <- singleSignOn(profileName)
    } yield singleSignOn

  /**
    * Returns a new [[CredentialsProvider]] which reads temporary
    * security credentials from the Single Sign-On (SSO) service.
    *
    * This provider integrates with the AWS CLI through:
    * - reading `~/.aws/config` for profile configuration details,
    * - reading `~/.aws/cli/cache` for temporary credentials.
    *
    * The `~/.aws/config` file is expected to contain a profile entry:
    * {{{
    * [profile ...]
    * sso_session = ...
    * sso_account_id = ...
    * sso_role_name = ...
    * }}}
    *
    * The provider will look for cached credentials in `~/.aws/cli/cache`
    * and cache credentials in memory. Credentials will be returned until
    * there is 1 minute or less left until the expiration. The provider
    * does not support refreshing SSO credentials, but will continue to
    * look for refreshed credentials in `~/.aws/cli/cache` once current
    * credentials have expired.
    *
    * Note the `~/.aws/config` is only read once when the provider is
    * created, so subsequent updates to the configuration are ignored.
    */
  def singleSignOn[F[_]: Async](profileName: AwsProfileName): F[CredentialsProvider[F]] =
    for {
      profile <- AwsConfig.default.read(profileName).flatMap(_.resolveSso)
      credentialsCache = AwsCredentialsCache.default[F]
      singleSignOn <- singleSignOn(profile, credentialsCache)
    } yield singleSignOn

  private[aws] def singleSignOn[F[_]](
    profile: AwsSsoProfileResolved,
    credentialsCache: AwsCredentialsCache[F]
  )(
    implicit F: Temporal[F]
  ): F[CredentialsProvider[F]] =
    Ref[F].of(Option.empty[AwsSsoCredentials]).map { ref =>
      new CredentialsProvider[F] {
        override def credentials: F[Credentials] =
          for {
            now <- Temporal[F].realTime.map(d => Instant.EPOCH.plusNanos(d.toNanos))
            credentials <- ref.get.flatMap {
              case Some(cached) if cached.isFresh(now) =>
                cached.credentials.pure
              case _ =>
                for {
                  cached <- credentialsCache
                    .readSso(profile)
                    .map(_.filter(_.isFresh(now)))
                    .map(_.toRight(MissingCredentials()))
                    .rethrow
                  _ <- ref.set(cached.some)
                } yield cached.credentials
            }
          } yield credentials
      }
    }

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
