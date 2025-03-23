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
import cats.effect.Deferred
import cats.effect.Outcome
import cats.effect.Poll
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.Temporal
import cats.effect.syntax.all.*
import cats.parse.Parser
import cats.syntax.all.*
import com.magine.http4s.aws.internal.AwsAssumedRole
import com.magine.http4s.aws.internal.AwsConfig
import com.magine.http4s.aws.internal.AwsCredentialsCache
import com.magine.http4s.aws.internal.AwsProfile
import com.magine.http4s.aws.internal.AwsSts
import com.magine.http4s.aws.internal.ExpiringCredentials
import com.magine.http4s.aws.internal.IniFile
import com.magine.http4s.aws.internal.Setting.*
import fs2.hashing.Hashing
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.NoSuchFileException
import java.nio.file.Path
import java.time.Instant
import java.time.temporal.ChronoUnit
import org.http4s.Method
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client
import scala.concurrent.duration.*

/**
  * Capability to return [[Credentials]] from one or multiple sources.
  *
  * Following are some use cases along with appropriate sources.
  *
  * - When running a service locally, one typically sets environment
  *   variables read by [[CredentialsProvider.environmentVariables]].
  * - When running command-line applications, one typically requests
  *   temporary security credentials from the Security Token Service
  *   (STS) using the credentials in `~/.aws/credentials`, which can
  *   be read by `CredentialsProvider.credentialsFile`.
  * - When running a service in Elastic Container Service (ECS) or on
  *   Fargate, credentials are provided by a container endpoint, which
  *   can be read by [[CredentialsProvider.containerEndpoint]].
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

object CredentialsProvider {

  /**
    * Returns a new [[CredentialsProvider]] which reads credentials
    * from an HTTP endpoint returning credentials in JSON format.
    *
    * The endpoint which should be queried is determined by either:
    *
    * - reading the `aws.containerCredentialsPath` system property,
    *   or the `AWS_CONTAINER_CREDENTIALS_RELATIVE_URI` environment
    *   variable, to get a relative path. This is then combined with
    *   the `aws.containerServiceEndpoint` system property, or the
    *   `AWS_CONTAINER_SERVICE_ENDPOINT` environment variable (with
    *   `http://169.254.170.2` as default), to form a full URI, or by
    * - reading the `aws.containerCredentialsFullUri` system property,
    *   or the `AWS_CONTAINER_CREDENTIALS_FULL_URI` environment variable,
    *   to get the full URI.
    *
    * If the `aws.containerAuthorizationToken` system property, or the
    * `AWS_CONTAINER_AUTHORIZATION_TOKEN` environment variable is set,
    * the contents will be passed as the value of the `Authorization`
    * header when querying the endpoint.
    *
    * GET requests will be issued to the endpoint and the endpoint is
    * expected to return JSON data in the following format.
    *
    * {{{
    * {
    *   "AccessKeyId": "ACCESS_KEY_ID",
    *   "Expiration": "EXPIRATION_DATE",
    *   "SecretAccessKey": "SECRET_ACCESS_KEY",
    *   "Token": "SECURITY_TOKEN_STRING"
    * }
    * }}}
    *
    * When the provider is first initialized, an attempt to request
    * credentials will be made. Additionally, a background resource
    * is started which ensures non-expired credentials are always
    * available, as long as the endpoint keeps returning new
    * credentials with later expiration values.
    *
    * Credentials are refreshed in the background as follows.
    *
    * - If there are no credentials available, keep checking for
    *   credentials every 10 seconds.
    * - If credentials are available, refresh 15 minutes before
    *   the expiry time of the credentials, or 1 hour from the
    *   last update attempt, whichever happens first. Regardless,
    *   there is a minimum time of 10 seconds between refreshes.
    *
    * If the provider fails to refresh credentials, but still have
    * active credentials available, it will continue to return the
    * active credentials while trying to refresh, until the
    * credentials expire in 1 minute or less.
    */
  def containerEndpoint[F[_]: Async](client: Client[F]): Resource[F, CredentialsProvider[F]] = {
    def credentialsUri: F[Uri] =
      for {
        path <- ContainerCredentialsRelativeUri[F].read
        endpoint <- ContainerServiceEndpoint[F].readOrDefault
        uri <- path match {
          case Some(path) =>
            Uri.fromString(endpoint ++ path).liftTo[F]
          case None =>
            ContainerCredentialsFullUri[F].read
              .map(_.toRight(MissingCredentials()))
              .rethrow
        }
      } yield uri

    def requestCredentials: F[ExpiringCredentials] =
      for {
        uri <- credentialsUri
        authorization <- ContainerAuthorizationToken[F].read
        request = Request[F](Method.GET, uri).withHeaders(authorization.toList)
        expiring <- client.expect[ExpiringCredentials](request)
      } yield expiring

    def sleepUntilRefresh(ref: Ref[F, Either[Throwable, ExpiringCredentials]]): F[Unit] =
      for {
        credentials <- ref.get
        now <- Async[F].realTimeInstant
        refreshAt = Ordering[Instant].min(
          now.plus(1, ChronoUnit.HOURS),
          credentials.fold(_ => now, _.expiresAt.minus(15, ChronoUnit.MINUTES))
        )
        untilRefreshAt = Math.max(now.until(refreshAt, ChronoUnit.SECONDS), 10L).seconds
        _ <- Async[F].sleep(untilRefreshAt)
      } yield ()

    def refreshCredentials(ref: Ref[F, Either[Throwable, ExpiringCredentials]]): F[Unit] =
      for {
        _ <- sleepUntilRefresh(ref)
        credentials <- requestCredentials.attempt
        now <- Async[F].realTimeInstant
        _ <- ref.update {
          case right @ Right(existing) if credentials.isLeft && existing.isFresh(now) => right
          case _ => credentials
        }
      } yield ()

    for {
      credentials <- requestCredentials.attempt.toResource
      ref <- Ref[F].of(credentials).toResource
      _ <- refreshCredentials(ref).foreverM[Unit].background
    } yield new CredentialsProvider[F] {
      override def credentials: F[Credentials] =
        ref.get.rethrow.map(_.credentials)
    }
  }

  /**
    * Returns a new [[CredentialsProvider]] which reads credentials
    * from `~/.aws/credentials` for a profile.
    *
    * The location of the credentials file can be set using either:
    *
    * - the `aws.sharedCredentialsFile` system property, or
    * - the `AWS_SHARED_CREDENTIALS_FILE` environment variable, or
    * - it will default to `~/.aws/credentials` if neither is set.
    *
    * The name of the profile can be set using either:
    *
    * - the `aws.profile` system property, or
    * - the `AWS_PROFILE` environment variable, or
    * - it will default to `default` if neither is set.
    *
    * Once credentials have successfully been read, the credentials
    * are cached indefinitely. This means subsequent updates to the
    * location, profile, or file will not be taken into account.
    */
  def credentialsFile[F[_]: Sync]: F[CredentialsProvider[F]] =
    Profile.readOrDefault.flatMap(credentialsFile(_))

    /**
      * Returns a new [[CredentialsProvider]] which reads credentials
      * from `~/.aws/credentials` for the specified profile.
      *
      * The location of the credentials file can be set using either:
      *
      * - the `aws.sharedCredentialsFile` system property, or
      * - the `AWS_SHARED_CREDENTIALS_FILE` environment variable, or
      * - it will default to `~/.aws/credentials` if neither is set.
      *
      * Once credentials have successfully been read, the credentials
      * are cached indefinitely. This means subsequent updates to the
      * location or file will not be taken into account.
      */
  def credentialsFile[F[_]: Sync](profileName: AwsProfileName): F[CredentialsProvider[F]] =
    Ref[F].of(Option.empty[Credentials]).map { ref =>
      new CredentialsProvider[F] {
        override val credentials: F[Credentials] =
          ref.get.flatMap {
            case Some(credentials) =>
              credentials.pure
            case None =>
              for {
                credentialsFilePath <- SharedCredentialsFile.read
                  .map(_.toRight(MissingCredentials()))
                  .rethrow
                credentialsFile <- readIniFile(credentialsFilePath)
                accessKeyId <- credentialsFile
                  .read(profileName.value, "aws_access_key_id")
                  .map(Credentials.AccessKeyId(_))
                  .toRight(MissingCredentials())
                  .liftTo
                secretAccessKey <- credentialsFile
                  .read(profileName.value, "aws_secret_access_key")
                  .map(Credentials.SecretAccessKey(_))
                  .toRight(MissingCredentials())
                  .liftTo
                sessionToken <- credentialsFile
                  .read(profileName.value, "aws_session_token")
                  .map(Credentials.SessionToken(_))
                  .pure
                credentials <- Credentials(accessKeyId, secretAccessKey, sessionToken).pure
                _ <- ref.set(credentials.some)
              } yield credentials
          }

        private def readIniFile(path: Path): F[IniFile] =
          Sync[F]
            .blocking(new String(Files.readAllBytes(path), UTF_8))
            .adaptError { case _: NoSuchFileException => MissingCredentials() }
            .flatMap(IniFile.parse(_).leftMap(failedToParse(path, _)).liftTo[F])

        private def failedToParse(path: Path, error: Parser.Error): Throwable =
          new RuntimeException(s"Failed to parse credentials file at $path: ${error.show}")
      }
    }

  /**
    * Returns a new [[CredentialsProvider]] which reads credentials
    * using one of multiple default providers.
    *
    * The first provider in the following list which is able to
    * successfully provide credentials will be used.
    *
    * - [[CredentialsProvider.systemProperties]] for system properties,
    * - [[CredentialsProvider.environmentVariables]] for environment variables,
    * - `CredentialsProvider.credentialsFile` for a shared credentials file,
    * - [[CredentialsProvider.containerEndpoint]] for a container endpoint.
    *
    * Subsequent credential requests will continue to use the first
    * successful provider in the above list, without attempting to
    * use other providers.
    */
  def default[F[_]: Async](client: Client[F]): Resource[F, CredentialsProvider[F]] =
    for {
      credentialsFile <- CredentialsProvider.credentialsFile.toResource
      containerEndpoint <- CredentialsProvider.containerEndpoint(client)
      firstSuccessfulProvider <- Ref[F].of(Option.empty[CredentialsProvider[F]]).toResource
      providers = List(systemProperties, environmentVariables, credentialsFile, containerEndpoint)
    } yield new CredentialsProvider[F] {
      override def credentials: F[Credentials] =
        firstSuccessfulProvider.get.flatMap {
          case Some(firstSuccessfulProvider) =>
            firstSuccessfulProvider.credentials
          case None =>
            providers
              .collectFirstSomeM { provider =>
                provider.credentials
                  .tupleLeft(provider)
                  .map(_.some)
                  .recover { case _: MissingCredentials => None }
              }
              .flatMap {
                case Some((provider, credentials)) =>
                  firstSuccessfulProvider.set(provider.some).as(credentials)
                case None =>
                  Async[F].raiseError(MissingCredentials())
              }
        }
    }

  /**
    * Returns a new [[CredentialsProvider]] which reads credentials using the
    * `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, and `AWS_SESSION_TOKEN`
    * environment variables.
    */
  def environmentVariables[F[_]: Sync]: CredentialsProvider[F] =
    new CredentialsProvider[F] {
      override def credentials: F[Credentials] =
        for {
          accessKeyId <- AccessKeyId.env.map(_.toRight(MissingCredentials())).rethrow
          secretAccessKey <- SecretAccessKey.env.map(_.toRight(MissingCredentials())).rethrow
          sessionToken <- SessionToken.env
        } yield Credentials(accessKeyId, secretAccessKey, sessionToken)
    }

  def securityTokenService[F[_]: Async: Hashing](
    client: Client[F]
  ): F[CredentialsProvider[F]] =
    Profile.readOrDefault.flatMap(securityTokenService(client, _))

  def securityTokenService[F[_]: Async: Hashing](
    client: Client[F],
    profileName: AwsProfileName
  ): F[CredentialsProvider[F]] =
    securityTokenService(client, profileName, TokenCodeProvider.default)

  def securityTokenService[F[_]: Async: Hashing](
    client: Client[F],
    profileName: AwsProfileName,
    tokenCodeProvider: TokenCodeProvider[F]
  ): F[CredentialsProvider[F]] =
    for {
      profile <- AwsConfig.default.read(profileName)
      region <- profile.resolveRegion
      provider <- CredentialsProvider.credentialsFile(profile.sourceProfile)
      securityTokenService <- securityTokenService(
        profile = profile,
        tokenCodeProvider = tokenCodeProvider,
        credentialsCache = AwsCredentialsCache.default[F],
        sts = AwsSts.fromClient(client, provider, region)
      )
    } yield securityTokenService

  private[aws] def securityTokenService[F[_]](
    profile: AwsProfile,
    tokenCodeProvider: TokenCodeProvider[F],
    credentialsCache: AwsCredentialsCache[F],
    sts: AwsSts[F]
  )(
    implicit F: Temporal[F]
  ): F[CredentialsProvider[F]] = {
    type Result = Either[Throwable, AwsAssumedRole]

    sealed trait State
    case class Cached(assumedRole: Option[AwsAssumedRole]) extends State
    case class Renewing(deferred: Deferred[F, Result]) extends State

    for {
      assumedRole <- credentialsCache.read(profile)
      ref <- Ref.of[F, State](Cached(assumedRole))
    } yield new CredentialsProvider[F] {
      sealed trait Action {
        def run(poll: Poll[F]): F[Credentials]
      }

      case class Renew(deferred: Deferred[F, Result]) extends Action {
        private def complete(outcome: Outcome[F, Throwable, AwsAssumedRole]): F[Unit] =
          for {
            result <- outcome.embedError.attempt
            _ <- deferred.complete(result)
            _ <- ref.update(_ => Cached(result.toOption))
          } yield ()

        private def assumeRole: F[AwsAssumedRole] =
          for {
            tokenCode <- tokenCodeProvider.tokenCode(profile.mfaSerial)
            assumedRole <- sts.assumeRole(
              roleArn = profile.roleArn,
              roleSessionName = profile.roleSessionName,
              durationSeconds = profile.durationSeconds,
              mfaSerial = profile.mfaSerial,
              tokenCode = tokenCode
            )
            _ <- credentialsCache.write(assumedRole)
          } yield assumedRole

        override def run(poll: Poll[F]): F[Credentials] =
          poll(assumeRole).guaranteeCase(complete).map(_.credentials)
      }

      case class Return(credentials: Credentials) extends Action {
        override def run(poll: Poll[F]): F[Credentials] =
          poll(credentials.pure)
      }

      case class Wait(deferred: Deferred[F, Result]) extends Action {
        override def run(poll: Poll[F]): F[Credentials] =
          poll(deferred.get.rethrow.map(_.credentials))
      }

      def action: F[Action] =
        Deferred[F, Result].flatMap { deferred =>
          F.realTimeInstant.flatMap { now =>
            ref.modify {
              case cached @ Cached(Some(assumedRole)) if assumedRole.isFresh(now) =>
                (cached, Return(assumedRole.credentials))
              case Cached(_) => (Renewing(deferred), Renew(deferred))
              case renewing @ Renewing(deferred) => (renewing, Wait(deferred))
            }
          }
        }

      override def credentials: F[Credentials] =
        F.uncancelable(poll => action.flatMap(_.run(poll)))
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

  /**
    * Returns a new [[CredentialsProvider]] which reads credentials using
    * the `aws.accessKeyId`, `aws.secretAccessKey`, and `aws.sessionToken`
    * system properties.
    */
  def systemProperties[F[_]: Sync]: CredentialsProvider[F] =
    new CredentialsProvider[F] {
      override def credentials: F[Credentials] =
        for {
          accessKeyId <- AccessKeyId.prop.map(_.toRight(MissingCredentials())).rethrow
          secretAccessKey <- SecretAccessKey.prop.map(_.toRight(MissingCredentials())).rethrow
          sessionToken <- SessionToken.prop
        } yield Credentials(accessKeyId, secretAccessKey, sessionToken)
    }
}
