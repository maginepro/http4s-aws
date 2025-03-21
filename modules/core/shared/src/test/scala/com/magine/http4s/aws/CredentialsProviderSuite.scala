package com.magine.http4s.aws

import cats.effect.IO
import cats.effect.Ref
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import com.magine.http4s.aws.internal.AwsAssumedRole
import com.magine.http4s.aws.internal.AwsCredentialsCache
import com.magine.http4s.aws.internal.AwsProfile.DurationSeconds
import com.magine.http4s.aws.internal.AwsProfile.RoleArn
import com.magine.http4s.aws.internal.AwsProfile.RoleSessionName
import com.magine.http4s.aws.internal.AwsSts
import java.util.concurrent.CancellationException
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import scala.util.control.NoStackTrace

final class CredentialsProviderSuite extends CatsEffectSuite with ScalaCheckEffectSuite {
  test("securityTokenService.cacheEmpty") {
    val gen =
      for {
        profile <- awsProfileGen
        tokenCode <- tokenCodeGen
        assumedRoleFresh <- awsAssumedRoleFreshGen(profile)
      } yield (profile, tokenCode, assumedRoleFresh)

    PropF.forAllNoShrinkF(gen) {
      case (
            profile,
            tokenCode,
            assumedRoleFresh
          ) =>
        val test =
          TestControl.executeEmbed {
            for {
              credentialsCache <- AwsCredentialsCache.empty[IO]
              provider <- CredentialsProvider.securityTokenService[IO](
                profile = profile,
                tokenCodeProvider = tokenCodeProvider(tokenCode),
                credentialsCache = credentialsCache,
                sts = sts(assumedRoleFresh)
              )
              credentials <- provider.credentials
              cached <- credentialsCache.read(profile)
              _ <- IO(assertEquals(cached, assumedRoleFresh.some))
            } yield credentials
          }

        test.assertEquals(assumedRoleFresh.credentials)
    }
  }

  test("securityTokenService.cacheFresh") {
    val gen =
      for {
        profile <- awsProfileGen
        assumedRoleFresh <- awsAssumedRoleFreshGen(profile)
      } yield (profile, assumedRoleFresh)

    PropF.forAllNoShrinkF(gen) { case (profile, assumedRoleFresh) =>
      val test =
        TestControl.executeEmbed {
          for {
            credentialsCache <- AwsCredentialsCache.one[IO](profile, assumedRoleFresh)
            tokenCodeProvider <- tokenCodeProviderError
            provider <- CredentialsProvider.securityTokenService[IO](
              profile = profile,
              tokenCodeProvider = tokenCodeProvider,
              credentialsCache = credentialsCache,
              sts = stsError
            )
            credentials <- provider.credentials
            cached <- credentialsCache.read(profile)
            _ <- IO(assertEquals(cached, assumedRoleFresh.some))
          } yield credentials
        }

      test.assertEquals(assumedRoleFresh.credentials)
    }
  }

  test("securityTokenService.cacheStale") {
    val gen =
      for {
        profile <- awsProfileGen
        tokenCode <- tokenCodeGen
        assumedRoleStale <- Gen.oneOf(
          awsAssumedRoleStaleGen(profile),
          awsAssumedRoleExpiredGen(profile)
        )
        assumedRoleFresh <- awsAssumedRoleFreshGen(profile)
      } yield (profile, tokenCode, assumedRoleStale, assumedRoleFresh)

    PropF.forAllNoShrinkF(gen) {
      case (
            profile,
            tokenCode,
            assumedRoleStale,
            assumedRoleFresh
          ) =>
        val test =
          TestControl.executeEmbed {
            for {
              credentialsCache <- AwsCredentialsCache.one[IO](profile, assumedRoleStale)
              provider <- CredentialsProvider.securityTokenService[IO](
                profile = profile,
                tokenCodeProvider = tokenCodeProvider(tokenCode),
                credentialsCache = credentialsCache,
                sts = sts(assumedRoleFresh)
              )
              credentials <- provider.credentials
              cached <- credentialsCache.read(profile)
              _ <- IO(assertEquals(cached, assumedRoleFresh.some))
            } yield credentials
          }

        test.assertEquals(assumedRoleFresh.credentials)
    }
  }

  test("securityTokenService.renewCanceled") {
    val gen =
      for {
        profile <- awsProfileGen
        tokenCode <- tokenCodeGen
        assumedRoleStale <- Gen.option(
          Gen.oneOf(
            awsAssumedRoleStaleGen(profile),
            awsAssumedRoleExpiredGen(profile)
          )
        )
      } yield (profile, tokenCode, assumedRoleStale)

    PropF.forAllNoShrinkF(gen) {
      case (
            profile,
            tokenCode,
            assumedRoleStale
          ) =>
        val test =
          TestControl.executeEmbed {
            for {
              credentialsCache <- AwsCredentialsCache.option[IO](profile, assumedRoleStale)
              provider <- CredentialsProvider.securityTokenService[IO](
                profile = profile,
                tokenCodeProvider = tokenCodeProviderCanceled(tokenCode),
                credentialsCache = credentialsCache,
                sts = stsError
              )
              _ <- provider.credentials.start
                .flatMap(_.join)
                .flatMap(_.embedError)
                .intercept[CancellationException]
              _ <- provider.credentials.start
                .flatMap(_.join)
                .flatMap(_.embedError)
                .intercept[CancellationException]
            } yield ()
          }

        test.assert
    }
  }

  test("securityTokenService.renewError") {
    val gen =
      for {
        profile <- awsProfileGen
        assumedRoleStale <- Gen.option(
          Gen.oneOf(
            awsAssumedRoleStaleGen(profile),
            awsAssumedRoleExpiredGen(profile)
          )
        )
      } yield (profile, assumedRoleStale)

    PropF.forAllNoShrinkF(gen) { case (profile, assumedRoleStale) =>
      val test =
        TestControl.executeEmbed {
          for {
            credentialsCache <- AwsCredentialsCache.option[IO](profile, assumedRoleStale)
            tokenCodeProvider <- tokenCodeProviderError
            provider <- CredentialsProvider.securityTokenService[IO](
              profile = profile,
              tokenCodeProvider = tokenCodeProvider,
              credentialsCache = credentialsCache,
              sts = stsError
            )
            first <- provider.credentials.intercept[TokenCodeProviderError]
            _ <- IO(assert(first.count == 1))
            second <- provider.credentials.intercept[TokenCodeProviderError]
            _ <- IO(assert(second.count == 2))
          } yield ()
        }

      test.assert
    }
  }

  test("securityTokenService.renewOnce") {
    val gen =
      for {
        profile <- awsProfileGen
        tokenCode <- tokenCodeGen
        assumedRoleStale <- Gen.option(
          Gen.oneOf(
            awsAssumedRoleStaleGen(profile),
            awsAssumedRoleExpiredGen(profile)
          )
        )
        assumedRoleFresh <- awsAssumedRoleFreshGen(profile)
      } yield (profile, tokenCode, assumedRoleStale, assumedRoleFresh)

    PropF.forAllNoShrinkF(gen) {
      case (
            profile,
            tokenCode,
            assumedRoleStale,
            assumedRoleFresh
          ) =>
        val test =
          TestControl.executeEmbed {
            for {
              credentialsCache <- AwsCredentialsCache.option[IO](profile, assumedRoleStale)
              tokenCodeProvider <- tokenCodeProviderOnce(tokenCode)
              sts <- stsOnce(assumedRoleFresh)
              provider <- CredentialsProvider.securityTokenService[IO](
                profile = profile,
                tokenCodeProvider = tokenCodeProvider,
                credentialsCache = credentialsCache,
                sts = sts
              )
              credentials <- List.fill(100)(provider.credentials).parSequence
              _ <- IO(assert(credentials.forall(_ == assumedRoleFresh.credentials)))
              cached <- credentialsCache.read(profile)
              _ <- IO(assertEquals(cached, assumedRoleFresh.some))
            } yield ()
          }

        test.assert
    }
  }

  case class TokenCodeProviderError(mfaSerial: MfaSerial, count: Int)
    extends RuntimeException
    with NoStackTrace {
    override def getMessage: String = toString
  }

  def tokenCodeProvider(tokenCode: TokenCode): TokenCodeProvider[IO] =
    TokenCodeProvider.static(tokenCode)

  def tokenCodeProviderCanceled(tokenCode: TokenCode): TokenCodeProvider[IO] = {
    val result = tokenCode
    new TokenCodeProvider[IO] {
      override def tokenCode(mfaSerial: MfaSerial): IO[TokenCode] =
        IO.canceled.as(result)
    }
  }

  def tokenCodeProviderError: IO[TokenCodeProvider[IO]] =
    Ref[IO].of(0).map { ref =>
      new TokenCodeProvider[IO] {
        override def tokenCode(mfaSerial: MfaSerial): IO[TokenCode] =
          ref
            .updateAndGet(_ + 1)
            .map(TokenCodeProviderError(mfaSerial, _))
            .flatMap(IO.raiseError)
      }
    }

  def tokenCodeProviderOnce(tokenCode: TokenCode): IO[TokenCodeProvider[IO]] = {
    val result = tokenCode
    Ref[IO].of(0).map { ref =>
      new TokenCodeProvider[IO] {
        override def tokenCode(mfaSerial: MfaSerial): IO[TokenCode] =
          ref.updateAndGet(_ + 1).flatMap {
            case 1 => IO.pure(result)
            case count => IO.raiseError(TokenCodeProviderError(mfaSerial, count))
          }
      }
    }
  }

  def sts(assumedRole: AwsAssumedRole): AwsSts[IO] =
    new AwsSts[IO] {
      override def assumeRole(
        roleArn: RoleArn,
        roleSessionName: RoleSessionName,
        durationSeconds: Option[DurationSeconds],
        mfaSerial: MfaSerial,
        tokenCode: TokenCode
      ): IO[AwsAssumedRole] =
        IO.pure(assumedRole)
    }

  def stsError: AwsSts[IO] =
    new AwsSts[IO] {
      override def assumeRole(
        roleArn: RoleArn,
        roleSessionName: RoleSessionName,
        durationSeconds: Option[DurationSeconds],
        mfaSerial: MfaSerial,
        tokenCode: TokenCode
      ): IO[AwsAssumedRole] =
        IO.raiseError(
          new RuntimeException(
            s"AwsSts#assumeRole($roleArn, $roleSessionName, $durationSeconds, $mfaSerial, $tokenCode)"
          )
        )
    }

  def stsOnce(assumedRole: AwsAssumedRole): IO[AwsSts[IO]] =
    Ref[IO].of(IO.pure(assumedRole)).map { ref =>
      new AwsSts[IO] {
        override def assumeRole(
          roleArn: RoleArn,
          roleSessionName: RoleSessionName,
          durationSeconds: Option[DurationSeconds],
          mfaSerial: MfaSerial,
          tokenCode: TokenCode
        ): IO[AwsAssumedRole] =
          ref
            .getAndSet(
              stsError.assumeRole(
                roleArn = roleArn,
                roleSessionName = roleSessionName,
                durationSeconds = durationSeconds,
                mfaSerial = mfaSerial,
                tokenCode = tokenCode
              )
            )
            .flatten
      }
    }
}
