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

package com.magine.http4s

import cats.effect.SyncIO
import com.magine.aws.Region
import com.magine.http4s.aws.internal.AwsAssumedRole
import com.magine.http4s.aws.internal.AwsCredentialsCache
import com.magine.http4s.aws.internal.AwsProfile
import com.magine.http4s.aws.internal.AwsSsoAccountId
import com.magine.http4s.aws.internal.AwsSsoRoleName
import com.magine.http4s.aws.internal.AwsSsoSessionName
import com.magine.http4s.aws.internal.AwsStsProfileResolved
import java.time.Instant
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

package object aws {
  val awsProfileRoleArnGen: Gen[AwsProfile.RoleArn] =
    arbitrary[String].map(AwsProfile.RoleArn(_))

  implicit val awsProfileRoleArnArbitrary: Arbitrary[AwsProfile.RoleArn] =
    Arbitrary(awsProfileRoleArnGen)

  val awsProfileRoleSessionNameGen: Gen[AwsProfile.RoleSessionName] =
    arbitrary[String].map(AwsProfile.RoleSessionName(_))

  implicit val awsProfileRoleSessionNameArbitrary: Arbitrary[AwsProfile.RoleSessionName] =
    Arbitrary(awsProfileRoleSessionNameGen)

  val awsProfileDurationSecondsGen: Gen[AwsProfile.DurationSeconds] =
    arbitrary[Int].map(AwsProfile.DurationSeconds(_))

  implicit val awsProfileDurationSecondsArbitrary: Arbitrary[AwsProfile.DurationSeconds] =
    Arbitrary(awsProfileDurationSecondsGen)

  val mfaSerialGen: Gen[MfaSerial] =
    arbitrary[String].map(MfaSerial(_))

  implicit val mfaSerialArbitrary: Arbitrary[MfaSerial] =
    Arbitrary(mfaSerialGen)

  val regionGen: Gen[Region] =
    Gen.oneOf(Region.values)

  implicit val regionArbitrary: Arbitrary[Region] =
    Arbitrary(regionGen)

  val awsProfileNameGen: Gen[AwsProfileName] =
    arbitrary[String].map(AwsProfileName(_))

  implicit val awsProfileNameArbitrary: Arbitrary[AwsProfileName] =
    Arbitrary(awsProfileNameGen)

  val awsSsoAccountIdGen: Gen[AwsSsoAccountId] =
    arbitrary[String].map(AwsSsoAccountId(_))

  implicit val awsSsoAccountIdArbitrary: Arbitrary[AwsSsoAccountId] =
    Arbitrary(awsSsoAccountIdGen)

  val awsSsoRoleNameGen: Gen[AwsSsoRoleName] =
    arbitrary[String].map(AwsSsoRoleName(_))

  implicit val awsSsoRoleNameArbitrary: Arbitrary[AwsSsoRoleName] =
    Arbitrary(awsSsoRoleNameGen)

  val awsSsoSessionNameGen: Gen[AwsSsoSessionName] =
    arbitrary[String].map(AwsSsoSessionName(_))

  implicit val awsSsoSessionNameArbitrary: Arbitrary[AwsSsoSessionName] =
    Arbitrary(awsSsoSessionNameGen)

  val awsProfileGen: Gen[AwsProfile] =
    for {
      profileName <- arbitrary[AwsProfileName]
      roleArn <- arbitrary[Option[AwsProfile.RoleArn]]
      roleSessionName <- arbitrary[Option[AwsProfile.RoleSessionName]]
      durationSeconds <- arbitrary[Option[AwsProfile.DurationSeconds]]
      sourceProfile <- arbitrary[Option[AwsProfileName]]
      mfaSerial <- arbitrary[Option[MfaSerial]]
      region <- arbitrary[Option[Region]]
      ssoAccountId <- arbitrary[Option[AwsSsoAccountId]]
      ssoRoleName <- arbitrary[Option[AwsSsoRoleName]]
      ssoSessionName <- arbitrary[Option[AwsSsoSessionName]]
    } yield AwsProfile(
      profileName = profileName,
      roleArn = roleArn,
      roleSessionName = roleSessionName,
      durationSeconds = durationSeconds,
      sourceProfile = sourceProfile,
      mfaSerial = mfaSerial,
      region = region,
      ssoAccountId = ssoAccountId,
      ssoRoleName = ssoRoleName,
      ssoSessionName = ssoSessionName
    )

  implicit val awsProfileArbitrary: Arbitrary[AwsProfile] =
    Arbitrary(awsProfileGen)

  val awsStsProfileResolvedGen: Gen[AwsStsProfileResolved] =
    for {
      profileName <- arbitrary[AwsProfileName]
      roleArn <- arbitrary[AwsProfile.RoleArn]
      roleSessionName <- arbitrary[AwsProfile.RoleSessionName]
      durationSeconds <- arbitrary[Option[AwsProfile.DurationSeconds]]
      sourceProfile <- arbitrary[AwsProfileName]
      mfaSerial <- arbitrary[MfaSerial]
      region <- arbitrary[Region]
    } yield AwsStsProfileResolved(
      profileName = profileName,
      roleArn = roleArn,
      roleSessionName = roleSessionName,
      durationSeconds = durationSeconds,
      sourceProfile = sourceProfile,
      mfaSerial = mfaSerial,
      region = region
    )

  implicit val awsStsProfileResolvedArbitrary: Arbitrary[AwsStsProfileResolved] =
    Arbitrary(awsStsProfileResolvedGen)

  val awsServiceNameGen: Gen[AwsServiceName] =
    arbitrary[String].map(AwsServiceName(_))

  implicit val awsServiceNameArbitrary: Arbitrary[AwsServiceName] =
    Arbitrary(awsServiceNameGen)

  val credentialsAccessKeyIdGen: Gen[Credentials.AccessKeyId] =
    arbitrary[String].map(Credentials.AccessKeyId(_))

  implicit val credentialsAccessKeyIdArbtirary: Arbitrary[Credentials.AccessKeyId] =
    Arbitrary(credentialsAccessKeyIdGen)

  val credentialsSecretAccessKeyGen: Gen[Credentials.SecretAccessKey] =
    arbitrary[String].map(Credentials.SecretAccessKey(_))

  implicit val credentialsSecretAccessKeyArbitrary: Arbitrary[Credentials.SecretAccessKey] =
    Arbitrary(credentialsSecretAccessKeyGen)

  val credentialsSessionTokenGen: Gen[Credentials.SessionToken] =
    arbitrary[String].map(Credentials.SessionToken(_))

  implicit val credentialsSessionTokenArbitrary: Arbitrary[Credentials.SessionToken] =
    Arbitrary(credentialsSessionTokenGen)

  val credentialsGen: Gen[Credentials] =
    for {
      accessKeyId <- arbitrary[Credentials.AccessKeyId]
      secretAccessKey <- arbitrary[Credentials.SecretAccessKey]
      sessionToken <- arbitrary[Option[Credentials.SessionToken]]
    } yield Credentials(
      accessKeyId = accessKeyId,
      secretAccessKey = secretAccessKey,
      sessionToken = sessionToken
    )

  implicit val credentialsArbitrary: Arbitrary[Credentials] =
    Arbitrary(credentialsGen)

  /* The effect here is to capture `Hashing` possibly being unavailable. */
  def awsCredentialsCacheFileName(profile: AwsStsProfileResolved): AwsCredentialsCache.FileName =
    AwsCredentialsCache.FileName.fromStsProfile[SyncIO](profile).unsafeRunSync()

  val awsCredentialsCacheFileNameGen: Gen[AwsCredentialsCache.FileName] =
    arbitrary[AwsStsProfileResolved].flatMap(awsCredentialsCacheFileName(_))

  implicit val awsCredentialsCacheFileNameArbitrary: Arbitrary[AwsCredentialsCache.FileName] =
    Arbitrary(awsCredentialsCacheFileNameGen)

  val awsAssumedRoleAssumedRoleIdGen: Gen[AwsAssumedRole.AssumedRoleId] =
    arbitrary[String].map(AwsAssumedRole.AssumedRoleId(_))

  implicit val awsAssumedRoleAssumedRoleIdArbitrary: Arbitrary[AwsAssumedRole.AssumedRoleId] =
    Arbitrary(awsAssumedRoleAssumedRoleIdGen)

  val awsAssumedRoleAssumedRoleArnGen: Gen[AwsAssumedRole.AssumedRoleArn] =
    arbitrary[String].map(AwsAssumedRole.AssumedRoleArn(_))

  implicit val awsAssumedRoleAssumedRoleArnArbitrary: Arbitrary[AwsAssumedRole.AssumedRoleArn] =
    Arbitrary(awsAssumedRoleAssumedRoleArnGen)

  def awsAssumedRoleGen(expiration: Instant, profile: AwsStsProfileResolved): Gen[AwsAssumedRole] =
    for {
      credentials <- arbitrary[Credentials]
      cacheFileName = awsCredentialsCacheFileName(profile)
      assumedRoleId <- arbitrary[AwsAssumedRole.AssumedRoleId]
      assumedRoleArn <- arbitrary[AwsAssumedRole.AssumedRoleArn]
    } yield AwsAssumedRole(
      credentials = credentials,
      expiration = expiration,
      cacheFileName = cacheFileName,
      assumedRoleId = assumedRoleId,
      assumedRoleArn = assumedRoleArn
    )

  /** AwsAssumedRole#isFresh will be `true` at epoch start. */
  def awsAssumedRoleFreshGen(profile: AwsStsProfileResolved): Gen[AwsAssumedRole] =
    for {
      expiration <- Gen
        .chooseNum(61L, Instant.MAX.getEpochSecond)
        .map(Instant.ofEpochSecond(_))
      assumedRole <- awsAssumedRoleGen(expiration, profile)
    } yield assumedRole

  /** AwsAssumedRole#isFresh will be `false` at epoch start but credentials active. */
  def awsAssumedRoleStaleGen(profile: AwsStsProfileResolved): Gen[AwsAssumedRole] =
    for {
      expiration <- Gen
        .chooseNum(1L, 60L)
        .map(Instant.ofEpochSecond(_))
      assumedRole <- awsAssumedRoleGen(expiration, profile)
    } yield assumedRole

  /** AwsAssumedRole#isFresh will be `false` at epoch start and credentials expired. */
  def awsAssumedRoleExpiredGen(profile: AwsStsProfileResolved): Gen[AwsAssumedRole] =
    for {
      expiration <- Gen
        .chooseNum(Instant.MIN.getEpochSecond, 0L)
        .map(Instant.ofEpochSecond(_))
      assumedRole <- awsAssumedRoleGen(expiration, profile)
    } yield assumedRole

  val awsAssumedRoleGen: Gen[AwsAssumedRole] =
    for {
      expiration <- arbitrary[Instant]
      profile <- arbitrary[AwsStsProfileResolved]
      assumedRole <- awsAssumedRoleGen(expiration, profile)
    } yield assumedRole

  implicit val awsAssumedRoleArbitrary: Arbitrary[AwsAssumedRole] =
    Arbitrary(awsAssumedRoleGen)

  val tokenCodeGen: Gen[TokenCode] =
    for {
      digits <- Gen.listOfN(6, Gen.numChar).map(_.mkString)
      tokenCode <- TokenCode(digits).map(Gen.const).getOrElse(Gen.fail)
    } yield tokenCode

  implicit val tokenCodeArbitrary: Arbitrary[TokenCode] =
    Arbitrary(tokenCodeGen)
}
