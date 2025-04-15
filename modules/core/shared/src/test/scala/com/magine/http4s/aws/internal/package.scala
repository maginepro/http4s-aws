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

import com.magine.aws.Region
import java.time.Instant
import java.time.LocalDate
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

package object internal {
  val canonicalRequestGen: Gen[CanonicalRequest] =
    for {
      httpMethod <- arbitrary[String]
      canonicalUri <- arbitrary[String]
      canonicalQueryString <- arbitrary[String]
      canonicalHeaders <- arbitrary[String]
      signedHeaders <- arbitrary[String]
      hashedPayload <- arbitrary[String]
    } yield CanonicalRequest(
      httpMethod = httpMethod,
      canonicalUri = canonicalUri,
      canonicalQueryString = canonicalQueryString,
      canonicalHeaders = canonicalHeaders,
      signedHeaders = signedHeaders,
      hashedPayload = hashedPayload
    )

  implicit val canonicalRequestArbitrary: Arbitrary[CanonicalRequest] =
    Arbitrary(canonicalRequestGen)

  val credentialScopeGen: Gen[CredentialScope] =
    for {
      region <- arbitrary[Region]
      requestDate <- arbitrary[RequestDate]
      serviceName <- arbitrary[AwsServiceName]
    } yield CredentialScope(
      region = region,
      requestDate = requestDate,
      serviceName = serviceName
    )

  implicit val credentialScopeArbitrary: Arbitrary[CredentialScope] =
    Arbitrary(credentialScopeGen)

  val requestDateGen: Gen[RequestDate] =
    arbitrary[LocalDate].map(RequestDate(_))

  implicit val requestDateArbitrary: Arbitrary[RequestDate] =
    Arbitrary(requestDateGen)

  val requestDateTimeGen: Gen[RequestDateTime] =
    arbitrary[Instant].map(RequestDateTime(_))

  implicit val requestDateTimeArbitrary: Arbitrary[RequestDateTime] =
    Arbitrary(requestDateTimeGen)
}
