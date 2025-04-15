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

import cats.effect.IO
import com.magine.aws.Region
import com.magine.http4s.aws.AwsServiceName
import com.magine.http4s.aws.Credentials
import com.magine.http4s.aws.internal.Signature.Legacy.algorithm
import fs2.Chunk
import java.time.LocalDate
import javax.crypto.spec.SecretKeySpec
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.effect.PropF

final class SignatureSuite extends CatsEffectSuite with ScalaCheckEffectSuite {
  test("Signature.sign consistent with legacy") {
    val gen =
      for {
        key <- arbitrary[Array[Byte]]
        if key.nonEmpty
        bytes <- arbitrary[Array[Byte]]
      } yield (key, bytes)

    PropF.forAllF(gen) { case (key, bytes) =>
      for {
        actual <- Signature.sign[IO](Chunk.array(key), Chunk.array(bytes))
        expected = Signature.Legacy.sign(new SecretKeySpec(key, algorithm), bytes)
        _ <- IO(assertEquals(actual, expected))
      } yield ()
    }
  }

  test("Signature.signingKey consistent with legacy") {
    val gen =
      for {
        region <- Gen.oneOf(Gen.oneOf(Region.values), arbitrary[String].map(Region(_)))
        requestDate <- arbitrary[LocalDate].map(RequestDate(_))
        secretAccessKey <- arbitrary[String].map(Credentials.SecretAccessKey(_))
        serviceName <- arbitrary[String].map(AwsServiceName(_))
      } yield (region, requestDate, secretAccessKey, serviceName)

    PropF.forAllF(gen) { case (region, requestDate, secretAccessKey, serviceName) =>
      for {
        actual <- Signature.signingKey[IO](region, requestDate, secretAccessKey, serviceName)
        expected = Signature.Legacy.signingKey(region, requestDate, secretAccessKey, serviceName)
        _ <- IO(assertEquals(actual, Chunk.array(expected.getEncoded)))
      } yield ()
    }
  }
}
