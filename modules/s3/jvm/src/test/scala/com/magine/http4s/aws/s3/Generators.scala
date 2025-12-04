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

package com.magine.http4s.aws.s3

import fs2.Chunk
import fs2.Stream
import org.http4s.Uri.Path
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

trait Generators {
  implicit def byteStreamArbitrary[F[_]]: Arbitrary[Stream[F, Byte]] =
    Arbitrary(
      arbitrary[Array[Byte]]
        .filter(_.nonEmpty)
        .map(Chunk.array(_))
        .map(Stream.chunk(_))
    )

  val s3BucketGen: Gen[S3Bucket] =
    for {
      length <- Gen.chooseNum(3, 63)
      name <- Gen
        .listOfN(
          length,
          Gen.frequency(
            9 -> Gen.oneOf(Gen.alphaLowerChar, Gen.numChar),
            1 -> Gen.oneOf('.', '-')
          )
        )
        .map(_.mkString)
      // LocalStack is too strict in rejecting IP-address-like names
      if !name.forall(c => c.isDigit || c == '.')
      bucket <- S3Bucket(name).map(Gen.const).getOrElse(Gen.fail)
    } yield bucket

  implicit val s3BucketArbitrary: Arbitrary[S3Bucket] =
    Arbitrary(s3BucketGen)

  val s3KeyGen: Gen[S3Key] =
    for {
      length <- Gen.chooseNum(1, 1024)
      chars <- Gen.listOfN(length, Gen.alphaNumChar)
      path = Path.unsafeFromString(chars.mkString)
      key <- S3Key(path).map(Gen.const).getOrElse(Gen.fail)
    } yield key

  implicit val s3KeyArbitrary: Arbitrary[S3Key] =
    Arbitrary(s3KeyGen)
}
