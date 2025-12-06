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

import cats.effect.IO
import fs2.Stream
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.http4s.headers.`Content-Type`
import org.scalacheck.effect.PropF

final class S3MultipartUploadSuite
  extends CatsEffectSuite
  with ScalaCheckEffectSuite
  with LocalStackFixture
  with Generators {

  test("multipartUpload") {
    PropF.forAllNoShrinkF {
      (
        bucket: S3Bucket,
        key: S3Key,
        bytes: Stream[IO, Byte]
      ) =>
        Stream
          .eval(localStack)
          .evalTap(_.createBucket(bucket))
          .flatMap(_.multipartUpload.uploadTo(bucket, key)(bytes))
          .compile
          .drain
    }
  }

  test("multipartUpload.contentType") {
    PropF.forAllNoShrinkF {
      (
        bucket: S3Bucket,
        key: S3Key,
        contentType: `Content-Type`,
        bytes: Stream[IO, Byte]
      ) =>
        Stream
          .eval(localStack)
          .evalTap(_.createBucket(bucket))
          .evalTap(localStack =>
            bytes
              .through(
                localStack.multipartUpload
                  .withContentType(contentType)
                  .uploadTo(bucket, key)
              )
              .compile
              .drain
          )
          .evalMap(_.getContentType(bucket, key))
          .compile
          .onlyOrError
          .assertEquals(Some(contentType))

    }
  }
}
