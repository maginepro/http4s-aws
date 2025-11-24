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

package com.magine.http4s.aws.headers

import cats.effect.IO
import munit.CatsEffectSuite
import org.http4s.Header
import org.http4s.Request
import org.http4s.TransferCoding
import org.http4s.headers.`Transfer-Encoding`
import org.typelevel.ci.*

final class ContentEncodingSuite extends CatsEffectSuite {
  test("notChunked") {
    val request = Request[IO]()
    check(request, None)
  }

  test("chunked.noEncoding") {
    val request = Request[IO]().putHeaders(
      `Transfer-Encoding`(TransferCoding.chunked)
    )

    check(request, Some("aws-chunked"))
  }

  test("chunked.awsChunkedEncoding") {
    val request = Request[IO]().putHeaders(
      `Content-Encoding`.`aws-chunked`,
      `Transfer-Encoding`(TransferCoding.chunked)
    )

    check(request, Some("aws-chunked"))
  }

  test("chunked.awsChunkedGzipEncoding") {
    val request = Request[IO]().putHeaders(
      Header.Raw(ci"Content-Encoding", "aws-chunked,gzip"),
      `Transfer-Encoding`(TransferCoding.chunked)
    )

    check(request, Some("aws-chunked,gzip"))
  }

  test("chunked.gzipEncoding") {
    val request = Request[IO]().putHeaders(
      Header.Raw(ci"Content-Encoding", "gzip"),
      `Transfer-Encoding`(TransferCoding.chunked)
    )

    check(request, Some("aws-chunked,gzip"))
  }

  private def check[F[_]](
    request: Request[F],
    encoding: Option[String]
  ): Unit =
    assertEquals(
      `Content-Encoding`
        .putIfChunked(request)
        .headers
        .get(ci"Content-Encoding")
        .toList
        .flatMap(_.toList.map(_.value)),
      encoding.toList
    )
}
