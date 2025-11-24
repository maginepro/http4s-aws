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

import cats.effect.IO
import cats.syntax.all.*
import com.magine.aws.Region
import com.magine.http4s.aws.Credentials.AccessKeyId
import com.magine.http4s.aws.Credentials.SecretAccessKey
import com.magine.http4s.aws.headers.`Content-Encoding`
import com.magine.http4s.aws.headers.`X-Amz-Content-SHA256`
import com.magine.http4s.aws.headers.`X-Amz-Date`
import com.magine.http4s.aws.headers.`X-Amz-Decoded-Content-Length`
import com.magine.http4s.aws.headers.`X-Amz-Storage-Class`
import fs2.Chunk
import fs2.Stream
import fs2.text.utf8
import java.time.Instant
import munit.CatsEffectSuite
import org.http4s.Method
import org.http4s.Request
import org.http4s.headers.Host
import org.http4s.headers.`Content-Length`
import org.http4s.syntax.all.*
import org.typelevel.ci.*

final class AwsSigningSuite extends CatsEffectSuite {

  /**
    * Chunked S3 PUT Object example from the documentation.
    *
    * https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-streaming.html#example-signature-calculations-streaming
    */
  test("s3.putObject.chunked.example") {
    val request =
      Request[IO](
        Method.PUT,
        uri"https://s3.amazonaws.com/examplebucket/chunkObject.txt"
      ).withHeaders(
        `Content-Encoding`.`aws-chunked`,
        `Content-Length`(66824),
        Host("s3.amazonaws.com"),
        `X-Amz-Content-SHA256`.`STREAMING-AWS4-HMAC-SHA256-PAYLOAD`,
        `X-Amz-Date`(Instant.parse("2013-05-24T00:00:00Z")),
        `X-Amz-Decoded-Content-Length`(66560),
        `X-Amz-Storage-Class`.REDUCED_REDUNDANCY
      ).withBodyStream(
        Stream.chunk(Chunk.array(Array.fill(65536)('a'.toByte))) ++
          Stream.chunk(Chunk.array(Array.fill(1024)('a'.toByte)))
      )

    val signing =
      AwsSigning[IO](
        CredentialsProvider.static(
          Credentials(
            AccessKeyId("AKIAIOSFODNN7EXAMPLE"),
            SecretAccessKey("wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"),
            sessionToken = None
          )
        ),
        Region.US_EAST_1,
        AwsServiceName.S3
      )

    val expectedAuthorization: Option[String] =
      "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request,SignedHeaders=content-encoding;content-length;host;x-amz-content-sha256;x-amz-date;x-amz-decoded-content-length;x-amz-storage-class,Signature=4f232c4386841ef735655705268965c44a0e4690baa4adea153f7db9fa80a0a9".some

    val expectedBody: String =
      List(
        "10000;chunk-signature=ad80c730a21e5b8d04586a2213dd63b9a0e99e0e2307b0ade35a65485a288648\r\n",
        s"${"a" * 65536}\r\n",
        "400;chunk-signature=0055627c9e194cb4542bae2aa5492e3c1575bbb81b612b7d234b86a503ef5497\r\n",
        s"${"a" * 1024}\r\n",
        "0;chunk-signature=b6c6ea8a5354eaf15b3cb7646744f4275b71ea724fed81ceb9323e279d449df9\r\n",
        "\r\n"
      ).mkString

    signing
      .sign(request)
      .flatMap { request =>
        val authorization = request.headers.get(ci"Authorization").map(_.head.value)
        val body = request.body.through(utf8.decode).compile.string
        body.tupleLeft(authorization)
      }
      .assertEquals((expectedAuthorization, expectedBody))
  }
}
