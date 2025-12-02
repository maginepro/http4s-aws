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

import cats.ApplicativeThrow
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import com.magine.http4s.aws.AwsServiceName
import com.magine.http4s.aws.AwsUrlEncoding.urlEncode
import com.magine.http4s.aws.AwsUrlEncoding.urlEncodePath
import com.magine.http4s.aws.headers.`X-Amz-Content-SHA256`
import fs2.Chunk
import fs2.hashing.HashAlgorithm
import fs2.hashing.Hashing
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import org.http4s.Header
import org.http4s.Request
import scala.util.matching.Regex
import scodec.bits.ByteVector

/**
  * Documentation: https://docs.aws.amazon.com/IAM/latest/UserGuide/create-signed-request.html
  */
private[aws] final case class CanonicalRequest(
  httpMethod: String,
  canonicalUri: String,
  canonicalQueryString: String,
  canonicalHeaders: String,
  signedHeaders: String,
  hashedPayload: String
) {
  def value: String =
    show"$httpMethod\n$canonicalUri\n$canonicalQueryString\n$canonicalHeaders\n$signedHeaders\n$hashedPayload"

  def valueHash[F[_]: Hashing: MonadCancelThrow]: F[String] =
    Hashing[F].hasher(HashAlgorithm.SHA256).use { hasher =>
      for {
        _ <- hasher.update(Chunk.array(value.getBytes(UTF_8)))
        hash <- hasher.hash.map(_.bytes.toByteVector.toHex)
      } yield hash
    }

  /* TODO: Remove for 7.0 release. */
  def valueHashLegacy: String = {
    val digest = MessageDigest.getInstance("SHA-256")
    ByteVector.view(digest.digest(value.getBytes(UTF_8))).toHex
  }
}

private[aws] object CanonicalRequest {
  def fromRequest[F[_]: ApplicativeThrow](request: Request[F], serviceName: AwsServiceName)
    : F[CanonicalRequest] =
    `X-Amz-Content-SHA256`.getOrError(request).map { hashedPayload =>
      CanonicalRequest(
        httpMethod = httpMethod(request),
        canonicalUri = canonicalUri(request, serviceName),
        canonicalQueryString = canonicalQueryString(request),
        canonicalHeaders = canonicalHeaders(request),
        signedHeaders = signedHeaders(request),
        hashedPayload = hashedPayload.value
      )
    }

  /**
    * Like [[fromRequest]], but does not use or calculate a hash
    * of the request body (payload). Instead, `UNSIGNED-PAYLOAD`
    * is used to indicate the payload is not being signed. This
    * can be used when the payload is not known in advance, for
    * example in [[AwsPresigning]] when presigning requests.
    */
  def fromRequestUnsignedPayload[F[_]](request: Request[F], serviceName: AwsServiceName): CanonicalRequest =
    CanonicalRequest(
      httpMethod = httpMethod(request),
      canonicalUri = canonicalUri(request, serviceName),
      canonicalQueryString = canonicalQueryString(request),
      canonicalHeaders = canonicalHeaders(request),
      signedHeaders = signedHeaders(request),
      hashedPayload = "UNSIGNED-PAYLOAD"
    )

  def canonicalHeaders[F[_]](request: Request[F]): String =
    request.headers.headers
      .groupBy(_.name)
      .toList
      .sortBy { case (name, _) => name }
      .map { case (name, headers) =>
        val values = headers.map(trim).mkString(",")
        s"${name.toString.toLowerCase}:$values\n"
      }
      .mkString

  def canonicalQueryString[F[_]](request: Request[F]): String =
    request.uri.query.pairs
      .map { case (key, value) => (urlEncode(key), value) }
      .sortBy { case (encodedKey, _) => encodedKey }
      .map {
        case (encodedKey, Some(value)) => s"$encodedKey=${urlEncode(value)}"
        case (encodedKey, None) => s"$encodedKey="
      }
      .mkString("&")

  def canonicalUri[F[_]](request: Request[F], serviceName: AwsServiceName): String = {
    val absolutePath = request.uri.path.toAbsolute.renderString
    if (serviceName != AwsServiceName.S3) urlEncodePath(absolutePath) else absolutePath
  }

  def httpMethod[F[_]](request: Request[F]): String =
    request.method.name.toUpperCase

  def signedHeaderNames[F[_]](request: Request[F]): List[String] =
    request.headers.headers
      .map(_.name.toString.toLowerCase)
      .distinct
      .sorted

  def signedHeaders[F[_]](request: Request[F]): String =
    signedHeaderNames(request).mkString(";")

  private val whitespace: Regex = "\\s+".r
  private def trim(header: Header.Raw): String =
    whitespace.replaceAllIn(header.value, " ").trim
}
