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

import com.magine.http4s.aws.internal.CanonicalRequest
import org.http4s.Header
import org.http4s.ParseResult
import org.http4s.Request
import org.typelevel.ci.*

final case class `X-Amz-SignedHeaders`(headerNames: List[CIString]) {
  def value: String =
    headerNames.map(_.toString).mkString(";")
}

object `X-Amz-SignedHeaders` {
  def get[F[_]](request: Request[F]): Option[`X-Amz-SignedHeaders`] =
    request.headers.get[`X-Amz-SignedHeaders`]

  def parse(s: String): ParseResult[`X-Amz-SignedHeaders`] =
    Right(apply(s.split(';').toList.map(CIString(_))))

  def put[F[_]](request: Request[F]): Request[F] = {
    val signedHeaderNames = CanonicalRequest.signedHeaderNames(request)
    request.putHeaders(apply(signedHeaderNames.map(CIString(_))))
  }

  def putIfAbsent[F[_]](request: Request[F]): Request[F] =
    if (request.headers.contains[`X-Amz-SignedHeaders`]) request else put(request)

  def putQueryParam[F[_]](request: Request[F]): Request[F] =
    request.withUri(
      request.uri.withQueryParam(
        "X-Amz-SignedHeaders",
        CanonicalRequest.signedHeaders(request)
      )
    )

  implicit val headerInstance: Header[`X-Amz-SignedHeaders`, Header.Single] =
    Header.createRendered(ci"X-Amz-SignedHeaders", _.value, parse)
}
