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

import org.http4s.Header
import org.http4s.ParseResult
import org.http4s.Request
import org.typelevel.ci.*

final case class `X-Amz-Algorithm`(value: String)

object `X-Amz-Algorithm` {
  val `AWS4-HMAC-SHA256`: `X-Amz-Algorithm` =
    apply("AWS4-HMAC-SHA256")

  def get[F[_]](request: Request[F]): Option[`X-Amz-Algorithm`] =
    request.headers.get[`X-Amz-Algorithm`]

  def parse(s: String): ParseResult[`X-Amz-Algorithm`] =
    Right(apply(s))

  def put[F[_]](algorithm: `X-Amz-Algorithm`)(request: Request[F]): Request[F] =
    request.putHeaders(algorithm)

  def putIfAbsent[F[_]](algorithm: `X-Amz-Algorithm`)(request: Request[F]): Request[F] =
    if (request.headers.contains[`X-Amz-Algorithm`]) request else put(algorithm)(request)

  def putQueryParam[F[_]](algorithm: `X-Amz-Algorithm`)(request: Request[F]): Request[F] =
    request.withUri(request.uri.withQueryParam("X-Amz-Algorithm", algorithm.value))

  implicit val headerInstance: Header[`X-Amz-Algorithm`, Header.Single] =
    Header.createRendered(ci"X-Amz-Algorithm", _.value, parse)
}
