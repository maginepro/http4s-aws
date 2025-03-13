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
import org.http4s.ParseFailure
import org.http4s.ParseResult
import org.http4s.Request
import org.typelevel.ci.*
import scala.concurrent.duration.*

final case class `X-Amz-Expires`(expiry: FiniteDuration) {
  def value: String =
    expiry.toSeconds.toString
}

object `X-Amz-Expires` {
  def get[F[_]](request: Request[F]): Option[`X-Amz-Expires`] =
    request.headers.get[`X-Amz-Expires`]

  def parse(s: String): ParseResult[`X-Amz-Expires`] =
    s.toLongOption.map(_.seconds).map(apply).toRight {
      ParseFailure("Invalid X-Amz-Expires header", s)
    }

  def put[F[_]](expiry: FiniteDuration)(request: Request[F]): Request[F] =
    request.putHeaders(apply(expiry))

  def putIfAbsent[F[_]](expiry: FiniteDuration)(request: Request[F]): Request[F] =
    if (request.headers.contains[`X-Amz-Expires`]) request else put(expiry)(request)

  def putQueryParam[F[_]](expiry: FiniteDuration)(request: Request[F]): Request[F] =
    request.withUri(request.uri.withQueryParam("X-Amz-Expires", apply(expiry).value))

  implicit val headerInstance: Header[`X-Amz-Expires`, Header.Single] =
    Header.createRendered(ci"X-Amz-Expires", _.value, parse)
}
