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

import cats.ApplicativeThrow
import com.magine.http4s.aws.MissingContentLength
import org.http4s.Header
import org.http4s.ParseFailure
import org.http4s.ParseResult
import org.http4s.Request
import org.typelevel.ci.*

final case class `X-Amz-Decoded-Content-Length`(value: Long)

object `X-Amz-Decoded-Content-Length` {
  private[aws] def ensureSet[F[_]: ApplicativeThrow](request: Request[F]): F[Unit] =
    if (request.isChunked && !request.headers.contains[`X-Amz-Decoded-Content-Length`])
      ApplicativeThrow[F].raiseError(MissingContentLength())
    else ApplicativeThrow[F].unit

  def get[F[_]](request: Request[F]): Option[`X-Amz-Decoded-Content-Length`] =
    request.headers.get[`X-Amz-Decoded-Content-Length`]

  def parse(s: String): ParseResult[`X-Amz-Decoded-Content-Length`] =
    s.toLongOption.map(apply).toRight {
      ParseFailure("Invalid X-Amz-Decoded-Content-Length header", s)
    }

  def put[F[_]](value: Long)(request: Request[F]): Request[F] =
    request.putHeaders(apply(value))

  def putIfAbsent[F[_]](value: Long)(request: Request[F]): Request[F] =
    if (request.headers.contains[`X-Amz-Decoded-Content-Length`]) request else put(value)(request)

  implicit val headerInstance: Header[`X-Amz-Decoded-Content-Length`, Header.Single] =
    Header.createRendered(ci"X-Amz-Decoded-Content-Length", _.value, parse)
}
