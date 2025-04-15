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
import cats.effect.Temporal
import cats.syntax.all.*
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import org.http4s.Header
import org.http4s.ParseFailure
import org.http4s.ParseResult
import org.http4s.Request
import org.http4s.headers.Date
import org.typelevel.ci.*

final case class `X-Amz-Date`(toInstant: Instant) {
  def value: String =
    toInstant.atZone(ZoneOffset.UTC).format(`X-Amz-Date`.format)
}

object `X-Amz-Date` {
  def get[F[_]](request: Request[F]): Option[`X-Amz-Date`] =
    request.headers.get[`X-Amz-Date`]

  def getQueryParam[F[_]: ApplicativeThrow](request: Request[F]): F[Instant] =
    request.uri.params
      .get("X-Amz-Date")
      .toRight(new IllegalArgumentException("The request must have a X-Amz-Date query param"))
      .flatMap(parse(_).map(_.toInstant))
      .liftTo[F]

  def parse(s: String): ParseResult[`X-Amz-Date`] =
    Either
      .catchNonFatal(
        `X-Amz-Date`(
          LocalDateTime
            .from(format.parse(s))
            .atZone(ZoneOffset.UTC)
            .toInstant
        )
      )
      .leftMap(_ => ParseFailure("Invalid X-Amz-Date header", s))

  def put[F[_]: Temporal](request: Request[F]): F[Request[F]] =
    Temporal[F].realTime
      .map(d => Instant.EPOCH.plusNanos(d.toNanos))
      .map(`X-Amz-Date`(_))
      .map(request.putHeaders(_))

  def putIfAbsent[F[_]: Temporal](request: Request[F]): F[Request[F]] =
    if (request.headers.contains[`X-Amz-Date`]) request.pure
    else if (request.headers.contains[Date]) request.pure
    else put(request)

  def putQueryParam[F[_]](instant: Instant)(request: Request[F]): Request[F] =
    request.withUri(request.uri.withQueryParam("X-Amz-Date", `X-Amz-Date`(instant).value))

  private def getDate[F[_]](request: Request[F]): Option[Instant] =
    request.headers.get[Date].map(_.date.toInstant)

  private[aws] def getOrDateOrError[F[_]: ApplicativeThrow](request: Request[F]): F[Instant] =
    get(request)
      .map(_.toInstant)
      .orElse(getDate(request))
      .toRight(new IllegalArgumentException("The request must have a Date or X-Amz-Date header"))
      .liftTo[F]

  private val format: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")

  implicit val headerInstance: Header[`X-Amz-Date`, Header.Single] =
    Header.createRendered(ci"X-Amz-Date", _.value, parse)
}
