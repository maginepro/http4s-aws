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
import cats.syntax.all.*
import com.magine.http4s.aws.headers.`X-Amz-Date`
import java.time.Instant
import java.time.LocalDate
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import org.http4s.Request

private[aws] final case class RequestDateTime(toInstant: Instant) {
  def date: RequestDate =
    RequestDate(toLocalDate)

  def toLocalDate: LocalDate =
    toZonedDateTime.toLocalDate

  def toZonedDateTime: ZonedDateTime =
    toInstant.atZone(ZoneOffset.UTC)

  val value: String =
    toInstant
      .atOffset(ZoneOffset.UTC)
      .format(RequestDateTime.format)
}

private[aws] object RequestDateTime {
  val format: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")

  def fromRequest[F[_]: ApplicativeThrow](request: Request[F]): F[RequestDateTime] =
    `X-Amz-Date`.getOrDateOrError(request).map(apply)

  def fromRequestQueryParam[F[_]: ApplicativeThrow](request: Request[F]): F[RequestDateTime] =
    `X-Amz-Date`.getQueryParam(request).map(apply)
}
