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

import cats.syntax.all.*
import io.circe.Decoder
import java.time.Instant
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatterBuilder

/**
  * Contains parsing code for ISO 8601 timestamp values.
  *
  * Used internally to decode the expiration time of credentials
  * in [[ExpiringCredentials]]. The parsing pretty much mimics
  * how the AWS Java SDK parses the expiration time.
  */
private[aws] object Iso8601 {
  private val formats: List[DateTimeFormatter] =
    List(
      DateTimeFormatter.ISO_INSTANT.withZone(ZoneOffset.UTC),
      new DateTimeFormatterBuilder()
        .appendPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")
        .toFormatter()
        .withZone(ZoneOffset.UTC),
      DateTimeFormatter.ISO_OFFSET_DATE_TIME
    )

  val decoder: Decoder[Instant] =
    Decoder[String].emap(expiration =>
      parseIso8601(toIso8601(expiration))
        .toRight(s"Unable to parse expiration: $expiration")
    )

  private def toIso8601(s: String): String =
    if (s.endsWith("+0000")) s.substring(0, s.length - 5).concat("Z") else s

  private def parseIso8601(s: String): Option[Instant] =
    formats.collectFirstSome(parse(s, _))

  private def parse(s: String, format: DateTimeFormatter): Option[Instant] =
    Either.catchNonFatal(format.parse(s, Instant.from(_))).toOption
}
