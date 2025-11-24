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

final case class `X-Amz-Storage-Class`(value: String)

object `X-Amz-Storage-Class` {
  val DEEP_ARCHIVE: `X-Amz-Storage-Class` = apply("DEEP_ARCHIVE")
  val EXPRESS_ONEZONE: `X-Amz-Storage-Class` = apply("EXPRESS_ONEZONE")
  val GLACIER_IR: `X-Amz-Storage-Class` = apply("GLACIER_IR")
  val GLACIER: `X-Amz-Storage-Class` = apply("GLACIER")
  val INTELLIGENT_TIERING: `X-Amz-Storage-Class` = apply("INTELLIGENT_TIERING")
  val ONEZONE_IA: `X-Amz-Storage-Class` = apply("ONEZONE_IA")
  val REDUCED_REDUNDANCY: `X-Amz-Storage-Class` = apply("REDUCED_REDUNDANCY")
  val STANDARD_IA: `X-Amz-Storage-Class` = apply("STANDARD_IA")
  val STANDARD: `X-Amz-Storage-Class` = apply("STANDARD")

  def get[F[_]](request: Request[F]): Option[`X-Amz-Storage-Class`] =
    request.headers.get[`X-Amz-Storage-Class`]

  def parse(s: String): ParseResult[`X-Amz-Storage-Class`] =
    Right(apply(s))

  def put[F[_]](storageClass: `X-Amz-Storage-Class`)(request: Request[F]): Request[F] =
    request.putHeaders(storageClass)

  def putIfAbsent[F[_]](storageClass: `X-Amz-Storage-Class`)(request: Request[F]): Request[F] =
    if (request.headers.contains[`X-Amz-Storage-Class`]) request else put(storageClass)(request)

  def putQueryParam[F[_]](storageClass: `X-Amz-Storage-Class`)(request: Request[F]): Request[F] =
    request.withUri(request.uri.withQueryParam("X-Amz-Storage-Class", storageClass.value))

  implicit val headerInstance: Header[`X-Amz-Storage-Class`, Header.Single] =
    Header.createRendered(ci"X-Amz-Storage-Class", _.value, parse)
}
