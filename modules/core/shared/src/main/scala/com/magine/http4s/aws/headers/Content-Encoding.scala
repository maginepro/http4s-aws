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

import org.http4s.ContentCoding
import org.http4s.Request

private[aws] object `Content-Encoding` {
  private type `Content-Encoding` = org.http4s.headers.`Content-Encoding`
  private val `Content-Encoding` = org.http4s.headers.`Content-Encoding`

  val `aws-chunked`: `Content-Encoding` =
    `Content-Encoding`(ContentCoding.unsafeFromString("aws-chunked"))

  def putIfChunked[F[_]](request: Request[F]): Request[F] =
    if (request.isChunked) request.putHeaders(`aws-chunked`) else request

  def putIfAbsentAndChunked[F[_]](request: Request[F]): Request[F] =
    if (request.headers.contains[`Content-Encoding`]) request else putIfChunked(request)
}
