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
import org.http4s.Request
import org.typelevel.ci.*

private[aws] object `Content-Encoding` {
  private val `aws-chunked`: String =
    "aws-chunked"

  private val `Content-Encoding`: CIString =
    ci"Content-Encoding"

  /**
    * The http4s `Content-Encoding` header type currently only allows
    * a single encoding: https://github.com/http4s/http4s/issues/2684.
    *
    * It is allowed to specify multiple encodings, and the signature
    * logic requires `aws-chunked`, while supporting an additional
    * encoding, such that e.g. `aws-chunked,gzip` is supported.
    *
    * The logic here crudely parses `Content-Encoding` and appends
    * `aws-chunked` if it's not already present for chunked requests.
    */
  def putIfAbsentAndChunked[F[_]](request: Request[F]): Request[F] =
    if (request.isChunked) {
      val encodings = request.headers
        .get(`Content-Encoding`)
        .toList
        .flatMap(_.toList.map(_.value))
        .flatMap(_.split(',').map(_.trim).toList)

      if (encodings.contains(`aws-chunked`)) request
      else {
        val value = encodings.prepended(`aws-chunked`).mkString(",")
        request.putHeaders(Header.Raw(`Content-Encoding`, value))
      }
    } else request
}
