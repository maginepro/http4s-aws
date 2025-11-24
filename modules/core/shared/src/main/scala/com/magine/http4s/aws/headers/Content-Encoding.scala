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
import org.http4s.Header
import org.http4s.Request

private[aws] object `Content-Encoding` {
  private type `Content-Encoding` = org.http4s.headers.`Content-Encoding`
  private val `Content-Encoding` = org.http4s.headers.`Content-Encoding`

  val `aws-chunked`: `Content-Encoding` =
    `Content-Encoding`(ContentCoding.unsafeFromString("aws-chunked"))

  /**
    * The http4s `Content-Encoding` header type currently only allows
    * a single encoding: https://github.com/http4s/http4s/issues/2684.
    *
    * It is allowed to specify multiple encodings, and the signature
    * logic requires `aws-chunked`, while supporting an additional
    * encoding, such that e.g. `aws-chunked,gzip` is supported.
    *
    * The logic here crudely parses `Content-Encoding` and prepends
    * `aws-chunked` if it's not already present for chunked requests.
    */
  def putIfChunked[F[_]](request: Request[F]): Request[F] =
    if (request.isChunked) {
      val headerName = Header[`Content-Encoding`].name
      val encoding = `aws-chunked`.contentCoding.coding

      val encodings =
        request.headers
          .get(headerName)
          .toList
          .flatMap(_.toList.map(_.value))
          .flatMap(_.split(',').map(_.trim).toList)

      if (encodings.contains(encoding)) request
      else {
        val value = encodings.prepended(encoding).mkString(",")
        request.putHeaders(Header.Raw(headerName, value))
      }
    } else request
}
