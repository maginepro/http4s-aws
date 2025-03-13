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
import cats.syntax.all.*
import org.http4s.Request

private[aws] object Host {
  private type Host = org.http4s.headers.Host
  private val Host = org.http4s.headers.Host

  private val defaultPorts: Set[Int] =
    Set(80, 443)

  def put[F[_]: ApplicativeThrow](request: Request[F]): F[Request[F]] =
    ApplicativeThrow[F].fromOption(
      request.uri.host.map { host =>
        val port = request.uri.port.filterNot(defaultPorts)
        request.putHeaders(Host(host.value, port))
      },
      new IllegalArgumentException("The request URI must be absolute")
    )

  def putIfAbsent[F[_]: ApplicativeThrow](request: Request[F]): F[Request[F]] =
    if (request.headers.contains[Host]) request.pure else put(request)
}
