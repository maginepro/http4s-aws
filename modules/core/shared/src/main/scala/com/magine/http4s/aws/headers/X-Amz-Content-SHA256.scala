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
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Chunk
import fs2.Stream
import fs2.hashing.HashAlgorithm
import fs2.hashing.Hashing
import fs2.text.hex
import org.http4s.Header
import org.http4s.ParseResult
import org.http4s.Request
import org.typelevel.ci.*

final case class `X-Amz-Content-SHA256`(value: String)

object `X-Amz-Content-SHA256` {
  def get[F[_]](request: Request[F]): Option[`X-Amz-Content-SHA256`] =
    request.headers.get[`X-Amz-Content-SHA256`]

  def getOrError[F[_]: ApplicativeThrow](request: Request[F]): F[`X-Amz-Content-SHA256`] =
    get(request)
      .toRight(new IllegalArgumentException("The request must have a X-Amz-Content-SHA256 header"))
      .liftTo[F]

  def parse(s: String): ParseResult[`X-Amz-Content-SHA256`] =
    Right(apply(s))

  def put[F[_]: Concurrent: Hashing](request: Request[F]): F[Request[F]] =
    unChunk(request).flatMap(request => bodyHash(request).map(request.putHeaders(_)))

  def putIfAbsent[F[_]: Concurrent: Hashing](request: Request[F]): F[Request[F]] =
    if (request.headers.contains[`X-Amz-Content-SHA256`]) request.pure else put(request)

  private def bodyHash[F[_]: Concurrent: Hashing](request: Request[F]): F[`X-Amz-Content-SHA256`] =
    request.body
      .through(Hashing[F].hash(HashAlgorithm.SHA256))
      .map(_.bytes)
      .unchunks
      .through(hex.encode)
      .compile
      .lastOrError
      .map(apply)

  private def unChunk[F[_]: Concurrent](request: Request[F]): F[Request[F]] =
    if (request.isChunked) {
      val unChunked = request.body.compile.to(Chunk)
      unChunked.map(Stream.chunk).map(request.withBodyStream)
    } else request.pure

  implicit val headerInstance: Header[`X-Amz-Content-SHA256`, Header.Single] =
    Header.createRendered(ci"X-Amz-Content-SHA256", _.value, parse)
}
