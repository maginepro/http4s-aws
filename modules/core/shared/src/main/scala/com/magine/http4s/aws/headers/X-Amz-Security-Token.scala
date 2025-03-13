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

import com.magine.http4s.aws.Credentials
import org.http4s.Header
import org.http4s.ParseResult
import org.http4s.Request
import org.typelevel.ci.*

final case class `X-Amz-Security-Token`(sessionToken: Credentials.SessionToken) {
  def value: String =
    sessionToken.value
}

object `X-Amz-Security-Token` {
  def get[F[_]](request: Request[F]): Option[`X-Amz-Security-Token`] =
    request.headers.get[`X-Amz-Security-Token`]

  def parse(s: String): ParseResult[`X-Amz-Security-Token`] =
    Right(`X-Amz-Security-Token`(Credentials.SessionToken(s)))

  def put[F[_]](sessionToken: Option[Credentials.SessionToken])(request: Request[F]): Request[F] =
    sessionToken.map(apply).map(request.putHeaders(_)).getOrElse(request)

  def putIfAbsent[F[_]](sessionToken: Option[Credentials.SessionToken])(request: Request[F]): Request[F] =
    if (request.headers.contains[`X-Amz-Security-Token`]) request else put(sessionToken)(request)

  def putQueryParam[F[_]](sessionToken: Option[Credentials.SessionToken])(request: Request[F]): Request[F] =
    sessionToken
      .map(_.value)
      .map(request.uri.withQueryParam("X-Amz-Security-Token", _))
      .map(request.withUri)
      .getOrElse(request)

  implicit val headerInstance: Header[`X-Amz-Security-Token`, Header.Single] =
    Header.createRendered(ci"X-Amz-Security-Token", _.value, parse)
}
