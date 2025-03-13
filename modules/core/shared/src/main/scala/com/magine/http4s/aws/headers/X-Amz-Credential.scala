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

import cats.parse.Parser
import cats.syntax.all.*
import com.magine.aws.Region
import com.magine.http4s.aws.AwsServiceName
import com.magine.http4s.aws.Credentials
import com.magine.http4s.aws.internal.RequestDate
import java.time.Instant
import java.time.LocalDate
import org.http4s.Header
import org.http4s.ParseFailure
import org.http4s.ParseResult
import org.http4s.Request
import org.typelevel.ci.*

final case class `X-Amz-Credential`(
  accessKeyId: Credentials.AccessKeyId,
  requestDate: LocalDate,
  region: Region,
  serviceName: AwsServiceName
) {
  def value: String =
    s"${accessKeyId.value}/${requestDate.format(RequestDate.format)}/${region.id}/${serviceName.value}/aws4_request"
}

object `X-Amz-Credential` {
  def apply(
    accessKeyId: Credentials.AccessKeyId,
    requestDate: Instant,
    region: Region,
    serviceName: AwsServiceName
  ): `X-Amz-Credential` =
    apply(
      accessKeyId = accessKeyId,
      requestDate = RequestDate.fromInstant(requestDate).toLocalDate,
      region = region,
      serviceName = serviceName
    )

  def get[F[_]](request: Request[F]): Option[`X-Amz-Credential`] =
    request.headers.get[`X-Amz-Credential`]

  val parser: Parser[`X-Amz-Credential`] = {
    val slash = Parser.char('/')
    val accessKeyId = Parser.charsWhile(_ != '/').map(Credentials.AccessKeyId(_)) <* slash
    val requestDate = Parser.charsWhile(_.isDigit).mapFilter(RequestDate.parse).map(_.toLocalDate) <* slash
    val region = Parser.charsWhile(_ != '/').mapFilter(Region.valid(_).toOption) <* slash
    val serviceName = Parser.charsWhile(_ != '/').map(AwsServiceName(_)) <* slash
    val credential = (accessKeyId, requestDate, region, serviceName).mapN(apply)
    val aws4Request = Parser.string("aws4_request")
    credential <* aws4Request
  }

  def parse(s: String): ParseResult[`X-Amz-Credential`] =
    try parser.parseAll(s).leftMap(e => ParseFailure("Invalid X-Amz-Credential header", e.show))
    catch { case p: ParseFailure => p.asLeft[`X-Amz-Credential`] }

  def put[F[_]](credential: `X-Amz-Credential`)(request: Request[F]): Request[F] =
    request.putHeaders(credential)

  def putIfAbsent[F[_]](credential: `X-Amz-Credential`)(request: Request[F]): Request[F] =
    if (request.headers.contains[`X-Amz-Credential`]) request else put(credential)(request)

  def putQueryParam[F[_]](credential: `X-Amz-Credential`)(request: Request[F]): Request[F] =
    request.withUri(request.uri.withQueryParam("X-Amz-Credential", credential.value))

  implicit val headerInstance: Header[`X-Amz-Credential`, Header.Single] =
    Header.createRendered(ci"X-Amz-Credential", _.value, parse)
}
