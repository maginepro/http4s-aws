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
import com.magine.aws.Region
import com.magine.http4s.aws.AwsServiceName
import com.magine.http4s.aws.Credentials
import java.nio.charset.StandardCharsets.UTF_8
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scala.util.chaining.*

private[aws] final case class Signature(value: String)

private[aws] object Signature {

  object Legacy {
    private val algorithm: String = "HmacSHA256"

    def sign(key: SecretKeySpec, bytes: Array[Byte]): Signature =
      Signature(Hex.encodeHex(signWithKey(key, bytes)))

    def signingContent(
      canonicalRequest: CanonicalRequest,
      credentialScope: CredentialScope,
      requestDateTime: RequestDateTime
    ): Array[Byte] =
      show"AWS4-HMAC-SHA256\n${requestDateTime.value}\n${credentialScope.value}\n${canonicalRequest.valueHash}"
        .getBytes(UTF_8)

    def signingKey(
      region: Region,
      requestDate: RequestDate,
      secretAccessKey: Credentials.SecretAccessKey,
      serviceName: AwsServiceName
    ): SecretKeySpec = {
      def sign(bytes: Array[Byte])(key: SecretKeySpec): SecretKeySpec =
        new SecretKeySpec(signWithKey(key, bytes), algorithm)

      new SecretKeySpec(s"AWS4${secretAccessKey.value}".getBytes(UTF_8), algorithm)
        .pipe(sign(requestDate.value.getBytes(UTF_8)))
        .pipe(sign(region.id.getBytes(UTF_8)))
        .pipe(sign(serviceName.value.getBytes(UTF_8)))
        .pipe(sign("aws4_request".getBytes(UTF_8)))
    }

    private def signWithKey(key: SecretKeySpec, bytes: Array[Byte]): Array[Byte] = {
      val mac = Mac.getInstance(algorithm)
      mac.init(key)
      mac.doFinal(bytes)
    }
  }
}
