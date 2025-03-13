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

package com.magine.http4s.aws

import io.circe.Decoder
import io.circe.Encoder

final case class Credentials(
  accessKeyId: Credentials.AccessKeyId,
  secretAccessKey: Credentials.SecretAccessKey,
  sessionToken: Option[Credentials.SessionToken]
)

object Credentials {
  final case class AccessKeyId(value: String)

  object AccessKeyId {
    implicit val accessKeyIdDecoder: Decoder[AccessKeyId] =
      Decoder[String].map(apply)

    implicit val accessKeyIdEncoder: Encoder[AccessKeyId] =
      Encoder[String].contramap(_.value)
  }

  final case class SecretAccessKey(value: String) {
    override def toString: String =
      "SecretAccessKey(***)"
  }

  object SecretAccessKey {
    implicit val secretAccessKeyDecoder: Decoder[SecretAccessKey] =
      Decoder[String].map(apply)

    implicit val secretAccessKeyEncoder: Encoder[SecretAccessKey] =
      Encoder[String].contramap(_.value)
  }

  final case class SessionToken(value: String) {
    override def toString: String =
      "SessionToken(***)"
  }

  object SessionToken {
    implicit val sessionTokenDecoder: Decoder[SessionToken] =
      Decoder[String].map(apply)

    implicit val sessionTokenEncoder: Encoder[SessionToken] =
      Encoder[String].contramap(_.value)
  }
}
