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

import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder

private[aws] final case class AwsAccountId(value: String)

private[aws] object AwsAccountId {
  implicit val awsAccountIdCodec: Codec[AwsAccountId] =
    Codec.from(
      Decoder[String].map(apply),
      Encoder[String].contramap(_.value)
    )
}
