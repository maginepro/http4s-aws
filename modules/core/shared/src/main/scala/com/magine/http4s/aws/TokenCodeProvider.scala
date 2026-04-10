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

import cats.Applicative
import cats.syntax.all.*

/**
  * Capability to return [[TokenCode]]s from [[MfaSerial]]s.
  */
trait TokenCodeProvider[F[_]] {

  /**
    * Returns a [[TokenCode]] from the specified [[MfaSerial]].
    */
  def tokenCode(mfaSerial: MfaSerial): F[TokenCode]
}

object TokenCodeProvider extends TokenCodeProviderPlatform {

  /**
    * Returns a new [[TokenCodeProvider]] which always returns
    * the specified [[TokenCode]].
    */
  def static[F[_]: Applicative](tokenCode: TokenCode): TokenCodeProvider[F] = {
    val result = tokenCode.pure[F]
    new TokenCodeProvider[F] {
      override def tokenCode(mfaSerial: MfaSerial): F[TokenCode] =
        result
    }
  }
}
