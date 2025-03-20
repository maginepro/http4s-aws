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

import cats.effect.Sync
import cats.syntax.all.*
import scala.io.StdIn

/**
  * Capability to return [[TokenCode]]s from [[MfaSerial]]s.
  */
trait TokenCodeProvider[F[_]] {

  /**
    * Returns a [[TokenCode]] from the specified [[MfaSerial]].
    */
  def tokenCode(mfaSerial: MfaSerial): F[TokenCode]
}

object TokenCodeProvider {

  /**
    * Returns a new [[TokenCodeProvider]] which prompts the user
    * for a [[TokenCode]] and then reads it from standard input.
    */
  def default[F[_]: Sync]: TokenCodeProvider[F] =
    new TokenCodeProvider[F] {
      override def tokenCode(mfaSerial: MfaSerial): F[TokenCode] =
        Sync[F]
          .blocking {
            Option(System.console()) match {
              case Some(console) =>
                print(s"Enter MFA code for ${mfaSerial.value}: ")
                Option(console.readPassword()).map(_.mkString.trim)
              case None =>
                println(s"Enter MFA code for ${mfaSerial.value}:")
                Option(StdIn.readLine()).map(_.trim)
            }
          }
          .flatMap {
            case Some(input) => TokenCode(input).pure[F]
            case None => unexpectedEndOfInput.raiseError[F, Option[TokenCode]]
          }
          .flatMap {
            case Some(tokenCode) =>
              tokenCode.pure[F]
            case None =>
              Sync[F].blocking(println("\nInvalid MFA code, must be 6 digits.")) >>
                tokenCode(mfaSerial)
          }

      private def unexpectedEndOfInput: Throwable =
        new RuntimeException("Unexpected end of input")
    }
}
