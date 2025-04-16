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

import scala.util.control.NoStackTrace

/**
  * Exception raised by [[TokenCodeProvider]]s to indicate
  * that a provided MFA code is not a valid [[TokenCode]].
  */
final case class InvalidTokenCode() extends RuntimeException with NoStackTrace {
  override def getMessage: String = "Invalid MFA code, must be 6 digits"
}
