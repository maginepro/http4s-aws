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

final case class MissingContentLength() extends RuntimeException {
  override def getMessage: String =
    "The request for signing has `Transfer-Encoding: chunked` set but does " +
      "not have the required `X-Amz-Decoded-Content-Length` header set. Note the " +
      "`Content-Length` header should generally not be set, since it will then have " +
      "to also account for the signing content length."
}
