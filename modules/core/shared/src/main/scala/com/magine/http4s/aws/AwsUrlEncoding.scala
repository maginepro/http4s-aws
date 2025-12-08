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

/* TODO: Remove for 7.0 release. */
private[aws] object AwsUrlEncoding {

  /**
    * Encode a `String` for use in the path of a URL.
    *
    * If the string is meant to represent a path, the
    * [[urlEncodePath]] function can be used instead,
    * so `/` is not encoded.
    *
    * @return
    */
  def urlEncode(s: String): String =
    AwsUriEncoding.uriEncode(s)

  /**
    * Encode a `String` representing the path of a URL.
    *
    * If the string is only a part of a URL (and not an
    * entire path), the [[urlEncode]] function can be
    * used instead, so `/` is encoded.
    */
  def urlEncodePath(s: String): String =
    AwsUriEncoding.uriEncodePath(s)
}
