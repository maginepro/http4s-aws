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

import java.lang.StringBuffer
import java.net.URLEncoder
import java.util.regex.Pattern

object AwsUrlEncoding {

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
    urlEncode(s, path = false)

  /**
    * Encode a `String` representing the path of a URL.
    *
    * If the string is only a part of a URL (and not an
    * entire path), the [[urlEncode]] function can be
    * used instead, so `/` is encoded.
    */
  def urlEncodePath(s: String): String =
    urlEncode(s, path = true)

  private val replacements: Pattern =
    s"${Pattern.quote("+")}|${Pattern.quote("*")}|${Pattern.quote("%7E")}|${Pattern.quote("%2F")}".r.pattern

  private def urlEncode(s: String, path: Boolean): String = {
    val encoded = URLEncoder.encode(s, "UTF-8")
    val output = new StringBuffer(encoded.length)
    val matcher = replacements.matcher(encoded)

    while (matcher.find()) {
      val _ = matcher.appendReplacement(
        output,
        matcher.group(0) match {
          case "+" => "%20"
          case "*" => "%2A"
          case "%7E" => "~"
          case "%2F" if path => "/"
          case "%2F" => "%2F"
        }
      )
    }

    matcher.appendTail(output)
    output.toString
  }
}
