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
import org.http4s.Uri.Path
import org.http4s.Uri.Path.Segment

/**
  * Provides functions for encoding segments and paths
  * using the AWS `UriEncode()` function documented at
  * the linked documentation page.
  *
  * @see https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_sigv-create-signed-request.html
  */
object AwsUriEncoding {

  /**
    * Returns a new `Segment` by encoding the specified
    * `Segment` with URI encoding.
    */
  def uriEncode(segment: Segment): Segment =
    Segment.encoded(uriEncode(segment.encoded))

  /**
    * Returns a new `String` by encoding the specified
    * segment `String` with URI encoding.
    */
  def uriEncode(segment: String): String =
    uriEncode(segment, path = false)

  /**
    * Returns a new `String` by encoding the specified
    * path `String` with URI encoding.
    */
  def uriEncodePath(path: String): String =
    uriEncode(path, path = true)

  /**
    * Returns a new `Path` by encoding the specified
    * `Path` segments with URI encoding.
    */
  def uriEncodePath(path: Path): Path =
    Path(
      segments = path.segments.map(uriEncode),
      absolute = path.absolute,
      endsWithSlash = path.endsWithSlash
    )

  private val replacements: Pattern =
    s"${Pattern.quote("+")}|${Pattern.quote("*")}|${Pattern.quote("%7E")}|${Pattern.quote("%2F")}".r.pattern

  private def uriEncode(s: String, path: Boolean): String = {
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
