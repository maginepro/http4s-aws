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

private[aws] object Hex {
  private val hex: Array[Char] =
    "0123456789abcdef".toCharArray

  def encodeHex(bytes: Array[Byte]): String = {
    val length = bytes.length
    val out = new Array[Char](length * 2)
    var i = 0
    while (i < length) {
      val b = bytes(i)
      out(i * 2) = hex((0xf0 & b) >>> 4)
      out(i * 2 + 1) = hex(0x0f & b)
      i = i + 1
    }
    new String(out)
  }
}
