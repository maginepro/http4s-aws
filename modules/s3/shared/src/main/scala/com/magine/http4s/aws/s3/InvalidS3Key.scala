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

package com.magine.http4s.aws.s3

import java.nio.charset.StandardCharsets.UTF_8
import org.http4s.Uri.Path
import scala.util.control.NoStackTrace

sealed abstract class InvalidS3Key(val path: Path) extends RuntimeException with NoStackTrace {
  def details: String

  override final def getMessage: String =
    message

  def message: String =
    s"Invalid S3Key: $path: $details"
}

object InvalidS3Key {
  def unapply(invalid: InvalidS3Key): Some[Path] =
    Some(invalid.path)

  private[s3] final case class Empty(override val path: Path) extends InvalidS3Key(path) {
    override def details: String =
      "the path is empty"

    override def message: String =
      s"Invalid S3Key: $details"
  }

  private[s3] final case class TooLong(override val path: Path) extends InvalidS3Key(path) {
    override def details: String = {
      val utf8Length = path.renderString.getBytes(UTF_8).length
      s"the path length in UTF-8 bytes ($utf8Length bytes) exceeds the maximum allowed length (1024 bytes)"
    }
  }
}
