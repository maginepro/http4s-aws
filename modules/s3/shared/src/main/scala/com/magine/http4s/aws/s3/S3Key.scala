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

import cats.Hash
import cats.Show
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import java.nio.charset.StandardCharsets.UTF_8
import org.http4s.Uri

/**
  * An object key (or key name) which uniquely
  * identifies an object in an Amazon S3 bucket.
  *
  * The object key name must be non-empty and is
  * not allowed to exceed 1,024 UTF-8 bytes.
  *
  * Note key names are normalized (empty segments
  * removed) during the creation of [[S3Key]]s.
  *
  * @see https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html
  */
sealed abstract case class S3Key(path: Uri.Path)

object S3Key {
  def apply(path: Uri.Path): Either[InvalidS3Key, S3Key] =
    fromPath(path)

  def fromPath(path: Uri.Path): Either[InvalidS3Key, S3Key] =
    fromPathNormalized(path.normalize)

  implicit val s3KeyDecoder: Decoder[S3Key] =
    Decoder[String].emap(s =>
      fromPath(Uri.Path.unsafeFromString(s))
        .leftMap(_.getMessage)
    )

  implicit val s3KeyEncoder: Encoder[S3Key] =
    Encoder[String].contramap(_.path.renderString)

  implicit val s3KeyHash: Hash[S3Key] =
    Hash.by(_.path)

  implicit val s3KeyShow: Show[S3Key] =
    Show.show(_.path.renderString)

  private def fromPathNormalized(path: Uri.Path): Either[InvalidS3Key, S3Key] =
    if (path.nonEmpty && utf8Length(path) <= 1024)
      Right(new S3Key(path) {})
    else Left(InvalidS3Key(path))

  private def utf8Length(path: Uri.Path): Int =
    path.renderString.getBytes(UTF_8).length
}
