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
import com.magine.http4s.aws.AwsUriEncoding
import io.circe.Decoder
import io.circe.Encoder
import java.nio.charset.StandardCharsets.UTF_8
import org.http4s.Uri.Path

/**
  * An object key (or key name) which uniquely identifies
  * an object in an Amazon S3 bucket.
  *
  * The object key name must be non-empty and is not
  * allowed to exceed 1,024 UTF-8 bytes.
  *
  * When [[S3Key]]s are created, paths are normalized and
  * encoded using [[AwsUriEncoding]]. If this is unwanted,
  * use one of the following alternatives.
  *
  * - If paths are already encoded, use [[S3Key.fromPathEncoded]].
  * - If paths are also normalized, use [[S3Key.fromPathUnmodified]].
  *
  * @see https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html
  */
sealed abstract case class S3Key(path: Path)

object S3Key {

  /**
    * Alias for [[S3Key.fromPath]].
    */
  def apply(path: Path): Either[InvalidS3Key, S3Key] =
    fromPath(path)

  /**
    * Returns a new [[S3Key]] for the specified path with
    * encoding and normalization; or an [[InvalidS3Key]]
    * if the path is not a valid key.
    */
  def fromPath(path: Path): Either[InvalidS3Key, S3Key] =
    fromPathEncoded(AwsUriEncoding.uriEncodePath(path))

  /**
    * Returns a new [[S3Key]] for the specified path without
    * encoding but with normalization; or an [[InvalidS3Key]]
    * if the path is not a valid key.
    */
  def fromPathEncoded(path: Path): Either[InvalidS3Key, S3Key] =
    fromPathUnmodified(path.normalize)

  /**
    * Returns a new [[S3Key]] for the specified path without
    * normalization or encoding; or an [[InvalidS3Key]] if
    * the path is not a valid key.
    */
  def fromPathUnmodified(path: Path): Either[InvalidS3Key, S3Key] =
    if (path.isEmpty) Left(InvalidS3Key.Empty(path))
    else if (utf8Length(path) > 1024) Left(InvalidS3Key.TooLong(path))
    else Right(new S3Key(path) {})

  /**
    * Returns a new [[S3Key]] for the specified path with
    * encoding and normalization; or an [[InvalidS3Key]]
    * if the path is not a valid key.
    */
  def fromString(path: String): Either[InvalidS3Key, S3Key] =
    fromPath(Path.unsafeFromString(path))

  implicit val s3KeyDecoder: Decoder[S3Key] =
    Decoder[String].emap(fromString(_).leftMap(_.getMessage))

  implicit val s3KeyEncoder: Encoder[S3Key] =
    Encoder[String].contramap(_.path.renderString)

  implicit val s3KeyHash: Hash[S3Key] =
    Hash.by(_.path)

  implicit val s3KeyShow: Show[S3Key] =
    Show.show(_.path.renderString)

  private def utf8Length(path: Path): Int =
    path.renderString.getBytes(UTF_8).length
}
