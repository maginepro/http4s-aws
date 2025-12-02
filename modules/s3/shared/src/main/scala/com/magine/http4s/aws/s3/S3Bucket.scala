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
import com.comcast.ip4s.Ipv4Address
import io.circe.Decoder
import io.circe.Encoder
import org.http4s.Uri.Path.SegmentEncoder

/**
  * A name of an Amazon S3 general purpose bucket.
  *
  * The linked bucket naming rules are all checked,
  * except for the following rule.
  *
  * - Buckets used with Amazon S3 Transfer Acceleration can't have periods (`.`) in their names.
  *
  * @see https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucketnamingrules.html
  */
sealed abstract case class S3Bucket(name: String)

object S3Bucket {
  def apply(name: String): Either[InvalidS3Bucket, S3Bucket] =
    fromName(name)

  def fromName(name: String): Either[InvalidS3Bucket, S3Bucket] =
    if (isValid(name)) Right(new S3Bucket(name) {}) else Left(InvalidS3Bucket(name))

  implicit val s3BucketDecoder: Decoder[S3Bucket] =
    Decoder[String].emap(fromName(_).leftMap(_.getMessage))

  implicit val s3BucketEncoder: Encoder[S3Bucket] =
    Encoder[String].contramap(_.name)

  implicit val s3BucketHash: Hash[S3Bucket] =
    Hash.by(_.name)

  implicit val s3BucketSegmentEncoder: SegmentEncoder[S3Bucket] =
    SegmentEncoder[String].contramap(_.name)

  implicit val s3BucketShow: Show[S3Bucket] =
    Show.show(_.name)

  private def isValid(name: String): Boolean =
    hasValidLength(name) &&
      hasValidChars(name) &&
      hasValidStartAndEndChars(name) &&
      hasNoTwoAdjacentPeriods(name) &&
      isNotAnIpAddress(name) &&
      hasValidPrefixAndSuffix(name)

  /**
    * Returns `true` if the following name rule is
    * true for the specified name; `false` otherwise.
    *
    * - Bucket names must be between 3 (min) and 63 (max) characters long.
    */
  private def hasValidLength(name: String): Boolean =
    3 <= name.length && name.length <= 63

  /**
    * Returns `true` if the following name rule is
    * true for the specified name; `false` otherwise.
    *
    * - Bucket names can consist only of lowercase letters, numbers, periods (`.`), and hyphens (`-`).
    */
  private def hasValidChars(name: String): Boolean =
    name.forall(c => c.isLetterOrDigit || c == '.' || c == '-')

  /**
    * Returns `true` if the following name rule is
    * true for the specified name; `false` otherwise.
    *
    * - Bucket names must begin and end with a letter or number.
    */
  private def hasValidStartAndEndChars(name: String): Boolean =
    name.head.isLetterOrDigit && name.last.isLetterOrDigit

  /**
    * Returns `true` if the following name rule is
    * true for the specified name; `false` otherwise.
    *
    * - Bucket names must not contain two adjacent periods.
    */
  private def hasNoTwoAdjacentPeriods(name: String): Boolean =
    !name.contains("..")

  /**
    * Returns `true` if the following name rule is
    * true for the specified name; `false` otherwise.
    *
    * - Bucket names must not be formatted as an IP address (for example, `192.168.5.4`).
    */
  private def isNotAnIpAddress(name: String): Boolean =
    Ipv4Address.fromString(name).isEmpty

  /**
    * Returns `true` if the following naming rules are
    * true for the specified name; `false` otherwise.
    *
    * - Bucket names must not start with the prefix `xn--`.
    * - Bucket names must not start with the prefix `sthree-`.
    * - Bucket names must not start with the prefix `amzn-s3-demo-`.
    * - Bucket names must not end with the suffix `-s3alias`.
    * - Bucket names must not end with the suffix `--ol-s3`.
    * - Bucket names must not end with the suffix `.mrap`.
    * - Bucket names must not end with the suffix `--x-s3`.
    * - Bucket names must not end with the suffix `--table-s3`.
    */
  private def hasValidPrefixAndSuffix(name: String): Boolean =
    !name.startsWith("xn--") &&
      !name.startsWith("sthree-") &&
      !name.startsWith("amzn-s3-demo-") &&
      !name.endsWith("-s3alias") &&
      !name.endsWith("--ol-s3") &&
      !name.endsWith(".mrap") &&
      !name.endsWith("--x-s3") &&
      !name.endsWith("--table-s3")
}
