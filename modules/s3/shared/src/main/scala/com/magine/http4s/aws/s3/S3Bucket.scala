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
import cats.data.ValidatedNel
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import org.http4s.Uri.Path.SegmentEncoder
import scala.util.matching.Regex

/**
  * A name of an Amazon S3 general purpose bucket.
  *
  * The linked bucket naming rules are all checked,
  * except for the following rule.
  *
  * - Buckets used with Amazon S3 Transfer Acceleration can't have periods (`.`) in their names.
  *
  * In addition, the following rule is checked.
  *
  * - Bucket names must not contain a dash next to a period.
  *
  * The rule is not included in the linked bucket naming
  * rules, but is enforced in practice.
  *
  * @see https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucketnamingrules.html
  */
sealed abstract case class S3Bucket(name: String)

object S3Bucket {
  def apply(name: String): Either[InvalidS3Bucket, S3Bucket] =
    fromName(name)

  def fromName(name: String): Either[InvalidS3Bucket, S3Bucket] =
    (
      checkAdjacentPeriods(name),
      checkChars(name),
      checkDashNextToPeriod(name),
      checkIpAddress(name),
      checkLength(name),
      checkPrefix(name),
      checkStartAndEnd(name),
      checkSuffix(name)
    ).mapN { case _ => new S3Bucket(name) {} }
      .leftMap {
        case errors if errors.tail.isEmpty => errors.head
        case errors => InvalidS3Bucket.Multiple(name, errors)
      }
      .toEither

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

  private val ipAddressRegex: Regex =
    """^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}$""".r

  private val valid: ValidatedNel[InvalidS3Bucket, Unit] =
    ().validNel

  /**
    * Returns `Valid` if the following name rule is
    * true for the specified name; `Invalid` otherwise.
    *
    * - Bucket names must not contain two adjacent periods.
    */
  private def checkAdjacentPeriods(name: String): ValidatedNel[InvalidS3Bucket, Unit] =
    if (!name.contains("..")) valid else InvalidS3Bucket.AdjacentPeriods(name).invalidNel

  /**
    * Returns `Valid` if the following name rule is
    * true for the specified name; `Invalid` otherwise.
    *
    * - Bucket names can consist only of lowercase letters, numbers, periods (`.`), and hyphens (`-`).
    */
  private def checkChars(name: String): ValidatedNel[InvalidS3Bucket, Unit] =
    if (name.forall(c => c.isLower || c.isDigit || c == '.' || c == '-')) valid
    else InvalidS3Bucket.InvalidChars(name).invalidNel

  /**
    * Returns `Valid` if the following name rule is
    * true for the specified name; `Invalid` otherwise.
    *
    * - Bucket names must not contain a dash next to a period.
    */
  private def checkDashNextToPeriod(name: String): ValidatedNel[InvalidS3Bucket, Unit] =
    if (!name.contains(".-") && !name.contains("-.")) valid
    else InvalidS3Bucket.DashNextToPeriod(name).invalidNel

  /**
    * Returns `Valid` if the following name rule is
    * true for the specified name; `Invalid` otherwise.
    *
    * - Bucket names must not be formatted as an IP address (for example, `192.168.5.4`).
    */
  private def checkIpAddress(name: String): ValidatedNel[InvalidS3Bucket, Unit] =
    if (!ipAddressRegex.matches(name)) valid
    else InvalidS3Bucket.IpAddress(name).invalidNel

  /**
    * Returns `Valid` if the following name rule is
    * true for the specified name; `Invalid` otherwise.
    *
    * - Bucket names must be between 3 (min) and 63 (max) characters long.
    */
  private def checkLength(name: String): ValidatedNel[InvalidS3Bucket, Unit] =
    if (3 <= name.length && name.length <= 63) valid
    else InvalidS3Bucket.InvalidLength(name).invalidNel

  /**
    * Returns `Valid` if the following naming rules are
    * true for the specified name; `Invalid` otherwise.
    *
    * - Bucket names must not start with the prefix `xn--`.
    * - Bucket names must not start with the prefix `sthree-`.
    * - Bucket names must not start with the prefix `amzn-s3-demo-`.
    */
  private def checkPrefix(name: String): ValidatedNel[InvalidS3Bucket, Unit] =
    if (
      !name.startsWith("xn--") &&
      !name.startsWith("sthree-") &&
      !name.startsWith("amzn-s3-demo-")
    ) valid
    else InvalidS3Bucket.InvalidPrefix(name).invalidNel

  /**
    * Returns `Valid` if the following name rule is
    * true for the specified name; `Invalid` otherwise.
    *
    * - Bucket names must begin and end with a letter or number.
    */
  private def checkStartAndEnd(name: String): ValidatedNel[InvalidS3Bucket, Unit] =
    if (
      name.headOption.exists(_.isLetterOrDigit) &&
      name.lastOption.exists(_.isLetterOrDigit)
    ) valid
    else InvalidS3Bucket.InvalidStartOrEnd(name).invalidNel

    /**
      * Returns `Valid` if the following naming rules are
      * true for the specified name; `Invalid` otherwise.
      *
      * - Bucket names must not end with the suffix `-s3alias`.
      * - Bucket names must not end with the suffix `--ol-s3`.
      * - Bucket names must not end with the suffix `.mrap`.
      * - Bucket names must not end with the suffix `--x-s3`.
      * - Bucket names must not end with the suffix `--table-s3`.
      */
  private def checkSuffix(name: String): ValidatedNel[InvalidS3Bucket, Unit] =
    if (
      !name.endsWith("-s3alias") &&
      !name.endsWith("--ol-s3") &&
      !name.endsWith(".mrap") &&
      !name.endsWith("--x-s3") &&
      !name.endsWith("--table-s3")
    ) valid
    else InvalidS3Bucket.InvalidSuffix(name).invalidNel
}
