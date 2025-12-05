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

import cats.data.NonEmptyList
import scala.util.control.NoStackTrace

sealed abstract class InvalidS3Bucket(val name: String) extends RuntimeException with NoStackTrace {
  def details: String

  override final def getMessage: String =
    message

  def message: String =
    if (name.isEmpty) s"Invalid S3Bucket: $details"
    else s"Invalid S3Bucket: $name: $details"
}

object InvalidS3Bucket {
  private[s3] final case class AdjacentPeriods(override val name: String) extends InvalidS3Bucket(name) {
    override def details: String =
      s"the bucket name must not contain two adjacent periods"
  }

  private[s3] final case class DashNextToPeriod(override val name: String) extends InvalidS3Bucket(name) {
    override def details: String =
      s"the bucket name must not contain a dash next to a period"
  }

  private[s3] final case class IpAddress(override val name: String) extends InvalidS3Bucket(name) {
    override def details: String =
      s"the bucket name must not be formatted as an IP address (for example, 192.168.5.4)"
  }

  private[s3] final case class InvalidChars(override val name: String) extends InvalidS3Bucket(name) {
    override def details: String =
      s"the bucket name can consist only of lowercase letters, numbers, periods (.), and hyphens (-)"
  }

  private[s3] final case class InvalidLength(override val name: String) extends InvalidS3Bucket(name) {
    override def details: String =
      s"the bucket name must be between 3 (min) and 63 (max) characters long"
  }

  private[s3] final case class InvalidPrefix(override val name: String) extends InvalidS3Bucket(name) {
    override def details: String =
      s"the bucket name must not start with the prefixes: xn--, sthree-, amzn-s3-demo-"
  }

  private[s3] final case class InvalidStartOrEnd(override val name: String) extends InvalidS3Bucket(name) {
    override def details: String =
      s"the bucket name must begin and end with a letter or number"
  }

  private[s3] final case class InvalidSuffix(override val name: String) extends InvalidS3Bucket(name) {
    override def details: String =
      s"the bucket name must not end with the suffixes: -s3alias, --ol-s3, .mrap, --x-s3, --table-s3"
  }

  private[s3] final case class Multiple(
    override val name: String,
    errors: NonEmptyList[InvalidS3Bucket]
  ) extends InvalidS3Bucket(name) {
    override def message: String =
      if (name.isEmpty) s"Invalid S3Bucket:\n$details"
      else s"Invalid S3Bucket: $name:\n$details"

    override def details: String =
      errors.toList.map(error => s"- ${error.details}").mkString("", ",\n", "")
  }
}
