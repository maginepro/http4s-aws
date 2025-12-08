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

import cats.parse.Numbers
import cats.parse.Parser
import com.magine.http4s.aws.s3.syntax.*
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop

final class S3BucketSuite extends ScalaCheckSuite {
  test("dashNextToPeriod") {
    val gen =
      for {
        length <- Gen.chooseNum(1, 61)
        chars <- Gen.listOfN(length, validCharGen)
        index <- Gen.chooseNum(0, length - 1)
        patch <- Gen.oneOf(".-", "-.")
      } yield chars.mkString.patch(index, patch, 0)

    Prop.forAll(gen)(checkInvalid(InvalidS3Bucket.DashNextToPeriod(_)))
  }

  test("empty") {
    checkInvalid(InvalidS3Bucket.InvalidLength(_))("")
  }

  test("invalidChars") {
    val gen =
      for {
        invalid <- invalidCharGen
        length <- Gen.chooseNum(3, 63)
        chars <- Gen.listOfN(length, validCharGen)
        index <- Gen.chooseNum(0, length - 1)
      } yield chars.updated(index, invalid).mkString

    Prop.forAll(gen)(checkInvalid(InvalidS3Bucket.InvalidChars(_)))
  }

  test("invalidEnd") {
    val gen =
      for {
        end <- periodOrDashGen
        length <- Gen.chooseNum(2, 62)
        chars <- Gen.listOfN(length, validCharGen)
      } yield (chars :+ end).mkString

    Prop.forAll(gen)(checkInvalid(InvalidS3Bucket.InvalidStartOrEnd(_)))
  }

  test("invalidPrefix") {
    val gen =
      for {
        prefix <- Gen.oneOf("xn--", "sthree-", "amzn-s3-demo-")
        length <- Gen.chooseNum(0, 63 - prefix.length)
        chars <- Gen.listOfN(length, validCharGen)
      } yield prefix ++ chars.mkString

    Prop.forAll(gen)(checkInvalid(InvalidS3Bucket.InvalidPrefix(_)))
  }

  test("invalidStart") {
    val gen =
      for {
        start <- periodOrDashGen
        length <- Gen.chooseNum(2, 62)
        chars <- Gen.listOfN(length, validCharGen)
      } yield (start :: chars).mkString

    Prop.forAll(gen)(checkInvalid(InvalidS3Bucket.InvalidStartOrEnd(_)))
  }

  test("invalidSuffix") {
    val gen =
      for {
        suffix <- Gen.oneOf("-s3alias", "--ol-s3", ".mrap", "--x-s3", "--table-s3")
        length <- Gen.chooseNum(0, 63 - suffix.length)
        chars <- Gen.listOfN(length, validCharGen)
      } yield chars.mkString ++ suffix

    Prop.forAll(gen)(checkInvalid(InvalidS3Bucket.InvalidSuffix(_)))
  }

  test("ipAddress") {
    val gen =
      Gen
        .listOfN(4, Gen.chooseNum(0, 999))
        .map(_.mkString("."))

    Prop.forAll(gen)(checkInvalid(InvalidS3Bucket.IpAddress(_)))
  }

  test("syntax") {
    assertEquals(
      S3Bucket("example-123.bucket"),
      Right(bucket"example-123.bucket")
    )
  }

  test("tooLong") {
    val gen =
      for {
        length <- Gen.chooseNum(64, 128)
        chars <- Gen.listOfN(length, validCharGen)
      } yield chars.mkString

    Prop.forAll(gen)(checkInvalid(InvalidS3Bucket.InvalidLength(_)))
  }

  test("tooShort") {
    val gen =
      for {
        length <- Gen.chooseNum(0, 2)
        chars <- Gen.listOfN(length, validCharGen)
      } yield chars.mkString

    Prop.forAll(gen)(checkInvalid(InvalidS3Bucket.InvalidLength(_)))
  }

  test("twoPeriods") {
    val gen =
      for {
        length <- Gen.chooseNum(1, 61)
        chars <- Gen.listOfN(length, validCharGen)
        index <- Gen.chooseNum(0, length - 1)
      } yield chars.mkString.patch(index, "..", 0)

    Prop.forAll(gen)(checkInvalid(InvalidS3Bucket.AdjacentPeriods(_)))
  }

  test("valid") {
    Prop.forAll(validGen)(checkValid)
  }

  private def checkInvalid(error: String => InvalidS3Bucket)(name: String): Unit =
    S3Bucket(name) match {
      case Left(InvalidS3Bucket.Multiple(_, errors)) =>
        val expected = error(name)
        assert(errors.exists(_ == expected))
      case Left(invalid) =>
        assertEquals(invalid, error(name))
      case Right(bucket) =>
        fail(s"expected ${error(name)}, got $bucket")
    }

  private def checkValid(name: String): Unit =
    assert(S3Bucket(name).isRight)

  private val periodOrDashGen: Gen[Char] =
    Gen.oneOf('.', '-')

  private val invalidCharGen: Gen[Char] =
    arbitrary[Char].filterNot(c => c.isLower || c.isDigit || c == '.' || c == '-')

  private val validCharGen: Gen[Char] =
    Gen.frequency(
      9 -> Gen.oneOf(Gen.alphaLowerChar, Gen.numChar),
      1 -> periodOrDashGen
    )

  private val ipAddressParser: Parser[String] = {
    val group = Numbers.digit.rep(1, 3)
    val dot = Parser.char('.')
    group.repSep(4, 4, dot).string
  }

  private def isIpAddress(name: String): Boolean =
    ipAddressParser.parseAll(name).isRight

  private val validGen: Gen[String] =
    for {
      length <- Gen.chooseNum(3, 63)
      name <- Gen.listOfN(length, validCharGen).map(_.mkString)
      if name.head.isLetterOrDigit
      if name.last.isLetterOrDigit
      if !name.contains("..")
      if !name.contains(".-") && !name.contains("-.")
      if !isIpAddress(name)
      if !name.startsWith("xn--")
      if !name.startsWith("sthree-")
      if !name.startsWith("amzn-s3-demo-")
      if !name.endsWith("-s3alias")
      if !name.endsWith("--ol-s3")
      if !name.endsWith(".mrap")
      if !name.endsWith("--x-s3")
      if !name.endsWith("--table-s3")
    } yield name
}
