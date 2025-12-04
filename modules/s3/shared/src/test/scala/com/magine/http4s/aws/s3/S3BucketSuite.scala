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

import com.comcast.ip4s.Ipv4Address
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop

final class S3BucketSuite extends ScalaCheckSuite {
  test("empty") {
    checkInvalid("")
  }

  test("invalidChars") {
    val gen =
      for {
        invalid <- invalidCharGen
        length <- Gen.chooseNum(3, 63)
        chars <- Gen.listOfN(length, validCharGen)
        index <- Gen.chooseNum(0, length - 1)
      } yield chars.updated(index, invalid).mkString

    Prop.forAll(gen)(checkInvalid)
  }

  test("invalidEnd") {
    val gen =
      for {
        end <- periodOrDashGen
        length <- Gen.chooseNum(2, 62)
        chars <- Gen.listOfN(length, validCharGen)
      } yield (chars :+ end).mkString

    Prop.forAll(gen)(checkInvalid)
  }

  test("invalidPrefix") {
    val gen =
      for {
        prefix <- Gen.oneOf("xn--", "sthree-", "amzn-s3-demo-")
        length <- Gen.chooseNum(0, 63 - prefix.length)
        chars <- Gen.listOfN(length, validCharGen)
      } yield prefix ++ chars.mkString

    Prop.forAll(gen)(checkInvalid)
  }

  test("invalidStart") {
    val gen =
      for {
        start <- periodOrDashGen
        length <- Gen.chooseNum(2, 62)
        chars <- Gen.listOfN(length, validCharGen)
      } yield (start :: chars).mkString

    Prop.forAll(gen)(checkInvalid)
  }

  test("invalidSuffix") {
    val gen =
      for {
        suffix <- Gen.oneOf("-s3alias", "--ol-s3", ".mrap", "--x-s3", "--table-s3")
        length <- Gen.chooseNum(0, 63 - suffix.length)
        chars <- Gen.listOfN(length, validCharGen)
      } yield chars.mkString ++ suffix

    Prop.forAll(gen)(checkInvalid)
  }

  test("ipAddress") {
    val gen =
      Gen
        .listOfN(4, Gen.chooseNum(0, 255))
        .map(_.mkString("."))

    Prop.forAll(gen)(checkInvalid)
  }

  test("tooLong") {
    val gen =
      for {
        length <- Gen.chooseNum(64, 128)
        chars <- Gen.listOfN(length, validCharGen)
      } yield chars.mkString

    Prop.forAll(gen)(checkInvalid)
  }

  test("tooShort") {
    val gen =
      for {
        length <- Gen.chooseNum(0, 2)
        chars <- Gen.listOfN(length, validCharGen)
      } yield chars.mkString

    Prop.forAll(gen)(checkInvalid)
  }

  test("twoPeriods") {
    val gen =
      for {
        length <- Gen.chooseNum(1, 61)
        chars <- Gen.listOfN(length, validCharGen)
        index <- Gen.chooseNum(0, length - 1)
      } yield chars.mkString.patch(index, "..", 0)

    Prop.forAll(gen)(checkInvalid)
  }

  test("valid") {
    Prop.forAll(validGen)(checkValid)
  }

  private def checkInvalid(name: String): Unit =
    assert(S3Bucket(name).isLeft)

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

  private val validGen: Gen[String] =
    for {
      length <- Gen.chooseNum(3, 63)
      name <- Gen.listOfN(length, validCharGen).map(_.mkString)
      if name.head.isLetterOrDigit
      if name.last.isLetterOrDigit
      if !name.contains("..")
      if Ipv4Address.fromString(name).isEmpty
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
