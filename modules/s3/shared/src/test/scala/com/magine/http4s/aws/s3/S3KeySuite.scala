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

import com.magine.http4s.aws.s3.syntax.*
import munit.ScalaCheckSuite
import org.http4s.Uri.Path
import org.http4s.syntax.all.*
import org.scalacheck.Gen
import org.scalacheck.Prop

final class S3KeySuite extends ScalaCheckSuite {
  test("empty") {
    checkInvalid(Path.empty, InvalidS3Key.Empty(_))
  }

  test("encode") {
    assertEquals(
      S3Key(path"test#123.txt").map(_.path),
      Right(path"test%23123.txt")
    )
  }

  test("normalize.invalid") {
    checkInvalid(path"///", InvalidS3Key.Empty(_))
  }

  test("normalize.valid") {
    assertEquals(S3Key(path"/a//bc/"), Right(key"a/bc"))
  }

  test("root") {
    checkInvalid(Path.Root, InvalidS3Key.Empty(_))
  }

  test("tooLong") {
    val gen =
      for {
        length <- Gen.chooseNum(1025, 2048)
        chars <- Gen.listOfN(length, Gen.alphaNumChar)
        path = Path.unsafeFromString(chars.mkString)
      } yield path

    Prop.forAll(gen)(checkInvalid(_, InvalidS3Key.TooLong(_)))
  }

  test("valid") {
    val gen =
      for {
        length <- Gen.chooseNum(1, 1024)
        chars <- Gen.listOfN(length, Gen.alphaNumChar)
        path = Path.unsafeFromString(chars.mkString)
      } yield path

    Prop.forAll(gen)(checkValid)
  }

  private def checkInvalid(path: Path, error: Path => InvalidS3Key): Unit =
    assertEquals(S3Key(path), Left(error(path.normalize)))

  private def checkValid(path: Path): Unit =
    assert(S3Key(path).isRight)
}
