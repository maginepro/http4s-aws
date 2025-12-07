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

import org.typelevel.literally.Literally

object syntax {
  implicit final class S3StringContext(val sc: StringContext) extends AnyVal {
    def bucket(args: Any*): S3Bucket = macro S3BucketLiteral.make

    def key(args: Any*): S3Key = macro S3KeyLiteral.make
  }

  object S3BucketLiteral extends Literally[S3Bucket] {
    def validate(c: Context)(s: String) = {
      import c.universe._
      S3Bucket(s) match {
        case Right(_) => Right(c.Expr(q"_root_.com.magine.http4s.aws.s3.S3Bucket($s).toOption.get"))
        case Left(e: InvalidS3Bucket.Multiple) => Left(s"invalid S3Bucket:\n${e.details}")
        case Left(e) => Left(s"invalid S3Bucket: ${e.details}")
      }
    }

    def make(c: Context)(args: c.Expr[Any]*): c.Expr[S3Bucket] =
      apply(c)(args: _*)
  }

  object S3KeyLiteral extends Literally[S3Key] {
    def validate(c: Context)(s: String) = {
      import c.universe._
      S3Key(s) match {
        case Right(_) =>
          Right(
            c.Expr(
              q"_root_.com.magine.http4s.aws.s3.S3Key($s).toOption.get"
            )
          )
        case Left(e) => Left(s"invalid S3Key: ${e.details}")
      }
    }

    def make(c: Context)(args: c.Expr[Any]*): c.Expr[S3Key] =
      apply(c)(args: _*)
  }
}
