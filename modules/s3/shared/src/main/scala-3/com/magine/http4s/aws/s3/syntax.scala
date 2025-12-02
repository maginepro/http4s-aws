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

import org.http4s.Uri
import org.typelevel.literally.Literally

object syntax {
  extension (inline ctx: StringContext) {
    inline def bucket(inline args: Any*): S3Bucket =
      ${ S3BucketLiteral('ctx, 'args) }

    inline def key(inline args: Any*): S3Key =
      ${ S3KeyLiteral('ctx, 'args) }
  }

  object S3BucketLiteral extends Literally[S3Bucket] {
    def validate(
      s: String
    )(
      using Quotes
    ) =
      S3Bucket(s) match {
        case Right(_) => Right('{ S3Bucket(${ Expr(s) }).toOption.get })
        case Left(_) => Left("invalid S3Bucket")
      }
  }

  object S3KeyLiteral extends Literally[S3Key] {
    def validate(
      s: String
    )(
      using Quotes
    ) =
      S3Key(Uri.Path.unsafeFromString(s)) match {
        case Right(_) => Right('{ S3Key(Uri.Path.unsafeFromString(${ Expr(s) })).toOption.get })
        case Left(_) => Left("invalid S3Key")
      }
  }
}
