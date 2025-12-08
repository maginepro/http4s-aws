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

import com.magine.aws.Region
import org.http4s.Query
import org.http4s.Uri
import org.http4s.Uri.Authority
import org.http4s.Uri.Path
import org.http4s.Uri.Scheme

/**
  * Capability to generate `Uri`s for Amazon S3, when a
  * [[S3Bucket]], [[S3Key]], and `Region` are provided.
  *
  * The following generators are available:
  *
  * - [[S3Uri.pathStyle]] for path-style requests (bucket in path),
  * - [[S3Uri.virtualHostedStyle]] for virtual-hosted-style requests.
  *
  * @see https://docs.aws.amazon.com/AmazonS3/latest/userguide/VirtualHosting.html
  */
trait S3Uri {

  /**
    * Returns a `Uri` for Amazon S3 for the specified
    * bucket, key, and region.
    */
  def apply(
    bucket: S3Bucket,
    key: S3Key,
    region: Region
  ): Uri
}

object S3Uri {

  /**
    * Returns a new [[S3Uri]] instance which generates
    * `Uri`s for Amazon S3 using the provided function.
    */
  def apply(uri: (S3Bucket, S3Key, Region) => Uri): S3Uri =
    new S3Uri {
      override def apply(
        bucket: S3Bucket,
        key: S3Key,
        region: Region
      ): Uri =
        uri(bucket, key, region)
    }

  /**
    * Generates `Uri`s for Amazon S3 for path-style requests.
    *
    * The path-style `Uri`s use the following format.
    *
    * `https://s3.region-code.amazonaws.com/bucket-name/key-name`
    *
    * @see https://docs.aws.amazon.com/AmazonS3/latest/userguide/VirtualHosting.html#path-style-access
    */
  val pathStyle: S3Uri =
    S3Uri { case (bucket, key, region) =>
      Uri(
        scheme = Some(Scheme.https),
        authority = Some(
          Authority(
            userInfo = None,
            host = Uri.RegName(s"s3.$region.amazonaws.com"),
            port = None,
          )
        ),
        path = (Path.Root / bucket).concat(key.path),
        query = Query.empty,
        fragment = None
      )
    }

  /**
    * Generates `Uri`s for Amazon S3 for virtual-hosted-style requests.
    *
    * The virtual-hosted–style `Uri`s use the following format.
    *
    * `https://bucket-name.s3.region-code.amazonaws.com/key-name`
    *
    * @see https://docs.aws.amazon.com/AmazonS3/latest/userguide/VirtualHosting.html#virtual-hosted-style-access
    */
  val virtualHostedStyle: S3Uri =
    S3Uri { case (bucket, key, region) =>
      Uri(
        scheme = Some(Scheme.https),
        authority = Some(
          Authority(
            userInfo = None,
            host = Uri.RegName(s"${bucket.name}.s3.$region.amazonaws.com"),
            port = None,
          )
        ),
        path = key.path,
        query = Query.empty,
        fragment = None
      )
    }
}
