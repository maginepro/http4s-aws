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

import cats.effect.Temporal
import com.magine.aws.Region
import com.magine.http4s.aws.AwsServiceName
import com.magine.http4s.aws.AwsSigningClient
import com.magine.http4s.aws.CredentialsProvider
import fs2.hashing.Hashing
import org.http4s.Method
import org.http4s.Request
import org.http4s.Uri
import org.http4s.Uri.Path
import org.http4s.client.Client

final case class LocalStack[F[_]: Hashing: Temporal](
  client: Client[F],
  provider: CredentialsProvider[F],
  region: Region,
  uri: Uri
) {
  def createBucket(bucket: S3Bucket): F[Unit] =
    signingClient.expect[Unit](Request[F](Method.PUT, uri.withPath(Path.Root / bucket)))

  def multipartUpload(bucket: S3Bucket, key: S3Key): S3MultipartUpload[F] =
    S3MultipartUpload(client, provider, region).withUri(s3Uri).uploadTo(bucket, key)

  def s3Uri: S3Uri =
    S3Uri { case (bucket, key, _) =>
      uri.withPath((Path.Root / bucket).concat(key.path))
    }

  def signingClient: Client[F] =
    AwsSigningClient(provider, region, AwsServiceName.S3)(client)
}
