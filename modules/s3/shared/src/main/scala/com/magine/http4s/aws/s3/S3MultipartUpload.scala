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

import cats.effect.Concurrent
import cats.effect.Temporal
import com.magine.aws.Region
import com.magine.http4s.aws.AwsServiceName
import com.magine.http4s.aws.AwsSigningClient
import com.magine.http4s.aws.CredentialsProvider
import fs2.Pipe
import fs2.Stream
import fs2.hashing.Hashing
import org.http4s.EntityTag
import org.http4s.client.Client

/**
  * Capability to perform S3 multipart uploads of `Stream[F, Byte]`s.
  *
  * Uploads are typically used with the `through` operation on `Stream`.
  */
trait S3MultipartUpload[F[_]] extends Pipe[F, Byte, EntityTag] {

  /**
    * Performs an S3 multipart upload of the specified source
    * stream and then returns the `EntityTag` for the upload.
    */
  override def apply(source: Stream[F, Byte]): Stream[F, EntityTag]
}

object S3MultipartUpload {

  /**
    * Alias for [[S3MultipartUpload.fromClient]].
    */
  def apply[F[_]: Hashing: Temporal](
    client: Client[F],
    provider: CredentialsProvider[F],
    region: Region
  ): S3MultipartUploadBuilder[F] =
    fromClient(client, provider, region)

  /**
    * Returns an [[S3MultipartUploadBuilder]] which performs
    * S3 multipart uploads using the specified `Client`.
    *
    * Requests will be signed using credentials from the
    * provided `CredentialsProvider` and target `Region`.
    */
  def fromClient[F[_]: Hashing: Temporal](
    client: Client[F],
    provider: CredentialsProvider[F],
    region: Region
  ): S3MultipartUploadBuilder[F] =
    fromSigningClient(AwsSigningClient(provider, region, AwsServiceName.S3)(client), region)

  /**
    * Returns an [[S3MultipartUploadBuilder]] which performs
    * S3 multipart uploads using the specified `Client`.
    *
    * Note the provided `Client` is responsible for signing
    * requests using credentials and the target `Region`.
    */
  def fromSigningClient[F[_]: Concurrent: Hashing](
    client: Client[F],
    region: Region
  ): S3MultipartUploadBuilder[F] =
    S3MultipartUploadBuilder.fromSigningClient(client, region)
}
