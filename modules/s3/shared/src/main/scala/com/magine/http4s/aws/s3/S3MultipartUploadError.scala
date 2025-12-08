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
import cats.syntax.all.*
import com.magine.http4s.aws.s3.S3MultipartUploadBuilder.PartNumber
import com.magine.http4s.aws.s3.S3MultipartUploadBuilder.UploadId
import org.http4s.Response

final case class S3MultipartUploadError(message: String) extends RuntimeException {
  override def getMessage: String =
    message
}

private[s3] object S3MultipartUploadError {
  def failedToAbort[F[_]: Concurrent](
    uploadId: UploadId
  )(
    response: Response[F]
  ): F[Throwable] =
    fromResponse(response)(bodyText =>
      s"Failed to abort multipart upload for $uploadId: response status ${response.status.code}: $bodyText"
    )

  def failedToComplete[F[_]: Concurrent](
    uploadId: UploadId,
    parts: Int
  )(
    response: Response[F]
  ): F[Throwable] =
    fromResponse(response)(bodyText =>
      s"Failed to complete multipart upload for $uploadId with $parts parts: response status ${response.status.code}: $bodyText"
    )

  def failedToStart[F[_]: Concurrent](response: Response[F]): F[Throwable] =
    fromResponse(response)(bodyText =>
      s"Failed to start multipart upload: response status ${response.status.code}: $bodyText"
    )

  def failedToUpload[F[_]: Concurrent](
    uploadId: UploadId,
    partNumber: PartNumber,
    response: Response[F]
  ): F[Throwable] =
    fromResponse(response)(bodyText =>
      s"Failed to upload $partNumber for $uploadId: response status ${response.status.code}: $bodyText"
    )

  def failedToVerifyETag(
    expectedETag: String,
    responseETag: String,
    uploadId: UploadId
  ): Throwable =
    S3MultipartUploadError(
      s"Failed to verify ETag for $uploadId: response ETag $responseETag did not match expected ETag $expectedETag"
    )

  private def fromResponse[F[_]: Concurrent](
    response: Response[F]
  )(
    message: String => String
  ): F[Throwable] =
    response.bodyText.compile.string.map(bodyText => S3MultipartUploadError(message(bodyText)))
}
