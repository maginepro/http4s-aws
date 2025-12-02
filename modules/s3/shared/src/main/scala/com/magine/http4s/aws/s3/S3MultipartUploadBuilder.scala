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
import cats.effect.MonadCancelThrow
import cats.effect.Resource.ExitCase.Canceled
import cats.effect.Resource.ExitCase.Errored
import cats.effect.Resource.ExitCase.Succeeded
import cats.syntax.all.*
import com.magine.aws.Region
import fs2.Chunk
import fs2.Stream
import fs2.data.xml
import fs2.data.xml.QName
import fs2.data.xml.XmlEvent
import fs2.data.xml.XmlEvent.EndTag
import fs2.data.xml.XmlEvent.StartTag
import fs2.data.xml.XmlEvent.XmlString
import fs2.data.xml.XmlException
import fs2.data.xml.xpath.filter
import fs2.data.xml.xpath.literals.*
import fs2.hashing.Hash
import fs2.hashing.HashAlgorithm
import fs2.hashing.Hashing
import fs2.text
import org.http4s.Charset
import org.http4s.DecodeFailure
import org.http4s.DecodeResult
import org.http4s.Entity
import org.http4s.EntityDecoder
import org.http4s.EntityEncoder
import org.http4s.EntityTag
import org.http4s.Headers
import org.http4s.InvalidMessageBodyFailure
import org.http4s.MalformedMessageBodyFailure
import org.http4s.MediaType
import org.http4s.Method
import org.http4s.ParseFailure
import org.http4s.QueryParamEncoder
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status.Successful
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.headers.ETag
import org.http4s.headers.`Content-Length`
import org.http4s.headers.`Content-Type`

/**
  * Builder helper class for configuring and creating [[S3MultipartUpload]]s.
  *
  * [[S3MultipartUploadBuilder]]s are created by [[S3MultipartUpload.apply]],
  * and [[S3MultipartUploadBuilder#uploadTo]] is used to return an instance
  * of [[S3MultipartUpload]].
  */
sealed trait S3MultipartUploadBuilder[F[_]] {

  /**
    * Returns the content type for the multipart upload
    * if one has been set; otherwise `None`.
    *
    * Default: `None`.
    */
  def contentType: Option[`Content-Type`]

  /**
    * Sets the content type for the multipart upload
    * to the specified one.
    *
    * Default: no content type.
    */
  def withContentType(contentType: `Content-Type`): S3MultipartUploadBuilder[F]

  /**
    * Returns the maximum number of concurrent part uploads.
    *
    * Default: `1`.
    */
  def maxConcurrentUploads: Int

  /**
    * Sets the maximum number of concurrent part uploads
    * to the specified value.
    *
    * If a value below `1` is specified, it will be ignored,
    * and replaced with `1`.
    *
    * Default: `1`.
    */
  def withMaxConcurrentUploads(maxConcurrentUploads: Int): S3MultipartUploadBuilder[F]

  /**
    * Returns the minimum part size in bytes.
    *
    * Default: `5242880` (5 MiB)
    */
  def minPartSizeBytes: Int

  /**
    * Sets the minimum part size in bytes to the specified value.
    *
    * If a value below `5242880` is specified, it will be ignored,
    * and replaced with `5242880`. The limit is enforced by Amazon
    * and applies to all parts but the last one.
    *
    * Note it is also possible to manually chunk the source `Stream`
    * before it is passed to the [[S3MultipartUpload]].
    *
    * Default: `5242880` (5 MiB)
    */
  def withMinPartSizeBytes(minPartSizeBytes: Int): S3MultipartUploadBuilder[F]

  /**
    * Returns the generator of `Uri`s for Amazon S3.
    *
    * Default: [[S3Uri.virtualHostedStyle]]
    */
  def uri: S3Uri

  /**
    * Set the generator of `Uri`s for Amazon S3 to the specified one.
    *
    * Default: [[S3Uri.virtualHostedStyle]]
    */
  def withUri(uri: S3Uri): S3MultipartUploadBuilder[F]

  /**
    * Returns whether the `ETag` for the upload should be verified or not.
    *
    * Default: `true`.
    *
    * @see [[S3MultipartUploadBuilder#withVerifyETag]]
    */
  def verifyETag: Boolean

  /**
    * Sets whether the ETag for the upload should be verified or not.
    *
    * For multipart uploads, the ETag of the uploaded object is an MD5
    * digest of each uploaded part's MD5 digest concatenated, followed
    * by a dash and the total number of uploaded parts.
    *
    * When verification is enabled, the expected ETag value will be
    * calculated and compared after the upload completes. If there
    * is a mismatch, a [[S3MultipartUploadError]] will be raised.
    *
    * Default: `true`.
    *
    * @see https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity-upload.html#ChecksumTypes-Uploads
    */
  def withVerifyETag(verifyETag: Boolean): S3MultipartUploadBuilder[F]

  /**
    * Returns a [[S3MultipartUpload]] which uploads to the specified
    * bucket and key.
    */
  def uploadTo(bucket: S3Bucket, key: S3Key): S3MultipartUpload[F]
}

private[s3] object S3MultipartUploadBuilder {
  def fromSigningClient[F[_]: Concurrent: Hashing](
    client: Client[F],
    region: Region
  ): S3MultipartUploadBuilder[F] =
    S3MultipartUploadBuilderImpl(
      client = client,
      region = region,
      contentType = Defaults.contentType,
      maxConcurrentUploads = Defaults.maxConcurrentUploads,
      minPartSizeBytes = Defaults.minPartSizeBytes,
      uri = Defaults.uri,
      verifyETag = Defaults.verifyETag
    )

  private final case class S3MultipartUploadBuilderImpl[F[_]](
    client: Client[F],
    region: Region,
    override val contentType: Option[`Content-Type`],
    override val maxConcurrentUploads: Int,
    override val minPartSizeBytes: Int,
    override val uri: S3Uri,
    override val verifyETag: Boolean
  )(
    implicit F: Concurrent[F],
    hashing: Hashing[F]
  ) extends S3MultipartUploadBuilder[F] { builder =>
    override def withContentType(contentType: `Content-Type`): S3MultipartUploadBuilder[F] =
      copy(contentType = Some(contentType))

    override def withMaxConcurrentUploads(maxConcurrentUploads: Int): S3MultipartUploadBuilder[F] =
      copy(maxConcurrentUploads = Math.max(1, maxConcurrentUploads))

    override def withMinPartSizeBytes(minPartSizeBytes: Int): S3MultipartUploadBuilder[F] =
      copy(minPartSizeBytes = Math.max(5242880, minPartSizeBytes))

    override def withUri(uri: S3Uri): S3MultipartUploadBuilder[F] =
      copy(uri = uri)

    override def withVerifyETag(verifyETag: Boolean): S3MultipartUploadBuilder[F] =
      copy(verifyETag = verifyETag)

    override def uploadTo(bucket: S3Bucket, key: S3Key): S3MultipartUpload[F] =
      new S3MultipartUpload[F] {
        private val uri: Uri =
          builder.uri(bucket, key, region)

        /**
          * @see https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html
          */
        private def start: F[UploadId] =
          client.expectOr[UploadId](
            Request[F](
              Method.POST,
              uri.withQueryParam("uploads"),
              headers = Headers(contentType)
            )
          )(S3MultipartUploadError.failedToStart)

        /**
          * @see https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html
          */
        private def upload(uploadId: UploadId): ((Chunk[Byte], PartNumber)) => F[UploadedPart] = {
          case (part, partNumber) =>
            client
              .run(
                Request(
                  Method.PUT,
                  uri
                    .withQueryParam("uploadId", uploadId)
                    .withQueryParam("partNumber", partNumber),
                  body = Stream.chunk(part),
                  headers = Headers(`Content-Length`(part.size.toLong))
                )
              )
              .use {
                case Successful(response) =>
                  UploadedPart(part, partNumber, response, uploadId, verifyETag)
                case response =>
                  S3MultipartUploadError
                    .failedToUpload(uploadId, partNumber, response)
                    .flatMap(_.raiseError)
              }
        }

        /**
          * @see https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html
          */
        private def abort(uploadId: UploadId): F[Unit] =
          client.expectOr[Unit](
            Request[F](
              Method.DELETE,
              uri.withQueryParam("uploadId", uploadId)
            )
          )(S3MultipartUploadError.failedToAbort(uploadId))

        /**
          * @see https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html
          */
        private def complete(uploadId: UploadId): Chunk[UploadedPart] => F[EntityTag] =
          parts =>
            client
              .expectOr[CompleteResponse](
                Request[F](Method.POST, uri.withQueryParam("uploadId", uploadId))
                  .withEntity(CompleteRequest(parts))
              )(S3MultipartUploadError.failedToComplete(uploadId, parts.size))
              .flatTap(_.verifyETag[F](parts, uploadId).whenA(verifyETag))
              .map(_.tag)

        override def apply(source: Stream[F, Byte]): Stream[F, EntityTag] =
          Stream
            .bracketCase(start) {
              case (_, Succeeded) => F.unit
              case (uploadId, Canceled | Errored(_)) => abort(uploadId)
            }
            .flatMap { uploadId =>
              source
                .chunkMin(minPartSizeBytes)
                .zip(Stream.iterate(PartNumber.first)(_.next))
                .parEvalMap(maxConcurrentUploads)(upload(uploadId))
                .chunkAll
                .evalMap(complete(uploadId))
            }
      }
  }

  private def xmlEventsDecoder[F[_]: Concurrent]: EntityDecoder[F, Stream[F, XmlEvent]] =
    EntityDecoder.decodeBy(MediaType.application.xml) { media =>
      DecodeResult.successT(
        media.bodyText
          .through(xml.events(includeComments = false))
          .adaptError { case e: XmlException =>
            MalformedMessageBodyFailure(s"Invalid XML (${e.error.name}): ${e.msg}", Some(e))
          }
      )
    }

  private object Defaults {
    val contentType: Option[`Content-Type`] = None

    val maxConcurrentUploads: Int = 1

    val minPartSizeBytes: Int = 5242880

    val uri: S3Uri = S3Uri.virtualHostedStyle

    val verifyETag: Boolean = true
  }

  final case class PartNumber(value: Int) {
    def next: PartNumber =
      PartNumber(value + 1)
  }

  private object PartNumber {
    val first: PartNumber =
      PartNumber(1)

    implicit val queryParamEncoder: QueryParamEncoder[PartNumber] =
      QueryParamEncoder[Int].contramap(_.value)
  }

  final case class UploadId(value: String)

  private object UploadId {
    implicit def entityDecoder[F[_]: Concurrent]: EntityDecoder[F, UploadId] =
      xmlEventsDecoder[F].flatMapR(events =>
        DecodeResult.success(
          events
            .through(filter.first(xpath"/InitiateMultipartUploadResult/UploadId"))
            .collectFirst { case XmlString(uploadId, _) => UploadId(uploadId) }
            .compile
            .onlyOrError
        )
      )

    implicit val queryParamEncoder: QueryParamEncoder[UploadId] =
      QueryParamEncoder[String].contramap(_.value)
  }

  private final case class UploadedPart(
    partNumber: PartNumber,
    tag: EntityTag,
    hash: Option[Hash]
  )

  private object UploadedPart {
    def apply[F[_]: Hashing: MonadCancelThrow](
      part: Chunk[Byte],
      partNumber: PartNumber,
      response: Response[F],
      uploadId: UploadId,
      verifyETag: Boolean
    ): F[UploadedPart] =
      for {
        etag <- response.headers.get[ETag].liftTo[F] {
          ParseFailure("Missing ETag", s"in response to upload of $partNumber for $uploadId")
        }
        hash <-
          if (verifyETag)
            Hashing[F].hasher(HashAlgorithm.MD5).use { hasher =>
              for {
                _ <- hasher.update(part)
                hash <- hasher.hash
              } yield hash.some
            }
          else Option.empty[Hash].pure[F]
      } yield UploadedPart(partNumber, etag.tag, hash)
  }

  private final case class CompleteRequest(parts: Chunk[UploadedPart])

  private object CompleteRequest {
    private val completeMultipartUpload = QName("CompleteMultipartUpload")
    private val startCompleteMultipartUpload = StartTag(completeMultipartUpload, Nil, false)
    private val endCompleteMultipartUpload = EndTag(completeMultipartUpload)

    private val part = QName("Part")
    private val startPart = StartTag(part, Nil, false)
    private val endPart = EndTag(part)

    private val partNumber = QName("PartNumber")
    private val startPartNumber = StartTag(partNumber, Nil, false)
    private val endPartNumber = EndTag(partNumber)

    private val etag = QName("ETag")
    private val startEtag = StartTag(etag, Nil, false)
    private val endEtag = EndTag(etag)

    private def partXmlEvents(part: UploadedPart): Chunk[XmlEvent] =
      Chunk(
        startPart,
        startPartNumber,
        XmlString(part.partNumber.value.toString, false),
        endPartNumber,
        startEtag,
        XmlString(part.tag.toString, false),
        endEtag,
        endPart
      )

    implicit def entityEncoder[F[_]]: EntityEncoder[F, CompleteRequest] =
      EntityEncoder.encodeBy(`Content-Type`(MediaType.application.xml, Charset.`UTF-8`)) {
        case CompleteRequest(parts) =>
          val events =
            Chunk(startCompleteMultipartUpload) ++
              parts.flatMap(partXmlEvents) ++
              Chunk(endCompleteMultipartUpload)

          val bytes =
            Stream
              .chunk(events)
              .through(xml.render.raw())
              .through(text.utf8.encode)
              .compile
              .to(Chunk)

          Entity(Stream.chunk(bytes), Some(bytes.size.toLong))
      }
  }

  private final case class CompleteResponse(tag: EntityTag) {
    def verifyETag[F[_]](
      parts: Chunk[UploadedPart],
      uploadId: UploadId
    )(
      implicit F: MonadCancelThrow[F],
      hashing: Hashing[F]
    ): F[Unit] = {
      val expectedETag =
        Hashing[F].hasher(HashAlgorithm.MD5).use { hasher =>
          for {
            _ <- parts.traverseVoid(_.hash.traverseVoid(hash => hasher.update(hash.bytes)))
            hash <- hasher.hash.map(_.bytes.toByteVector.toHex)
          } yield s"\"$hash-${parts.size}\""
        }

      val responseETag =
        tag.renderString

      expectedETag.flatMap {
        case expectedETag if responseETag === expectedETag => F.unit
        case expectedETag =>
          S3MultipartUploadError
            .failedToVerifyETag(
              expectedETag = expectedETag,
              responseETag = responseETag,
              uploadId = uploadId
            )
            .raiseError
      }
    }
  }

  private object CompleteResponse {
    private def fromETagString(etag: String): Either[DecodeFailure, CompleteResponse] =
      ETag
        .parse(etag)
        .map(etag => CompleteResponse(etag.tag))
        .leftMap(e => InvalidMessageBodyFailure(s"Invalid ETag: $etag", Some(e)))

    implicit def entityDecoder[F[_]: Concurrent]: EntityDecoder[F, CompleteResponse] =
      xmlEventsDecoder[F].flatMapR(events =>
        DecodeResult.success(
          events
            .through(filter.first(xpath"/CompleteMultipartUploadResult/ETag"))
            .collectFirst { case XmlString(etag, _) => fromETagString(etag) }
            .rethrow
            .compile
            .onlyOrError
        )
      )
  }
}
