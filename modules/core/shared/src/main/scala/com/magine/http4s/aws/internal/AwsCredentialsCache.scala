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

package com.magine.http4s.aws.internal

import cats.effect.Async
import cats.effect.Ref
import cats.effect.Sync
import cats.syntax.all.*
import com.magine.http4s.aws.MfaSerial
import fs2.Chunk
import fs2.Stream
import fs2.hashing.HashAlgorithm
import fs2.hashing.Hashing
import fs2.io.file.Files
import fs2.io.file.NoSuchFileException
import fs2.io.file.Path
import fs2.text.utf8
import io.circe.Decoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.Printer
import io.circe.parser.decode
import io.circe.syntax.*
import java.nio.charset.StandardCharsets.UTF_8

/**
  * Capability to read and write credentials in `~/.aws/cli/cache`.
  */
private[aws] trait AwsCredentialsCache[F[_]] {
  def read(profile: AwsProfileResolved): F[Option[AwsAssumedRole]]

  def write(assumedRole: AwsAssumedRole): F[Unit]
}

private[aws] object AwsCredentialsCache {
  def default[F[_]: Async]: AwsCredentialsCache[F] =
    new AwsCredentialsCache[F] {
      private val files: Files[F] = Files.forAsync[F]

      private def ensurePathExists(path: Path): F[Path] =
        files.exists(path).ifM(path.pure, files.createDirectories(path).as(path))

      private def cachePath: F[Path] =
        files.userHome.map(_.resolve(".aws/cli/cache"))

      private def existingCachePath: F[Path] =
        cachePath.flatMap(ensurePathExists)

      private def writeFile(path: Path, json: Json): F[Unit] =
        Stream(json.spaces2).through(files.writeUtf8(path)).compile.drain

      override def read(profile: AwsProfileResolved): F[Option[AwsAssumedRole]] =
        FileName.fromProfile(profile).flatMap { fileName =>
          cachePath.map(_.resolve(fileName.path)).flatMap { path =>
            implicit val decoder: Decoder[AwsAssumedRole] =
              AwsAssumedRole.decoder(fileName)

            files
              .readAll(path)
              .through(utf8.decode)
              .compile
              .foldMonoid
              .flatMap(decode[AwsAssumedRole](_).map(_.some).liftTo[F])
              .recover { case _: NoSuchFileException => none }
          }
        }

      override def write(assumedRole: AwsAssumedRole): F[Unit] =
        existingCachePath
          .map(_.resolve(assumedRole.cacheFileName.path))
          .flatMap(writeFile(_, assumedRole.asJson))
    }

  def empty[F[_]: Sync]: F[AwsCredentialsCache[F]] =
    Ref[F].of(Map.empty[FileName, AwsAssumedRole]).map(fromRef[F])

  def one[F[_]: Sync](profile: AwsProfileResolved, assumedRole: AwsAssumedRole): F[AwsCredentialsCache[F]] =
    for {
      fileName <- FileName.fromProfile(profile)
      ref <- Ref[F].of(Map(fileName -> assumedRole))
    } yield fromRef(ref)

  def option[F[_]: Sync](
    profile: AwsProfileResolved,
    assumedRole: Option[AwsAssumedRole]
  ): F[AwsCredentialsCache[F]] =
    assumedRole.map(one(profile, _)).getOrElse(empty)

  def fromRef[F[_]: Sync](ref: Ref[F, Map[FileName, AwsAssumedRole]]): AwsCredentialsCache[F] =
    new AwsCredentialsCache[F] {
      override def read(profile: AwsProfileResolved): F[Option[AwsAssumedRole]] =
        FileName.fromProfile(profile).flatMap(fileName => ref.get.map(_.get(fileName)))

      override def write(assumedRole: AwsAssumedRole): F[Unit] =
        ref.update(_.updated(assumedRole.cacheFileName, assumedRole))
    }

  /**
    * The file name of a credentials file in `~/.aws/cli/cache`.
    */
  sealed abstract case class FileName(path: Path)

  object FileName {

    /**
      * Generates a file name for a credentials file based on
      * parts of an [[AwsProfile]].
      *
      * At the time of writing, the file name is generated in
      * the same way as done by the `aws` cli, although this
      * is subject to change.
      */
    def apply[F[_]: Sync](
      roleArn: AwsProfile.RoleArn,
      roleSessionName: AwsProfile.RoleSessionName,
      durationSeconds: Option[AwsProfile.DurationSeconds],
      mfaSerial: MfaSerial
    ): F[FileName] = {
      val json =
        JsonObject
          .fromIterable(
            List(
              durationSeconds.map("DurationSeconds" -> _.value.asJson),
              Some("RoleArn" -> roleArn.value.asJson),
              Some("RoleSessionName" -> roleSessionName.value.asJson),
              Some("SerialNumber" -> mfaSerial.value.asJson)
            ).flatten
          )
          .toJson

      val hash =
        sha1Hex(
          json.printWith(
            Printer.noSpaces.copy(
              colonRight = " ",
              objectCommaRight = " "
            )
          )
        )

      hash.map(hash => new FileName(Path(s"$hash.json")) {})
    }

    def fromProfile[F[_]: Sync](profile: AwsProfileResolved): F[FileName] =
      FileName(
        roleArn = profile.roleArn,
        roleSessionName = profile.roleSessionName,
        durationSeconds = profile.durationSeconds,
        mfaSerial = profile.mfaSerial
      )

    private def sha1Hex[F[_]: Sync](s: String): F[String] =
      Hashing.forSync[F].hasher(HashAlgorithm.SHA1).use { hasher =>
        for {
          _ <- hasher.update(Chunk.array(s.getBytes(UTF_8)))
          hash <- hasher.hash.map(_.bytes.toArray)
        } yield Hex.encodeHex(hash)
      }
  }
}
