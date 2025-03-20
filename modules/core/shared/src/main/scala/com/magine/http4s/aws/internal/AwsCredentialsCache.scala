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

import cats.effect.Sync
import cats.syntax.all.*
import com.magine.http4s.aws.MfaSerial
import fs2.io.IOException
import io.circe.Decoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.Printer
import io.circe.parser.decode
import io.circe.syntax.*
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.security.MessageDigest

/**
  * Capability to read and write credentials in `~/.aws/cli/cache`.
  */
private[aws] trait AwsCredentialsCache[F[_]] {
  def read(profile: AwsProfile): F[Option[AwsAssumedRole]]

  def write(assumedRole: AwsAssumedRole): F[Unit]
}

private[aws] object AwsCredentialsCache {
  def default[F[_]: Sync]: AwsCredentialsCache[F] =
    new AwsCredentialsCache[F] {
      private def ensurePathExists(path: Path): F[Path] =
        Sync[F].blocking(
          if (path.toFile.exists) path
          else Files.createDirectories(path)
        )

      private def existingCachePath: F[Path] =
        Sync[F]
          .delay(Option(System.getProperty("user.home")).map(Paths.get(_, ".aws/cli/cache")))
          .flatMap(_.traverse(ensurePathExists))
          .flatMap {
            case Some(path) => path.pure
            case None => new RuntimeException("Missing system property user.home").raiseError
          }

      private def writeFile(path: Path, json: Json): F[Unit] =
        Sync[F].blocking(Files.write(path, json.spaces2.getBytes(StandardCharsets.UTF_8))).void

      override def read(profile: AwsProfile): F[Option[AwsAssumedRole]] =
        FileName.fromProfile(profile).flatMap { fileName =>
          existingCachePath.map(_.resolve(fileName.path)).flatMap { path =>
            implicit val decoder: Decoder[AwsAssumedRole] =
              AwsAssumedRole.decoder(fileName)

            Sync[F]
              .blocking(new String(Files.readAllBytes(path), StandardCharsets.UTF_8))
              .flatMap(decode[AwsAssumedRole](_).map(_.some).liftTo[F])
              .recover { case _: IOException => none }
          }
        }

      override def write(assumedRole: AwsAssumedRole): F[Unit] =
        existingCachePath
          .map(_.resolve(assumedRole.cacheFileName.path))
          .flatMap(writeFile(_, assumedRole.asJson))
    }

  /**
    * The file name of a credentials file in `~/.aws/cli/cache`.
    */
  final case class FileName(path: Path)

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
    ): F[FileName] =
      Sync[F].delay {
        def sha1Hex(s: String): String =
          MessageDigest
            .getInstance("SHA-1")
            .digest(s.getBytes(StandardCharsets.UTF_8))
            .map("%02x".format(_))
            .mkString

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

        FileName(Paths.get(s"$hash.json"))
      }

    def fromProfile[F[_]: Sync](profile: AwsProfile): F[FileName] =
      apply(
        roleArn = profile.roleArn,
        roleSessionName = profile.roleSessionName,
        durationSeconds = profile.durationSeconds,
        mfaSerial = profile.mfaSerial
      )
  }
}
