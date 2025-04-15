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
import cats.parse.Parser
import cats.syntax.all.*
import com.magine.http4s.aws.AwsProfileName
import com.magine.http4s.aws.MissingCredentials
import com.magine.http4s.aws.internal.Setting.ConfigFile
import fs2.io.file.Files
import fs2.io.file.NoSuchFileException
import fs2.io.file.Path
import fs2.text.utf8

/**
  * Capability to read configuration in `~/.aws/config`.
  */
private[aws] trait AwsConfig[F[_]] {
  def read(profileName: AwsProfileName): F[AwsProfile]
}

private[aws] object AwsConfig {
  def default[F[_]: Async]: AwsConfig[F] =
    new AwsConfig[F] {
      override def read(profileName: AwsProfileName): F[AwsProfile] =
        for {
          configFilePath <- ConfigFile.read
            .map(_.toRight(MissingCredentials()))
            .rethrow
          configFile <- readIniFile(configFilePath)
          title = if (profileName.isDefault) "default" else s"profile ${profileName.value}"
          profile <- configFile.sections
            .find(_.title == title)
            .map(AwsProfile.parse(_, profileName))
            .getOrElse(MissingCredentials().asLeft)
            .liftTo[F]
        } yield profile

      def readIniFile(path: Path): F[IniFile] =
        Files
          .forAsync[F]
          .readAll(path)
          .through(utf8.decode)
          .compile
          .foldMonoid
          .adaptError { case _: NoSuchFileException => MissingCredentials() }
          .flatMap(IniFile.parse(_).leftMap(failedToParse(path, _)).liftTo[F])

      def failedToParse(path: Path, error: Parser.Error): Throwable =
        new RuntimeException(s"Failed to parse config file at $path: ${error.show}")
    }
}
