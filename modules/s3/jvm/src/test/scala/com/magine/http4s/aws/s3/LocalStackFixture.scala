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

import cats.effect.IO
import cats.effect.Resource
import com.magine.aws.Region
import com.magine.http4s.aws.Credentials
import com.magine.http4s.aws.Credentials.AccessKeyId
import com.magine.http4s.aws.Credentials.SecretAccessKey
import com.magine.http4s.aws.CredentialsProvider
import munit.AnyFixture
import munit.CatsEffectSuite
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder
import org.testcontainers.localstack.LocalStackContainer
import scala.concurrent.duration.*

trait LocalStackFixture extends CatsEffectSuite {
  val localStackFixture: AnyFixture[LocalStack[IO]] = {
    val localStackContainer: Resource[IO, LocalStackContainer] =
      Resource.make {
        IO.blocking {
          val container =
            new LocalStackContainer("localstack/localstack:s3-latest")
              .withServices("s3")
          container.start()
          container
        }
      }(container => IO.blocking(container.stop()))

    def localStack(container: LocalStackContainer): Resource[IO, LocalStack[IO]] =
      for {
        client <- EmberClientBuilder.default[IO].build
        provider = CredentialsProvider.static[IO](
          Credentials(
            AccessKeyId(container.getAccessKey),
            SecretAccessKey(container.getSecretKey),
            sessionToken = None
          )
        )
        region = Region(container.getRegion)
        uri <- IO(Uri.unsafeFromString(container.getEndpoint.toString)).toResource
      } yield LocalStack(client, provider, region, uri)

    ResourceSuiteLocalFixture(
      "localStack",
      localStackContainer.flatMap(localStack)
    )
  }

  override def munitFixtures: Seq[AnyFixture[?]] =
    localStackFixture +: super.munitFixtures

  override def munitIOTimeout: Duration =
    2.minutes

  def localStack: IO[LocalStack[IO]] =
    IO(localStackFixture())
}
