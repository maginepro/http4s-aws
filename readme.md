# :lock: http4s-aws

Provides [http4s](https://http4s.org) request authentication using [AWS Signature Version 4](https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_aws-signing.html) signing.<br>
The library is inspired by [filosganga/http4s-aws](https://github.com/filosganga/http4s-aws) but with significant differences.

## Usage

You can add the following line to `build.sbt` to use the library.

```scala
libraryDependencies += "com.magine" %% "http4s-aws" % http4sAwsVersion
```

Make sure to replace `http4sAwsVersion` with a [release version](https://github.com/maginepro/http4s-aws/releases).<br>
Replace `%%` with `%%%` if you are using [Scala.js](https://www.scala-js.org).

## Signing

Create a `CredentialsProvider` and use `AwsSigningClient` to wrap an existing client.

```scala
import cats.effect.IO
import cats.effect.IOApp
import com.magine.aws.Region
import com.magine.http4s.aws.AwsServiceName
import com.magine.http4s.aws.AwsSigningClient
import com.magine.http4s.aws.CredentialsProvider
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.Request

object Main extends IOApp.Simple {
  def run: IO[Unit] = {
    val client =
      for {
        client <- EmberClientBuilder.default[IO].build
        provider <- CredentialsProvider.default(client)
        region = Region.EU_WEST_1
        serviceName = AwsServiceName.Elasticsearch
        signingClient = AwsSigningClient(provider, region, serviceName)(client)
      } yield signingClient

    val request: Request[IO] =
      Request()

    client.use(_.expect[Unit](request))
  }
}
```

### Chunking

For chunked uploads, set `Transfer-Encoding: chunked` and set the `X-Amz-Decoded-Content-Length` header to the total length of the request body. In this case, avoid setting the `Content-Length` header, since it will then also have to account for the signing content length.

## Pre-signing

If pre-signing of requests is necessary, use `AwsPresigning`.

```scala
import cats.effect.IO
import cats.effect.IOApp
import com.magine.aws.Region
import com.magine.http4s.aws.AwsPresigning
import com.magine.http4s.aws.AwsServiceName
import com.magine.http4s.aws.CredentialsProvider
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.Request
import scala.concurrent.duration._

object Main extends IOApp.Simple {
  def run: IO[Unit] = {
    val presigning =
      for {
        client <- EmberClientBuilder.default[IO].build
        provider <- CredentialsProvider.default(client)
        region = Region.EU_WEST_1
        serviceName = AwsServiceName.S3
        expiry = 5.minutes
        presigning = AwsPresigning(provider, region, serviceName, expiry)
      } yield presigning

    val request: Request[IO] =
      Request()

    presigning
      .use(_.presign(request))
      .map(_.uri.renderString)
      .flatMap(IO.println)
  }
}
```
