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

import cats.syntax.all.*
import com.magine.aws.Region
import com.magine.http4s.aws.AwsServiceName

private[aws] final case class CredentialScope(
  region: Region,
  requestDate: RequestDate,
  serviceName: AwsServiceName
) {
  def value: String =
    show"${requestDate.value}/${region.id}/${serviceName.value}/aws4_request"
}
