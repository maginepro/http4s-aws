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

import cats.parse.Parser as P
import cats.parse.Parser0 as P0

/**
  * An INI file parser used to parse `~/.aws/credentials`.
  */
private[aws] final case class IniFile(
  keys: List[IniFile.Key],
  sections: List[IniFile.Section]
) {
  def read(section: String, key: String): Option[String] =
    sections
      .find(_.title == section)
      .flatMap(_.keys.find(_.name == key))
      .map(_.value)
}

private[aws] object IniFile {
  final case class Key(name: String, value: String)
  final case class Section(title: String, keys: List[Key])

  def parse(s: String): Either[P.Error, IniFile] =
    parser.parseAll(s)

  private val parser: P0[IniFile] = {
    val whitespace0 = P.char(' ').rep0.void
    val newlines0 = P.char('\n').rep0.void
    val newlines = P.char('\n').rep.void

    val comment =
      (P.char('#') *> P.charsWhile(_ != '\n'))
        .surroundedBy(whitespace0)
        .void

    val name = P.charsWhile(c => c != '=' && c != '\n').map(_.trim)
    val value = P.charsWhile0(c => c != '\n').map(_.trim)
    val key = ((name <* P.char('=')) ~ value).map((Key(_, _)).tupled)

    val keys =
      comment.backtrack
        .map(Left(_))
        .orElse(key.backtrack.map(Right(_)))
        .repSep0(newlines)
        .map(_.collect { case Right(key_) => key_ })

    val title =
      P.charsWhile(_ != ']')
        .between(P.char('['), P.char(']'))
        .surroundedBy(whitespace0)

    val sections =
      ((title <* newlines0) ~ keys)
        .map((Section(_, _)).tupled)
        .backtrack
        .repSep0(newlines)

    ((keys <* newlines0) ~ sections)
      .surroundedBy(P.charsWhile0(_.isWhitespace))
      .map((IniFile(_, _)).tupled)
  }
}
