/*
 * Copyright 2010 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package com.google.jribble

/**
 * Simple class that makes it easier to use Parsers from Java code.
 */
class DefParser {

  val parsers = new Parsers {
    val defParser: Parser[ast.DeclaredType] = classDef | interfaceDef
  }
  import parsers._

  def parse(in: java.io.Reader, name: String): Either[ast.DeclaredType, String] =
    parsers.parse(phrase(defParser), in) match {
      case Success(x, _) => Left(x)
      case x => Right("Could not parse the input " + x)
    }

}
