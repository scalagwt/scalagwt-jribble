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

import com.google.jribble.ast._
import scala.util.parsing.input.CharSequenceReader
import org.scalacheck.{Arbitrary, Prop, Properties}

object PrintersParsersTestCase extends Properties("formatAndParseField") {
  import Generators._

  val parsers: Parsers = new Parsers {}
  val printers: Printers = new Printers {}

  def checkEqual[T: Arbitrary,U](f: T => U, g: T => U) = Prop.forAll((x: T) => f(x) == g(x))

  def checkIdentityEqual[T: Arbitrary](f: T => T) = checkEqual[T,T](f, identity)

  def parseUntilEof[T](p: parsers.Parser[T]) = p <~ parsers.literal(scala.util.parsing.input.CharSequenceReader.EofCh.toString)

  implicit def liftParser[T](p: parsers.Parser[T]): String => T =
    (new CharSequenceReader(_: String)) andThen p andThen {
      _ match {
        case parsers.Success(result, _) => result
        case x => error("Could not parse the input because " + x)
      }
  }

  property("Ref") = checkIdentityEqual(printers.RefPrinter andThen parsers.ref)

  property("primitive") = checkIdentityEqual(printers.PrimitivePrinter andThen parsers.primitive)

  property("array") = checkIdentityEqual(printers.ArrayPrinter andThen parsers.array)

  property("typ") = checkIdentityEqual(printers.TypePrinter andThen parsers.typ)

  property("paramsDef") = checkIdentityEqual(printers.ParamsDefPrinter andThen parsers.paramsDef)

  property("literal") = checkIdentityEqual(printers.LiteralPrinter andThen liftParser(parsers.literal))

  property("signature") = checkIdentityEqual(printers.SignaturePrinter andThen parsers.signature)

  property("newCall") = checkIdentityEqual(printers.NewCallPrinter andThen parsers.newCall)

  //todo (grek): possibly tests for methodCalls and staticMethodCalls?

  property("expression") = checkIdentityEqual(printers.ExpressionPrinter andThen parsers.expression)

  property("varDef") = checkIdentityEqual(printers.VarDefPrinter andThen parsers.varDef)

  property("assignment") = checkIdentityEqual(printers.AssignmentPrinter andThen parsers.assignment)

  property("methodBody") = checkIdentityEqual(printers.MethodBodyPrinter andThen parsers.methodBody)

  property("constructorBody") = checkIdentityEqual(printers.ConstructorBodyPrinter andThen parsers.constructorBody)

  property("constructor") = checkIdentityEqual(printers.ConstructorPrinter andThen parsers.constructor)

  property("methodDef") = checkIdentityEqual(printers.MethodDefPrinter andThen parsers.methodDef)

  property("classDef") = checkIdentityEqual(printers.ClassDefPrinter andThen parsers.classDef)

  property("interfaceDef") = checkIdentityEqual(printers.InterfaceDefPrinter andThen parsers.interfaceDef)

}
