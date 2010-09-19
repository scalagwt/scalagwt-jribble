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
import org.scalacheck._

object PrintersParsersTestCase extends Properties("formatAndParseField") {
  import Generators._
  import Shrinkers._

  val parsers: Parsers = new Parsers {}
  val printers: Printers = new Printers {}

  import printers._

  def checkEqual[T: Arbitrary: Shrink: printers.Printer,U](f: T => U, g: T => U) = {
    implicit val pretty: T => Pretty = (x: T) => Pretty { p =>
      "-- Formatted:\n%1s\n-- using toString:\n%2s".format(implicitly[Printer[T]].apply(x), x.toString) +
      "\n f returned: " + f(x)
    }
    Prop.forAll((x: T) => f(x) == g(x))
  }

  def checkPrinterParser[T: Arbitrary: Shrink: printers.Printer](printer: printers.Printer[T], parser: parsers.Parser[T]) =
    checkEqual(printer andThen liftParser(parser), Left(_: T))

  implicit def liftParser[T](p: parsers.Parser[T]): String => Either[T,String] =
    (parsers.parse(p, _: String)) andThen {
      _ match {
        case parsers.Success(result, _) => Left(result)
        case x => Right("Could not parse the input because " + x)
      }
  }

  property("Ref") = checkPrinterParser(printers.RefPrinter, parsers.ref)

  property("primitive") = checkPrinterParser(printers.PrimitivePrinter, parsers.primitive)

  property("array") = checkPrinterParser(printers.ArrayPrinter, parsers.array)

  property("typ") = checkPrinterParser(printers.TypePrinter, parsers.typ)

  property("paramsDef") = checkPrinterParser(printers.ParamsDefPrinter, parsers.paramsDef)

  property("literal") = checkPrinterParser(printers.LiteralPrinter, parsers.literal)

  property("signature") = checkPrinterParser(printers.SignaturePrinter, parsers.signature)

  property("newCall") = checkPrinterParser(printers.NewCallPrinter, parsers.newCall)

  //todo (grek): possibly tests for methodCalls and staticMethodCalls?

  property("conditional") = checkPrinterParser(printers.ConditionalPrinter, parsers.conditional)

  //TODO(grek): These tests are disabled due to probable bug in PackratParsers, see
  //http://article.gmane.org/gmane.comp.lang.scala.user/31210
//  property("instanceof") = checkPrinterParser(printers.InstanceOfPrinter, parsers.instanceOf)
//  property("cast") = checkPrinterParser(printers.CastPrinter, parsers.cast)

  property("expression") = checkPrinterParser(printers.ExpressionPrinter, parsers.expression)

  property("varDef") = checkPrinterParser(printers.VarDefPrinter, parsers.varDef)

  property("assignment") = checkPrinterParser(printers.AssignmentPrinter, parsers.assignment)

  property("if") = checkPrinterParser(printers.IfPrinter, parsers.ifStatement)

  property("try") = checkPrinterParser(printers.TryPrinter, parsers.tryStatement)

  property("methodBody") = checkPrinterParser(printers.BlockPrinter, parsers.methodBody)

  property("constructorBody") = checkPrinterParser(printers.BlockPrinter, parsers.constructorBody)

  property("constructor") = checkPrinterParser(printers.ConstructorPrinter, parsers.constructor)

  property("methodDef") = checkPrinterParser(printers.MethodDefPrinter, parsers.methodDef)

  property("fieldDef") = checkPrinterParser(printers.FieldDefPrinter, parsers.fieldDef)

  property("classDef") = checkPrinterParser(printers.ClassDefPrinter, parsers.classDef)

  property("interfaceDef") = checkPrinterParser(printers.InterfaceDefPrinter, parsers.interfaceDef)

}
