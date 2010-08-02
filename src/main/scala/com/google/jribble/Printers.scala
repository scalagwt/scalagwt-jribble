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

trait Printers {

  trait Printer[T] extends (T => String) {
    def apply(x: T): String
  }

  implicit object PackagePrinter extends Printer[Package] {
    def apply(x: Package) = "package " + x.name
  }

  implicit object ClassRefPrinter extends Printer[ClassRef] {
    def apply(x: ClassRef) = "(" + PackagePrinter(x.pkg) + ")." + x.name   
  }

  implicit object PrimitivePrinter extends Printer[Primitive] {
    def apply(x: Primitive) = x.name
  }

  implicit object ArrayPrinter extends Printer[Array] {
    def apply(x: Array) = TypePrinter(x.typ) + "[]"
  }

  implicit object TypePrinter extends Printer[Type] {
    def apply(x: Type) = x match {
      case x: ClassRef => ClassRefPrinter(x)
      case x: Primitive => PrimitivePrinter(x)
      case x: Array => ArrayPrinter(x)
      case Void => "void"
    }
  }

  implicit object ParamsDefPrinter extends Printer[List[ParamDef]] {
    def printParamDef(x: ParamDef) = TypePrinter(x.typ) + " " + x.name
    def apply(xs: List[ParamDef]) = xs.map(printParamDef).mkString("(", ", ", ")")
  }

  implicit object VarDefPrinter extends Printer[VarDef] {
    def apply(x: VarDef) = TypePrinter(x.typ) + " " + x.name + " = " + ExpressionPrinter(x.value)
  }

  implicit object AssignmentPrinter extends Printer[Assignment] {
    def apply(x: Assignment) = x.name + " = " + ExpressionPrinter(x.value)
  }

  //todo (grek): implement printing of literals
  implicit object LiteralPrinter extends Printer[Literal] {
    def apply(x: Literal) = x match {
      case CharLiteral(v) => "'" + x + "'"
      //todo (grek): implement escaping
      case StringLiteral(v) => "\"" + v + "\""
      case _ => error("to be implemented")
    }
  }

  implicit object ParamsPrinter extends Printer[List[Expression]] {
    def apply(xs: List[Expression]) = xs.map(ExpressionPrinter).mkString("(", ", ", ")")
  }

  implicit object SignaturePrinter extends Printer[Signature] {
    def apply(x: Signature) = (x.returnType :: x.paramTypes).map(TypePrinter).mkString("<", ", ", ">")
  }

  implicit object NewCallPrinter extends Printer[NewCall] {
    def apply(x: NewCall) = "new " + ClassRefPrinter(x.classRef) + SignaturePrinter(x.signature) +
            ParamsPrinter(x.params)
  }

  implicit object MethodCallPrinter extends Printer[MethodCall] {
    def apply(x: MethodCall) = ExpressionPrinter(x.on) + "." + x.name + SignaturePrinter(x.signature) +
            ParamsPrinter(x.params)
  }

  implicit object StaticMethodCallPrinter extends Printer[StaticMethodCall] {
    def apply(x: StaticMethodCall) = ClassRefPrinter(x.classRef) + "." + x.name + SignaturePrinter(x.signature) +
            ParamsPrinter(x.params)
  }

  implicit object ExpressionPrinter extends Printer[Expression] {
    def apply(x: Expression) = x match {
      case x: Literal => LiteralPrinter(x)
      case ThisRef => "this"
      case x: VarRef => x.name
      case x: NewCall => NewCallPrinter(x)
      case x: MethodCall => MethodCallPrinter(x)
      case x: StaticMethodCall => StaticMethodCallPrinter(x)
    }
  }

  implicit object SuperConstructorCall extends Printer[SuperConstructorCall] {
    def apply(x: SuperConstructorCall) = "super" + ParamsPrinter(x.params)
  }

  implicit object StatementPrinter extends Printer[Statement] {
    def apply(x: Statement) = (x match {
      case x: VarDef => VarDefPrinter(x)
      case x: Assignment => AssignmentPrinter(x)
      case x: Expression => ExpressionPrinter(x)
      case x: SuperConstructorCall => SuperConstructorCall(x)
    }) + ";"
  }

  implicit object MethodBodyPrinter extends Printer[List[MethodStatement]] {
    def apply(xs: List[MethodStatement]) = xs.map(StatementPrinter).map(_ + "\n").mkString("{\n", "", "}\n")
  }

  implicit object ConstructorBodyPrinter extends Printer[List[ConstructorStatement]] {
    def apply(xs: List[ConstructorStatement]) = xs.map(StatementPrinter).map(_ + "\n").mkString("{\n", "", "}\n")
  }

  implicit object ConstructorPrinter extends Printer[Constructor] {
    //todo (grek): hard-coded public
    def apply(x: Constructor) = "public " + x.name + ParamsDefPrinter(x.params) + " " + ConstructorBodyPrinter(x.body)
  }

  implicit object MethodDefPrinter extends Printer[MethodDef] {
    def apply(x: MethodDef) =
      "public " + TypePrinter(x.returnType) + " " + x.name + ParamsDefPrinter(x.params) + " " + MethodBodyPrinter(x.body)
  }

  implicit object ClassDefPrinter extends Printer[ClassDef] {
    def apply(x: ClassDef) = PackagePrinter(x.pkg) + ";\n\n" + {
      val body = (x.body.map {
        case Left(constructor) => ConstructorPrinter(constructor)
        case Right(methodDef) => MethodDefPrinter(methodDef)
      }).mkString("{\n", "\n", "}\n")
      val implements = x.implements match {
        case Nil => ""
        case xs => xs.map(ClassRefPrinter).mkString("implements ", ", ", " ")
      }
      val ext = x.ext.map("extends " + ClassRefPrinter(_) + " ").getOrElse("")

      x.modifs.mkString(" ") + " class " + x.name + " " + ext + implements + body
    }
  }

}
