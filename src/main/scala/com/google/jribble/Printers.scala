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
    def apply(x: Package) = x.name
  }

  implicit object RefPrinter extends Printer[Ref] {
    def apply(x: Ref) = "L" + x.pkg.map(PackagePrinter(_) + "/").getOrElse("") + x.name + ";"
  }

  implicit object PrimitivePrinter extends Printer[Primitive] {
    def apply(x: Primitive) = x.name + ";"
  }

  implicit object ArrayPrinter extends Printer[Array] {
    def apply(x: Array) = TypePrinter(x.typ) + "["
  }

  implicit object TypePrinter extends Printer[Type] {
    def apply(x: Type) = x match {
      case x: Ref => RefPrinter(x)
      case x: Primitive => PrimitivePrinter(x)
      case x: Array => ArrayPrinter(x)
      case Void => "V;"
    }
  }

  implicit object ParamsDefPrinter extends Printer[List[ParamDef]] {
    def printParamDef(x: ParamDef) = TypePrinter(x.typ) + " " + x.name
    def apply(xs: List[ParamDef]) = xs.map(printParamDef).mkString("(", ", ", ")")
  }

  implicit object VarDefPrinter extends Printer[VarDef] {
    def apply(x: VarDef) = TypePrinter(x.typ) + " " + x.name + x.value.map(x => " = " +
            ExpressionPrinter(x)).getOrElse("")
  }

  implicit object AssignmentPrinter extends Printer[Assignment] {
    def apply(x: Assignment) = ExpressionPrinter(x.lhs) + " = " + ExpressionPrinter(x.rhs)
  }

  //todo (grek): implement printing of literals
  implicit object LiteralPrinter extends Printer[Literal] {
    def apply(x: Literal) = x match {
      case CharLiteral(v) => "'" + v + "'"
      //todo (grek): implement escaping
      case StringLiteral(v) => "\"" + v + "\""
      case BooleanLiteral(v) => v.toString
      case IntLiteral(v) => v.toString
      case LongLiteral(v) => v.toString + "L"
      case _ => error("to be implemented")
    }
  }

  implicit object ParamsPrinter extends Printer[List[Expression]] {
    def apply(xs: List[Expression]) = xs.map(ExpressionPrinter).mkString("(", ", ", ")")
  }

  implicit object SignaturePrinter extends Printer[Signature] {
    def apply(x: Signature) = "(" + RefPrinter(x.on) + "::" + x.name +
            x.paramTypes.map(TypePrinter).mkString("(", "",")") + TypePrinter(x.returnType) + ")"
  }

  implicit object NewCallPrinter extends Printer[NewCall] {
    def apply(x: NewCall) = "new " + ConstructorCallPrinter(x.constructor)
  }

  implicit object MethodCallPrinter extends Printer[MethodCall] {
    def apply(x: MethodCall) =
      NestedExpressionPrinter(x.precedence, x.on) + "." + SignaturePrinter(x.signature) + ParamsPrinter(x.params)
  }

  implicit object StaticMethodCallPrinter extends Printer[StaticMethodCall] {
    def apply(x: StaticMethodCall) = RefPrinter(x.classRef) + "." + SignaturePrinter(x.signature) +
            ParamsPrinter(x.params)
  }

  implicit object ConditionalPrinter extends Printer[Conditional] {
    def apply(x: Conditional) =
      NestedExpressionPrinter(0, x.condition) +
      " ?(" + TypePrinter(x.typ) + ") " +
      NestedExpressionPrinter(0, x.then) +
      " : " + NestedExpressionPrinter(0, x.elsee)
  }

  implicit object InstanceOfPrinter extends Printer[InstanceOf] {
    def apply(x: InstanceOf) =
      NestedExpressionPrinter(x.precedence, x.on) + "." + "<instanceof>" + "(" + RefPrinter(x.typ) + ")"
  }

  implicit object CastPrinter extends Printer[Cast] {
    def apply(x: Cast) = NestedExpressionPrinter(x.precedence, x.on) + "." + "<cast>" + "(" + TypePrinter(x.typ) + ")"
  }

  implicit object ArrayInitializerPrinter extends Printer[ArrayInitializer] {
    def apply(x: ArrayInitializer) =
      "<" + TypePrinter(x.typ) + ">" + x.elements.map(ExpressionPrinter).mkString("{", ", ", "}")
  }

  implicit object FieldRefPrinter extends Printer[FieldRef] {
    def apply(x: FieldRef) = NestedExpressionPrinter(x.precedence, x.on) + "." + "(" + TypePrinter(x.onType) + ")" + x.name
  }

  implicit object StaticFieldRefPrinter extends Printer[StaticFieldRef] {
    def apply(x: StaticFieldRef) = RefPrinter(x.on) + "." + x.name
  }

  implicit object BinaryOpPrinter extends Printer[BinaryOp] {
    def apply(x: BinaryOp) = "%1s %2s %3s".format(
      NestedExpressionPrinter(x.precedence, x.lhs),
      x.symbol,
      //we pass precedence-1 because operators are left associative
      NestedExpressionPrinter(x.precedence-1, x.rhs))
  }

  implicit object UnaryOpPrinter extends Printer[UnaryOp] {
    def apply(x: UnaryOp) = x match {
      //need a special case to distinguish between UnaryMinus(IntLiteral(1)) and IntLiteral(-1)
      case UnaryMinus(IntLiteral(x)) => "-(%1d)".format(x)
      case UnaryMinus(LongLiteral(x)) => "-(%1dL)".format(x)
      case x => x.symbol + NestedExpressionPrinter(x.precedence-1, x.expression)
    }
  }

  implicit object ArrayRefPrinter extends Printer[ArrayRef] {
    def apply(x: ArrayRef) = NestedExpressionPrinter(x.precedence, x.on) + "[" + ExpressionPrinter(x.index) + "]"
  }

  implicit object NewArrayPrinter extends Printer[NewArray] {
    def apply(x: NewArray) = "new " + TypePrinter(x.typ) +
      x.dims.map("[" + _.map(ExpressionPrinter).getOrElse("") + "]").mkString("")
  }

  implicit object ExpressionPrinter extends Printer[Expression] {
    //surrounding for expression which is not nested in another expression have implicit infinite precedence
    def apply(x: Expression) = NestedExpressionPrinter(10000, x)
  }

  object NestedExpressionPrinter extends Printer[(Int, Expression)] {
    def apply(x: (Int, Expression)) = {
      val (surroundingPrecedence, nested) = x
      val printed = nested match {
        case x: Literal => LiteralPrinter(x)
        case ThisRef => "this"
        case x: VarRef => x.name
        case x: NewCall => NewCallPrinter(x)
        case x: MethodCall => MethodCallPrinter(x)
        case x: StaticMethodCall => StaticMethodCallPrinter(x)
        case x: Conditional => ConditionalPrinter(x)
        case x: InstanceOf => InstanceOfPrinter(x)
        case x: Cast => CastPrinter(x)
        case x: ArrayInitializer => ArrayInitializerPrinter(x)
        case x: FieldRef => FieldRefPrinter(x)
        case x: StaticFieldRef => StaticFieldRefPrinter(x)
        case x: BinaryOp => BinaryOpPrinter(x)
        case x: ArrayRef => ArrayRefPrinter(x)
        case x: NewArray => NewArrayPrinter(x)
        case x: UnaryOp => UnaryOpPrinter(x)
      }
      if (surroundingPrecedence < nested.precedence) "(" + printed + ")" else printed
    }
  }

  implicit object ConstructorCallPrinter extends Printer[ConstructorCall] {
    def apply(x: ConstructorCall) = SignaturePrinter(x.signature) + ParamsPrinter(x.params)
  }

  implicit object IfPrinter extends Printer[If] {
    def apply(x: If) = "if (" + ExpressionPrinter(x.condition) + ") " + BlockPrinter(x.then) + (x.elsee match {
      case None => ""
      case Some(block) => " else " + BlockPrinter(block)
    })
  }

  implicit object TryPrinter extends Printer[Try] {
    def apply(x: Try) = "try " + BlockPrinter(x.block) + (x.catches.map {
      case (ref, name, block) => " catch (" + RefPrinter(ref) + " " + name + ") " + BlockPrinter(block)
    }).mkString("\n") + x.finalizer.map(" finally " + BlockPrinter(_)).getOrElse("")
  }

  implicit object WhilePrinter extends Printer[While] {
    def apply(x: While) = x.label.map(_ + ": ").getOrElse("") +
            "while (" + ExpressionPrinter(x.condition) + ") " + BlockPrinter(x.block)
  }

  implicit object ContinuePrinter extends Printer[Continue] {
    def apply(x: Continue) = "continue" + x.label.map(" " + _).getOrElse("")
  }

  implicit object BreakPrinter extends Printer[Break] {
    def apply(x: Break) = "break" + x.label.map(" " + _).getOrElse("")
  }

  implicit object SwitchPrinter extends Printer[Switch] {
    def apply(x: Switch) = {
      val groups = {
        def groupPrinter(label: Expression, b: Block) = (ExpressionPrinter(label) + ":\n") + BlockPrinter(b)
        x.groups.map((groupPrinter _).tupled) mkString "\n"
      }
      val default = x.default.map(b => "default:\n" + BlockPrinter(b))
      "switch (" + ExpressionPrinter(x.expression) + ") {\n" + groups + default.getOrElse("") + "}\n"
    }
  }

  implicit object ReturnPrinter extends Printer[Return] {
    def apply(x: Return) = "return" + x.expression.map(" " + ExpressionPrinter(_)).getOrElse("")
  }

  implicit object ThrowPrinter extends Printer[Throw] {
    def apply(x: Throw) = "throw " + ExpressionPrinter(x.expression)
  }

  implicit object StatementPrinter extends Printer[Statement] {
    def apply(x: Statement) = x match {
      case x: VarDef => VarDefPrinter(x) + ";"
      case x: Assignment => AssignmentPrinter(x) + ";"
      case x: Expression => ExpressionPrinter(x) + ";"
      case x: ConstructorCall => ConstructorCallPrinter(x) + ";"
      case x: If => IfPrinter(x)
      case x: Try => TryPrinter(x)
      case x: While => WhilePrinter(x)
      case x: Break => BreakPrinter(x) + ";"
      case x: Continue => ContinuePrinter(x) + ";"
      case x: Switch => SwitchPrinter(x)
      case x: Return => ReturnPrinter(x) + ";"
      case x: Throw => ThrowPrinter(x) + ";"
      case x: Block => BlockPrinter(x)
    }
  }

  implicit object BlockPrinter extends Printer[Block] {
    def apply(block: Block) = block.statements.map(StatementPrinter).map(_ + "\n").mkString("{\n", "", "}")
  }

  implicit object ConstructorPrinter extends Printer[Constructor] {
    //todo (grek): hard-coded public
    def apply(x: Constructor) = "public " + x.name + ParamsDefPrinter(x.params) + " " + BlockPrinter(x.body)
  }

  implicit object MethodDefPrinter extends Printer[MethodDef] {
    def apply(x: MethodDef) =
      x.modifs.map(_ + " ").mkString + TypePrinter(x.returnType) + " " + x.name + ParamsDefPrinter(x.params) +
              x.body.map(b => " " + BlockPrinter(b) + "\n").getOrElse(";") 
  }

  implicit object FieldDefPrinter extends Printer[FieldDef] {
    def apply(x: FieldDef) = x.modifs.map(_ + " ").mkString + TypePrinter(x.typ) + " " + x.name +
      x.value.map(" = " + ExpressionPrinter(_)).getOrElse("") + ";"
  }

  implicit object ClassDefPrinter extends Printer[ClassDef] {
    def apply(x: ClassDef) = {
      val body = (x.body.map {
        case constructor: Constructor => ConstructorPrinter(constructor)
        case methodDef: MethodDef => MethodDefPrinter(methodDef)
        case fieldDef: FieldDef => FieldDefPrinter(fieldDef)
      }).mkString("{\n", "\n", "}\n")
      val implements = x.implements match {
        case Nil => ""
        case xs => xs.map(RefPrinter).mkString("implements ", ", ", " ")
      }
      val ext = x.ext.map("extends " + RefPrinter(_) + " ").getOrElse("")

      x.modifs.map(_ + " ").mkString + "class " + RefPrinter(x.name) + " " + ext + implements + body
    }
  }

  implicit object InterfaceDefPrinter extends Printer[InterfaceDef] {
    def apply(x: InterfaceDef) = {
      val body = x.body.map(MethodDefPrinter).mkString("{\n", "\n", "}\n")
      val ext = x.ext match {
        case Nil => ""
        case xs => xs.map(RefPrinter).mkString("extends ", ", ", "")
      }

      x.modifs.map(_ + " ").mkString + "interface " + RefPrinter(x.name) + " " + ext + body
    }
  }

}
