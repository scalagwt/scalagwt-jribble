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
package com.google.jribble.ast

import scala.collection.JavaConversions._
import java.util.{List => JList}

sealed abstract class AST

sealed abstract class DeclaredType {
  def name: Ref
}

case class Package(name: String) extends AST

sealed trait InterfaceBodyElement extends AST
sealed trait ClassBodyElement extends AST

case class ClassDef(modifs: Set[String], name: Ref, ext: Option[Ref], implements: List[Ref],
        body: List[ClassBodyElement]) extends DeclaredType {

  def jconstructors: JList[Constructor] = body collect { case x: Constructor => x}

  def jmethodDefs: JList[MethodDef] = body collect { case x: MethodDef => x }

  def jfieldDefs: JList[FieldDef] = body collect { case x: FieldDef => x }

  def jimplements: JList[Ref] = implements
}

case class InterfaceDef(modifs: Set[String], name: Ref, ext: List[Ref], body: List[MethodDef]) extends DeclaredType {
  def jbody: JList[MethodDef] = body
  def jext: JList[Ref] = ext
}

case class ParamDef(name: String, typ: Type) extends AST

case class Constructor(modifs: Set[String], name: String, params: List[ParamDef], body: Block)
  extends ClassBodyElement {
  def jparams: JList[ParamDef] = params
  def signature(enclosing: Ref) = Signature(enclosing, enclosing.name, params.map(_.typ), Void)
}
case class MethodDef(modifs: Set[String], returnType: Type, name: String, params: List[ParamDef],
                     body: Option[Block]) extends ClassBodyElement with InterfaceBodyElement {
  def jparams: JList[ParamDef] = params
  def signature(enclosing: Ref) = Signature(enclosing, name, params.map(_.typ), returnType)
}

case class FieldDef(modifs: Set[String], typ: Type, name: String, value: Option[Expression]) extends ClassBodyElement

sealed abstract class Statement extends AST

case class Block(statements: List[Statement]) extends Statement {
  def jstatements: JList[Statement] = statements
}

case class VarDef(typ: Type, name: String, value: Option[Expression]) extends Statement
case class Assignment(lhs: Expression, rhs: Expression) extends Statement
//TODO(grek): by using Block[MethodStatement] we are making it impossible to call super constructors from
//blocks in if statements. Not sure if this is important.
case class If(condition: Expression, then: Block, elsee: Option[Block]) extends Statement

case class Try(block: Block, catches: List[(Ref, String, Block)], finalizer: Option[Block]) extends Statement {
  //TODO(grek): Figure out if this requirement is needed for jribble
  assert(!(catches.isEmpty && finalizer.isEmpty))
  def jcatches: JList[(Ref, String, Block)] = catches
}

case class While(label: Option[String], condition: Expression, block: Block) extends Statement

case class Continue(label: Option[String]) extends Statement
case class Break(label: Option[String]) extends Statement

case class Switch(expression: Expression, groups: List[(Literal, Block)], default: Option[Block]) extends Statement {
  def jgroups: JList[(Literal, Block)] = groups
  def jdefault: Option[Block] = default
}

case class Return(expression: Option[Expression]) extends Statement

case class Throw(expression: Expression) extends Statement

case class ConstructorCall(signature: Signature, params: List[Expression]) extends Statement {
  def jparams: JList[Expression] = params
}

sealed abstract class Expression extends Statement {
  /**
   * Precedence for every AST node is assigned according to
   * Introduction to Programming in Java by Robert Sedgewick and Kevin Wayne,
   * Appendix A: Operator Precedence in Java
   */
  val precedence: Int
}

sealed abstract class Literal extends Expression { val precedence = 1 }
case class BooleanLiteral(v: Boolean) extends Literal
case class CharLiteral(v: Char) extends Literal
case class DoubleLiteral(v: Double) extends Literal
case class FloatLiteral(v: Float) extends Literal
case class IntLiteral(v: Int) extends Literal
case class LongLiteral(v: Long) extends Literal
case object NullLiteral extends Literal
case class StringLiteral(v: String) extends Literal

case class VarRef(name: String) extends Expression { val precedence = 1 }
case object ThisRef extends Expression { val precedence = 1 }
case object SuperRef extends Expression { val precedence = 1 }

case class ArrayInitializer(typ: Type, elements: List[Expression]) extends Expression {
  val precedence = 1
  val jelements: JList[Expression] = elements
}

case class Signature(on: Ref, name: String, paramTypes: List[Type], returnType: Type) extends AST {
  def jparamTypes: JList[Type] = paramTypes
}
case class NewCall(constructor: ConstructorCall) extends Expression {
  val precedence = 3
}
case class MethodCall(on: Expression, signature: Signature, params: List[Expression]) extends Expression {
  val precedence = 1
  def jparams: JList[Expression] = params
}
case class StaticMethodCall(classRef: Ref, signature: Signature, params: List[Expression]) extends Expression {
  val precedence = 1
  def jparams: JList[Expression] = params
}

case class Conditional(condition: Expression, typ: Type, then: Expression, elsee: Expression) extends Expression {
  val precedence = 14
}

case class InstanceOf(on: Expression, typ: Type) extends Expression { val precedence = 1 }

case class Cast(on: Expression, typ: Type) extends Expression { val precedence = 1 }

case class FieldRef(on: Expression, onType: Type, name: String) extends Expression { val precedence = 1 }
case class StaticFieldRef(on: Ref, name: String) extends Expression { val precedence = 1 }

sealed abstract class BinaryOp(val symbol: String) extends Expression {
  val lhs: Expression
  val rhs: Expression
}
case class Multiply(lhs: Expression, rhs: Expression) extends BinaryOp("*") { val precedence = 4 }
case class Divide(lhs: Expression, rhs: Expression) extends BinaryOp("/") { val precedence = 4 }
case class Modulus(lhs: Expression, rhs: Expression) extends BinaryOp("%") { val precedence = 4 }
case class Minus(lhs: Expression, rhs: Expression) extends BinaryOp("-") { val precedence = 5 }
case class Plus(lhs: Expression, rhs: Expression) extends BinaryOp("+") { val precedence = 5 }
case class Greater(lhs: Expression, rhs: Expression) extends BinaryOp(">") { val precedence = 7 }
case class GreaterOrEqual(lhs: Expression, rhs: Expression) extends BinaryOp(">=") { val precedence = 7 }
case class Lesser(lhs: Expression, rhs: Expression) extends BinaryOp("<") { val precedence = 7 }
case class LesserOrEqual(lhs: Expression, rhs: Expression) extends BinaryOp("<=") { val precedence = 7 }
case class Equal(lhs: Expression, rhs: Expression) extends BinaryOp("==") { val precedence = 8 }
case class NotEqual(lhs: Expression, rhs: Expression) extends BinaryOp("!=") { val precedence = 8 }
case class And(lhs: Expression, rhs: Expression) extends BinaryOp("&&") { val precedence = 12 }
case class Or(lhs: Expression, rhs: Expression) extends BinaryOp("||") { val precedence = 13 }
case class BitLShift(lhs: Expression, rhs: Expression) extends BinaryOp("<<") { val precedence = 6 }
case class BitRShift(lhs: Expression, rhs: Expression) extends BinaryOp(">>") { val precedence = 6 }
case class BitUnsignedRShift(lhs: Expression, rhs: Expression) extends BinaryOp(">>>") { val precedence = 6 }
case class BitAnd(lhs: Expression, rhs: Expression) extends BinaryOp("&") { val precedence = 9 }
case class BitXor(lhs: Expression, rhs: Expression) extends BinaryOp("^") { val precedence = 10 }
case class BitOr(lhs: Expression, rhs: Expression) extends BinaryOp("|") { val precedence = 11 }

sealed abstract class UnaryOp(val symbol: String) extends Expression {
  val expression: Expression
}

case class Not(expression: Expression) extends UnaryOp("!") { val precedence = 2 }
case class UnaryMinus(expression: Expression) extends UnaryOp("-") { val precedence = 2 }
case class BitNot(expression: Expression) extends UnaryOp("~") { val precedence = 2 }

case class ArrayRef(on: Expression, index: Expression) extends Expression {
  val precedence = 1
}

case class NewArray(typ: Type, dims: List[Option[Expression]]) extends Expression {
  assert(!dims.isEmpty)
  val precedence = 3
  def jdims: JList[Option[Expression]] = dims
}

case class ArrayLength(on: Expression) extends Expression {
  val precedence = 1
}

sealed abstract class Type extends AST
case class Ref(pkg: Option[Package], name: String) extends Type {
  def javaName: String = pkg.map(_.name.replace("/", ".") + ".").getOrElse("") + name
}
case class Primitive(name: String) extends Type
case class Array(typ: Type) extends Type
case object Void extends Type
