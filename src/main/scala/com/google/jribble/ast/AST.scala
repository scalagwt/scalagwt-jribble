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

  def jimplements: JList[Ref] = implements
}

case class InterfaceDef(modifs: Set[String], name: Ref, ext: Option[Ref], body: List[MethodDef]) extends DeclaredType {
  def jbody: JList[MethodDef] = body
}

case class ParamDef(name: String, typ: Type) extends AST

case class Constructor(name: String, params: List[ParamDef], body: Block) extends ClassBodyElement {
  def jparams: JList[ParamDef] = params
  def signature(enclosing: Ref) = Signature(enclosing, enclosing.name, params.map(_.typ), Void)
}
case class MethodDef(modifs: Set[String], returnType: Type, name: String, params: List[ParamDef],
                     body: Block) extends ClassBodyElement with InterfaceBodyElement {
  def jparams: JList[ParamDef] = params
  def signature(enclosing: Ref) = Signature(enclosing, name, params.map(_.typ), returnType)
}

case class FieldDef(modifs: Set[String], typ: Type, name: String, value: Option[Expression]) extends ClassBodyElement

case class Block(statements: List[Statement]) {
  def jstatements: JList[Statement] = statements
}

sealed abstract class Statement extends AST
case class VarDef(typ: Type, name: String, value: Expression) extends Statement
case class Assignment(name: String, value: Expression) extends Statement
//TODO(grek): by using Block[MethodStatement] we are making it impossible to call super constructors from
//blocks in if statements. Not sure if this is important.
case class If(condition: Expression, then: Block, elsee: Option[Block]) extends Statement

case class Try(block: Block, catches: List[(Ref, String, Block)], finalizer: Option[Block]) extends Statement {
  //TODO(grek): Figure out if this requirement is needed for jribble
  assert(!(catches.isEmpty && finalizer.isEmpty))
}

case class While(label: Option[String], condition: Expression, block: Block) extends Statement

case class Continue(label: Option[String]) extends Statement
case class Break(label: Option[String]) extends Statement

case class SuperConstructorCall(signature: Signature, params: List[Expression]) extends Statement {
  def jparams: JList[Expression] = params
}

sealed abstract class Expression extends Statement

sealed abstract class Literal extends Expression
case class BooleanLiteral(v: Boolean) extends Literal
case class CharLiteral(v: Char) extends Literal
case class DoubleLiteral(v: Double) extends Literal
case class FloatLiteral(v: Float) extends Literal
case class IntLiteral(v: Int) extends Literal
case class LongLiteral(v: Long) extends Literal
case object NullLiteral extends Literal
case class StringLiteral(v: String) extends Literal

case class VarRef(name: String) extends Expression
case object ThisRef extends Expression

case class Signature(on: Ref, name: String, paramTypes: List[Type], returnType: Type) extends AST {
  def jparamTypes: JList[Type] = paramTypes
}
case class NewCall(signature: Signature, params: List[Expression]) extends Expression {
  def jparams: JList[Expression] = params
}
case class MethodCall(on: Expression, signature: Signature, params: List[Expression]) extends Expression {
  def jparams: JList[Expression] = params
}
case class StaticMethodCall(classRef: Ref, signature: Signature, params: List[Expression]) extends Expression {
  def jparams: JList[Expression] = params
}

case class Conditional(condition: Expression, typ: Type, then: Expression, elsee: Expression) extends Expression

case class InstanceOf(on: Expression, typ: Ref) extends Expression

case class Cast(on: Expression, typ: Ref) extends Expression

sealed abstract class Type extends AST
case class Ref(pkg: Option[Package], name: String) extends Type {
  def javaName: String = pkg.map(_.name.replace("/", ".") + ".").getOrElse("") + name
}
case class Primitive(name: String) extends Type
case class Array(typ: Type) extends Type
case object Void extends Type
