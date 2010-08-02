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

sealed abstract class AST

case class Package(name: String) extends AST

case class ClassDef(pkg: Package, modifs: Set[String], name: String, ext: Option[ClassRef], implements: List[ClassRef],
        body: List[Either[Constructor, MethodDef]]) extends AST

case class ParamDef(name: String, typ: Type) extends AST

case class Constructor(name: String, params: List[ParamDef], body: List[ConstructorStatement]) extends AST
case class MethodDef(returnType: Type, name: String, params: List[ParamDef], body: List[MethodStatement]) extends AST

sealed abstract class Statement extends AST
sealed trait ConstructorStatement extends Statement
sealed abstract class MethodStatement extends Statement with ConstructorStatement
case class VarDef(typ: Type, name: String, value: Expression) extends MethodStatement
case class Assignment(name: String, value: Expression) extends MethodStatement

case class SuperConstructorCall(signature: Signature, params: List[Expression]) extends ConstructorStatement

sealed abstract class Expression extends MethodStatement

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
case class Signature(returnType: Type, paramTypes: List[Type]) extends AST
case class NewCall(classRef: ClassRef, signature: Signature, params: List[Expression]) extends Expression
case class MethodCall(on: Expression, signature: Signature, name: String, params: List[Expression]) extends Expression
case object ThisRef extends Expression

case class StaticMethodCall(classRef: ClassRef, signature: Signature, name: String, params: List[Expression])
        extends Expression

sealed abstract class Type extends AST
case class ClassRef(pkg: Package, name: String) extends Type
case class Primitive(name: String) extends Type
case class Array(typ: Type) extends Type
case object Void extends Type
