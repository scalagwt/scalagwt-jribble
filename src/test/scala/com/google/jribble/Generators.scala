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
import org.scalacheck.{Arbitrary, Gen}

object Generators {

  private case class ExprDepth(x: Int) {
    def map(f: Int => Int) = ExprDepth(f(x))
  }
  private case class StmtDepth(x: Int) {
    def map(f: Int => Int) = StmtDepth(f(x))
  }

  //todo (grek): jribble identifiers (names) allow more characters than those generated by standard Gen.identifier
  def identifier = {
    def isValid(x: String) = !(Parsers.reserved contains x)
    for (v <- Gen.resize(7, Gen.identifier) if isValid(v)) yield v
  }

  def pkg: Gen[Package] = for(x <- identifier; xs <- Gen.resize(2, Gen.listOf(identifier))) yield
    Package(x + (xs.map("/" + _) mkString))

  def ref: Gen[Ref] = {
    implicit val arbPackage = Arbitrary(pkg)
    for(name <- identifier; p <- Arbitrary.arbitrary[Option[Package]]) yield Ref(p, name)
  }

  def primitive: Gen[Primitive] =
    for (name <- Gen.oneOf("Z", "B", "C", "D", "F", "I", "J", "S")) yield Primitive(name)

  def array: Gen[Array] = for (t <- typ) yield Array(t)

  def typ: Gen[Type] = Gen.oneOf(ref, primitive, Gen.lzy(array))

  def paramsDef: Gen[List[ParamDef]] = {
    val paramDef: Gen[ParamDef] = for (n <- identifier; t <- typ) yield ParamDef(n, t)
    Gen.resize(4, Gen.listOf(paramDef))
  }

  def params: Gen[List[Expression]] = Gen.resize(4, Gen.listOf(expression(ExprDepth(0))))

  //todo (grek): implement generation of all literals
  def literal: Gen[Literal] = {
    import Arbitrary._
    //todo (grek): once parsing of escaped is implemented we should switch to broader domain of strings
    val stringLiteral = for (x <- Gen.listOf(Gen.alphaNumChar)) yield StringLiteral(x mkString)
    //TODO(grek): figure out what's the biggest subset of Chars that is supported in jribble or implement
    //support for encoding all chars
    val charLiteral = for (x <- Gen.alphaNumChar) yield CharLiteral(x)
    val booleanLiteral = for (x <- arbitrary[Boolean]) yield BooleanLiteral(x)
    stringLiteral | charLiteral | booleanLiteral
  }
  def varRef = identifier.map(VarRef)
  def signature: Gen[Signature] = for {
    on <- ref
    n <- identifier
    paramTypes <- Gen.listOf(typ)
    r <- returnType
  } yield Signature(on, n, paramTypes, r)
  
  def newCall(implicit depth: ExprDepth): Gen[NewCall] = for (s <- signature; p <- params) yield NewCall(s, p)
  def methodCall(implicit depth: ExprDepth): Gen[MethodCall] =
    for (on <- expression; s <- signature; p <- params) yield MethodCall(on, s, p)
  def staticMethodCall(implicit depth: ExprDepth): Gen[StaticMethodCall] =
    for (c <- ref; s <- signature; p <- params) yield StaticMethodCall(c, s, p)
  def conditional(implicit depth: ExprDepth): Gen[Conditional] = for {
    condition <- expression
    typ <- typ
    then <- expression
    elsee <- expression
  } yield Conditional(condition, typ, then, elsee)

  def instanceOf(implicit depth: ExprDepth): Gen[InstanceOf] =
    for (on <- expression; t <- ref) yield InstanceOf(on, t)

  def cast(implicit depth: ExprDepth): Gen[Cast] = for (on <- expression; t <- ref) yield Cast(on, t)

  def arrayInitializer(implicit depth: ExprDepth): Gen[ArrayInitializer] =
    for (t <- typ; elements <- Gen.listOf(expression)) yield ArrayInitializer(t, elements)

  def expression(implicit depth: ExprDepth): Gen[Expression] = {
    val nonRecursive = Gen.frequency((2, literal), (1, varRef), (1, Gen.value(ThisRef)))
    val newDepth = depth.map(_+1)
    val recursive = Gen.oneOf(Gen.lzy(newCall(newDepth)), Gen.lzy(methodCall(newDepth)),
      Gen.lzy(staticMethodCall(newDepth)), Gen.lzy(conditional(newDepth)), Gen.lzy(instanceOf(newDepth)),
      Gen.lzy(cast(newDepth)), Gen.lzy(arrayInitializer(newDepth)))

    Gen.frequency((3*(depth.x+1), nonRecursive), (1, recursive))
  }

  def varDef: Gen[VarDef] = for (t <- typ; n <- identifier; v <- expression(ExprDepth(0))) yield VarDef(t, n, v)
  def assignment: Gen[Assignment] = for (n <- identifier; v <- expression(ExprDepth(0))) yield Assignment(n, v)
  def ifStatement(implicit depth: StmtDepth): Gen[If] = for {
    condition <- expression(ExprDepth(0));
    then <- block;
    elsee <- Arbitrary.arbitrary[Option[Block]]
  } yield If(condition, then, elsee)

  def tryStatement(implicit depth: StmtDepth): Gen[Try] = for {
    b <- block
    val catchGen = for (r <- ref; n <- identifier; b <- block) yield (r, n, b)
    catches <- Gen.resize(2, Gen.listOf(catchGen))
    finalizer <- Arbitrary.arbitrary[Option[Block]]
    if (!(catches.isEmpty && finalizer.isEmpty))
  } yield Try(b, catches, finalizer)

  def whileStatement(implicit depth: StmtDepth): Gen[While] = for {
    label <- Arbitrary.arbOption(Arbitrary(identifier)).arbitrary
    condition <- expression(ExprDepth(0))
    b <- block
  } yield While(label, condition, b)

  def continueStatement: Gen[Continue] = for {
    label <- Arbitrary.arbOption(Arbitrary(identifier)).arbitrary
  } yield Continue(label)

  def breakStatement: Gen[Break] = for {
    label <- Arbitrary.arbOption(Arbitrary(identifier)).arbitrary
  } yield Break(label)

  def methodStatement(implicit depth: StmtDepth): Gen[Statement] = {
    val nonRecursive = Gen.frequency(
      (15, Gen.oneOf(varDef, assignment, expression(ExprDepth(0)))),
      (1, continueStatement),
      (1, breakStatement))
    val newDepth = depth.map(_+1)
    val recursive = Gen.oneOf(Gen.lzy(ifStatement(newDepth)), Gen.lzy(tryStatement(newDepth)),
      Gen.lzy(whileStatement(newDepth)))
    Gen.frequency((3*(depth.x+1), nonRecursive), (1, recursive))
  }
  def methodStatements(implicit depth: StmtDepth): Gen[List[Statement]] =
    Gen.resize(3, Gen.listOf(methodStatement))

  def constructorSuperCall: Gen[SuperConstructorCall] =
    for (s <- signature; p <- params) yield SuperConstructorCall(s.copy(name = "super", returnType = Void), p)
  //we shuffle the list because in jribble there is no requirement that super constructor call is
  def constructorBody: Gen[Block] = for {
      s <- Gen.resize(1, Gen.listOf(constructorSuperCall))
      ss <- methodStatements(StmtDepth(0))
    } yield Block(scala.util.Random.shuffle(s ::: ss))
  def constructor: Gen[Constructor] =
    for (n <- identifier; p <- paramsDef; b <- constructorBody) yield Constructor(n, p, b)

  def block(implicit depth: StmtDepth): Gen[Block] = methodStatements map (Block(_))
  def methodBody: Gen[Block] = block(StmtDepth(0))
  def returnType = typ | Void
  def methodModifiers: Gen[Set[String]] = {
    val modif = Gen.oneOf("public", "final", "static")
    Gen.resize(3, Gen.listOf(modif)).map(_.toSet)
  }
  def methodDef: Gen[MethodDef] =
    for (m <- methodModifiers; t <- returnType; n <- identifier; p <- paramsDef; b <- methodBody) yield
      MethodDef(m, t, n, p, b)

  def fieldDef: Gen[FieldDef] = {
    def modifiers: Gen[Set[String]] = {
      val modif = Gen.oneOf("public", "final", "static", "private")
      Gen.resize(3, Gen.listOf(modif)).map(_.toSet)
    }
    import Arbitrary._
    for (m <- modifiers; t <- typ; n <- identifier; v <- arbitrary[Option[Expression]]) yield FieldDef(m, t, n, v)
  }

  def classModifiers: Gen[Set[String]] = {
    val modif = Gen.oneOf("public", "final")
    Gen.resize(2, Gen.listOf(modif)).map(_.toSet)
  }

  def interfaceModifiers: Gen[Set[String]] = {
    val modif = Gen.oneOf("public", "abstract")
    Gen.resize(2, Gen.listOf(modif)).map(_.toSet)
  }

  def extendsDef: Gen[Option[Ref]] = Arbitrary.arbitrary[Option[Ref]]

  def implementsDef: Gen[List[Ref]] = Gen.resize(3, Gen.listOf(ref))

  def classBody: Gen[List[ClassBodyElement]] = for {
    cs <- Gen.resize(3, Gen.listOf(constructor))
    ms <- Gen.resize(5, Gen.listOf(methodDef))
    fs <- Gen.resize(3, Gen.listOf(fieldDef))
  } yield cs ++ ms ++ fs

  def interfaceBody: Gen[List[MethodDef]] = for {
    ms <- Gen.resize(5, Gen.listOf(methodDef.filter(_.body.statements.isEmpty)))
  } yield ms

  def classDef: Gen[ClassDef] = for {
    m <- classModifiers
    n <- ref
    e <- extendsDef
    i <- implementsDef
    b <- classBody
  } yield ClassDef(m, n, e, i, b)

  def interfaceDef: Gen[InterfaceDef] = for {
    m <- interfaceModifiers
    n <- ref
    e <- extendsDef
    b <- interfaceBody
  } yield InterfaceDef(m, n, e, b)

  implicit val arbRef = Arbitrary(ref)
  implicit val arbPackage = Arbitrary(pkg)
  implicit val arbPrimitive = Arbitrary(primitive)
  implicit val arbArray = Arbitrary(array)
  implicit val arbType = Arbitrary(typ)
  implicit val arbParamsDef = Arbitrary(paramsDef)
  implicit val arbLiteral = Arbitrary(literal)
  implicit val arbSignature = Arbitrary(signature)
  implicit val arbNewCall = Arbitrary(newCall(ExprDepth(0)))
  implicit val arbMethodCall = Arbitrary(methodCall(ExprDepth(0)))
  implicit val arbStaticMethodCall = Arbitrary(staticMethodCall(ExprDepth(0)))
  implicit val arbConditional = Arbitrary(conditional(ExprDepth(0)))
  implicit val arbInstanceOf = Arbitrary(instanceOf(ExprDepth(0)))
  implicit val arbCast = Arbitrary(cast(ExprDepth(0)))
  implicit val arbArrayInitializer = Arbitrary(arrayInitializer(ExprDepth(0)))
  implicit val arbExpression = Arbitrary(expression(ExprDepth(0)))
  implicit val arbVarDef = Arbitrary(varDef)
  implicit val arbAssignment = Arbitrary(assignment)
  implicit val arbIf = Arbitrary(ifStatement(StmtDepth(0)))
  implicit val arbTry = Arbitrary(tryStatement(StmtDepth(0)))
  implicit val arbWhile = Arbitrary(whileStatement(StmtDepth(0)))
  implicit val arbContinue = Arbitrary(continueStatement)
  implicit val arbBreak = Arbitrary(breakStatement)
  implicit val arbMethodStatement = Arbitrary(methodStatement(StmtDepth(0)))
  implicit val arbMethodBody = Arbitrary(methodBody) 
  implicit val arbConstructor = Arbitrary(constructor)
  implicit val arbMethodDef = Arbitrary(methodDef)
  implicit val arbFieldDef = Arbitrary(fieldDef)
  implicit val arbClassDef = Arbitrary(classDef)
  implicit val arbInterfaceDef = Arbitrary(interfaceDef)
}
