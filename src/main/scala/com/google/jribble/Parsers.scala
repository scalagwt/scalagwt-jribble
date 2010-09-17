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
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.{PackratParsers, ImplicitConversions}

/**
 * Collection of jribble parsers.
 *
 * All String literals occuring in Parser definition are either Keywords or delimiters (also represented as Keyword
 * token).
 */
trait Parsers extends StdTokenParsers with PackratParsers with ImplicitConversions {

  type Tokens = Lexer
  val lexical = new Tokens

  lexical.reserved ++= Parsers.reserved

  lexical.delimiters ++= Parsers.delimiters

  import lexical.{Keyword, Identifier}

  def modifs(allowed: Set[String]): Parser[List[String]] =
    rep(accept("modifier", { case Keyword(x) if allowed contains x => x }))

  val classModifs: Parser[Set[String]] = {
    val allowed = Set("public", "final")
    modifs(allowed).map(_.toSet)
  }

  val interfaceModifs: Parser[Set[String]] = {
    val allowed = Set("public", "abstract")
    modifs(allowed).map(_.toSet)
  }

  val classDef: Parser[ClassDef] = ((classModifs <~ "class") ~! ref ~! opt(extendsDef) ~!
          opt(implementsDef) ~! classBody) ^^ {
      case modifs ~ classRef ~ ext ~ impl ~ body => ClassDef(modifs, classRef, ext, impl.getOrElse(Nil), body)
    }

  val interfaceDef: Parser[InterfaceDef] = ((interfaceModifs <~ "interface") ~! ref ~!
            opt(extendsDef) ~! interfaceBody) ^^ InterfaceDef

  def name: Parser[String] = accept("identifier", { case Identifier(x) => x})

  def ref: Parser[Ref] = ((rep1sep(name, "/") <~";") into {
    case x :: xs if x.startsWith("L") => success((x drop 1) :: xs)
    case _ => failure("Reference must start with 'L'")
  }) ^^ { xs =>
    val pkg = xs.init match {
      case Nil => None
      case xs => Some(Package(xs mkString "/"))
    }
    Ref(pkg, xs.last)
  }

  def extendsDef: Parser[Ref] = "extends" ~> ref

  def implementsDef: Parser[List[Ref]] = "implements" ~> rep1sep(ref, ",")

  def classBody: Parser[List[ClassBodyElement]] = "{" ~> rep(constructor | methodDef) <~ "}"

  def interfaceBody: Parser[List[MethodDef]] = {
    val methodDef = this.methodDef into {
      case x : MethodDef if x.body.statements.isEmpty => success(x)
      case x => failure("Method definition should have an empty body.")
    }
    "{" ~> rep(methodDef) <~ "}"
  }

  //todo (grek): introduce separate nodes for all primitive types
  def primitive: Parser[Primitive] = {
    // BOOLEAN | BYTE | CHAR | DOUBLE | FLOAT | INT | LONG | SHORT
    val allowed = List("Z", "B", "C", "D", "F", "I", "J", "S")
    accept("primitive type", { case Identifier(x) if allowed contains x => Primitive(x)}) <~ ";"
  }
  def array: Parser[Array] = (primitive | ref) ~ rep1(Keyword("[")) ^^ {
    case typ ~ xs => {
      def iterateTimes[T](f: T => T, v: T, n: Int): T = if (n <= 0) v else f(iterateTimes(f, v, n-1))
      Array(iterateTimes(Array: (Type => Type), typ, xs.length-1))
    }
  }
  def typ: Parser[Type] = array | primitive | ref

  def paramsDef: Parser[List[ParamDef]] = {
    val paramDef: Parser[ParamDef] = typ ~ name ^^ { case t ~ i => ParamDef(i, t) }
    val noParams: Parser[List[ParamDef]] = "(" ~ ")" ^^^ List()
    val atLeastOneParam = "(" ~> rep1sep(paramDef, ",") <~ ")"
    noParams | atLeastOneParam
  }

  def statements[T <: Statement](statement: Parser[T]): Parser[List[T]] =
    "{" ~> rep(statement) <~ "}"

  def block: Parser[Block] = statements(methodStatement) ^^ (Block(_))

  def methodBody: Parser[Block] = block

  def superConstructorCallStatement: Parser[SuperConstructorCall] = {
    val superSignature = "(" ~> ((ref <~ "::") <~ "super") ~! ("(" ~> (typ *) <~ ")") ~! VoidType <~ ")" ^^ {
      case on ~ paramTypes ~ returnType => Signature(on, "super", paramTypes, returnType)
    }
    (superSignature ~! params <~ ";") ^^ { case signature ~ params => SuperConstructorCall(signature, params) }
  }
  def constructorBody: Parser[Block] =
    statements(superConstructorCallStatement | methodStatement) ^^ (Block(_))

  //todo (grek): hard-coded "public"
  //todo (grek): should we check if the name of enclosing class watches constructor's name?
  def constructor: Parser[Constructor] = ("public" ~> name ~ paramsDef) ~! constructorBody ^^ Constructor

  val methodModifs: Parser[Set[String]] = {
    val allowed = Set("public", "final", "static")
    modifs(allowed).map(_.toSet)
  }

  def methodDef: Parser[MethodDef] = methodModifs ~ returnType ~! name ~! paramsDef ~! methodBody ^^ MethodDef


  def VoidType: Parser[Type] = (Identifier("V") ~! ";" ^^^ Void)

  def returnType: Parser[Type] =   VoidType | typ

  def varDef: Parser[VarDef] = typ ~ (ident <~ "=") ~ expression ^^ VarDef

  def methodStatement: Parser[Statement] = ifStatement | ((varDef | assignment | expression) <~ ";")

  def ifStatement: Parser[If] =
    ("if" ~> "(" ~> expression <~ ")") ~! block ~ opt("else" ~> block) ^^ If

  lazy val conditional: PackratParser[Conditional] = "(" ~> (expression <~ "?") ~! ("(" ~> typ <~ ")") ~! expression ~!
          (":" ~> expression) <~ ")" ^^ Conditional

  lazy val instanceOf: PackratParser[InstanceOf] = expression ~
          ("." ~> "<" ~> "instanceof" ~> ">" ~> ("(" ~> ref <~ ")")) ^^ InstanceOf

  lazy val cast: PackratParser[Cast] = expression ~ ("." ~> "<" ~> "cast" ~> ">" ~> ("(" ~> ref <~ ")")) ^^ Cast

  lazy val expression: PackratParser[Expression] = {
    val thisRef: Parser[Expression] = "this" ^^^ ThisRef
    val varRef: Parser[VarRef] = ident ^^ (VarRef)
    methodCall | instanceOf | cast | staticCall | newCall | conditional | literal | thisRef | varRef
  }

  lazy val signature: Parser[Signature] = "(" ~> ((ref <~ "::") ~ name) ~!
          ("(" ~> (typ *) <~ ")") ~! returnType <~ ")" ^^ Signature

  lazy val staticCall: PackratParser[StaticMethodCall] = (ref ~ ("." ~> signature) ~! params) ^^ StaticMethodCall

  lazy val methodCall: PackratParser[MethodCall] = expression ~ ("." ~> signature) ~! params ^^ MethodCall

  lazy val newCall: PackratParser[NewCall] = ("new" ~> signature ~! params) ^^ NewCall

  lazy val params: PackratParser[List[Expression]] = {
    val noParams: Parser[List[Expression]] = "(" ~ ")" ^^^ List()
    val atLeastOneParam: Parser[List[Expression]] = "(" ~> rep1sep(expression, ",") <~ ")"
    noParams | atLeastOneParam
  }

  def varRef = ident

  def assignment: Parser[Assignment] = ident ~ ("=" ~> expression) ^^ Assignment

  //todo (grek): implement parsing of all literals, recheck what we already have and make it less hacky
  def literal: Parser[Literal] = {
    val bool: Parser[BooleanLiteral] = (Keyword("false") | Keyword("true")) ^^ {
      case Keyword(x) => BooleanLiteral(x != "false")
    }
    val char: Parser[CharLiteral] =
      accept("character literal", { case lexical.CharLit(x) => CharLiteral(x)})
    bool | char | (stringLit ^^ StringLiteral)
  }

  /** Parse some prefix of reader `in' with parser `p' */
  def parse[T](p: Parser[T], in: scala.util.parsing.input.Reader[Char]): ParseResult[T] =
    phrase(p)(new lexical.Scanner(in))

  /** Parse some prefix of reader `in' with parser `p' */
  def parse[T](p: Parser[T], in: java.io.Reader): ParseResult[T] = {
    import scala.collection.immutable.PagedSeq
    import scala.util.parsing.input.PagedSeqReader
    parse(p, new PagedSeqReader(PagedSeq.fromReader(in)))
  }

  /** Parse some prefix of character sequence `in' with parser `p' */
  def parse[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] =
    parse(p, new scala.util.parsing.input.CharSequenceReader(in))

}

object Parsers {
  val reserved = List("public", "final", "abstract", "class", "interface",
                            "extends", "implements", "static", "super", "this",
                            "new", "false", "true", "if", "else", "instanceof", "cast")
  val delimiters = List("{", "}", ":", ";", "/", "(", ")", "?", "[", "::", ".", ",", "=", "<", ">")
}
