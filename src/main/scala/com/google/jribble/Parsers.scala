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

trait Parsers extends scala.util.parsing.combinator.RegexParsers {

  override val skipWhitespace = false

  private val ident: Parser[String] = "[a-zA-Z][a-zA-Z0-9_]*".r

  def modifs(allowed: Set[String]): Parser[List[String]] = {
    val modif = ident into { x =>
      if (allowed contains x)
        success(x)
      else
        failure("Modifier can be only one of " + allowed)
    }
    ((modif <~ ws) *)
  }

  val classModifs: Parser[Set[String]] = {
    val allowed = Set("public", "final")
    modifs(allowed).map(_.toSet)
  }

  val interfaceModifs: Parser[Set[String]] = {
    val allowed = Set("public", "abstract")
    modifs(allowed).map(_.toSet)
  }

  val classDef: Parser[ClassDef] = ((classModifs <~ "class" <~ ws) ~! (ref <~ ws) ~!
            ((extendsDef <~ ws)?) ~! ((implementsDef <~ ws)?) ~! classBody) ^^ {
      case modifs ~ classRef ~ ext ~ impl ~ body => ClassDef(modifs, classRef, ext, impl.getOrElse(Nil), body)
    }

  val interfaceDef: Parser[InterfaceDef] = ((interfaceModifs <~ "interface" <~ ws) ~! (ref <~ ws) ~!
            ((extendsDef <~ ws)?) ~! interfaceBody) ^^ {
      case modifs ~ interfaceRef ~ ext ~ body => InterfaceDef(modifs, interfaceRef, ext, body)
    }

  def name: Parser[String] = "[a-zA-Z$][a-zA-Z0-9_$]*".r

  def coord: Parser[List[String]] = name ~ (("/" ~> name)*) ^^ { case x ~ xs => x :: xs }

  def ref: Parser[Ref] = ("L" ~> coord <~ ";") ^^ { xs =>
    val pkg = xs.init match {
      case Nil => None
      case xs => Some(Package(xs mkString "/"))
    }
    Ref(pkg, xs.last)
  }

  def extendsDef: Parser[Ref] = "extends" ~> ws ~> ref

  def implementsDef: Parser[List[Ref]] = "implements" ~> ws ~> ref ~(("," ~> ws ~> ref)*) ^^ {
    case x ~ xs => x :: xs
  }

  def classBody: Parser[List[Either[Constructor, MethodDef]]] = {
    val constructor = this.constructor ^^ (Left(_))
    val methodDef = this.methodDef ^^ (Right(_))
    val bodyElement = constructor | methodDef
    "{" ~> ignoreWsLF ~> ((ignoreWsLF ~> bodyElement <~ ignoreWsLF)*) <~ "}"
  }

  def interfaceBody: Parser[List[MethodDef]] = {
    val methodDef = this.methodDef into {
      case x @ MethodDef(_, _, _, body) if body.isEmpty => success(x)
      case x => failure("Method definition should have an empty body.")
    }
    "{" ~> ignoreWsLF ~> ((ignoreWsLF ~> methodDef <~ ignoreWsLF)*) <~ "}"
  }

  // BOOLEAN | BYTE | CHAR | DOUBLE | FLOAT | INT | LONG | SHORT
  //todo (grek): introduce separate nodes for all primitive types
  def primitive: Parser[Primitive] = ("Z" | "B" | "C" | "D" | "F" | "I" | "J" | "S") ^^ (Primitive(_))
  def array: Parser[Array] = ((primitive | ref) ~ (literal("[")+)) ^^ {
    case typ ~ xs => {
      def iterateTimes[T](f: T => T, v: T, n: Int): T = if (n <= 0) v else f(iterateTimes(f, v, n-1))
      Array(iterateTimes(Array: (Type => Type), typ, xs.length-1))
    }
  }
  def typ: Parser[Type] = array | primitive | ref

  def paramsDef: Parser[List[ParamDef]] = {
    val paramDef: Parser[ParamDef] = ((typ <~ ws) ~ name) ^^ { case t ~ i => ParamDef(i, t) }
    val noParams: Parser[List[ParamDef]] = "()" ^^^ List()
    val atLeastOneParam = ("(" ~> paramDef ~ (("," ~> ws ~> paramDef)*) <~ ")") ^^ {
      case x ~ xs => x :: xs
    }
    noParams | atLeastOneParam
  }

  def statements[T <: Statement](statement: Parser[T]): Parser[List[T]] =
    (("{" ~ ignoreWsLF ~ "}") ^^^ List()) | ("{" ~> (((ignoreWsLF ~> statement) <~ ignoreWsLF)+) <~ "}")

  def methodBody: Parser[List[MethodStatement]] = statements(methodStatement)

  def superConstructorCallStatement: Parser[SuperConstructorCall] = {
    val superSignature = signature into {
      case s @ Signature(_, name, _, _) if name == "super" => success(s)
      case s => failure("Super call must have 'super' as name of a method in singature.")
    }
    (superSignature ~ params <~ ";") ^^ { case signature ~ params => SuperConstructorCall(signature, params) }
  }
  def constructorBody: Parser[List[ConstructorStatement]] = statements(superConstructorCallStatement | methodStatement)

  //todo (grek): hard-coded "public"
  def constructor: Parser[Constructor] = ("public" ~> ws ~> name ~ paramsDef) ~! (ws ~> constructorBody) ^^ {
    //todo (grek): should we check if the name of enclosing class watches constructor's name?
    case name ~ paramsDef ~ body => Constructor(name, paramsDef, body)
  }

  //todo (grek): hard-coded "public"
  def methodDef: Parser[MethodDef] = ("public" ~> ws ~> returnType <~ ws) ~! name ~! (paramsDef <~ ws) ~! methodBody ^^ {
    case returnType ~ name ~ paramsDef ~ body => MethodDef(returnType, name, paramsDef, body)
  }

  def returnType: Parser[Type] = typ | ("V" ^^^ Void)

  def varDef: Parser[VarDef] = typ ~ ((ws ~> ident) <~ (ws ~ "=" ~ ws)) ~ expression ^^ {
    case typ ~ ident ~ expression => VarDef(typ, ident, expression)
  }

  def methodStatement: Parser[MethodStatement] = (varDef | assignment | expression) <~ ";"

  def expression: Parser[Expression] = {
    val staticCall: Parser[StaticMethodCall] = (ref ~ methodCall) ^^ {
      case ref ~ (signature ~ params) => StaticMethodCall(ref, signature, params)
    }
    val thisRef: Parser[Expression] = "this" ^^^ ThisRef
    val varRef: Parser[VarRef] = ident ^^ (VarRef)
    ((literal | newCall | staticCall | thisRef | varRef) ~ (methodCall *)) ^^ {
      case on ~ calls => {
        //todo (grek): this fragment even if involves simple folding of calls might be slightly dense and might deserve
        //todo (grek): a few words of more elaborate explanation. Must ask others for opinion
        val fs: List[Expression => Expression] = calls map {
          case signature ~ params => MethodCall(_: Expression, signature, params)
        }
        //type ascription is needed because otherwise Expression with Product is inferred which causes type checking
        //errors in f(x) expression because it result in Expression and not Expression with Product
        fs.foldLeft(on: Expression) {
          case (x, f) => f(x)
        }
      }
    }
  }

  def signature: Parser[Signature] = "(" ~> ((ref <~ "::") ~ name) ~! ("(" ~> (typ *) <~ ")") ~! returnType <~ ")" ^^ {
    case on ~ name ~ paramTypes ~ returnType => Signature(on, name, paramTypes, returnType)
  }

  def methodCall: Parser[Signature ~ List[Expression]] = "." ~> signature ~! params

  def newCall: Parser[NewCall] = ("new" ~> ws ~> signature ~! params) ^^ {
    case signature ~ params => NewCall(signature, params)
  }

  def params: Parser[List[Expression]] = {
    val noParams: Parser[List[Expression]] = "()" ^^^ List()
    val atLeastOneParam: Parser[List[Expression]] = "(" ~> ( expression ~ (("," ~> ws ~> expression)*) ) <~ ")" ^^ {
      case x ~ xs => x :: xs
    }
    noParams | atLeastOneParam
  }

  def varRef = ident

  def assignment: Parser[Assignment] = ident ~ ((ws ~ "=" ~ ws) ~> expression) ^^ {
    case ident ~ expression => Assignment(ident, expression)
  }

  //todo (grek): implement parsing of all literals, recheck what we already have and make it less hacky
  def literal: Parser[Literal] = {
    val stringChar = chrExcept('\n', '\"')
    val stringLiteral: Parser[StringLiteral] = ('\"' ~> (stringChar *) <~ '\"') ^^ (x => StringLiteral(x mkString))
    val bool: Parser[BooleanLiteral] = ("false" | "true") ^^ (x => BooleanLiteral(x != "false"))
    val char: Parser[CharLiteral] = ("'" ~> chrExcept('\'') <~ "'") ^^ (CharLiteral)
    bool | char | stringLiteral
  }

  import scala.util.parsing.input.CharSequenceReader.EofCh
  private def chrExcept(except: Char*): Parser[Char] = elem("", ch =>
    (  ch != EofCh && !except.contains(ch))
  )

  private def ws: Parser[Char] = ' '

  private def LF: Parser[Char] = '\n'

  private def ignoreWsLF: Parser[Unit] = ((LF | ws)*) ^^^ (())

}
