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

  //todo (grek): deal with default (empty) packages
  val packageCoord: Parser[Package] = ("package" ~> ws ~> (ident ~ (("." ~> ident)*))) ^^ {
    case x ~ xs => Package((x :: xs) mkString ".")
  }

  val packageDeclaration: Parser[Package] = packageCoord <~ ";" <~ LF

  val classModifs: Parser[Set[String]] = {
    val clazzModif: Parser[String] = {
      val allowedModifs = Set("public", "final")
      ident into { x =>
        if (allowedModifs contains x)
          success(x)
        else
          failure("Class modifier can be only one of " + allowedModifs)
      }
    }
    ((clazzModif <~ ws) *).map(_.toSet)
  }

  val classDef: Parser[ClassDef] =
    ((packageDeclaration <~ ignoreWsLF) ~ (classModifs <~ "class" <~ ws) ~ (name <~ ws) ~
            ((extendsDef <~ ws)?) ~ ((implementsDef <~ ws)?) ~! classBody) ^^ {
      case pkg ~ modifs ~ name ~ ext ~ impl ~ body => ClassDef(pkg, modifs, name, ext, impl.getOrElse(Nil), body)
    }

  def name: Parser[String] = "[a-zA-Z$][a-zA-Z0-9_$]*".r

  def classRef: Parser[ClassRef] = ("(" ~> packageCoord <~ ")") ~! (("." ~> name)) ^^ {
    case x ~ y => ClassRef(x, y)
  }

  def extendsDef: Parser[ClassRef] = "extends" ~> ws ~> classRef

  def implementsDef: Parser[List[ClassRef]] = "implements" ~> ws ~> classRef ~ (("," ~> ws ~> classRef)*) ^^ {
    case x ~ xs => x :: xs
  }

  def classBody: Parser[List[Either[Constructor, MethodDef]]] = {
    val constructor = this.constructor ^^ (Left(_))
    val methodDef = this.methodDef ^^ (Right(_))
    val bodyElement = constructor | methodDef
    "{" ~> ignoreWsLF ~> ((ignoreWsLF ~> bodyElement <~ ignoreWsLF)*) <~ "}"
  }

  def primitive: Parser[Primitive] =
    ("byte" | "short" | "int" | "long" | "float" | "double" | "boolean" | "char") ^^ (Primitive(_))
  def array: Parser[Array] = ((primitive | classRef) ~ (literal("[]")+)) ^^ {
    case typ ~ xs => {
      def iterateTimes[T](f: T => T, v: T, n: Int): T = if (n <= 0) v else f(iterateTimes(f, v, n-1))
      Array(iterateTimes(Array: (Type => Type), typ, xs.length-1))
    }
  }
  def typ: Parser[Type] = array | primitive | classRef

  def paramsDef: Parser[List[ParamDef]] = {
    val paramDef: Parser[ParamDef] = ((typ <~ ws) ~ name) ^^ { case t ~ i => ParamDef(i, t) }
    val noParams: Parser[List[ParamDef]] = "()" ^^^ List()
    val atLeastOneParam = ("(" ~> paramDef ~ (("," ~> ws ~> paramDef)*) <~ ")") ^^ {
      case x ~ xs => x :: xs
    }
    noParams | atLeastOneParam
  }

  def statements[T <: Statement](statement: Parser[T]): Parser[List[T]] =
    (("{" ~ LF ~ "}") ^^^ List()) | ("{" ~> (((ignoreWsLF ~> statement) <~ ignoreWsLF)+) <~ "}")

  def methodBody: Parser[List[MethodStatement]] = statements(methodStatement)

  def superConstructorCallStatement: Parser[SuperConstructorCall] = ("super" ~> signature ~ params <~ ";") ^^ {
    case signature ~ params => SuperConstructorCall(signature, params)
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

  def returnType: Parser[Type] = typ | ("void" ^^^ Void)

  def varDef: Parser[VarDef] = typ ~ ((ws ~> ident) <~ (ws ~ "=" ~ ws)) ~ expression ^^ {
    case typ ~ ident ~ expression => VarDef(typ, ident, expression)
  }

  def methodStatement: Parser[MethodStatement] = (varDef | assignment | expression) <~ ";"

  def expression: Parser[Expression] = {
    val staticCall: Parser[StaticMethodCall] = (classRef ~ methodCall) ^^ {
      case classRef ~ (name ~ signature ~ params) => StaticMethodCall(classRef, signature, name, params)
    }
    val thisRef: Parser[Expression] = "this" ^^^ ThisRef
    val varRef: Parser[VarRef] = ident ^^ (VarRef)
    ((literal | newCall | staticCall | thisRef | varRef) ~ (methodCall *)) ^^ {
      case on ~ calls => {
        //todo (grek): this fragment even if involves simple folding of calls might be slightly dense and might deserve
        //todo (grek): a few words of more elaborate explanation. Must ask others for opinion
        val fs: List[Expression => Expression] = calls map {
          case name ~ signature ~ params => MethodCall(_: Expression, signature, name, params)
        }
        //type ascription is needed because otherwise Expression with Product is inferred which causes type checking
        //errors in f(x) expression because it result in Expression and not Expression with Product
        fs.foldLeft(on: Expression) {
          case (x, f) => f(x)
        }
      }
    }
  }

  def signature: Parser[Signature] = ("<" ~> returnType) ~! ((("," ~! ws) ~> typ)*) <~ ">" ^^ {
    case x ~ ys => Signature(x, ys)
  }

  def methodCall: Parser[String ~ Signature ~ List[Expression]] = "." ~> name ~ signature ~ params

  def newCall: Parser[NewCall] = ("new" ~> ws ~> classRef ~! signature ~! params) ^^ {
    case classRef ~ signature ~ params => NewCall(classRef, signature, params)
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
