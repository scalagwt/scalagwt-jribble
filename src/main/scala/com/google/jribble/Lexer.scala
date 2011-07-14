package com.google.jribble

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh

class Lexer extends StdLexical with JribbleTokens {

  // see `token' in `Scanners'
  override lazy val token: Parser[Token] =
    ( identChar ~ rep(identChar | digit)             ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
    | '`' ~> rep1(identChar) <~ '`'                     ^? {
      case chars if reserved contains (chars mkString "") => Identifier(chars mkString "") 
    }
    | opt('-') ~ rep1(digit) ~ ('.' ~ rep1(digit)) ~ opt('E' ~> integer) ~ (elem('D') | elem('F'))   ^^ {
      case minus ~ integer ~ fractional ~ exp ~ suffix =>
        FloatingPointLit(minus.toList ::: integer ::: mkList(fractional) ::: exp.map("E"+).toList ::: suffix :: Nil mkString "")
    }
    | integer ~ opt('L')                 ^^ {
      case integer ~ ending => NumericLit(integer + ending.getOrElse(""))
    }
    | '\'' ~ '\\' ~ rep1(digit) ~ '\''                  ^^ { case '\'' ~ '\\' ~ digits ~ '\'' =>
      CharLit(Integer.decode(digits mkString "").intValue.asInstanceOf[Char])
    }
    | '\'' ~ escapedChar ~ '\''   ^^ { case '\'' ~ char ~ '\'' => CharLit(char) }
    | '\"' ~ escapedString ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit(chars mkString "") }
    | EofCh                                             ^^^ EOF
    | '\'' ~> failure("unclosed string literal")
    | '\"' ~> failure("unclosed string literal")
    | '`'  ~> failure("unclosed escaped keyword")
    | delim
    | failure("illegal character")
    )

  private val escapingCodeChar: Parser[Char] = {
    val escapes = Map('b' -> '\b', 't' -> '\t', 'n' -> '\n', 'f' -> '\f',
          'r' -> '\r', '"' -> '\"', '\'' -> '\'', '\\' -> '\\')
    '\\' ~> elem("escaped code", _ => true) ^? escapes
  }

  private val escapingOctalChar: Parser[Char] =
    '\\' ~> '0' ~ digit ~ digit ^^ {
      case zero ~ digit1 ~ digit2 =>
        Integer.decode(zero.toString + digit1.toString + digit2.toString).intValue.asInstanceOf[Char]
    }

  private val escapedString: Parser[List[Char]] =
    rep(escapingCodeChar | escapingOctalChar | chrExcept('\"', '\n', EofCh, '\\'))

  private val escapedChar: Parser[Char] =
    escapingCodeChar | escapingOctalChar | chrExcept('\'', '\n', EofCh, '\\')

  private val integer = opt('-') ~ rep1(digit) ^^ {
    case minus ~ digits => minus.toList ::: digits mkString ""
  }

  override val identChar = letter | elem('_') | elem('$')

  override val whitespace = rep(whitespaceChar)

}
