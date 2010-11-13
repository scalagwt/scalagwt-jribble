package com.google.jribble

import util.parsing.combinator.lexical.StdLexical
import util.parsing.input.CharArrayReader.EofCh

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
    | '\'' ~ chrExcept('\'', '\n', EofCh) ~ '\''        ^^ { case '\'' ~ char ~ '\'' => CharLit(char) }
    | '\"' ~ rep( chrExcept('\"', '\n', EofCh) ) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit(chars mkString "") }
    | EofCh                                             ^^^ EOF
    | '\'' ~> failure("unclosed string literal")
    | '\"' ~> failure("unclosed string literal")
    | '`'  ~> failure("unclosed escaped keyword")
    | delim
    | failure("illegal character")
    )

  private val integer = opt('-') ~ rep1(digit) ^^ {
    case minus ~ digits => minus.toList ::: digits mkString ""
  }

  override val identChar = letter | elem('_') | elem('$')

  override val whitespace = rep(whitespaceChar)

}
