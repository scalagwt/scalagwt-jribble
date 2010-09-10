package com.google.jribble

import util.parsing.combinator.lexical.StdLexical
import util.parsing.input.CharArrayReader.EofCh

class Lexer extends StdLexical with JribbleTokens {

  // see `token' in `Scanners'
  override def token: Parser[Token] =
    ( identChar ~ rep( identChar | digit )              ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
    | digit ~ rep( digit )                              ^^ { case first ~ rest => NumericLit(first :: rest mkString "") }
    | '\'' ~ chrExcept('\'', '\n', EofCh) ~ '\'' ^^ { case '\'' ~ char ~ '\'' => CharLit(char) }
    | '\"' ~ rep( chrExcept('\"', '\n', EofCh) ) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit(chars mkString "") }
    | EofCh                                             ^^^ EOF
    | '\'' ~> failure("unclosed string literal")
    | '\"' ~> failure("unclosed string literal")
    | delim
    | failure("illegal character")
    )

  override def identChar = letter | elem('_') | elem('$')

  override def whitespace = rep(whitespaceChar)

}
