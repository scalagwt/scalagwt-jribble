package com.google.jribble

import scala.util.parsing.combinator.token.StdTokens

trait JribbleTokens extends StdTokens {

  case class CharLit(char: Char) extends Token {
    def chars = char.toString
  }

  case class FloatingPointLit(chars: String) extends Token

}
