package ru.innopolis.interpreter.syntax.analyzer.parser
import ru.innopolis.interpreter.lexer.Token
import ru.innopolis.interpreter.exception.InvalidTokenException
import ru.innopolis.interpreter.lexer.Code

class TokenStream(tokens: List[Token[_]]) {
  var index: Int = 0

  def hasNext: Boolean = index < tokens.length

  def current: Token[_] = tokens(index)

  def next(): Token[_] = {
    val t = tokens(index)
    index += 1
    t
  }

  def peek(offset: Int = 0): Option[Token[_]] =
    tokens.lift(index + offset)

  def expect(code: Code): Token[_] = {
    val t = next()
    if (t.code != code)
      throw new InvalidTokenException(t, code)
    t
  }

  def skipIf(code: Code): Boolean = {
    if (hasNext && current.code == code) { next(); true }
    else false
  }
}