package ru.innopolis.interpreter.analyzer.semantic.utils

import ru.innopolis.interpreter.lexer.{Code, Span, Token}
import ru.innopolis.interpreter.syntax.analyzer.parser.{AASTParser, TokenStream}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.CodeBlock

object TestUtils {
  val dummySpan = Span(0, 0, 0)

  def token(code: Code, value: Any = null): Token[_] =
    Token(dummySpan, code, value)


  def parse(tokens: List[Token[_]]): CodeBlock = {
    val parser = new AASTParser(new TokenStream(tokens))
    parser.parse()
  }
}
