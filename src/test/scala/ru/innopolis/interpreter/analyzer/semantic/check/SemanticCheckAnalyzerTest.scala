package ru.innopolis.interpreter.analyzer.semantic.check

import org.scalatest.funsuite.AnyFunSuite
import ru.innopolis.interpreter.lexer.{Code, Span, Token}
import ru.innopolis.interpreter.syntax.analyzer.parser.{AASTParser, TokenStream}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.CodeBlock

class SemanticCheckAnalyzerTest extends AnyFunSuite {

  private val dummySpan = Span(0, 0, 0)

  private def token(code: Code, value: Any = null): Token[_] =
    Token(dummySpan, code, value)


  private def parse(tokens: List[Token[_]]): CodeBlock = {
    val parser = new AASTParser(new TokenStream(tokens))
    parser.parse()
  }



}
