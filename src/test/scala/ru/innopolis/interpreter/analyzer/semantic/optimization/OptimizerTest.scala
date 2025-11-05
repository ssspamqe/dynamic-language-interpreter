package ru.innopolis.interpreter.analyzer.semantic.optimization

import org.scalatest.funsuite.AnyFunSuite
import ru.innopolis.interpreter.lexer.{Code, Span, Token}
import ru.innopolis.interpreter.syntax.analyzer.parser.{AASTParser, TokenStream}
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.literal.Literal
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.{CodeBlock, ExpressionStatement}
import ru.innopolis.interpreter.analyzer.semantic.optimization.Optimizer

class OptimizerTest extends AnyFunSuite {

  private val dummySpan = Span(0, 0, 0)

  private def token(code: Code, value: Any = null): Token[_] = {
    Token(dummySpan, code, value)
  }

  private def parse(tokens: List[Token[_]]): CodeBlock = {
    val parser = new AASTParser(new TokenStream(tokens))
    parser.parse()
  }

  test("sum constants") {
    val tokens = List(
      token(Code.INT_LITERAL, 1L),
      token(Code.PLUS),
      token(Code.INT_LITERAL, 2L)
    )
    val result = Optimizer.optimize(parse(tokens))

    assert(
      result == CodeBlock(
        List(
          ExpressionStatement(
            Literal(42L)
          )
        )
      )
    )
  }
}
