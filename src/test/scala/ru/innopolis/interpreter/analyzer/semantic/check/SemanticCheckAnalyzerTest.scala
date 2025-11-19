package ru.innopolis.interpreter.analyzer.semantic.check

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.{be, noException}
import org.scalatest.matchers.should.Matchers.{a, convertToAnyShouldWrapper}
import ru.innopolis.interpreter.analyzer.semantic.utils.TestUtils.{parse, token}
import ru.innopolis.interpreter.exception.SemanticCheckException
import ru.innopolis.interpreter.lexer.Code
import ru.innopolis.interpreter.syntax.analyzer.semantic.SemanticCheckAnalyzer

class SemanticCheckAnalyzerTest extends AnyFunSuite {

  val semanticCheckAnalyzer = new SemanticCheckAnalyzer()

  test("should throw exception when summing int and bool") {
    val codeBlock = parse(
      List(
        token(Code.INT_LITERAL, 1L),
        token(Code.PLUS),
        token(Code.TRUE)
      )
    )

    val exception = intercept[SemanticCheckException](semanticCheckAnalyzer.analyze(codeBlock))

    exception shouldBe a[SemanticCheckException]
  }

  test("should not throw exception when summing int and real") {
    val codeBlock = parse(
      List(
        token(Code.INT_LITERAL, 1L),
        token(Code.PLUS),
        token(Code.REAL_LITERAL, 2.0)
      )
    )

    noException should be thrownBy semanticCheckAnalyzer.analyze(codeBlock)
  }

  test("should throw exception when using undeclared variable") {
    val codeBlock = parse(
      List(
        token(Code.IDENTIFIER, "a"),
        token(Code.PLUS),
        token(Code.INT_LITERAL, 1L)
      )
    )

    val exception = intercept[SemanticCheckException](semanticCheckAnalyzer.analyze(codeBlock))

    exception shouldBe a[SemanticCheckException]
  }

  test("should throw exception when redeclaring variable") {
    val codeBlock = parse(
      List(
        token(Code.VAR),
        token(Code.IDENTIFIER, "a"),
        token(Code.ASSIGNMENT),
        token(Code.INT_LITERAL, 1L),

        token(Code.NEWLINE),

        token(Code.VAR),
        token(Code.IDENTIFIER, "a"),
        token(Code.ASSIGNMENT),
        token(Code.INT_LITERAL, 2L)
      )
    )

    val exception = intercept[SemanticCheckException](semanticCheckAnalyzer.analyze(codeBlock))

    exception shouldBe a[SemanticCheckException]
  }


}
