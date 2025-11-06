package ru.innopolis.interpreter.analyzer.semantic.optimization

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import ru.innopolis.interpreter.analyzer.semantic.utils.TestUtils._
import ru.innopolis.interpreter.lexer.Code
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Variable
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.literal.Literal
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.assignment.VariableAssignment
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration.VariableDeclaration
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.{CodeBlock, ExpressionStatement}

class OptimizerTest extends AnyFunSuite {

  test("sum constants") {
    val tokens = List(
      token(Code.INT_LITERAL, 1L),
      token(Code.PLUS),
      token(Code.INT_LITERAL, 2L)
    )
    val result = Optimizer.optimize(parse(tokens))

    result shouldBe CodeBlock(List(ExpressionStatement(Literal(3L))))
  }

  test("subtract constants") {
    val tokens = List(
      token(Code.INT_LITERAL, 10L),
      token(Code.MINUS),
      token(Code.INT_LITERAL, 3L)
    )
    val result = Optimizer.optimize(parse(tokens))

    result shouldBe CodeBlock(List(ExpressionStatement(Literal(7.0))))
  }

  test("multiply constants") {
    val tokens = List(
      token(Code.INT_LITERAL, 4L),
      token(Code.MULTIPLICATION),
      token(Code.INT_LITERAL, 5L)
    )
    val result = Optimizer.optimize(parse(tokens))

    result shouldBe CodeBlock(List(ExpressionStatement(Literal(20.0))))
  }

  test("divide constants") {
    val tokens = List(
      token(Code.INT_LITERAL, 8L),
      token(Code.DIVISION),
      token(Code.INT_LITERAL, 2L)
    )
    val result = Optimizer.optimize(parse(tokens))

    result shouldBe CodeBlock(List(ExpressionStatement(Literal(4.0))))
  }

  test("comparison less-than constants") {
    val tokens = List(
      token(Code.INT_LITERAL, 3L),
      token(Code.LESS),
      token(Code.INT_LITERAL, 5L)
    )
    val result = Optimizer.optimize(parse(tokens))

    result shouldBe CodeBlock(List(ExpressionStatement(Literal(true))))
  }

  test("boolean and/or/xor constants") {
    val andTokens = List(
      token(Code.VAR), token(Code.IDENTIFIER, "a"),
      token(Code.ASSIGNMENT),
      token(Code.TRUE), token(Code.AND), token(Code.FALSE),
      token(Code.NEWLINE), token(Code.IDENTIFIER, "a")
    )

    val orTokens = List(token(Code.TRUE), token(Code.OR), token(Code.FALSE))
    val xorTokens = List(token(Code.TRUE), token(Code.XOR), token(Code.FALSE))

    val andResult = Optimizer.optimize(parse(andTokens))
    val orResult = Optimizer.optimize(parse(orTokens))
    val xorResult = Optimizer.optimize(parse(xorTokens))

    andResult shouldBe CodeBlock(List(
      VariableDeclaration("a", Literal(false)),
      ExpressionStatement(Variable("a"))
    ))
    orResult shouldBe CodeBlock(List(ExpressionStatement(Literal(true))))
    xorResult shouldBe CodeBlock(List(ExpressionStatement(Literal(true))))
  }

  test("unary minus constant") {
    val tokens = List(token(Code.MINUS), token(Code.INT_LITERAL, 5L))
    val result = Optimizer.optimize(parse(tokens))

    result shouldBe CodeBlock(List(ExpressionStatement(Literal(-5.0))))
  }

  test("unary not constant") {
    val tokens = List(token(Code.NOT), token(Code.TRUE))
    val result = Optimizer.optimize(parse(tokens))

    result shouldBe CodeBlock(List(ExpressionStatement(Literal(false))))
  }

  test("non-constant expression should remain unchanged") {
    val tokens = List(
      token(Code.IDENTIFIER, "x"),
      token(Code.PLUS),
      token(Code.INT_LITERAL, 2L)
    )
    val result = Optimizer.optimize(parse(tokens))

    result should not be CodeBlock(List(ExpressionStatement(Literal(2L))))
  }

  test("sum float constants") {
    val tokens = List(
      token(Code.REAL_LITERAL, 1.5f),
      token(Code.PLUS),
      token(Code.REAL_LITERAL, 2.5f)
    )
    val result = Optimizer.optimize(parse(tokens))

    val Literal(value: Double) =
      result.statements.head.asInstanceOf[ExpressionStatement].expression

    value shouldBe 4.0 +- 1e-6
  }

  test("subtract float constants") {
    val tokens = List(
      token(Code.REAL_LITERAL, 5.5f),
      token(Code.MINUS),
      token(Code.REAL_LITERAL, 1.2f)
    )
    val result = Optimizer.optimize(parse(tokens))

    val Literal(value: Double) =
      result.statements.head.asInstanceOf[ExpressionStatement].expression

    value shouldBe 4.3 +- 1e-5
  }

  test("multiply float constants") {
    val tokens = List(
      token(Code.REAL_LITERAL, 2.5f),
      token(Code.MULTIPLICATION),
      token(Code.REAL_LITERAL, 4.0f)
    )
    val result = Optimizer.optimize(parse(tokens))

    result shouldBe CodeBlock(List(ExpressionStatement(Literal(10.0))))
  }

  test("divide float constants") {
    val tokens = List(
      token(Code.REAL_LITERAL, 9.0f),
      token(Code.DIVISION),
      token(Code.REAL_LITERAL, 3.0f)
    )
    val result = Optimizer.optimize(parse(tokens))

    result shouldBe CodeBlock(List(ExpressionStatement(Literal(3.0))))
  }

  test("compare float constants less and greater") {
    val lessTokens = List(
      token(Code.REAL_LITERAL, 1.5f),
      token(Code.LESS),
      token(Code.REAL_LITERAL, 2.0f)
    )
    val greaterTokens = List(
      token(Code.REAL_LITERAL, 3.2f),
      token(Code.MORE),
      token(Code.REAL_LITERAL, 1.1f)
    )

    val lessResult = Optimizer.optimize(parse(lessTokens))
    val greaterResult = Optimizer.optimize(parse(greaterTokens))

    lessResult shouldBe CodeBlock(List(ExpressionStatement(Literal(true))))
    greaterResult shouldBe CodeBlock(List(ExpressionStatement(Literal(true))))
  }

  test("mixed int and float constants") {
    val tokens = List(
      token(Code.INT_LITERAL, 2L),
      token(Code.PLUS),
      token(Code.REAL_LITERAL, 3.5f)
    )
    val result = Optimizer.optimize(parse(tokens))

    val Literal(value: Double) =
      result.statements.head.asInstanceOf[ExpressionStatement].expression

    value shouldBe 5.5 +- 1e-6
  }

  test("remove unused variable") {
    val tokens = List(
      token(Code.VAR), token(Code.IDENTIFIER, "x"),
      token(Code.ASSIGNMENT), token(Code.INT_LITERAL, 42L),
      token(Code.NEWLINE),
      token(Code.IDENTIFIER, "y"), token(Code.ASSIGNMENT), token(Code.INT_LITERAL, 5L)
    )

    val result = Optimizer.optimize(parse(tokens))

    val hasY = result.statements.exists {
      case VariableDeclaration(name, _) => name == "y"
      case VariableAssignment(name, _) => name == "y"
      case _ => false
    }

    hasY shouldBe true
  }

  test("keep used variable") {
    val tokens = List(
      token(Code.VAR), token(Code.IDENTIFIER, "x"),
      token(Code.ASSIGNMENT), token(Code.INT_LITERAL, 10L),
      token(Code.NEWLINE),
      token(Code.IDENTIFIER, "x"),
      token(Code.PLUS), token(Code.INT_LITERAL, 5L)
    )

    val result = Optimizer.optimize(parse(tokens))

    val declNames = result.statements.collect {
      case VariableDeclaration(name, _) => name
    }

    declNames should contain("x")
  }

  test("remove unused variable but keep side effect") {
    val tokens = List(
      token(Code.VAR), token(Code.IDENTIFIER, "x"),
      token(Code.ASSIGNMENT),
      token(Code.IDENTIFIER, "foo"),
      token(Code.ROUND_BRACKET_LEFT), token(Code.ROUND_BRACKET_RIGHT),
      token(Code.NEWLINE),
      token(Code.IDENTIFIER, "y"),
      token(Code.ASSIGNMENT), token(Code.INT_LITERAL, 7L)
    )

    val result = Optimizer.optimize(parse(tokens))

    val hasFooCall = result.statements.exists {
      case ExpressionStatement(expr) => expr.toString.contains("foo")
      case _ => false
    }

    hasFooCall shouldBe true
  }

  test("multiple unused variables only used one kept") {
    val tokens = List(
      token(Code.VAR), token(Code.IDENTIFIER, "a"),
      token(Code.ASSIGNMENT), token(Code.INT_LITERAL, 1L), token(Code.NEWLINE),
      token(Code.VAR), token(Code.IDENTIFIER, "b"),
      token(Code.ASSIGNMENT), token(Code.INT_LITERAL, 2L), token(Code.NEWLINE),
      token(Code.VAR), token(Code.IDENTIFIER, "c"),
      token(Code.ASSIGNMENT), token(Code.INT_LITERAL, 3L), token(Code.NEWLINE),
      token(Code.IDENTIFIER, "b"), token(Code.PLUS), token(Code.INT_LITERAL, 10L)
    )

    val result = Optimizer.optimize(parse(tokens))

    val declNames = result.statements.collect {
      case VariableDeclaration(name, _) => name
    }.toSet

    declNames shouldBe Set("b")
  }

  test("keep variable if used in expression chain") {
    val tokens = List(
      token(Code.VAR), token(Code.IDENTIFIER, "x"),
      token(Code.ASSIGNMENT), token(Code.INT_LITERAL, 2L), token(Code.NEWLINE),
      token(Code.VAR), token(Code.IDENTIFIER, "y"),
      token(Code.ASSIGNMENT), token(Code.IDENTIFIER, "x"),
      token(Code.PLUS), token(Code.INT_LITERAL, 3L)
    )

    val result = Optimizer.optimize(parse(tokens))

    val declNames = result.statements.collect {
      case VariableDeclaration(name, _) => name
    }.toSet

    declNames shouldBe Set("x")
  }
}
