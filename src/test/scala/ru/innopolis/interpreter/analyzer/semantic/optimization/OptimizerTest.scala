package ru.innopolis.interpreter.analyzer.semantic.optimization

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import ru.innopolis.interpreter.RegexLexer
import ru.innopolis.interpreter.lexer.{Code, Span, Token}
import ru.innopolis.interpreter.analyzer.semantic.utils.TestUtils._
import ru.innopolis.interpreter.lexer.Code
import ru.innopolis.interpreter.syntax.analyzer.parser.{AASTParser, TokenStream}
import ru.innopolis.interpreter.syntax.analyzer.tree.expression._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.literal.Literal
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.references.{ArrayAccess, FunctionCall, TupleIndexAccess}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.assignment._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.{Binary, Variable}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.assignment.{ArrayElementAssignment, VariableAssignment}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration.VariableDeclaration
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.loop.{Loop, _}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.loop.{CollectionLoop, Loop, RangeLoop, WhileLoop}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement._

class OptimizerTest extends AnyFunSuite {

  private val dummySpan = Span(0, 0, 0)

  private def token(code: Code, value: Any = null): Token[_] =
    Token(dummySpan, code, value)

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

  test("evaluate constants in array index") {
    val code = "arr[1 + 2 + 3]"

    val lexer = new RegexLexer()
    val tokens = lexer.tokenize(code)
    val stream = new TokenStream(tokens)
    val parser = new AASTParser(stream)
    val expression = parser.parse()
    val result = Optimizer.optimize(expression)

    result shouldBe CodeBlock(List(ExpressionStatement(
      ArrayAccess(Variable("arr"), Literal(6))
    )))
  }

  test("evaluate constants in args of function call") {
    val code = "f(1 + 2 + 3)"

    val lexer = new RegexLexer()
    val tokens = lexer.tokenize(code)
    val stream = new TokenStream(tokens)
    val parser = new AASTParser(stream)
    val expression = parser.parse()
    val result = Optimizer.optimize(expression)

    result shouldBe CodeBlock(List(ExpressionStatement(
      FunctionCall(Variable("f"), List(Literal(6)))
    )))
  }

  test("optimize array element assignment") {
    val stmt = ArrayElementAssignment(
      Variable("arr"),
      Binary(Code.PLUS, Literal(1), Literal(2)),
      Binary(Code.MULTIPLICATION, Literal(3), Literal(4))
    )

    val block = CodeBlock(List(stmt))
    val result = Optimizer.optimize(block)

    result shouldBe CodeBlock(List(
      ArrayElementAssignment(Variable("arr"), Literal(3.0), Literal(12.0))
    ))
  }

  test("optimize variable assignment with constant expression") {
    val stmt = VariableAssignment("x", Binary(Code.PLUS, Literal(1), Literal(2)))
    val block = CodeBlock(List(stmt))

    val result = Optimizer.optimize(block)

    result shouldBe CodeBlock(List(VariableAssignment("x", Literal(3.0))))
  }

  test("optimize variable declaration with constant expression") {
    val block = CodeBlock(List(
      VariableDeclaration("a", Binary(Code.MINUS, Literal(10), Literal(5))),
      ExpressionStatement(Variable("a"))
    ))

    val result = Optimizer.optimize(block)

    result shouldBe CodeBlock(List(
      VariableDeclaration("a", Literal(5.0)),
      ExpressionStatement(Variable("a"))
    ))
  }

  test("optimize while loop with constant condition and expression inside") {
    val whileLoop = WhileLoop(
      Binary(Code.LESS, Literal(1), Literal(2)),
      CodeBlock(List(ExpressionStatement(Binary(Code.PLUS, Literal(2), Literal(3)))))
    )

    val result = Optimizer.optimize(CodeBlock(List(whileLoop)))

    result shouldBe CodeBlock(List(
      WhileLoop(Literal(true), CodeBlock(List(ExpressionStatement(Literal(5.0)))))
    ))
  }

  test("optimize simple loop") {
    val input = CodeBlock(List(
      new Loop(CodeBlock(List(
        ExpressionStatement(Binary(Code.PLUS, Literal(1), Literal(1)))
      )))
    ))

    val result = Optimizer.optimize(input)
    val loop = result.statements.head.asInstanceOf[Loop]

    loop.body shouldBe CodeBlock(List(
      ExpressionStatement(Literal(2.0))
    ))
  }

  test("optimize if statement with constant condition and branches") {
    val ifStmt = IfStatement(
      Binary(Code.LESS, Literal(1), Literal(2)),
      CodeBlock(List(ExpressionStatement(Binary(Code.PLUS, Literal(2), Literal(3))))),
      Some(CodeBlock(List(ExpressionStatement(Literal("else branch")))))
    )

    val result = Optimizer.optimize(CodeBlock(List(ifStmt)))

    result shouldBe CodeBlock(List(
      IfStatement(Literal(true),
        CodeBlock(List(ExpressionStatement(Literal(5.0)))),
        Some(CodeBlock(List(ExpressionStatement(Literal("else branch")))))
      )
    ))
  }

  test("optimize print statement with constant expression") {
    val stmt = PrintStatement(List(Binary(Code.PLUS, Literal(4), Literal(1))))
    val block = CodeBlock(List(stmt))

    val result = Optimizer.optimize(block)

    result shouldBe CodeBlock(List(PrintStatement(List(Literal(5.0)))))
  }

  test("optimize return statement with constant expression") {
    val stmt = ReturnStatement(Some(Binary(Code.MULTIPLICATION, Literal(2), Literal(3))))
    val block = CodeBlock(List(stmt))

    val result = Optimizer.optimize(block)

    result shouldBe CodeBlock(List(ReturnStatement(Some(Literal(6.0)))))
  }

  test("optimize collection loop") {
    val collectionLoop = CollectionLoop(
      "x",
      Binary(Code.PLUS, Literal(1), Literal(2)),
      CodeBlock(List(ExpressionStatement(Binary(Code.MULTIPLICATION, Literal(2), Literal(3)))))
    )

    val result = Optimizer.optimize(CodeBlock(List(collectionLoop)))

    result shouldBe CodeBlock(List(
      CollectionLoop("x", Literal(3.0), CodeBlock(List(ExpressionStatement(Literal(6.0)))))
    ))
  }

  test("optimize range loop") {
    val rangeLoop = RangeLoop(
      Some("i"),
      Binary(Code.PLUS, Literal(1), Literal(2)),
      Binary(Code.PLUS, Literal(3), Literal(4)),
      CodeBlock(List(ExpressionStatement(Binary(Code.PLUS, Literal(2), Literal(3)))))
    )

    val result = Optimizer.optimize(CodeBlock(List(rangeLoop)))

    result shouldBe CodeBlock(List(
      RangeLoop(Some("i"), Literal(3.0), Literal(7.0), CodeBlock(List(ExpressionStatement(Literal(5.0)))))
    ))
  }

  test("optimize collection loop collection expression") {
    val loop = CollectionLoop(
      "item",
      Binary(Code.PLUS, Literal(2), Literal(3)),
      CodeBlock(List(ExpressionStatement(Literal("body"))))
    )

    val result = Optimizer.optimize(CodeBlock(List(loop)))
    val optimized = result.statements.head.asInstanceOf[CollectionLoop]

    optimized.collection shouldBe Literal(5.0)
  }

  test("division by zero should not be simplified") {
    val expr = Binary(Code.DIVISION, Literal(5), Literal(0))
    Optimizer.optimizeExpr(expr) shouldBe Binary(Code.DIVISION, Literal(5.0), Literal(0.0))
  }

  // Remove testing

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

  test("keep variable if used in array access") {
    val tokens = List(
      token(Code.VAR), token(Code.IDENTIFIER, "x"),
      token(Code.ASSIGNMENT), token(Code.INT_LITERAL, 2L), token(Code.NEWLINE),
      token(Code.IDENTIFIER, "arr"),
      token(Code.SQUARE_BRACKET_LEFT), token(Code.IDENTIFIER, "x"), token(Code.SQUARE_BRACKET_RIGHT)
    )

    val result = Optimizer.optimize(parse(tokens))

    val declNames = result.statements.collect {
      case VariableDeclaration(name, _) => name
    }.toSet

    declNames shouldBe Set("x")
  }


  test("remove variable if not used in array access") {
    val tokens = List(
      token(Code.VAR), token(Code.IDENTIFIER, "x"),
      token(Code.ASSIGNMENT), token(Code.INT_LITERAL, 2L), token(Code.NEWLINE),
      token(Code.IDENTIFIER, "arr"),
      token(Code.SQUARE_BRACKET_LEFT), token(Code.INT_LITERAL, 1), token(Code.SQUARE_BRACKET_RIGHT)
    )

    val result = Optimizer.optimize(parse(tokens))

    val declNames = result.statements.collect {
      case VariableDeclaration(name, _) => name
    }.toSet

    declNames shouldBe Set()
  }

  test("collect used variables from nested structures") {
    val stmt = ExpressionStatement(
      FunctionCall(
        Variable("f"),
        List(ArrayAccess(Variable("arr"), Variable("x")), TupleIndexAccess(Variable("t"), 0))
      )
    )
    val vars = Optimizer.collectUsedVariables(List(stmt))
    vars shouldBe Set("f", "arr", "x", "t")
  }


  test("hasSideEffect should detect nested function calls") {
    val expr = Binary(Code.PLUS,
      FunctionCall(Variable("foo"), List(Literal(1))),
      Literal(2)
    )
    Optimizer.hasSideEffect(expr) shouldBe true
  }
}
