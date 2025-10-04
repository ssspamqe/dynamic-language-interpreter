package ru.innopolis.interpreter

import org.scalatest.funsuite.AnyFunSuite
import ru.innopolis.interpreter.lexer.{Code, Span, Token}
import ru.innopolis.interpreter.syntax.analyzer.parser.ExpressionParser
import ru.innopolis.interpreter.syntax.analyzer.tree.expression._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.literal._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.references.{ArrayAccess, FunctionCall, TupleFieldAccess, TupleIndexAccess}
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.types.TypeCheck
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.types.indicator.TypeIndicator
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.{CodeBlock, PrintStatement}

class ExpressionParserTest extends AnyFunSuite {

  private val parser = new ExpressionParser
  private val dummySpan = Span(0, 0, 0)

  // Helper method to create tokens
  private def token(code: Code, value: Any = null): Token[_] = {
    Token(dummySpan, code, value)
  }

  // Helper method to parse expression from tokens
  private def parse(tokens: List[Token[_]]): Expression = {
    parser.parseExpression(tokens.iterator.buffered)
  }

  test("parse integer literal") {
    val tokens = List(token(Code.INT_LITERAL, 42L))
    val result = parse(tokens)
    assert(result == Literal(42L))
  }

  test("parse real literal") {
    val tokens = List(token(Code.REAL_LITERAL, 3.14))
    val result = parse(tokens)
    assert(result == Literal(3.14))
  }

  test("parse string literal") {
    val tokens = List(token(Code.STRING_LITERAL, "hello"))
    val result = parse(tokens)
    assert(result == Literal("hello"))
  }

  test("parse boolean literals") {
    val trueTokens = List(token(Code.TRUE))
    val falseTokens = List(token(Code.FALSE))

    assert(parse(trueTokens) == Literal(true))
    assert(parse(falseTokens) == Literal(false))
  }

  test("parse variable") {
    val tokens = List(token(Code.IDENTIFIER, "x"))
    val result = parse(tokens)
    assert(result == Variable("x"))
  }

  test("parse binary addition") {
    val tokens = List(
      token(Code.INT_LITERAL, 1L),
      token(Code.PLUS),
      token(Code.INT_LITERAL, 2L)
    )
    val result = parse(tokens)
    assert(result == Binary(Code.PLUS, Literal(1L), Literal(2L)))
  }

  test("parse binary subtraction") {
    val tokens = List(
      token(Code.INT_LITERAL, 5L),
      token(Code.MINUS),
      token(Code.INT_LITERAL, 3L)
    )
    val result = parse(tokens)
    assert(result == Binary(Code.MINUS, Literal(5L), Literal(3L)))
  }

  test("parse binary multiplication") {
    val tokens = List(
      token(Code.INT_LITERAL, 4L),
      token(Code.MULTIPLICATION),
      token(Code.INT_LITERAL, 3L)
    )
    val result = parse(tokens)
    assert(result == Binary(Code.MULTIPLICATION, Literal(4L), Literal(3L)))
  }

  test("parse binary division") {
    val tokens = List(
      token(Code.INT_LITERAL, 8L),
      token(Code.DIVISION),
      token(Code.INT_LITERAL, 2L)
    )
    val result = parse(tokens)
    assert(result == Binary(Code.DIVISION, Literal(8L), Literal(2L)))
  }

  test("parse comparison operators") {
    val lessTokens = List(
      token(Code.INT_LITERAL, 1L),
      token(Code.LESS),
      token(Code.INT_LITERAL, 2L)
    )
    assert(parse(lessTokens) == Binary(Code.LESS, Literal(1L), Literal(2L)))

    val lessEqualTokens = List(
      token(Code.INT_LITERAL, 1L),
      token(Code.LESS_OR_EQUAL),
      token(Code.INT_LITERAL, 2L)
    )
    assert(parse(lessEqualTokens) == Binary(Code.LESS_OR_EQUAL, Literal(1L), Literal(2L)))

    val moreTokens = List(
      token(Code.INT_LITERAL, 3L),
      token(Code.MORE),
      token(Code.INT_LITERAL, 2L)
    )
    assert(parse(moreTokens) == Binary(Code.MORE, Literal(3L), Literal(2L)))

    val moreEqualTokens = List(
      token(Code.INT_LITERAL, 3L),
      token(Code.MORE_OR_EQUAL),
      token(Code.INT_LITERAL, 2L)
    )
    assert(parse(moreEqualTokens) == Binary(Code.MORE_OR_EQUAL, Literal(3L), Literal(2L)))
  }

  test("parse equality operators") {
    val equalTokens = List(
      token(Code.INT_LITERAL, 1L),
      token(Code.EQUAL),
      token(Code.INT_LITERAL, 1L)
    )
    assert(parse(equalTokens) == Binary(Code.EQUAL, Literal(1L), Literal(1L)))

    val notEqualTokens = List(
      token(Code.INT_LITERAL, 1L),
      token(Code.NOT_EQUAL),
      token(Code.INT_LITERAL, 2L)
    )
    assert(parse(notEqualTokens) == Binary(Code.NOT_EQUAL, Literal(1L), Literal(2L)))
  }

  test("parse logical operators") {
    val andTokens = List(
      token(Code.TRUE),
      token(Code.AND),
      token(Code.FALSE)
    )
    assert(parse(andTokens) == Binary(Code.AND, Literal(true), Literal(false)))

    val orTokens = List(
      token(Code.TRUE),
      token(Code.OR),
      token(Code.FALSE)
    )
    assert(parse(orTokens) == Binary(Code.OR, Literal(true), Literal(false)))

    val xorTokens = List(
      token(Code.TRUE),
      token(Code.XOR),
      token(Code.FALSE)
    )
    assert(parse(xorTokens) == Binary(Code.XOR, Literal(true), Literal(false)))
  }

  test("parse unary operators") {
    val minusTokens = List(
      token(Code.MINUS),
      token(Code.INT_LITERAL, 5L)
    )
    assert(parse(minusTokens) == Unary(Code.MINUS, Literal(5L)))

    val notTokens = List(
      token(Code.NOT),
      token(Code.TRUE)
    )
    assert(parse(notTokens) == Unary(Code.NOT, Literal(true)))
  }

  test("parse parentheses") {
    val tokens = List(
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.INT_LITERAL, 1L),
      token(Code.PLUS),
      token(Code.INT_LITERAL, 2L),
      token(Code.ROUND_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    assert(result == Binary(Code.PLUS, Literal(1L), Literal(2L)))
  }

  test("parse operator precedence") {
    // 1 + 2 * 3 should be parsed as 1 + (2 * 3)
    val tokens = List(
      token(Code.INT_LITERAL, 1L),
      token(Code.PLUS),
      token(Code.INT_LITERAL, 2L),
      token(Code.MULTIPLICATION),
      token(Code.INT_LITERAL, 3L)
    )
    val result = parse(tokens)
    val expected = Binary(Code.PLUS,
      Literal(1L),
      Binary(Code.MULTIPLICATION, Literal(2L), Literal(3L))
    )
    assert(result == expected)
  }

  test("parse complex expression with correct precedence") {
    // 1 + 2 * 3 < 4 + 5 should be parsed as (1 + (2 * 3)) < (4 + 5)
    val tokens = List(
      token(Code.INT_LITERAL, 1L),
      token(Code.PLUS),
      token(Code.INT_LITERAL, 2L),
      token(Code.MULTIPLICATION),
      token(Code.INT_LITERAL, 3L),
      token(Code.LESS),
      token(Code.INT_LITERAL, 4L),
      token(Code.PLUS),
      token(Code.INT_LITERAL, 5L)
    )
    val result = parse(tokens)
    val expected = Binary(Code.LESS,
      Binary(Code.PLUS,
        Literal(1L),
        Binary(Code.MULTIPLICATION, Literal(2L), Literal(3L))
      ),
      Binary(Code.PLUS, Literal(4L), Literal(5L))
    )
    assert(result == expected)
  }

  test("parse empty array") {
    val tokens = List(
      token(Code.SQUARE_BRACKET_LEFT),
      token(Code.SQUARE_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    assert(result == ArrayLiteral(List.empty))
  }

  test("parse array with single element") {
    val tokens = List(
      token(Code.SQUARE_BRACKET_LEFT),
      token(Code.INT_LITERAL, 1L),
      token(Code.SQUARE_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    assert(result == ArrayLiteral(List(Literal(1L))))
  }

  test("parse array with multiple elements") {
    val tokens = List(
      token(Code.SQUARE_BRACKET_LEFT),
      token(Code.INT_LITERAL, 1L),
      token(Code.COMMA),
      token(Code.INT_LITERAL, 2L),
      token(Code.COMMA),
      token(Code.INT_LITERAL, 3L),
      token(Code.SQUARE_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    assert(result == ArrayLiteral(List(Literal(1L), Literal(2L), Literal(3L))))
  }

  test("parse empty tuple") {
    val tokens = List(
      token(Code.CURLY_BRACKET_LEFT),
      token(Code.CURLY_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    assert(result == TupleLiteral(List.empty))
  }

  test("parse tuple with unnamed elements") {
    val tokens = List(
      token(Code.CURLY_BRACKET_LEFT),
      token(Code.INT_LITERAL, 1L),
      token(Code.COMMA),
      token(Code.INT_LITERAL, 2L),
      token(Code.CURLY_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    val expected = TupleLiteral(List(
      TupleEntry(None, Literal(1L)),
      TupleEntry(None, Literal(2L))
    ))
    assert(result == expected)
  }

  test("parse tuple with named elements") {
    val tokens = List(
      token(Code.CURLY_BRACKET_LEFT),
      token(Code.IDENTIFIER, "x"),
      token(Code.ASSIGNMENT),
      token(Code.INT_LITERAL, 1L),
      token(Code.COMMA),
      token(Code.IDENTIFIER, "y"),
      token(Code.ASSIGNMENT),
      token(Code.INT_LITERAL, 2L),
      token(Code.CURLY_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    val expected = TupleLiteral(List(
      TupleEntry(Some("x"), Literal(1L)),
      TupleEntry(Some("y"), Literal(2L))
    ))
    assert(result == expected)
  }

  test("parse mixed tuple with named and unnamed elements") {
    val tokens = List(
      token(Code.CURLY_BRACKET_LEFT),
      token(Code.IDENTIFIER, "x"),
      token(Code.ASSIGNMENT),
      token(Code.INT_LITERAL, 1L),
      token(Code.COMMA),
      token(Code.INT_LITERAL, 2L),
      token(Code.CURLY_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    val expected = TupleLiteral(List(
      TupleEntry(Some("x"), Literal(1L)),
      TupleEntry(None, Literal(2L))
    ))
    assert(result == expected)
  }

  test("parse function call with no arguments") {
    val tokens = List(
      token(Code.IDENTIFIER, "func"),
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.ROUND_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    assert(result == FunctionCall(Variable("func"), List.empty))
  }

  test("parse function call with single argument") {
    val tokens = List(
      token(Code.IDENTIFIER, "func"),
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.INT_LITERAL, 42L),
      token(Code.ROUND_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    assert(result == FunctionCall(Variable("func"), List(Literal(42L))))
  }

  test("parse function call with multiple arguments") {
    val tokens = List(
      token(Code.IDENTIFIER, "func"),
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.INT_LITERAL, 1L),
      token(Code.COMMA),
      token(Code.INT_LITERAL, 2L),
      token(Code.COMMA),
      token(Code.INT_LITERAL, 3L),
      token(Code.ROUND_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    assert(result == FunctionCall(Variable("func"), List(Literal(1L), Literal(2L), Literal(3L))))
  }

  test("parse array access") {
    val tokens = List(
      token(Code.IDENTIFIER, "arr"),
      token(Code.SQUARE_BRACKET_LEFT),
      token(Code.INT_LITERAL, 0L),
      token(Code.SQUARE_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    assert(result == ArrayAccess(Variable("arr"), Literal(0L)))
  }

  test("parse tuple field access by name") {
    val tokens = List(
      token(Code.IDENTIFIER, "tuple"),
      token(Code.DOT),
      token(Code.IDENTIFIER, "field")
    )
    val result = parse(tokens)
    assert(result == TupleFieldAccess(Variable("tuple"), "field"))
  }

  test("parse tuple field access by index") {
    val tokens = List(
      token(Code.IDENTIFIER, "tuple"),
      token(Code.DOT),
      token(Code.INT_LITERAL, 0L)
    )
    val result = parse(tokens)
    assert(result == TupleIndexAccess(Variable("tuple"), 0))
  }

  test("parse chained operations") {
    // arr[0].field.func(1, 2)
    val tokens = List(
      token(Code.IDENTIFIER, "arr"),
      token(Code.SQUARE_BRACKET_LEFT),
      token(Code.INT_LITERAL, 0L),
      token(Code.SQUARE_BRACKET_RIGHT),
      token(Code.DOT),
      token(Code.IDENTIFIER, "field"),
      token(Code.DOT),
      token(Code.IDENTIFIER, "func"),
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.INT_LITERAL, 1L),
      token(Code.COMMA),
      token(Code.INT_LITERAL, 2L),
      token(Code.ROUND_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    val expected = FunctionCall(
      TupleFieldAccess(
        TupleFieldAccess(
          ArrayAccess(Variable("arr"), Literal(0)),
          "field"
        ),
        "func"
      ),
      List(Literal(1), Literal(2))
    )
    assert(result == expected)
  }

  test("parse nested expressions") {
    // (1 + 2) * (3 - 4)
    val tokens = List(
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.INT_LITERAL, 1L),
      token(Code.PLUS),
      token(Code.INT_LITERAL, 2L),
      token(Code.ROUND_BRACKET_RIGHT),
      token(Code.MULTIPLICATION),
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.INT_LITERAL, 3L),
      token(Code.MINUS),
      token(Code.INT_LITERAL, 4L),
      token(Code.ROUND_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    val expected = Binary(Code.MULTIPLICATION,
      Binary(Code.PLUS, Literal(1L), Literal(2L)),
      Binary(Code.MINUS, Literal(3L), Literal(4L))
    )
    assert(result == expected)
  }

  test("parse complex nested array and tuple") {
    // [[1, 2], {x := 3, 4}]
    val tokens = List(
      token(Code.SQUARE_BRACKET_LEFT),
      token(Code.SQUARE_BRACKET_LEFT),
      token(Code.INT_LITERAL, 1L),
      token(Code.COMMA),
      token(Code.INT_LITERAL, 2L),
      token(Code.SQUARE_BRACKET_RIGHT),
      token(Code.COMMA),
      token(Code.CURLY_BRACKET_LEFT),
      token(Code.IDENTIFIER, "x"),
      token(Code.ASSIGNMENT),
      token(Code.INT_LITERAL, 3L),
      token(Code.COMMA),
      token(Code.INT_LITERAL, 4L),
      token(Code.CURLY_BRACKET_RIGHT),
      token(Code.SQUARE_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    val expected = ArrayLiteral(List(
      ArrayLiteral(List(Literal(1L), Literal(2L))),
      TupleLiteral(List(
        TupleEntry(Some("x"), Literal(3L)),
        TupleEntry(None, Literal(4L))
      ))
    ))
    assert(result == expected)
  }

  test("parse multiple unary operators") {
    // --5 should be parsed as -(-5)
    val tokens = List(
      token(Code.MINUS),
      token(Code.MINUS),
      token(Code.INT_LITERAL, 5L)
    )
    val result = parse(tokens)
    val expected = Unary(Code.MINUS, Unary(Code.MINUS, Literal(5L)))
    assert(result == expected)
  }

  test("parse multiple logical operators with correct precedence") {
    // true and false or true should be parsed as (true and false) or true
    val tokens = List(
      token(Code.TRUE),
      token(Code.AND),
      token(Code.FALSE),
      token(Code.OR),
      token(Code.TRUE)
    )
    val result = parse(tokens)
    val expected = Binary(Code.OR,
      Binary(Code.AND, Literal(true), Literal(false)),
      Literal(true)
    )
    assert(result == expected)
  }

  test("parse empty input throws exception") {
    val tokens = List.empty
    assertThrows[ru.innopolis.interpreter.exception.InvalidTokenException] {
      parse(tokens)
    }
  }

  test("parse invalid token throws exception") {
    val tokens = List(token(Code.IF)) // IF is not a valid expression token
    assertThrows[ru.innopolis.interpreter.exception.InvalidTokenException] {
      parse(tokens)
    }
  }

  test("parse incomplete parentheses throws exception") {
    val tokens = List(
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.INT_LITERAL, 1L)
      // Missing closing parenthesis
    )
    assertThrows[java.util.NoSuchElementException] {
      parse(tokens)
    }
  }

  test("parse incomplete array throws exception") {
    val tokens = List(
      token(Code.SQUARE_BRACKET_LEFT),
      token(Code.INT_LITERAL, 1L)
      // Missing closing bracket
    )
    assertThrows[java.util.NoSuchElementException] {
      parse(tokens)
    }
  }

  test("parse incomplete tuple throws exception") {
    val tokens = List(
      token(Code.CURLY_BRACKET_LEFT),
      token(Code.INT_LITERAL, 1L)
      // Missing closing brace
    )
    assertThrows[java.util.NoSuchElementException] {
      parse(tokens)
    }
  }

  test("parse incomplete function call throws exception") {
    val tokens = List(
      token(Code.IDENTIFIER, "func"),
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.INT_LITERAL, 1L)
      // Missing closing parenthesis
    )
    assertThrows[java.util.NoSuchElementException] {
      parse(tokens)
    }
  }

  test("parse incomplete array access throws exception") {
    val tokens = List(
      token(Code.IDENTIFIER, "arr"),
      token(Code.SQUARE_BRACKET_LEFT),
      token(Code.INT_LITERAL, 0L)
      // Missing closing bracket
    )
    assertThrows[java.util.NoSuchElementException] {
      parse(tokens)
    }
  }

  test("parse invalid field access throws exception") {
    val tokens = List(
      token(Code.IDENTIFIER, "tuple"),
      token(Code.DOT),
      token(Code.STRING_LITERAL, "invalid") // String literal not allowed as field
    )
    assertThrows[ru.innopolis.interpreter.exception.InvalidTokenException] {
      parse(tokens)
    }
  }

  test("parse int + real produces Binary with correct codes") {
    val tokens = List(
      token(Code.INT_LITERAL, 5L),
      token(Code.PLUS),
      token(Code.REAL_LITERAL, 2.5)
    )
    val result = parse(tokens)
    assert(result == Binary(Code.PLUS, Literal(5L), Literal(2.5)))
  }

  test("parse string concatenation") {
    val tokens = List(
      token(Code.STRING_LITERAL, "a"),
      token(Code.PLUS),
      token(Code.STRING_LITERAL, "b")
    )
    val result = parse(tokens)
    assert(result == Binary(Code.PLUS, Literal("a"), Literal("b")))
  }

  test("parse array concatenation") {
    val tokens = List(
      token(Code.SQUARE_BRACKET_LEFT),
      token(Code.INT_LITERAL, 1L),
      token(Code.SQUARE_BRACKET_RIGHT),
      token(Code.PLUS),
      token(Code.SQUARE_BRACKET_LEFT),
      token(Code.INT_LITERAL, 2L),
      token(Code.SQUARE_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    assert(result == Binary(Code.PLUS,
      ArrayLiteral(List(Literal(1L))),
      ArrayLiteral(List(Literal(2L)))
    ))
  }

  test("parse tuple concatenation") {
    val tokens = List(
      token(Code.CURLY_BRACKET_LEFT),
      token(Code.IDENTIFIER, "a"),
      token(Code.ASSIGNMENT),
      token(Code.INT_LITERAL, 1L),
      token(Code.CURLY_BRACKET_RIGHT),
      token(Code.PLUS),
      token(Code.CURLY_BRACKET_LEFT),
      token(Code.IDENTIFIER, "b"),
      token(Code.ASSIGNMENT),
      token(Code.INT_LITERAL, 2L),
      token(Code.CURLY_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    val expected = Binary(Code.PLUS,
      TupleLiteral(List(TupleEntry(Some("a"), Literal(1L)))),
      TupleLiteral(List(TupleEntry(Some("b"), Literal(2L))))
    )
    assert(result == expected)
  }

  test("parse nested function calls and array access") {
    // f(g(1))[0]
    val tokens = List(
      token(Code.IDENTIFIER, "f"),
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.IDENTIFIER, "g"),
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.INT_LITERAL, 1L),
      token(Code.ROUND_BRACKET_RIGHT),
      token(Code.ROUND_BRACKET_RIGHT),
      token(Code.SQUARE_BRACKET_LEFT),
      token(Code.INT_LITERAL, 0L),
      token(Code.SQUARE_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    val expected = ArrayAccess(
      FunctionCall(Variable("f"),
        List(FunctionCall(Variable("g"), List(Literal(1L))))
      ),
      Literal(0L)
    )
    assert(result == expected)
  }

  test("parse none literal") {
    val tokens = List(token(Code.NONE))
    val result = parse(tokens)
    assert(result == Literal(None))
  }

  test("parse type check is int") {
    val tokens = List(
      token(Code.IDENTIFIER, "x"),
      token(Code.IS),
      token(Code.INT)
    )
    val result = parse(tokens)
    assert(result == TypeCheck(Variable("x"), TypeIndicator.IntType))
  }

  test("parse type check is real") {
    val tokens = List(
      token(Code.IDENTIFIER, "y"),
      token(Code.IS),
      token(Code.REAL)
    )
    val result = parse(tokens)
    assert(result == TypeCheck(Variable("y"), TypeIndicator.RealType))
  }

  test("parse multiple is type checks with logical and") {
    val tokens = List(
      token(Code.IDENTIFIER, "x"),
      token(Code.IS),
      token(Code.INT),
      token(Code.AND),
      token(Code.IDENTIFIER, "y"),
      token(Code.IS),
      token(Code.REAL)
    )
    val result = parse(tokens)
    val expected = Binary(Code.AND,
      TypeCheck(Variable("x"), TypeIndicator.IntType),
      TypeCheck(Variable("y"), TypeIndicator.RealType)
    )
    assert(result == expected)
  }

  test("parse function literal with expression body") {
    // func(x, y) => x + y
    val tokens = List(
      token(Code.FUNC),
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.IDENTIFIER, "x"),
      token(Code.COMMA),
      token(Code.IDENTIFIER, "y"),
      token(Code.ROUND_BRACKET_RIGHT),
      token(Code.LAMBDA),
      token(Code.IDENTIFIER, "x"),
      token(Code.PLUS),
      token(Code.IDENTIFIER, "y")
    )
    val result = parse(tokens)
    val expected = FunctionLiteral(
      args = List(Variable("x"), Variable("y")),
      body = Left(Binary(Code.PLUS, Variable("x"), Variable("y")))
    )
    assert(result == expected)
  }

  test("parse function literal with block body") {
    // func() is print 1 end
    val tokens = List(
      token(Code.FUNC),
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.ROUND_BRACKET_RIGHT),
      token(Code.IS),
      token(Code.PRINT),
      token(Code.INT_LITERAL, 1L),
      token(Code.END)
    )
    val result = parse(tokens)
    val expected = FunctionLiteral(
      args = List(),
      body = Right(CodeBlock(
        List(
          PrintStatement(List(Literal(1L)))
        )
      ))
    )
    assert(result == expected)
  }

  test("parse function literal without parameters") {
    // func() => 1
    val tokens = List(
      token(Code.FUNC),
      token(Code.LAMBDA),
      token(Code.INT_LITERAL, 1L)
    )
    val result = parse(tokens)
    val expected = FunctionLiteral(
      List(),
      Left(Literal(1L)),
    )
    assert(result == expected)
  }

  test("parse function call with function literal argument") {
    // func(func(x) => x + 1)
    val tokens = List(
      token(Code.IDENTIFIER, "func"),
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.FUNC),
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.IDENTIFIER, "x"),
      token(Code.ROUND_BRACKET_RIGHT),
      token(Code.LAMBDA),
      token(Code.IDENTIFIER, "x"),
      token(Code.PLUS),
      token(Code.INT_LITERAL, 1L),
      token(Code.ROUND_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    val innerFunc = FunctionLiteral(
      args = List(Variable("x")),
      body = Left(Binary(Code.PLUS, Variable("x"), Literal(1L))),
    )
    val expected = FunctionCall(Variable("func"), List(innerFunc))
    assert(result == expected)
  }

  test("parse nested type check inside parentheses") {
    // (x is int) or (y is bool)
    val tokens = List(
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.IDENTIFIER, "x"),
      token(Code.IS),
      token(Code.INT),
      token(Code.ROUND_BRACKET_RIGHT),
      token(Code.OR),
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.IDENTIFIER, "y"),
      token(Code.IS),
      token(Code.BOOL),
      token(Code.ROUND_BRACKET_RIGHT)
    )
    val result = parse(tokens)
    val expected = Binary(Code.OR,
      TypeCheck(Variable("x"), TypeIndicator.IntType),
      TypeCheck(Variable("y"), TypeIndicator.BoolType)
    )
    assert(result == expected)
  }

  test("parse invalid function literal missing body") {
    // func(x)
    val tokens = List(
      token(Code.FUNC),
      token(Code.ROUND_BRACKET_LEFT),
      token(Code.IDENTIFIER, "x"),
      token(Code.ROUND_BRACKET_RIGHT)
      // missing => or is ... end
    )
    assertThrows[java.util.NoSuchElementException] {
      parse(tokens)
    }
  }


}
