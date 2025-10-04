package ru.innopolis.interpreter.syntax.analyzer.parser

import ru.innopolis.interpreter.exception.InvalidTokenException
import ru.innopolis.interpreter.lexer.{Code, Token}
import ru.innopolis.interpreter.syntax.analyzer.tree.expression._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.literal.{ArrayLiteral, Literal, TupleEntry, TupleLiteral}
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.references.{ArrayAccess, FunctionCall, TupleFieldAccess, TupleIndexAccess}
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.types.TypeCheck
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.types.indicator.TypeIndicator

import scala.collection.BufferedIterator

class ExpressionParser {

  def parseExpression(it: BufferedIterator[Token[_]]): Expression = parseOr(it)

  private def parseOr(it: BufferedIterator[Token[_]]): Expression = {
    var expr = parseXor(it)
    while (it.hasNext && it.head.code == Code.OR) {
      val op = it.next().code
      val right = parseXor(it)
      expr = Binary(op, expr, right)
    }
    expr
  }

  private def parseXor(it: BufferedIterator[Token[_]]): Expression = {
    var expr = parseAnd(it)
    while (it.hasNext && it.head.code == Code.XOR) {
      val op = it.next().code
      val right = parseAnd(it)
      expr = Binary(op, expr, right)
    }
    expr
  }

  private def parseAnd(it: BufferedIterator[Token[_]]): Expression = {
    var expr = parseEquality(it)
    while (it.hasNext && it.head.code == Code.AND) {
      val op = it.next().code
      val right = parseEquality(it)
      expr = Binary(op, expr, right)
    }
    expr
  }

  private def parseEquality(it: BufferedIterator[Token[_]]): Expression = {
    var expr = parseComparison(it)
    while (it.hasNext && Set(Code.EQUAL, Code.NOT_EQUAL).contains(it.head.code)) {
      val op = it.next().code
      val right = parseComparison(it)
      expr = Binary(op, expr, right)
    }
    expr
  }

  private def parseComparison(it: BufferedIterator[Token[_]]): Expression = {
    var expr = parseTerm(it)
    while (it.hasNext && Set(Code.LESS, Code.LESS_OR_EQUAL, Code.MORE, Code.MORE_OR_EQUAL).contains(it.head.code)) {
      val op = it.next().code
      val right = parseTerm(it)
      expr = Binary(op, expr, right)
    }
    expr
  }

  private def parseTerm(it: BufferedIterator[Token[_]]): Expression = {
    var expr = parseFactor(it)
    while (it.hasNext && Set(Code.PLUS, Code.MINUS).contains(it.head.code)) {
      val op = it.next().code
      val right = parseFactor(it)
      expr = Binary(op, expr, right)
    }
    expr
  }

  private def parseFactor(it: BufferedIterator[Token[_]]): Expression = {
    var expr = parseUnary(it)
    while (it.hasNext && Set(Code.MULTIPLICATION, Code.DIVISION).contains(it.head.code)) {
      val op = it.next().code
      val right = parseUnary(it)
      expr = Binary(op, expr, right)
    }
    expr
  }

  private def parseUnary(it: BufferedIterator[Token[_]]): Expression = {
    if (it.hasNext && Set(Code.MINUS, Code.NOT).contains(it.head.code)) {
      val op = it.next().code
      val right = parseUnary(it)
      Unary(op, right)
    } else parseTypeCheck(it)
  }


  private def parseTypeCheck(it: BufferedIterator[Token[_]]): Expression = {
    var expr = parsePrimary(it)

    // check for 'is'
    while (it.hasNext && it.head.code == Code.IS) {
      it.next() // skip 'is'
      val typeToken = it.next()
      val typeIndicator = typeToken.code match {
        case Code.INT => TypeIndicator.IntType
        case Code.REAL => TypeIndicator.RealType
        case Code.BOOL => TypeIndicator.BoolType
        case Code.STRING => TypeIndicator.StringType
        case Code.NONE => TypeIndicator.NoneType
        case _ => throw new InvalidTokenException(typeToken, null)
      }
      expr = TypeCheck(expr, typeIndicator)
    }

    expr
  }

  private def parsePrimary(it: BufferedIterator[Token[_]]): Expression = {
    if (!it.hasNext) throw new InvalidTokenException(null, null)

    val tok = it.next()
    var expr: Expression = tok.code match {
      case Code.INT_LITERAL => Literal(tok.value)
      case Code.REAL_LITERAL => Literal(tok.value)
      case Code.STRING_LITERAL => Literal(tok.value)
      case Code.TRUE => Literal(true)
      case Code.FALSE => Literal(false)
      case Code.IDENTIFIER => Variable(tok.value.toString)

      // (expr)
      case Code.ROUND_BRACKET_LEFT =>
        val inner = parseExpression(it)
        assertTokenCode(it.next(), Code.ROUND_BRACKET_RIGHT)
        inner

      // [expr, expr, ...]
      case Code.SQUARE_BRACKET_LEFT =>
        var elements = List.empty[Expression]
        if (it.hasNext && it.head.code != Code.SQUARE_BRACKET_RIGHT) {
          elements ::= parseExpression(it)
          while (it.hasNext && it.head.code == Code.COMMA) {
            it.next()
            elements ::= parseExpression(it)
          }
        }
        assertTokenCode(it.next(), Code.SQUARE_BRACKET_RIGHT)
        ArrayLiteral(elements.reverse)

      // { name := expr, expr, ... }
      case Code.CURLY_BRACKET_LEFT =>
        var elements = List.empty[(Option[String], Expression)]
        if (it.hasNext && it.head.code != Code.CURLY_BRACKET_RIGHT) {
          elements ::= parseTupleElement(it)
          while (it.hasNext && it.head.code == Code.COMMA) {
            it.next()
            elements ::= parseTupleElement(it)
          }
        }
        assertTokenCode(it.next(), Code.CURLY_BRACKET_RIGHT)
        TupleLiteral(elements.reverse.map(element => TupleEntry(element._1, element._2)))

      case Code.NONE => Literal(None)

      case _ =>
        throw new InvalidTokenException(tok, null)
    }

    // Handle postfix (calls, indexing, field access)
    while (it.hasNext) {
      it.head.code match {
        case Code.ROUND_BRACKET_LEFT => // function call
          it.next()
          var args = List.empty[Expression]
          if (it.hasNext && it.head.code != Code.ROUND_BRACKET_RIGHT) {
            args ::= parseExpression(it)
            while (it.hasNext && it.head.code == Code.COMMA) {
              it.next()
              args ::= parseExpression(it)
            }
          }
          assertTokenCode(it.next(), Code.ROUND_BRACKET_RIGHT)
          expr = FunctionCall(expr, args.reverse)

        case Code.SQUARE_BRACKET_LEFT => // array indexing
          it.next()
          val indexExpr = parseExpression(it)
          assertTokenCode(it.next(), Code.SQUARE_BRACKET_RIGHT)
          expr = ArrayAccess(expr, indexExpr)

        case Code.DOT => // field access
          it.next()
          val fieldTok = it.next()
          fieldTok.code match {
            case Code.IDENTIFIER => expr = TupleFieldAccess(expr, fieldTok.value.toString)
            case Code.INT_LITERAL => expr = TupleIndexAccess(expr, fieldTok.value.toString.toInt)
            case _ => throw new InvalidTokenException(fieldTok, null)
          }

        case _ => return expr
      }
    }

    expr
  }

  private def parseTupleElement(it: BufferedIterator[Token[_]]): (Option[String], Expression) = {
    if (it.head.code == Code.IDENTIFIER) {
      val nameTok = it.next()
      if (it.hasNext && it.head.code == Code.ASSIGNMENT) {
        it.next() // :=
        val expr = parseExpression(it)
        return (Some(nameTok.value.toString), expr)
      } else {
        // not an assignment, just expr starting with identifier
        return (None, Variable(nameTok.value.toString))
      }
    }
    // unnamed element
    (None, parseExpression(it))
  }

  private def assertTokenCode(actualToken: Token[_], expectedCode: Code): Unit = {
    if (actualToken.code != expectedCode) {
      throw new InvalidTokenException(actualToken, expectedCode)
    }
  }
}
