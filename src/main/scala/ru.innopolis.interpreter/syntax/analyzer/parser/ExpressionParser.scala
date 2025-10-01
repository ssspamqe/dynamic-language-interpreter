package ru.innopolis.interpreter.syntax.analyzer.parser

import ru.innopolis.interpreter.exception.InvalidTokenException
import ru.innopolis.interpreter.lexer.{Code, Token}
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.references.{ArrayAccess, FieldAccess, FunctionCall, IndexAccess}
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.{Binary, Expression, Ident, Literal, Unary}

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
    } else parsePrimary(it)
  }

  private def parsePrimary(it: BufferedIterator[Token[_]]): Expression = {
    if (!it.hasNext) throw new InvalidTokenException(null,null)

    val tok = it.next()
    var expr: Expression = tok.code match {
      case Code.INT_LITERAL    => Literal(tok.value)
      case Code.REAL_LITERAL   => Literal(tok.value)
      case Code.STRING_LITERAL => Literal(tok.value)
      case Code.TRUE           => Literal(true)
      case Code.FALSE          => Literal(false)
      case Code.IDENTIFIER     => Ident(tok.value.toString)
      case Code.ROUND_BRACKET_LEFT =>
        val inner = parseExpression(it)
        assertTokenCode(it.next().code, Code.ROUND_BRACKET_RIGHT)
        inner
      case _ =>
        throw new InvalidTokenException(tok, null)
    }

    // parsing references
    while (it.hasNext) {
      it.head.code match {
      //function call
        case Code.ROUND_BRACKET_LEFT =>
          it.next()
          var args = List.empty[Expression]
          if (it.hasNext && it.head.code != Code.ROUND_BRACKET_RIGHT) {
            args ::= parseExpression(it)
            while (it.hasNext && it.head.code == Code.COMMA) {
              it.next()
              args ::= parseExpression(it)
            }
          }
          assertTokenCode(it.next().code, Code.ROUND_BRACKET_RIGHT)
          expr = FunctionCall(expr, args.reverse)

        // array access
        case Code.SQUARE_BRACKET_LEFT =>
          it.next()
          val indexExpr = parseExpression(it)
          assertTokenCode(it.next().code, Code.SQUARE_BRACKET_RIGHT)
          expr = ArrayAccess(expr, indexExpr)

        // tuple access
        case Code.DOT =>
          it.next()
          val fieldTok = it.next()
          fieldTok.code match {
            case Code.IDENTIFIER   => expr = FieldAccess(expr, fieldTok.value.toString)
            case Code.INT_LITERAL  => expr = IndexAccess(expr, fieldTok.value.toString.toInt)
            case _ => throw new InvalidTokenException(fieldTok,null)
          }
        case _ => return expr
      }
    }
    expr
  }

  private def assertTokenCode(actualCode:Code, expectedCode:Code): Unit = {
    if(actualCode != expectedCode){
      throw new InvalidTokenException(null,null)
    }
  }
}
