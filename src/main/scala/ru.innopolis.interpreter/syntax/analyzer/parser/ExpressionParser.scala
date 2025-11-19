package ru.innopolis.interpreter.syntax.analyzer.parser

import ru.innopolis.interpreter.exception.{InvalidTokenException, UnexpectedTokenException}
import ru.innopolis.interpreter.lexer.Code
import ru.innopolis.interpreter.syntax.analyzer.tree.expression._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.literal._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.references._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.types._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.types.indicator.TypeIndicator

class ExpressionParser(private val stream: TokenStream) {

  def parseExpression(): Expression = parseOr()

  private def parseOr(): Expression = {
    var expr = parseXor()
    while (stream.hasNext && stream.current.code == Code.OR) {
      val op = stream.next().code
      val right = parseXor()
      expr = Binary(op, expr, right)
    }
    expr
  }

  private def parseXor(): Expression = {
    var expr = parseAnd()
    while (stream.hasNext && stream.current.code == Code.XOR) {
      val op = stream.next().code
      val right = parseAnd()
      expr = Binary(op, expr, right)
    }
    expr
  }

  private def parseAnd(): Expression = {
    var expr = parseEquality()
    while (stream.hasNext && stream.current.code == Code.AND) {
      val op = stream.next().code
      val right = parseEquality()
      expr = Binary(op, expr, right)
    }
    expr
  }

  private def parseEquality(): Expression = {
    var expr = parseComparison()
    while (stream.hasNext && Set(Code.EQUAL, Code.NOT_EQUAL).contains(stream.current.code)) {
      val op = stream.next().code
      val right = parseComparison()
      expr = Binary(op, expr, right)
    }
    expr
  }

  private def parseComparison(): Expression = {
    var expr = parseTerm()
    while (stream.hasNext && Set(Code.LESS, Code.LESS_OR_EQUAL, Code.MORE, Code.MORE_OR_EQUAL).contains(stream.current.code)) {
      val op = stream.next().code
      val right = parseTerm()
      expr = Binary(op, expr, right)
    }
    expr
  }

  private def parseTerm(): Expression = {
    var expr = parseFactor()
    while (stream.hasNext && Set(Code.PLUS, Code.MINUS).contains(stream.current.code)) {
      val op = stream.next().code
      val right = parseFactor()
      expr = Binary(op, expr, right)
    }
    expr
  }

  private def parseFactor(): Expression = {
    var expr = parseUnary()
    while (stream.hasNext && Set(Code.MULTIPLICATION, Code.DIVISION).contains(stream.current.code)) {
      val op = stream.next().code
      val right = parseUnary()
      expr = Binary(op, expr, right)
    }
    expr
  }

  private def parseUnary(): Expression = {
    if (stream.hasNext && Set(Code.MINUS, Code.NOT).contains(stream.current.code)) {
      val op = stream.next().code
      val right = parseUnary()
      Unary(op, right)
    } else parseTypeCheck()
  }

  private def parseTypeCheck(): Expression = {
    var expr = parsePrimary()
    while (stream.hasNext && stream.current.code == Code.IS) {
      stream.next()
      val indicator = parseTypeIndicator()
      expr = TypeCheck(expr, indicator)
    }
    expr
  }

  private def parseTypeIndicator(): TypeIndicator = {
    if (!stream.hasNext)
      throw new UnexpectedTokenException(null, null)

    val tok = stream.current

    tok.code match {
      case Code.INT => stream.next(); TypeIndicator.IntType
      case Code.REAL => stream.next(); TypeIndicator.RealType
      case Code.BOOL => stream.next(); TypeIndicator.BoolType
      case Code.STRING => stream.next(); TypeIndicator.StringType
      case Code.NONE => stream.next(); TypeIndicator.NoneType

      case Code.SQUARE_BRACKET_LEFT =>
        stream.next()
        stream.expect(Code.SQUARE_BRACKET_RIGHT)
        TypeIndicator.ArrayType

      case Code.CURLY_BRACKET_LEFT =>
        stream.next()
        stream.expect(Code.CURLY_BRACKET_RIGHT)
        TypeIndicator.TupleType

      case Code.FUNC =>
        stream.next()
        TypeIndicator.FuncType

      case _ =>
        throw new UnexpectedTokenException(tok, null)
    }
  }

  private def parsePrimary(): Expression = {
    if (!stream.hasNext) throw new UnexpectedTokenException(null, null)

    val tok = stream.next()
    val isIdent = tok.code == Code.IDENTIFIER
    var expr: Expression = tok.code match {
      case Code.REAL_LITERAL => Literal(tok.value)
      case Code.STRING_LITERAL => Literal(tok.value)
      case Code.TRUE => Literal(true)
      case Code.FALSE => Literal(false)
      case Code.IDENTIFIER => Variable(tok.value.toString)
      case Code.INT_LITERAL =>
        if (stream.hasNext && stream.current.code == Code.DOT) {
          stream.peek(1) match {
            case Some(nextTok) if nextTok.code == Code.INT_LITERAL =>
              stream.next()
              val right = stream.next()
              Literal(s"${tok.value}.${right.value}".toDouble)
            case _ => Literal(tok.value)
          }
        } else Literal(tok.value)


      case Code.ROUND_BRACKET_LEFT =>
        val inner = parseExpression()
        stream.expect(Code.ROUND_BRACKET_RIGHT)
        inner

      case Code.SQUARE_BRACKET_LEFT =>
        ArrayLiteral(parseArrayElements())

      case Code.CURLY_BRACKET_LEFT =>
        val elems = parseTupleElements()
        TupleLiteral(elems.map { case (k, v) => TupleEntry(k, v) })

      case Code.FUNC =>
        parseFunctionLiteral()

      case Code.NONE =>
        Literal(None)

      case _ =>
        throw new UnexpectedTokenException(tok, null)
    }

    if(isIdent){
      while (stream.hasNext) {
        stream.current.code match {
          case Code.ROUND_BRACKET_LEFT => expr = parseFunctionCall(expr)
          case Code.SQUARE_BRACKET_LEFT => expr = parseArrayAccess(expr)
          case Code.DOT => expr = parseDotAccess(expr)
          case _ => return expr
        }
      }
    }
    expr
  }

  private def parseFunctionLiteral(): Expression = {
    var args = List.empty[Variable]

    if (stream.hasNext && stream.current.code == Code.ROUND_BRACKET_LEFT) {
      stream.next()
      if (stream.hasNext && stream.current.code == Code.IDENTIFIER) {
        args ::= Variable(stream.next().value.toString)
        while (stream.hasNext && stream.current.code == Code.COMMA) {
          stream.next()
          args ::= Variable(stream.next().value.toString)
        }
      }
      stream.expect(Code.ROUND_BRACKET_RIGHT)
    }

    val bodyParser = new AASTParser(stream)

    if (stream.hasNext && stream.current.code == Code.IS) {
      stream.next()
      val codeBlock = bodyParser.parseCodeBlock(Set(Code.END))
      stream.expect(Code.END)
      FunctionLiteral(args.reverse, codeBlock)
    } else if (stream.hasNext && stream.current.code == Code.LAMBDA) {
      stream.next()
      val expr = parseExpression()
      LambdaLiteral(args.reverse, expr)
    } else {
      throw new UnexpectedTokenException(stream.current, Code.IS)
    }
  }

  private def parseArrayElements(): List[Expression] = {
    var elements = List.empty[Expression]
    if (stream.hasNext && stream.current.code != Code.SQUARE_BRACKET_RIGHT) {
      elements ::= parseExpression()
      while (stream.hasNext && stream.current.code == Code.COMMA) {
        stream.next()
        elements ::= parseExpression()
      }
    }
    stream.expect(Code.SQUARE_BRACKET_RIGHT)
    elements.reverse
  }

  private def parseTupleElements(): List[(Option[String], Expression)] = {
    var elements = List.empty[(Option[String], Expression)]
    if (stream.hasNext && stream.current.code != Code.CURLY_BRACKET_RIGHT) {
      elements ::= parseTupleElement()
      while (stream.hasNext && stream.current.code == Code.COMMA) {
        stream.next()
        elements ::= parseTupleElement()
      }
    }
    stream.expect(Code.CURLY_BRACKET_RIGHT)
    elements.reverse
  }

  private def parseTupleElement(): (Option[String], Expression) = {
    val first = stream.current
    if (first.code == Code.IDENTIFIER && stream.peek(1).exists(_.code == Code.ASSIGNMENT)) {
      val id = stream.next()
      stream.next()
      val expr = parseExpression()
      (Some(id.value.toString), expr)
    } else (None, parseExpression())
  }

  private def parseFunctionCall(expr: Expression): Expression = {
    stream.next()
    var args = List.empty[Expression]
    if (stream.hasNext && stream.current.code != Code.ROUND_BRACKET_RIGHT) {
      args ::= parseExpression()
      while (stream.hasNext && stream.current.code == Code.COMMA) {
        stream.next()
        args ::= parseExpression()
      }
    }
    stream.expect(Code.ROUND_BRACKET_RIGHT)
    FunctionCall(expr, args.reverse)
  }

  private def parseArrayAccess(expr: Expression): Expression = {
    stream.next()
    val idxExpr = parseExpression()
    stream.expect(Code.SQUARE_BRACKET_RIGHT)
    ArrayAccess(expr, idxExpr)
  }

  private def parseDotAccess(expr: Expression): Expression = {
    stream.expect(Code.DOT)
    val fieldTok = stream.next()
    fieldTok.code match {
      case Code.IDENTIFIER => TupleFieldAccess(expr, fieldTok.value.toString)
      case Code.INT_LITERAL => TupleIndexAccess(expr, fieldTok.value.toString.toInt)
      case _ => throw new InvalidTokenException(fieldTok)
    }
  }
}
