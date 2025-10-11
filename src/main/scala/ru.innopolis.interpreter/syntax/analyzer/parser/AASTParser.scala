package ru.innopolis.interpreter.syntax.analyzer.parser

import ru.innopolis.interpreter.exception.InvalidTokenException
import ru.innopolis.interpreter.lexer.{Code, Token}
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression
import ru.innopolis.interpreter.syntax.analyzer.tree.statement._
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration.VariableDeclaration
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.references.ArrayAccess

class AASTParser(tokens: List[Token[_]]) {

  private val stream = new TokenStream(tokens.filter(_.code != Code.SPACE))
  private val exprParser = new ExpressionParser(stream)

  def parse(): CodeBlock = {
    val stmts = scala.collection.mutable.ListBuffer.empty[Statement]
    while (stream.hasNext) {
      skip(Set(Code.NEWLINE, Code.SEMICOLON))
      if (stream.hasNext) stmts += parseStatement()
    }
    CodeBlock(stmts.toList)
  }

  private def parseStatement(): Statement = {
    if (!stream.hasNext) throw new InvalidTokenException(null, null)

    stream.current.code match {
      case Code.VAR   => parseVariableDeclaration()
      case Code.IF    => parseIfStatement()
      case Code.PRINT => parsePrintStatement()

      // handle t[10] := something
      case Code.IDENTIFIER =>
        // Peek ahead to detect array element assignment
        val expr = exprParser.parseExpression()
        if (stream.hasNext && stream.current.code == Code.ASSIGNMENT)
          parseAssignment(expr)
        else
          ExpressionStatement(expr)

      case _ => throw new InvalidTokenException(stream.current, null)
    }
  }

  /** Handles t[10] := expr type assignment */
  private def parseAssignment(lhs: Expression): Statement = lhs match {
    case ArrayAccess(target, index) =>
      stream.expect(Code.ASSIGNMENT)
      val valueExpr = exprParser.parseExpression()
      ArrayElementAssignment(target, index, valueExpr)

    case _ =>
      throw new InvalidTokenException(stream.current, Code.ASSIGNMENT)
  }

  private def parseVariableDeclaration(): VariableDeclaration = {
    stream.expect(Code.VAR)
    val id = stream.expect(Code.IDENTIFIER)
    stream.expect(Code.ASSIGNMENT)
    val expr = exprParser.parseExpression()
    VariableDeclaration(id.value.toString, expr)
  }

  private def parseIfStatement(): IfStatement = {
    stream.expect(Code.IF)
    val condition = exprParser.parseExpression()
    if (stream.current.code == Code.THEN) {
      stream.next()
      val thenBlock = parseCodeBlock(Set(Code.ELSE, Code.END))
      val elseBlock =
        if (stream.hasNext && stream.current.code == Code.ELSE) {
          stream.next()
          Some(parseCodeBlock(Set(Code.END)))
        } else None
      stream.expect(Code.END)
      IfStatement(condition, thenBlock, elseBlock)
    } else if (stream.current.code == Code.LAMBDA) {
      stream.next()
      val thenBlock = parseCodeBlock(Set(Code.NEWLINE, Code.SEMICOLON))
      IfStatement(condition, thenBlock, None)
    } else throw new InvalidTokenException(stream.current, Code.THEN)
  }

  private def parsePrintStatement(): PrintStatement = {
    stream.expect(Code.PRINT)
    val exprs = scala.collection.mutable.ListBuffer(exprParser.parseExpression())
    while (stream.hasNext && stream.current.code == Code.COMMA) {
      stream.next()
      exprs += exprParser.parseExpression()
    }
    stream.skipIf(Code.NEWLINE)
    PrintStatement(exprs.toList)
  }

  private def parseCodeBlock(until: Set[Code]): CodeBlock = {
    val stmts = scala.collection.mutable.ListBuffer.empty[Statement]
    while (stream.hasNext && !until.contains(stream.current.code)) {
      skip(Set(Code.NEWLINE, Code.SEMICOLON))
      if (stream.hasNext && !until.contains(stream.current.code)) stmts += parseStatement()
    }
    CodeBlock(stmts.toList)
  }

  private def skip(codes: Set[Code]): Unit =
    while (stream.hasNext && codes.contains(stream.current.code)) stream.next()
}
