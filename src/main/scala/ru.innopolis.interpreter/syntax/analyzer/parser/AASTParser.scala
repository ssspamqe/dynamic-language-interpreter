package ru.innopolis.interpreter.syntax.analyzer.parser

import ru.innopolis.interpreter.exception.InvalidTokenException
import ru.innopolis.interpreter.lexer.{Code, Token}
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.{Expression, Variable}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement._
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration.VariableDeclaration
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.references.ArrayAccess
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.assignment.{ArrayElementAssignment, VariableAssignment}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.loop.{Loop, WhileLoop}

class AASTParser(tokens: List[Token[_]]) {

  private val stream = new TokenStream(tokens.filter(_.code != Code.SPACE))
  private val expressionParser = new ExpressionParser(stream)

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
      case Code.LOOP => parseLoopStatement()
      case Code.EXIT => parseExitStatement()
      case Code.WHILE => parseWhileStatement()
      case Code.IDENTIFIER =>
        // Parse expression and check if it's an assignment
        val expr = expressionParser.parseExpression()
        if (stream.hasNext && stream.current.code == Code.ASSIGNMENT)
          parseAssignment(expr)
        else
          ExpressionStatement(expr)

      case _ =>
        val expr = expressionParser.parseExpression()
        ExpressionStatement(expr)
    }
  }

  private def parseWhileStatement() : WhileLoop = {
    stream.expect(Code.WHILE)
    val condition = expressionParser.parseExpression()
    stream.expect(Code.LOOP)
    val body = parseCodeBlock(Set(Code.END))
    stream.expect(Code.END)
    WhileLoop(condition, body)
  }

  private def parseLoopStatement(): Loop  = {
    stream.expect(Code.LOOP)
    val body = parseCodeBlock(Set(Code.END))
    stream.expect(Code.END)
    new Loop(body)
  }

  private def parseExitStatement():ExitStatement = {
    stream.expect(Code.EXIT)
    ExitStatement()
  }

  private def parseAssignment(lhs: Expression): Statement = {
    stream.expect(Code.ASSIGNMENT)
    val valueExpr = expressionParser.parseExpression()

    lhs match {
      case Variable(name) =>
        VariableAssignment(name, valueExpr)

      case ArrayAccess(target, index) =>
        ArrayElementAssignment(target, index, valueExpr)

      case _ =>
        throw new InvalidTokenException(stream.current, Code.ASSIGNMENT)
    }
  }

  private def parseVariableDeclaration(): VariableDeclaration = {
    stream.expect(Code.VAR)
    val id = stream.expect(Code.IDENTIFIER)
    stream.expect(Code.ASSIGNMENT)
    val expr = expressionParser.parseExpression()
    VariableDeclaration(id.value.toString, expr)
  }

  private def parseIfStatement(): IfStatement = {
    stream.expect(Code.IF)
    val condition = expressionParser.parseExpression()
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
    val exprs = scala.collection.mutable.ListBuffer(expressionParser.parseExpression())
    while (stream.hasNext && stream.current.code == Code.COMMA) {
      stream.next()
      exprs += expressionParser.parseExpression()
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
