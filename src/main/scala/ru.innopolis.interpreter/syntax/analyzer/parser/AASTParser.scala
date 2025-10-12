package ru.innopolis.interpreter.syntax.analyzer.parser

import ru.innopolis.interpreter.exception.InvalidTokenException
import ru.innopolis.interpreter.lexer.Code
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.references.ArrayAccess
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.{Expression, Variable}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement._
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.assignment.{ArrayElementAssignment, VariableAssignment}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration.VariableDeclaration
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.loop.{CollectionLoop, Loop, RangeLoop, WhileLoop}

class AASTParser(private val stream: TokenStream) {

  private val exprParser = new ExpressionParser(stream)

  def parse(): CodeBlock = {
    val stmts = scala.collection.mutable.ListBuffer.empty[Statement]
    skip(Set(Code.NEWLINE))
    while (stream.hasNext) {
      stmts += parseStatement()
      if (stream.hasNext && stream.current.code != Code.END)
        stream.expect(Code.NEWLINE)
      skip(Set(Code.NEWLINE))
    }
    CodeBlock(stmts.toList)
  }

  def parseStatement(): Statement = {
    if (!stream.hasNext) throw new InvalidTokenException(null, null)
    stream.current.code match {
      case Code.VAR    => parseVariableDeclaration()
      case Code.IF     => parseIfStatement()
      case Code.PRINT  => parsePrintStatement()
      case Code.LOOP   => parseInfiniteLoop()
      case Code.FOR    => parseForLoop()
      case Code.EXIT   => parseExitStatement()
      case Code.WHILE  => parseWhileStatement()
      case Code.RETURN => parseReturnStatement()
      case Code.IDENTIFIER =>
        val expr = exprParser.parseExpression()
        if (stream.hasNext && stream.current.code == Code.ASSIGNMENT)
          parseAssignment(expr)
        else ExpressionStatement(expr)
      case _ =>
        val expr = exprParser.parseExpression()
        ExpressionStatement(expr)
    }
  }

  // ---------- return ----------
  private def parseReturnStatement(): ReturnStatement = {
    stream.expect(Code.RETURN)
    if (!stream.hasNext || stream.current.code == Code.NEWLINE || stream.current.code == Code.END)
      ReturnStatement(None)
    else
      ReturnStatement(Some(exprParser.parseExpression()))
  }

  // ---------- variable declaration ----------
  private def parseVariableDeclaration(): VariableDeclaration = {
    stream.expect(Code.VAR)
    val id = stream.expect(Code.IDENTIFIER)
    stream.expect(Code.ASSIGNMENT)
    val expr = exprParser.parseExpression()
    VariableDeclaration(id.value.toString, expr)
  }

  // ---------- assignment ----------
  private def parseAssignment(lhs: Expression): Statement = {
    stream.expect(Code.ASSIGNMENT)
    val valueExpr = exprParser.parseExpression()
    lhs match {
      case Variable(name) => VariableAssignment(name, valueExpr)
      case ArrayAccess(target, index) => ArrayElementAssignment(target, index, valueExpr)
      case _ => throw new InvalidTokenException(stream.current, Code.ASSIGNMENT)
    }
  }

  // ---------- if ----------
  private def parseIfStatement(): Statement = {
    stream.expect(Code.IF)
    val cond = exprParser.parseExpression()

    // Short if form: "if expr => statement"
    if (stream.hasNext && stream.current.code == Code.LAMBDA) {
      stream.next() // consume =>
      val bodyStmt = parseStatement() // no newline required
      return IfStatement(cond, CodeBlock(List(bodyStmt)), None)
    }

    // Normal form: "if expr then ... end"
    stream.expect(Code.THEN)
    if (stream.current.code == Code.NEWLINE) stream.next()
    val thenBlock = parseCodeBlock(Set(Code.ELSE, Code.END))
    val elseBlock =
      if (stream.hasNext && stream.current.code == Code.ELSE) {
        stream.next()
        if (stream.current.code == Code.NEWLINE) stream.next()
        Some(parseCodeBlock(Set(Code.END)))
      } else None
    stream.expect(Code.END)
    IfStatement(cond, thenBlock, elseBlock)
  }

  // ---------- print ----------
  private def parsePrintStatement(): PrintStatement = {
    stream.expect(Code.PRINT)
    val exprs = scala.collection.mutable.ListBuffer(exprParser.parseExpression())
    while (stream.hasNext && stream.current.code == Code.COMMA) {
      stream.next()
      exprs += exprParser.parseExpression()
    }
    PrintStatement(exprs.toList)
  }

  // ---------- loops ----------
  private def parseForLoop(): Loop = {
    stream.expect(Code.FOR)
    if (stream.peek().exists(_.code == Code.IDENTIFIER) && stream.peek(1).exists(_.code == Code.IN)) {
      val id = stream.expect(Code.IDENTIFIER)
      stream.expect(Code.IN)
      val rangeStart = exprParser.parseExpression()

      if (stream.peek().exists(_.code == Code.RANGE)) {
        stream.expect(Code.RANGE)
        val rangeEnd = exprParser.parseExpression()
        val body = parseInfiniteLoop()
        return RangeLoop(Some(id.value.toString), rangeStart, rangeEnd, body.body)
      }

      val body = parseInfiniteLoop()
      return CollectionLoop(id.value.toString, rangeStart, body.body)
    }

    val rangeFrom = exprParser.parseExpression()
    stream.expect(Code.RANGE)
    val rangeTo = exprParser.parseExpression()
    val body = parseInfiniteLoop()
    RangeLoop(None, rangeFrom, rangeTo, body.body)
  }

  private def parseWhileStatement(): WhileLoop = {
    stream.expect(Code.WHILE)
    val cond = exprParser.parseExpression()
    val loop = parseInfiniteLoop()
    WhileLoop(cond, loop.body)
  }

  private def parseInfiniteLoop(): Loop = {
    stream.expect(Code.LOOP)
    if (stream.current.code == Code.NEWLINE) stream.next()
    val body = parseCodeBlock(Set(Code.END))
    stream.expect(Code.END)
    new Loop(body)
  }

  private def parseExitStatement(): ExitStatement = {
    stream.expect(Code.EXIT)
    ExitStatement()
  }

  // ---------- function body ----------
  def parseFunctionBody(): CodeBlock = {
    if (stream.hasNext && stream.current.code == Code.IS) {
      stream.next()
      if (stream.current.code == Code.NEWLINE) stream.next()
      val codeBlock = parseCodeBlock(Set(Code.END))
      stream.expect(Code.END)
      codeBlock
    } else if (stream.hasNext && stream.current.code == Code.LAMBDA) {
      stream.next()
      val expr = exprParser.parseExpression()
      CodeBlock(List(ExpressionStatement(expr)))
    } else {
      throw new InvalidTokenException(stream.current, Code.IS)
    }
  }

  // ---------- generic code block ----------
  def parseCodeBlock(until: Set[Code]): CodeBlock = {
    val stmts = scala.collection.mutable.ListBuffer.empty[Statement]
    skip(Set(Code.NEWLINE))
    while (stream.hasNext && !until.contains(stream.current.code)) {
      stmts += parseStatement()
      if (stream.hasNext && stream.current.code != Code.END && !until.contains(stream.current.code))
        stream.expect(Code.NEWLINE)
      skip(Set(Code.NEWLINE))
    }
    CodeBlock(stmts.toList)
  }

  private def skip(codes: Set[Code]): Unit =
    while (stream.hasNext && codes.contains(stream.current.code)) stream.next()
}
