package ru.innopolis.interpreter.syntax.analyzer.parser

import ru.innopolis.interpreter.exception.InvalidTokenException
import ru.innopolis.interpreter.lexer.{Code, Token}
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression
import ru.innopolis.interpreter.syntax.analyzer.tree.statement._
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration.VariableDeclaration

class AASTParser(tokens: List[Token[_]]) {

  private val stream = new TokenStream(tokens.filter(_.code != Code.SPACE))
  private val expressionParser = new ExpressionParser(stream)

  def parse(): CodeBlock = {
    val stmts = scala.collection.mutable.ListBuffer.empty[Statement]
    while (stream.hasNext) stmts += parseStatement()
    CodeBlock(stmts.toList)
  }

  private def parseStatement(): Statement = {
    skip(Set(Code.NEWLINE))
    if (!stream.hasNext) throw new InvalidTokenException(null, null)
    stream.current.code match {
      case Code.VAR   => parseVariableDeclaration()
      case Code.IF    => parseIfStatement()
      case Code.PRINT => parsePrintStatement()
      case _          => throw new InvalidTokenException(stream.current, null)
    }
  }

  private def parseVariableDeclaration(): VariableDeclaration = {
    stream.expect(Code.VAR)
    val identifier = stream.next()
    stream.expect(Code.ASSIGNMENT)
    val expr = expressionParser.parseExpression()
    VariableDeclaration(identifier.value.toString, expr)
  }

  private def parseIfStatement(): IfStatement = {
    stream.expect(Code.IF)
    val cond = expressionParser.parseExpression()
    if (stream.current.code == Code.THEN) {
      stream.next()
      val thenBlock = parseCodeBlock(Set(Code.ELSE, Code.END))
      val elseBlock =
        if (stream.current.code == Code.ELSE) { stream.next(); Some(parseCodeBlock(Set(Code.END))) }
        else None
      stream.expect(Code.END)
      IfStatement(cond, thenBlock, elseBlock)
    } else if (stream.current.code == Code.LAMBDA) {
      stream.next()
      val thenBody = parseCodeBlock(Set(Code.NEWLINE))
      IfStatement(cond, thenBody, None)
    } else throw new InvalidTokenException(stream.current, Code.THEN)
  }

  private def parsePrintStatement(): PrintStatement = {
    stream.expect(Code.PRINT)
    val exprs = scala.collection.mutable.ListBuffer.empty[Expression]
    exprs += expressionParser.parseExpression()
    while (stream.hasNext && stream.current.code == Code.COMMA) {
      stream.next()
      exprs += expressionParser.parseExpression()
    }
    stream.skipIf(Code.NEWLINE)
    PrintStatement(exprs.toList)
  }

  private def parseCodeBlock(endTokens: Set[Code]): CodeBlock = {
    val statements = scala.collection.mutable.ListBuffer.empty[Statement]
    while (stream.hasNext && !endTokens.contains(stream.current.code)) {
      statements += parseStatement()
      skip(Set(Code.NEWLINE))
    }
    CodeBlock(statements.toList)
  }

  private def skip(codes: Set[Code]): Unit = {
    while (stream.hasNext && codes.contains(stream.current.code)) stream.next()
  }
}
