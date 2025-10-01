package ru.innopolis.interpreter.syntax.analyzer.parser

import ru.innopolis.interpreter.exception.InvalidTokenException
import ru.innopolis.interpreter.lexer.{Code, Token}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.{IfStatement, Statement, CodeBlock}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration.VariableDeclaration

import scala.collection.BufferedIterator

class AASTParser {

  private val expressionParser = new ExpressionParser

  def parse(tokens: List[Token[_]]):CodeBlock = {
    val tokenIterator = tokens.filter(t => t.code != Code.SPACE).iterator.buffered
    val topLevelStatements = scala.collection.mutable.ListBuffer.empty[Statement]

    while (tokenIterator.hasNext) {
      topLevelStatements.addOne(parseStatement(tokenIterator))
    }

    CodeBlock(topLevelStatements.toList)
  }


  private def parseStatement(tokenIterator: BufferedIterator[Token[_]]): Statement = {
    if (!tokenIterator.hasNext) {
      throw new InvalidTokenException(null,null);
    }
    skipTokens(tokenIterator, Set(Code.NEWLINE, Code.SPACE))
    tokenIterator.head.code match {
      case Code.VAR => parseVariableDeclaration(tokenIterator)
      case Code.IF => parseIfStatement(tokenIterator)
    }
  }

  private def parseVariableDeclaration(iterator: BufferedIterator[Token[_]]): VariableDeclaration = {
    assertTokenCode(iterator.next(), Code.VAR)
    val identifierToken = iterator.next()
    assertTokenCode(identifierToken, Code.IDENTIFIER)
    assertTokenCode(iterator.next(), Code.ASSIGNMENT)
    val expr = expressionParser.parseExpression(iterator)

    VariableDeclaration(identifierToken.value.toString, expr)
  }


  private def parseIfStatement(iterator: BufferedIterator[Token[_]]): IfStatement = {
    assertTokenCode(iterator.next(), Code.IF) // consume 'if'
    assertTokenCode(iterator.next(), Code.SPACE) // consume space
    val condition = expressionParser.parseExpression(iterator)
    assertTokenCode(iterator.next(), Code.SPACE) // consume space
    // two possible forms: "then ... end" or "=> ..."
    if (iterator.hasNext && iterator.head.code == Code.THEN) {
      iterator.next() // consume 'then'
      val thenBody = parseCodeBlock(iterator)

      // optional else
      var elseBody: Option[CodeBlock] = None
      if (iterator.hasNext && iterator.head.code == Code.ELSE) {
        iterator.next() // consume 'else'
        elseBody = Some(parseCodeBlock(iterator))
      }

      // must close with 'end'
      assertTokenCode(iterator.next(), Code.END)

      IfStatement(condition, thenBody, elseBody)

    } else if (iterator.hasNext && iterator.head.code == Code.LAMBDA) {
      // short if: "if expr => Body"
      iterator.next() // consume "=>"
      val thenBody = parseCodeBlock(iterator)
      IfStatement(condition, thenBody, None)

    } else {
      throw new InvalidTokenException(iterator.head, Code.THEN) // expected THEN or =>
    }
  }

  private def parseCodeBlock(iterator: BufferedIterator[Token[_]], endTokens: Set[Code] = Set(Code.END)): CodeBlock = {
    val statements = scala.collection.mutable.ListBuffer.empty[Statement]

    while (iterator.hasNext && !endTokens.contains(iterator.head.code)) {
      statements.addOne(parseStatement(iterator))
    }

    CodeBlock(statements.toList)
  }

  private def skipTokens(iterator: BufferedIterator[Token[_]], tokensToSkip:Set[Code]): Unit = {
    while(iterator.hasNext && tokensToSkip.contains(iterator.head.code)){
      iterator.next()
    }
  }


  private def assertTokenCode(actualToken: Token[_], expectedCode: Code): Unit = {
    if (actualToken.code != expectedCode) {
      throw new InvalidTokenException(actualToken, expectedCode)
    }
  }


}
