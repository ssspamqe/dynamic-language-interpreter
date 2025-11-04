package ru.innopolis.interpreter


import ru.innopolis.interpreter.syntax.analyzer.parser.{AASTParser, TokenStream}

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val code = Source.fromResource("CodeInput.txt").mkString

    val lexer = new RegexLexer()
    val tokens = lexer.tokenize(code)
    val stream = new TokenStream(tokens)
    val parser = new AASTParser(stream)
    val expression = parser.parse()
    val analyzedExpression = SemanticAnalyzer.analyze(expression)

    CaseClassPrinter.printCaseClass(analyzedExpression)
  }

}