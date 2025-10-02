package ru.innopolis.interpreter


import ru.innopolis.interpreter.syntax.analyzer.parser.{AASTParser, ExpressionParser}
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression

object Main {
  def main(args: Array[String]): Unit = {
    val lexer = new RegexLexer()

    val code =
      """var a:=""".stripMargin

    val tokens = lexer.tokenize(code)

    val parser = new AASTParser
    var expressionParser = new ExpressionParser
    var expression = parser.parse(tokens)


    for (token <- tokens) {
      println(s"$token")
    }
  }
}