package ru.innopolis.interpreter


import ru.innopolis.interpreter.syntax.analyzer.parser.{AASTParser, ExpressionParser, TokenStream}
object Main {
  def main(args: Array[String]): Unit = {
    val lexer = new RegexLexer()

    val code =
      """var x := {1,2,1+2}
        |if x > 0 then
        |    print "positive"
        |else
        |    print "non-positive"
        |end""".stripMargin

    val tokens = lexer.tokenize(code)
    val parser = new AASTParser(tokens)
    var expression = parser.parse()


    for (token <- tokens) {
      println(s"$token")
    }
  }
}