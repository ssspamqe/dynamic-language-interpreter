package ru.innopolis.interpreter


import ru.innopolis.interpreter.syntax.analyzer.parser.{AASTParser, ExpressionParser, TokenStream}
object Main {
  def main(args: Array[String]): Unit = {
    val lexer = new RegexLexer()

//    val code =
//      """loop
//        |  x := 10
//        |  y := 20
//        |  print x+y
//        |end
//        |""".stripMargin

    val code =
      """while x>10 loop
        |print 10
        |end
        |""".stripMargin

    val tokens = lexer.tokenize(code)
    val parser = new AASTParser(tokens)
    var expression = parser.parse()


    for (token <- tokens) {
      println(s"$token")
    }
  }
}