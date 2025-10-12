package ru.innopolis.interpreter


import ru.innopolis.interpreter.lexer.Code
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
      """var i := func => a+b
        |
        |print(func(a,b) => a+b)
        |""".stripMargin

    val tokens = lexer.tokenize(code).filter(t => t.code != Code.SPACE)
    val stream = new TokenStream(tokens)
    val parser = new AASTParser(stream)
    var expression = parser.parse()


    for (token <- tokens) {
      println(s"$token")
    }
  }
}