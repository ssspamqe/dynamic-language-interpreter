package ru.innopolis.interpreter


import ru.innopolis.interpreter.lexer.{Code, Token}
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
      """var i := a.1.1
        |""".stripMargin

    val tokens = lexer.tokenize(code)
      .filter(_.code != Code.SPACE)
      .map(t => if (t.code == Code.SEMICOLON) t.copy(code = Code.NEWLINE) else t)
      .foldRight(List.empty[Token[_]]) {
        case (t, acc) if acc.headOption.exists(_.code == Code.NEWLINE) && t.code == Code.NEWLINE => acc
        case (t, acc) => t :: acc
      }

    val stream = new TokenStream(tokens)
    val parser = new AASTParser(stream)
    var expression = parser.parse()

    for (token <- tokens) {
      println(s"$token")
    }
  }
}