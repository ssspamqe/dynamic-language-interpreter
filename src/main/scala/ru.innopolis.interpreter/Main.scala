package ru.innopolis.interpreter

import ru.innopolis.interpreter.syntax.analyzer.semantic.SemanticAnalyzer
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
      """var i := 0
        |loop
        | print "Hello"
        | i := i + 1
        | if i=100 => exit
        |end
        |for 1..3 loop
        | print "Hello"
        |end
        |var array := [1,2,3,4,5]
        |var sum := 0
        |for i in array loop
        | sum := sum+i
        | exit
        |end
        |exit
        |
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
    var tree = parser.parse()

    var analyzer =  new SemanticAnalyzer()
    analyzer.analyze(tree)
    printCaseClass(tree)
    println(tree)
  }

  def printCaseClass(obj: Any, indent: String = ""): Unit = {
    obj match {
      case p: Product if p.productPrefix != "Tuple" =>
        val clsName = p.productPrefix
        val fields = p.getClass.getDeclaredFields.map(_.getName).zip(p.productIterator.toList)

        println(s"${clsName}(")
        for (((name, value), idx) <- fields.zipWithIndex) {
          val isLast = idx == fields.size - 1
          print(s"${indent} $name = ")
          value match {
            case inner: Product if inner.productPrefix != "Tuple" =>
              printCaseClass(inner, indent + "  ")
            case other =>
              println(other.toString)
          }
        }
        println(s"${indent})")
      case other =>
        println(s"${indent}${other.toString}")
    }
  }
}