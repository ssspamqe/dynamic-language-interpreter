package ru.innopolis.interpreter

import scala.jdk.CollectionConverters._

object Main {
  def main(args: Array[String]): Unit = {
    val lexer = new RegexLexer()

    val code =
      """
        |var t := {a:=1, b:=2}
        |print t.c
      """.stripMargin

    val tokens = lexer.tokenize(code)

    for (token <- tokens) {
      println(s"$token")
    }
  }
}