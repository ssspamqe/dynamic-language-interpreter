package ru.innopolis.interpreter


import ru.innopolis.interpreter.analyzer.semantic.optimization.Optimizer
import ru.innopolis.interpreter.runtime.Interpreter
import ru.innopolis.interpreter.syntax.analyzer.parser.{AASTParser, TokenStream}

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    // Parse command line arguments
    val inputFile = if (args.length > 0) args(0) else "CodeInput.txt"
    val showAst = args.length > 1 && args(1) == "ast"

    // Read source code
    val code = if (inputFile.startsWith("/") || inputFile.contains(":")) {
      Source.fromFile(inputFile).mkString
    } else {
      Source.fromResource(inputFile).mkString
    }

    // Lexing and parsing
    val lexer = new RegexLexer()
    val tokens = lexer.tokenize(code)
    val stream = new TokenStream(tokens)
    val parser = new AASTParser(stream)
    val ast = parser.parse()
    val optimizedAst = Optimizer.optimize(ast)

    if (showAst) {
      println("=== AST OUTPUT ===")
      CaseClassPrinter.printCaseClass(optimizedAst)
    } else {
      // Interpretation
      println("=== INTERPRETATION ===")
      val interpreter = new Interpreter()
      interpreter.interpret(optimizedAst)
    }
  }
}