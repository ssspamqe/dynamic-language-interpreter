package ru.innopolis.interpreter


import ru.innopolis.interpreter.analyzer.semantic.optimization.Optimizer
import ru.innopolis.interpreter.runtime.Interpreter
import ru.innopolis.interpreter.syntax.analyzer.parser.{AASTParser, TokenStream}
import ru.innopolis.interpreter.syntax.analyzer.semantic.SemanticCheckAnalyzer

import scala.io.Source


//Usage:
//  non flagged arg - file path (default "CodeInput.txt")
//  --show-ast [true|false] - show AST instead of interpreting (default false)
//  --semantic-check [true|false] - enable/disable semantic check (default true)
//  --semantic-optimize [true|false] - enable/disable semantic optimization (default true)
//  --pretty-print [true|false] - show stage progress and pretty output (default false)

object Main {
  def main(args: Array[String]): Unit = {
    val parsedArgs = parseArgs(args)
    val inputFile = parsedArgs.getOrElse("file", "CodeInput.txt")
    val showAst = parsedArgs.getOrElse("show-ast", "false").toBoolean
    val semanticCheck = parsedArgs.getOrElse("semantic-check", "true").toBoolean
    val semanticOptimize = parsedArgs.getOrElse("semantic-optimize", "true").toBoolean
    val prettyPrint = parsedArgs.getOrElse("pretty-print", "false").toBoolean

    try {
      // Read source code
      if (prettyPrint) printStage("Reading source file")
      val code = if (inputFile.startsWith("/") || inputFile.contains(":")) {
        Source.fromFile(inputFile).mkString
      } else {
        Source.fromResource(inputFile).mkString
      }
      if (prettyPrint) printSuccess()

      // Lexing
      if (prettyPrint) printStage("Lexing")
      val lexer = new RegexLexer()
      val tokens = lexer.tokenize(code)
      if (prettyPrint) printSuccess()

      // Parsing
      if (prettyPrint) printStage("Parsing and building AAST")
      val stream = new TokenStream(tokens)
      val parser = new AASTParser(stream)
      val ast = parser.parse()
      if (prettyPrint) printSuccess()

      // Semantic check
      if (semanticCheck) {
        if (prettyPrint) printStage("Semantic check")
        val semanticCheckAnalyzer = new SemanticCheckAnalyzer()
        semanticCheckAnalyzer.analyze(ast)
        if (prettyPrint) printSuccess()
      }

      // Optimization
      if (prettyPrint && semanticOptimize) printStage("Semantic optimizations")
      val optimizedAst = if (semanticOptimize) Optimizer.optimize(ast) else ast
      if (prettyPrint && semanticOptimize) printSuccess()

      if (showAst) {
        if (prettyPrint) println("\n=== AST OUTPUT ===")
        else println("=== AST OUTPUT ===")
        CaseClassPrinter.printCaseClass(optimizedAst)
      } else {
        // Interpretation
        if (prettyPrint) {
          printStage("Interpretation")
          println()
        }
        val interpreter = new Interpreter()
        interpreter.interpret(optimizedAst)
      }

    } catch {
      case e: Exception =>
        printError(e.getMessage)
    }
  }

  private def printStage(stageName: String): Unit = {
    print(s"[$stageName] ... ")
  }

  private def printSuccess(): Unit = {
    println("✓")
  }

  private def printError(message: String): Unit = {
    println(s"\nERROR")
    println(message)
  }


  private def parseArgs(args: Array[String]): Map[String, String] = {
    var result = Map.empty[String, String]
    var i = 0

    while (i < args.length) {
      args(i) match {
        case arg if arg.startsWith("--") =>
          val key = arg.substring(2)
          if (i + 1 < args.length && !args(i + 1).startsWith("--")) {
            result = result + (key -> args(i + 1))
            i += 2
          } else {
            i += 1
          }
        case arg =>
          // First non-flag argument is the file path
          if (!result.contains("file")) {
            result = result + ("file" -> arg)
          }
          i += 1
      }
    }
    result
  }
}