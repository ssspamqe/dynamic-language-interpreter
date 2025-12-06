package ru.innopolis.interpreter.cli

import ru.innopolis.interpreter.RegexLexer
import ru.innopolis.interpreter.analyzer.semantic.optimization.Optimizer
import ru.innopolis.interpreter.cli.args.ProgramArgs
import ru.innopolis.interpreter.cli.prettyprint.{PrettyPrinter, SimplePrettyPrinter}
import ru.innopolis.interpreter.runtime.Interpreter
import ru.innopolis.interpreter.syntax.analyzer.parser.{AASTParser, TokenStream}
import ru.innopolis.interpreter.syntax.analyzer.semantic.SemanticCheckAnalyzer
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.CodeBlock

import scala.io.Source

class InterpreterPipeline(programArgs: ProgramArgs) {

  private val prettyPrinter: PrettyPrinter = new SimplePrettyPrinter()

  def execute(): Unit = {
    try {
      val code = readSourceCode()
      val tokens = parseCode(code)
      val ast = buildAast(tokens)

      val checkedAst = if (programArgs.semanticCheck) performSemanticCheck(ast) else ast
      val optimizedAst = if (programArgs.semanticOptimize) optimizeCode(checkedAst) else checkedAst

      if (programArgs.showAst) {
        displayAst(optimizedAst)
      } else {
        interpretCode(optimizedAst)
      }
    } catch {
      case e: Exception =>
        // Print user-friendly error message first
        prettyPrinter.printError(e.getMessage)
        // If debug flag enabled, print stacktrace to stderr
        if (programArgs.debug) {
          e.printStackTrace()
        }
    }
  }

  private def readSourceCode(): String = {
    if (programArgs.prettyPrint) prettyPrinter.printStage("Reading source file")
    val inputFile = programArgs.codePath
    val code = if (inputFile.startsWith("/") || inputFile.contains(":")) {
      Source.fromFile(inputFile).mkString
    } else {
      Source.fromResource(inputFile).mkString
    }

    if (programArgs.prettyPrint) prettyPrinter.printSuccess()
    code
  }

  private def parseCode(code: String): List[ru.innopolis.interpreter.lexer.Token[_]] = {
    if (programArgs.prettyPrint) prettyPrinter.printStage("Lexing")
    val lexer = new RegexLexer()
    val tokens = lexer.tokenize(code)
    if (programArgs.prettyPrint) prettyPrinter.printSuccess()
    tokens
  }

  private def buildAast(tokens: List[ru.innopolis.interpreter.lexer.Token[_]]): CodeBlock = {
    if (programArgs.prettyPrint) prettyPrinter.printStage("Parsing and building AAST")
    val stream = new TokenStream(tokens)
    val parser = new AASTParser(stream)
    val ast = parser.parse()
    if (programArgs.prettyPrint) prettyPrinter.printSuccess()
    ast
  }

  private def performSemanticCheck(ast: CodeBlock): CodeBlock = {
    if (programArgs.prettyPrint) prettyPrinter.printStage("Semantic check")
    val semanticCheckAnalyzer = new SemanticCheckAnalyzer()
    semanticCheckAnalyzer.analyze(ast)
    if (programArgs.prettyPrint) prettyPrinter.printSuccess()
    ast
  }

  private def optimizeCode(ast: CodeBlock): CodeBlock = {
    if (programArgs.prettyPrint) prettyPrinter.printStage("Semantic optimizations")
    val optimizedAst = Optimizer.optimize(ast)
    if (programArgs.prettyPrint) prettyPrinter.printSuccess()
    optimizedAst
  }

  private def displayAst(ast: CodeBlock): Unit = {
    if (programArgs.prettyPrint) println("\n=== AST OUTPUT ===")
    else println("=== AST OUTPUT ===")
    CaseClassPrinter.printCaseClass(ast)
  }

  private def interpretCode(ast: CodeBlock): Unit = {
    if (programArgs.prettyPrint) {
      prettyPrinter.printStage("Interpretation")
      println()
    }
    val interpreter = new Interpreter()
    interpreter.interpret(ast)
  }
}
