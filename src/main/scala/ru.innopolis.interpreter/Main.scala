package ru.innopolis.interpreter

import ru.innopolis.interpreter.cli.InterpreterPipeline
import ru.innopolis.interpreter.cli.args.ProgramArgsParser

//Usage:
//  non flagged arg - file path (default "CodeInput.txt")
//  --show-ast - show AST instead of interpreting (default: interpret)
//  --semantic-check-off - disable semantic check (default: enabled)
//  --semantic-optimize-off - disable semantic optimization (default: enabled)
//  --pretty-print - show stage progress and pretty output (default: disabled)

object Main {

  private val programArgsParser = new ProgramArgsParser()

  def main(args: Array[String]): Unit = {
    val programArgs = programArgsParser.parseArgs(args)
    val pipeline = new InterpreterPipeline(programArgs)
    pipeline.execute()
  }
}