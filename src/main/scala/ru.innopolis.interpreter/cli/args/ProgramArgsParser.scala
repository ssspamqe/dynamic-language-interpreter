package ru.innopolis.interpreter.cli.args

class ProgramArgsParser {
  def parseArgs(args: Array[String]): ProgramArgs = {
    var filePath: Option[String] = None
    var flags = List.empty[String]
    var i = 0

    while (i < args.length) {
      args(i) match {
        case arg if arg.startsWith("--") || arg.startsWith("-") =>
          val key = arg.replaceFirst("^-+", "")
          flags = flags :+ key
          i += 1
        case arg =>
          if (filePath.isEmpty) {
            filePath = Some(arg)
          }
          i += 1
      }
    }

    ProgramArgs(
      codePath = filePath.getOrElse("CodeInput.txt"),
      showAst = flags.contains("show-ast"),
      semanticCheck = !flags.contains("semantic-check-off"),
      semanticOptimize = !flags.contains("semantic-optimize-off"),
      prettyPrint = flags.contains("pretty-print"),
      debug = flags.contains("debug")
    )
  }
}
