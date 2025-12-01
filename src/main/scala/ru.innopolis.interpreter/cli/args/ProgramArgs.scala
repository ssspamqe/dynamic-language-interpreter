package ru.innopolis.interpreter.cli.args

case class ProgramArgs(
                        codePath: String,
                        showAst: Boolean,
                        semanticCheck: Boolean,
                        semanticOptimize: Boolean,
                        prettyPrint: Boolean
                      )