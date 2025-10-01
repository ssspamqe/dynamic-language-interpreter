package ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration

case class FunctionHeader(name: String,
                          parameters: List[String]
                         )
