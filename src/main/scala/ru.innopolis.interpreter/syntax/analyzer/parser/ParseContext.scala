package ru.innopolis.interpreter.syntax.analyzer.parser

sealed trait ParseContext {
  def description: String
}

object ParseContext {
  case object IfStatement extends ParseContext {
    override def description: String = "if statement"
  }

  case object Loop extends ParseContext {
    override def description: String = "loop"
  }

  case object FunctionDeclaration extends ParseContext {
    override def description: String = "function declaration"
  }
}
