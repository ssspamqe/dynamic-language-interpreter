package ru.innopolis.interpreter.cli.prettyprint

trait PrettyPrinter {
  def printStage(stageName: String): Unit

  def printSuccess(): Unit

  def printError(message: String): Unit
}
