package ru.innopolis.interpreter.cli.prettyprint

class SimplePrettyPrinter extends PrettyPrinter {
  override def printStage(stageName: String): Unit = {
    print(s"[$stageName] ... ")
  }

  override def printSuccess(): Unit = {
    println("✓")
  }

  override def printError(message: String): Unit = {
    println(s"\nERROR")
    println(message)
  }
}
