package ru.innopolis.interpreter.io

import scala.io.Source

object FileReader {
  def readInputFile(filePath: String): String = {
    if (filePath.startsWith("/") || filePath.contains(":")) {
      Source.fromFile(filePath).mkString
    } else {
      Source.fromResource(filePath).mkString
    }
  }
}

