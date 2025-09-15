package ru.innopolis.interpreter

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import org.scalatest.funsuite.AnyFunSuite
import ru.innopolis.interpreter.lexer.Token

class RegexLexerTest extends AnyFunSuite {

  private val lexer = new RegexLexer()

  test("run all example tests") {
    val examplesDir = Path.of("src/test/resources/lexer_tests")
    assert(Files.exists(examplesDir), s"There is no folder with code examples: $examplesDir")

    val files = Files.list(examplesDir).iterator().asScala
      .filter(_.toString.endsWith(".txt"))
      .toList
      .sortBy(_.getFileName.toString)

    assert(files.nonEmpty, s"There are no test files in $examplesDir")

    println(s"Found ${files.size} test files in $examplesDir")

    var passed = 0
    files.foreach { file =>
      runTestFile(file)
      passed += 1
      println(s"✔ Passed: ${file.getFileName}")
    }

    println(s"✅ All $passed tests passed successfully.")
  }

  private def runTestFile(file: Path): Unit = {
    val content = Files.readString(file)
    val parts = content.split("(?m)^---$").map(_.trim)
    assert(parts.length == 2, s"File must contain 2 sections: $file")

    val code = parts(0)
    val expectedOutput = parts(1)

    // tokenization
    val tokens: List[Token[_]] = lexer.tokenize(code)

    // Collect tokens as strings
    val actualTokens = tokens.map(_.toString).mkString("\n")

    // Assert tokens
    assert(actualTokens == expectedOutput,
      s"File: ${file.getFileName} - tokens did not match")
  }
}
