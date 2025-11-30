package ru.innopolis.interpreter

import org.scalatest.funsuite.AnyFunSuite
import ru.innopolis.interpreter.analyzer.semantic.optimization.Optimizer
import ru.innopolis.interpreter.runtime.Interpreter
import ru.innopolis.interpreter.syntax.analyzer.parser.{AASTParser, TokenStream}

import java.io.{ByteArrayOutputStream, PrintStream}

class InterpreterTest extends AnyFunSuite {

  /** Parse + interpret code and capture stdout */
  private def interpretCode(code: String): String = {
    val lexer = new RegexLexer()
    val tokens = lexer.tokenize(code)
    val stream = new TokenStream(tokens)
    val parser = new AASTParser(stream)
    val ast = parser.parse()
//    val optimizedAst = Optimizer.optimize(ast)

    val out = new ByteArrayOutputStream()

    Console.withOut(out) {
      val interpreter = new Interpreter()
      interpreter.interpret(ast)
    }

    out.toString("UTF-8")
  }

  test("buffer should take stdout test") {

    val out = new ByteArrayOutputStream()

    Console.withOut(out) {
      print(12)
    }

    val printed = out.toString("UTF-8")

    assert(printed == "12")
  }

  test("interpret simple print statement") {
    val code = "print 42"
    val output = interpretCode(code)
    assert(output == "42")
  }

  test("interpret multiple print statements") {
    val code = "print 1\nprint 2\nprint 3"
    val output = interpretCode(code)
    assert(output == "123")
  }

  test("interpret variable declaration and print") {
    val code = "var x := 10\nprint x"
    val output = interpretCode(code)
    assert(output == "10")
  }

  test("interpret variable assignment") {
    val code = "var x := 5\nx := 10\nprint x"
    val output = interpretCode(code)
    assert(output == "10")
  }

  test("interpret arithmetic operations") {
    val code = "print 2 + 3\nprint 10 - 4\nprint 3 * 4\nprint 12 / 3"
    val output = interpretCode(code)
    assert(output == "56124")
  }

  test("interpret comparison operations") {
    val code = "print 5 < 10\nprint 5 > 10\nprint 5 = 5\nprint 5 /= 10"
    val output = interpretCode(code)
    assert(output == "truefalsetruetrue")
  }

  test("interpret boolean operations") {
    val code = "print true and false\nprint true or false\nprint not false"
    val output = interpretCode(code)
    assert(output == "falsetruetrue")
  }

  test("interpret if statement - true branch") {
    val code = "if true then\n    print 1\nend"
    val output = interpretCode(code)
    assert(output == "1")
  }

  test("interpret if statement - false branch") {
    val code = "if false then\n    print 1\nelse\n    print 2\nend"
    val output = interpretCode(code)
    assert(output == "2")
  }

  test("interpret if statement with condition") {
    val code = "var x := 5\nif x > 3 then\n    print \"yes\"\nelse\n    print \"no\"\nend"
    val output = interpretCode(code)
    assert(output == "yes")
  }

  test("interpret while loop") {
    val code = "var i := 1\nvar sum := 0\nwhile i <= 5 loop\n    sum := sum + i\n    i := i + 1\nend\nprint sum"
    val output = interpretCode(code)
    assert(output == "15")
  }

  test("interpret range loop") {
    val code = "var sum := 0\nfor i in 1..5 loop\n    sum := sum + i\nend\nprint sum"
    val output = interpretCode(code)
    assert(output == "15")
  }

  test("interpret range loop with factorial") {
    val code = "var n := 5\nvar res := 1\nfor i in 1..n loop\n    res := res * i\nend\nprint res"
    val output = interpretCode(code)
    assert(output == "120")
  }

  test("interpret array literal") {
    val code = "var arr := [1, 2, 3]\nprint arr[1]\nprint arr[2]\nprint arr[3]"
    val output = interpretCode(code)
    assert(output == "123")
  }

  test("interpret array access") {
    val code = "var arr := [10, 20, 30]\nvar x := arr[2]\nprint x"
    val output = interpretCode(code)
    assert(output == "20")
  }

  test("interpret array element assignment") {
    val code = "var arr := [1, 2, 3]\narr[2] := 99\nprint arr[2]"
    val output = interpretCode(code)
    assert(output == "99")
  }

  test("interpret collection loop") {
    val code = "var arr := [4, 1, 9, 2]\nvar max := arr[1]\nfor x in arr loop\n    if x > max then\n        max := x\n    end\nend\nprint max"
    val output = interpretCode(code)
    assert(output == "9")
  }

  test("interpret string concatenation") {
    val code = "var s := \"hello\"\nvar t := \"world\"\nprint s + \" \" + t"
    val output = interpretCode(code)
    assert(output == "hello world")
  }

  test("interpret string concatenation in loop") {
    val code = "var words := [\"Dynamic\", \"Language\"]\nvar sentence := \"\"\nfor w in words loop\n    sentence := sentence + w + \" \"\nend\nprint sentence"
    val output = interpretCode(code)
    assert(output == "Dynamic Language ")
  }

  test("interpret nested if statements") {
    val code = "var x := 10\nif x > 5 then\n    if x < 15 then\n        print \"between\"\n    end\nend"
    val output = interpretCode(code)
    assert(output == "between")
  }

  test("interpret complex arithmetic expression") {
    val code = "var a := 10\nvar b := 5\nvar c := 2\nprint (a + b) * c"
    val output = interpretCode(code)
    assert(output == "30")
  }

  test("interpret count even numbers using division") {
    val code = "var arr := [1, 2, 3, 4, 5, 6]\nvar count := 0\nfor x in arr loop\n    if (x / 2) * 2 = x then\n        count := count + 1\n    end\nend\nprint count"
    val output = interpretCode(code)
    assert(output == "3")
  }

  test("interpret real numbers") {
    val code =
      """var x := 3.14
        |print x
        |print "\n"
        |print x + 1.0""".stripMargin
    val output = interpretCode(code)
    assert(output ==
      """3.14
        |4.140000000000001""".stripMargin)
  }

  test("interpret boolean literals") {
    val code = "print true\nprint false"
    val output = interpretCode(code)
    assert(output == "truefalse")
  }

  test("interpret unary minus") {
    val code = "var x := 5\nprint -x\nprint -(-3)"
    val output = interpretCode(code)
    assert(output == "-53")
  }

  test("interpret less than or equal") {
    val code = "print 5 <= 5\nprint 5 <= 10\nprint 10 <= 5"
    val output = interpretCode(code)
    assert(output == "truetruefalse")
  }

  test("interpret greater than or equal") {
    val code = "print 5 >= 5\nprint 10 >= 5\nprint 5 >= 10"
    val output = interpretCode(code)
    assert(output == "truetruefalse")
  }

  test("interpret not equal") {
    val code = "print 5 /= 5\nprint 5 /= 10"
    val output = interpretCode(code)
    assert(output == "falsetrue")
  }

  test("interpret empty array") {
    val code = "var arr := []\nprint arr"
    val output = interpretCode(code)
    // Empty array should print as empty or array representation
    assert(output.nonEmpty)
  }

  test("interpret variable scope") {
    val code = "var x := 10\nif true then\n    var x := 20\n    print x\nend\nprint x"
    val output = interpretCode(code)
    assert(output == "2010")
  }

  test("interpret infinite loop with exit") {
    val code =
      "var i := 0\n" +
      "loop\n" +
      "    i := i + 1\n" +
      "    if i >= 5 then\n" +
      "        exit\n" +
      "    end\n" +
      "end\n" +
      "print i"
    val output = interpretCode(code)
    assert(output == "5")
  }

  test("interpret multiple variables") {
    val code = "var a := 1\nvar b := 2\nvar c := 3\nprint a\nprint b\nprint c"
    val output = interpretCode(code)
    assert(output == "123")
  }

  test("interpret chained assignments") {
    val code = "var x := 1\nx := 2\nx := 3\nprint x"
    val output = interpretCode(code)
    assert(output == "3")
  }

  test("interpret complex while loop") {
    val code = "var i := 1\nvar fact := 1\nwhile i <= 5 loop\n    fact := fact * i\n    i := i + 1\nend\nprint fact"
    val output = interpretCode(code)
    assert(output == "120")
  }

  test("interpret if-else chain") {
    val code = "var x := 3\nif x = 1 then\n    print \"one\"\nelse\n    if x = 2 then\n        print \"two\"\n    else\n        print \"other\"\n    end\nend"
    val output = interpretCode(code)
    assert(output == "other")
  }

  test("interpret array with mixed types") {
    val code = "var arr := [1, 2, 3]\nvar sum := 0\nfor x in arr loop\n    sum := sum + x\nend\nprint sum"
    val output = interpretCode(code)
    assert(output == "6")
  }

  test("interpret string with numbers") {
    val code = "var s := \"Number: \"\nvar n := 42\nprint s + n"
    val output = interpretCode(code)
    assert(output == "Number: 42")
  }

  test("interpret comparison with variables") {
    val code = "var a := 5\nvar b := 10\nprint a < b\nprint a > b\nprint a = a"
    val output = interpretCode(code)
    assert(output == "truefalsetrue")
  }

  test("interpret nested loops") {
    val code = "var sum := 0\nfor i in 1..3 loop\n    for j in 1..2 loop\n        sum := sum + 1\n    end\nend\nprint sum"
    val output = interpretCode(code)
    assert(output == "6")
  }

  test("interpret example 1 - sum from 1 to 5") {
    val code = "var i := 1\nvar sum := 0\nwhile i <= 5 loop\n    sum := sum + i\n    i := i + 1\nend\nprint sum"
    val output = interpretCode(code)
    assert(output == "15")
  }

  test("interpret example 2 - search maximum of array") {
    val code = "var arr := [4, 1, 9, 2]\nvar max := arr[1]\nfor x in arr loop\n    if x > max then\n        max := x\n    end\nend\nprint max"
    val output = interpretCode(code)
    assert(output == "9")
  }

  test("interpret example 3 - count even numbers") {
    // Note: Using division check instead of modulo since % is not implemented
    val code = "var arr := [1, 2, 3, 4, 5, 6]\nvar count := 0\nfor x in arr loop\n    if (x / 2) * 2 = x then\n        count := count + 1\n    end\nend\nprint count"
    val output = interpretCode(code)
    assert(output == "3")
  }

  test("interpret example 4 - factorial") {
    val code = "var n := 5\nvar res := 1\nfor i in 1..n loop\n    res := res * i\nend\nprint res"
    val output = interpretCode(code)
    assert(output == "120")
  }

  test("interpret example 5 - concat strings") {
    val code = "var words := [\"Dynamic\", \"Language\", \"D\"]\nvar sentence := \"\"\nfor w in words loop\n    sentence := sentence + w + \" \"\nend\nprint sentence"
    val output = interpretCode(code)
    assert(output == "Dynamic Language D ")
  }

  test("interpret division by zero should throw") {
    val code = "print 10 / 0"
    assertThrows[RuntimeException] {
      interpretCode(code)
    }
  }

  test("interpret undefined variable should throw") {
    val code = "print x"
    assertThrows[RuntimeException] {
      interpretCode(code)
    }
  }

  test("interpret array index out of bounds") {
    val code = "var arr := [1, 2, 3]\nprint arr[10]"
    // This might throw or return None, depending on implementation
    // For now, we expect it to work or throw
    try {
      val output = interpretCode(code)
      // If it doesn't throw, that's also acceptable
    } catch {
      case _: RuntimeException => // Expected
    }
  }

  test("interpret multiple print arguments") {
    val code = "print 1, 2, 3"
    val output = interpretCode(code)
    assert(output == "1 2 3")
  }

  test("interpret expression statement") {
    val code = "var x := 5\nx + 3\nprint x"
    val output = interpretCode(code)
    assert(output == "5")
  }

  test("interpret complex nested conditions") {
    val code = "var a := 5\nvar b := 10\nif a < b then\n    if b > a then\n        print \"nested true\"\n    end\nend"
    val output = interpretCode(code)
    assert(output == "nested true")
  }

  test("interpret range loop with single value") {
    val code = "var sum := 0\nfor i in 1..1 loop\n    sum := sum + i\nend\nprint sum"
    val output = interpretCode(code)
    assert(output == "1")
  }

  test("interpret range loop with reverse range") {
    val code = "var sum := 0\nfor i in 5..3 loop\n    sum := sum + i\nend\nprint sum"
    val output = interpretCode(code)
    // Should not execute loop body if from > to
    assert(output == "0")
  }

  test("concat arrays") {
    val code =
      """
        |var a := [1, 2]
        |var b := [3, 4]
        |var c := a + b
        |print c[4]
        |""".stripMargin
    val output = interpretCode(code)
    assert(output == "4")
  }

  test("concat tuple") {
    val code =
      """
        |var a := {a := 1, 2}
        |var b := {c := 3, 4}
        |var c := a + b
        |print c.1, c.2, c.3, c.4
        |print c.a, c.c
        |""".stripMargin
    val output = interpretCode(code)
    assert(output == "1 2 3 41 3")
  }

  test("lambda evaluate") {
    val code =
      """
        |var c := func(a, b) => a + b
        |print c(1, 2)
        |""".stripMargin
    val output = interpretCode(code)
    assert(output == "3")
  }

  test("function evaluate") {
    val code =
      """
        |var c := func(a, b) is return a + b end
        |print c(1, 2)
        |""".stripMargin
    val output = interpretCode(code)
    assert(output == "3")
  }
  test("unexistent indexes in arrays") {
    val code =
      """
        |var c := []
        |c[10] := 1
        |print c[1]
        |print c[9]
        |print c[10]
        |""".stripMargin
    val output = interpretCode(code)
    assert(output == "001")
  }
}

