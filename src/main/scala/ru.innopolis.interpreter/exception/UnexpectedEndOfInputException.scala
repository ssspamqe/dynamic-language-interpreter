package ru.innopolis.interpreter.exception

import ru.innopolis.interpreter.lexer.Code

class UnexpectedEndOfInputException(expectedCode: Code = null, context: String = null)
  extends TokenException(
    if (expectedCode != null) {
      if (context != null && context.nonEmpty)
        s"Unexpected end of input! Expected '$expectedCode' to close $context"
      else
        s"Unexpected end of input! Expected: $expectedCode"
    } else {
      s"Unexpected end of input! While parsing $context"
    }
  ) {
}