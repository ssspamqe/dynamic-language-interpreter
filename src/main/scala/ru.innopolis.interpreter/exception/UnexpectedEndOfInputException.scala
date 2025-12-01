package ru.innopolis.interpreter.exception

import ru.innopolis.interpreter.lexer.Code

class UnexpectedEndOfInputException(expectedCode: Code, context: String = "")
  extends TokenException(
    if (context.nonEmpty)
      s"Unexpected end of input! Expected '$expectedCode' to close $context"
    else
      s"Unexpected end of input! Expected: $expectedCode"
  )

