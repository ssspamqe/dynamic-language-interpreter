package ru.innopolis.interpreter.exception

import ru.innopolis.interpreter.lexer.{Code, Token}

class UnexpectedTokenException(actualToken: Token[_], expectedCode: Code)
  extends TokenException(f"Token $actualToken is unexpected! Expected: $expectedCode " +
    f"at ${actualToken.span.line}:${actualToken.span.begin}") {
}
