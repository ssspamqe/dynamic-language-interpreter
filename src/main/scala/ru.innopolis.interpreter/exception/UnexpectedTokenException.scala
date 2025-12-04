package ru.innopolis.interpreter.exception

import ru.innopolis.interpreter.lexer.{Code, Token}

class UnexpectedTokenException(actualToken: Token[_], expectedCode: Option[Code] = None)
  extends TokenException(
    expectedCode match {
      case None => f"Token $actualToken is unexpected! At ${actualToken.span.line}:${actualToken.span.begin}"
      case null => f"Token $actualToken is unexpected! At ${actualToken.span.line}:${actualToken.span.begin}"
      case Some(expected) => f"Token $actualToken is unexpected! Expected: ${expected} at ${actualToken.span.line}:${actualToken.span.begin}"
    }) {
}
