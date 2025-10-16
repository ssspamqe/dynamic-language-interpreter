package ru.innopolis.interpreter.exception

import ru.innopolis.interpreter.lexer.Token

class InvalidTokenException(token:Token[_]) extends TokenException(
  s"Invalid token: ${token.value} at ${token.span.line}:${token.span.begin}"
) {
}
