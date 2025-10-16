package ru.innopolis.interpreter.exception

import ru.innopolis.interpreter.lexer.Token

class InvalidTokenContextException(token: Token[_]) extends TokenException(
  f"Invalid context for $token at ${token.span.line}:${token.span.begin}"
) {

}
