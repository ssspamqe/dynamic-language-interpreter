package ru.innopolis.interpreter.exception

import ru.innopolis.interpreter.lexer.{Code, Token}

class InvalidTokenException(actualToken:Token[_], expectedCode:Code)
  extends RuntimeException(f"Token $actualToken is invalid! Expected: $expectedCode"){
}
