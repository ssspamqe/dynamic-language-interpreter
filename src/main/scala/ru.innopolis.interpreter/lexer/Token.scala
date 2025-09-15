package ru.innopolis.interpreter.lexer

case class Token[T](span: Span, code: Code, value: T) {
  override def toString: String = s"$code($value)"
}

object Token {
  def apply(code: Code, rawToken: String, span: Span): Token[_] = {
    val base = (value: Any) => Token(span, code, value)

    code match {
      case Code.INT_LITERAL    => base(rawToken.toLong)
      case Code.REAL_LITERAL   => base(rawToken.toDouble)
      case Code.STRING_LITERAL => base(rawToken)
      case Code.IDENTIFIER     => base(rawToken)
      case Code.NEWLINE        => base("\\n")
      case _                   => base(rawToken)
    }
  }
}
