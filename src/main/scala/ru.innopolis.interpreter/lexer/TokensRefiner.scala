package ru.innopolis.interpreter.lexer

class TokensRefiner {

  def refineTokens(tokens: List[Token[_]]) =
    tokens
      .filter(_.code != Code.SPACE)
      .map(t => if (t.code == Code.SEMICOLON) t.copy(code = Code.NEWLINE) else t)
      .foldRight(List.empty[Token[_]]) {
        case (t, acc) if acc.headOption.exists(_.code == Code.NEWLINE) && t.code == Code.NEWLINE => acc
        case (t, acc) => t :: acc
      }

}
