package ru.innopolis.interpreter

import ru.innopolis.interpreter.lexer.{Code, Span, Token, TokensRefiner}

import scala.util.matching.Regex

class RegexLexer {

  private case class Rule(code: Code, pattern: Regex)

  private val refiner = new TokensRefiner()

  private val rules: List[Rule] = {
    val keywords = Code.values().toList
      .filter(code =>
        code.getStringRepresentation != null
          && code != Code.NEWLINE // specific regex
          && code != Code.SPACE // specific regex
      )
      .map(c => Rule(c, Regex.quote(c.getStringRepresentation).r))

    val commentRule = Rule(null, "//.*(?:\\r?\\n|$)".r)
    val newlineRule = Rule(Code.NEWLINE, "\\r?\\n".r) // todo collapse sequence
    val spaceRule = Rule(Code.SPACE, "^[ ]+".r) // collapse sequence of spaces to 1 token
    val stringRule = Rule(Code.STRING_LITERAL, "\"([^\"\\\\]|\\\\.)*\"".r)
    val identifiers = List(Rule(Code.IDENTIFIER, "[a-zA-Z_][a-zA-Z0-9_]*".r))

    val numbers = List(
      Rule(Code.INT_LITERAL, ("\\d+").r)
    )

    commentRule :: newlineRule :: spaceRule :: stringRule :: (keywords ++ numbers ++ identifiers)
  }

  def tokenize(input: String): List[Token[_]] = {
    var pos = 0
    var line = 1L
    var col = 0
    val tokens = scala.collection.mutable.ListBuffer.empty[Token[_]]

    while (pos < input.length) {
      val substring = input.substring(pos)

      // Match rules
      val bestMatchRule = rules
        .flatMap(r => r.pattern.findPrefixOf(substring).map(m => (r, m)))
        .sortBy(-_._2.length)
        .headOption

      val beginLine = line
      val beginCol = col

      bestMatchRule match {
        case Some((Rule(null, _), bestMatch)) =>
          // skip, e.g. comments
          line += 1
          col = 0
          pos += bestMatch.length

        case Some((Rule(Code.NEWLINE, _), bestMatch)) =>
          line += 1
          col = 0
          pos += bestMatch.length
          tokens += Token(Code.NEWLINE, bestMatch, Span(beginLine, beginCol, beginCol + 1))

        case Some((rule, bestMatch)) =>
          col += bestMatch.length
          pos += bestMatch.length
          tokens += Token(rule.code, bestMatch, Span(beginLine, beginCol, col))

        case None =>
          throw new RuntimeException(
            s"Lexical Error:\nUnexpected character: ${substring.head} at line $line, column $col"
          )
      }
    }

    refiner.refineTokens(tokens.toList)
  }
}
