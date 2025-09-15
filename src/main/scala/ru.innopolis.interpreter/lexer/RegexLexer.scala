package ru.innopolis.interpreter

import ru.innopolis.interpreter.lexer.{Code, Span, Token}

import scala.util.matching.Regex

class RegexLexer {

  private case class Rule(code: Code, pattern: Regex)

  private val rules: List[Rule] = {
    val commentRule = Rule(null, ("//.*(?:\\r?\\n|$)").r)
    val newlineRule = Rule(Code.NEWLINE, ("\\r?\\n").r)
    val stringRule = Rule(Code.STRING_LITERAL, ("\"([^\"\\\\]|\\\\.)*\"").r)

    val keywords = Code.values().toList
      .filter(_.getStringRepresentation != null)
      .map(c => Rule(c, Regex.quote(c.getStringRepresentation).r))

    val numbers = List(
      Rule(Code.REAL_LITERAL, ("\\d+\\.\\d+").r),
      Rule(Code.INT_LITERAL, ("\\d+").r)
    )

    val identifiers = List(Rule(Code.IDENTIFIER, ("[a-zA-Z_][a-zA-Z0-9_]*").r))

    commentRule :: newlineRule :: stringRule :: (keywords ++ numbers ++ identifiers)
  }

  def tokenize(input: String): List[Token[_]] = {
    var pos = 0
    var line = 1L
    var col = 0
    val tokens = scala.collection.mutable.ListBuffer.empty[Token[_]]

    while (pos < input.length) {
      var substring = input.substring(pos)

      // Spaces and tabs
      val spaceMatch = "^[ \\t]+".r.findPrefixOf(substring)
      val count = spaceMatch.map(_.length).getOrElse(0)
      substring = substring.drop(count)
      pos += count
      col += count
      if (substring.isEmpty) return tokens.toList

      // Match rules
      val bestMatchRule = rules
        .flatMap(r => r.pattern.findPrefixOf(substring).map(m => (r, m)))
        .sortBy(-_._2.length)
        .headOption

      bestMatchRule match {
        case Some((rule, bestMatch)) =>
          var tokenType = rule.code

          // Skip null types (comments)
          if (tokenType == null) {
            bestMatch.foreach {
              case '\n' =>
                line += 1
                col = 0
              case _ => col += 1
            }
            pos += bestMatch.length
          } else {
            // Identifier might be a keyword
            if (tokenType == Code.IDENTIFIER) {
              tokenType = Code.values().find { c =>
                c.getStringRepresentation != null &&
                  c.getStringRepresentation == bestMatch
              }.getOrElse(Code.IDENTIFIER)
            }

            val span = Span(line, col, col + bestMatch.length)
            tokens += Token(tokenType, bestMatch, span)

            // Update position
            bestMatch.foreach {
              case '\n' =>
                line += 1
                col = 0
              case _ => col += 1
            }
            pos += bestMatch.length
          }

        case None =>
          throw new RuntimeException(s"Lexical Error:\nUnexpected character: ${substring.head} at line $line, column $col")
      }
    }

    tokens.toList
  }
}
