package ru.innopolis.interpeter;

import ru.innopolis.interpeter.lexer.Code;
import ru.innopolis.interpeter.lexer.tokens.Span;
import ru.innopolis.interpeter.lexer.tokens.Token;
import ru.innopolis.interpeter.lexer.tokens.TokenFactory;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexLexer {

    private static class Rule {
        final Code type;
        final Pattern pattern;

        Rule(Code type, String regex) {
            this.type = type;
            this.pattern = Pattern.compile("^(" + regex + ")");
        }
    }

    private final List<Rule> rules = new ArrayList<>();

    public RegexLexer() {
        // комментарии
        rules.add(new Rule(null, "//.*(?:\\r?\\n|$)"));
        // перенос строки
        rules.add(new Rule(Code.NEWLINE, "\\r?\\n"));
        // строковые литералы
        rules.add(new Rule(Code.STRING_LITERAL, "\"([^\"\\\\]|\\\\.)*\""));

        for (Code code : Code.values()) {
            if (code.getStringRepresentation() != null) {
                rules.add(new Rule(code, Pattern.quote(code.getStringRepresentation())));
            }
        }
        // числа
        rules.add(new Rule(Code.REAL_LITERAL, "\\d+\\.\\d+"));
        rules.add(new Rule(Code.INT_LITERAL, "\\d+"));
        // идентификаторы
        rules.add(new Rule(Code.IDENTIFIER, "[a-zA-Z_][a-zA-Z0-9_]*"));
    }

    public List<Token> tokenize(String input) {
        List<Token> tokens = new ArrayList<>();
        int pos = 0;
        long line = 1;
        int col = 0;

        while (pos < input.length()) {
            String substring = input.substring(pos);

            // пробелы (не переносы строк)
            Matcher m = Pattern.compile("^(\\s+)").matcher(substring);
            int count = 0;
            if (m.find()) {
                count = m.group(1).length(); // number of spaces
            }
            substring = substring.replaceFirst("^\\s+", "");
            pos+=count;
            col+=count;

            Rule bestRule = null;
            String bestMatch = null;

            for (Rule rule : rules) {
                Matcher matcher = rule.pattern.matcher(substring);
                if (matcher.find()) {
                    String value = matcher.group();
                    if (bestMatch == null || value.length() > bestMatch.length()) {
                        bestRule = rule;
                        bestMatch = value;
                    }
                }
            }

            if (bestMatch != null) {
                Code type = bestRule.type;

                // комментарий → пропускаем
                if (type == null) {
                    for (char c : bestMatch.toCharArray()) {
                        if (c == '\n') {
                            line++;
                            col = 0;
                        } else {
                            col++;
                        }
                    }
                    pos += bestMatch.length();
                    continue;
                }

                // идентификатор → проверка ключевых слов
                if (type == Code.IDENTIFIER) {
                    String finalBestMatch = bestMatch;
                    type = Arrays.stream(Code.values())
                            .filter(c -> c.getStringRepresentation() != null
                                    && c.getStringRepresentation().equals(finalBestMatch))
                            .findFirst()
                            .orElse(Code.IDENTIFIER);
                }

                Span span = new Span(line, col, col + bestMatch.length());

                tokens.add(
                        TokenFactory.getTokenByCode(type, bestMatch, span)
                );


                // обновляем позиции
                for (char c : bestMatch.toCharArray()) {
                    if (c == '\n') {
                        line++;
                        col = 0;
                    } else {
                        col++;
                    }
                }

                pos += bestMatch.length();
            } else {
                throw new RuntimeException("Unexpected character: " + substring.charAt(0) +
                        " at line " + line + ", column " + col);
            }
        }

        return tokens;
    }
}
