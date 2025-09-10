package ru.innopolis.interpeter;

import ru.innopolis.interpeter.lexer.Code;
import ru.innopolis.interpeter.lexer.tokens.Token;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
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
        // ключевые слова и операторы из Code
        for (Code code : Code.values()) {
            if (code.getStringRepresentation() != null) {
                String regex = Pattern.quote(code.getStringRepresentation());
                rules.add(new Rule(code, regex));
            }
        }

        // Идентификаторы (отдельно, чтобы не перетирали ключевые слова)
        rules.add(new Rule(Code.IDENTIFIER, "[a-zA-Z_][a-zA-Z0-9_]*"));
        rules.add(new Rule(Code.NEWLINE, "\\r?\\n"));
        rules.add(new Rule(null, "//.*(?:\\r?\\n|$)"));
        rules.add(new Rule(Code.STRING_LITERAL, "\"([^\"\\\\]|\\\\.)*\""));

    }

    public List<Token> tokenize(String input) {
        List<Token> tokens = new ArrayList<>();
        int pos = 0;

        while (pos < input.length()) {
            String substring = input.substring(pos);

            // пробелы пропускаем (но не переносы строк)
            if (substring.matches("^ +.*")) {
                pos += substring.length() - substring.replaceFirst("^ +", "").length();
                continue;
            }

            Rule bestRule = null;
            String bestMatch = null;

            // ищем самое длинное совпадение
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

                // если идентификатор совпал с ключевым словом → переопределяем
                if (type == Code.IDENTIFIER) {
                    type = Arrays.stream(Code.values())
                            .filter(c -> c.getStringRepresentation() != null
                                    && c.getStringRepresentation().equals(bestMatch))
                            .findFirst()
                            .orElse(Code.IDENTIFIER);
                }

                tokens.add(new Token(type, bestMatch));
                pos += bestMatch.length();
            } else {
                throw new RuntimeException("Unexpected character: " + substring.charAt(0));
            }
        }

        return tokens;
    }

}