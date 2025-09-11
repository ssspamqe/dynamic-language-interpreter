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
        // Comments
        rules.add(new Rule(null, "//.*(?:\\r?\\n|$)"));
        // New line
        rules.add(new Rule(Code.NEWLINE, "\\r?\\n"));
        // Strings
        rules.add(new Rule(Code.STRING_LITERAL, "\"([^\"\\\\]|\\\\.)*\""));

        for (Code code : Code.values()) {
            if (code.getStringRepresentation() != null) {
                rules.add(new Rule(code, Pattern.quote(code.getStringRepresentation())));
            }
        }
        // Numbers
        rules.add(new Rule(Code.REAL_LITERAL, "\\d+\\.\\d+"));
        rules.add(new Rule(Code.INT_LITERAL, "\\d+"));
        // Ids
        rules.add(new Rule(Code.IDENTIFIER, "[a-zA-Z_][a-zA-Z0-9_]*"));
    }

    public List<Token> tokenize(String input) {
        List<Token> tokens = new ArrayList<>();
        int pos = 0;
        long line = 1;
        int col = 0;

        while (pos < input.length()) {
            String substring = input.substring(pos);

            // Spaces and tabs
            Matcher m = Pattern.compile("^[ \\t]+").matcher(substring);
            int count = 0;
            if (m.find()) {
                count = m.group().length();
            }
            substring = substring.replaceFirst("^[ \\t]+", "");
            if(substring.isEmpty()){
                break;
            }
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

                // Skip all null types except newline
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

                // Check if Id is not keyword
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


                // Update position
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
