package ru.innopolis.interpeter;

import ru.innopolis.interpeter.lexer.tokens.Token;

import java.util.LinkedList;
import java.util.List;

public class Main {
    public static void main(String[] args) {

        RegexLexer lexer = new RegexLexer();

        String code = """
            var x := 42
            print("Hello, world!")
            if x = 42 then
                x := x + 1
            end
            """;

        List<Token> tokens = lexer.tokenize(code);

        for (Token token : tokens) {
            System.out.printf("%s at line %d, col %d-%d%n",
                    token.getCode(),
                    token.getSpan().line(),
                    token.getSpan().begin(),
                    token.getSpan().end());
        }
    }
}