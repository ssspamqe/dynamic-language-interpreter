package ru.innopolis.interpeter;

import ru.innopolis.interpeter.lexer.tokens.Token;

import java.util.LinkedList;
import java.util.List;

public class Main {
    public static void main(String[] args) {

        RegexLexer lexer = new RegexLexer();

        String code = """
            var n := 5
            var res := 1;
            for i in 1..n loop
                res := res * i
            end
            print res
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