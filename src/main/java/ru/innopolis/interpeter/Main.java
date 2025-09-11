package ru.innopolis.interpeter;

import ru.innopolis.interpeter.lexer.tokens.Token;

import java.util.LinkedList;
import java.util.List;

public class Main {
    public static void main(String[] args) {

        RegexLexer lexer = new RegexLexer();

        String code = """
            var i := 1
            var sum := 0
            while i <= 5 loop
                sum := sum + i
                i := i + 1
            end
            print sum""";

        List<Token> tokens = lexer.tokenize(code);

        for (Token token : tokens) {
            System.out.printf(token.toString() + '\n');
        }
    }
}