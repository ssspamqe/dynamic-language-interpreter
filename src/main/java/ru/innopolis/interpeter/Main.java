package ru.innopolis.interpeter;

import ru.innopolis.interpeter.lexer.tokens.Token;

import java.util.LinkedList;
import java.util.List;

public class Main {
    public static void main(String[] args) {

        RegexLexer lexer = new RegexLexer();

        String code = """
            var t := {a:=1, b:=2}
            print t.c""";

        List<Token> tokens = lexer.tokenize(code);

        for (Token token : tokens) {
            System.out.printf(token.toString() + ' ' + token.getSpan() + '\n');
        }
    }
}