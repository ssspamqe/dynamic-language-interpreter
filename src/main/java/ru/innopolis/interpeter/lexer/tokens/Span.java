package ru.innopolis.interpeter.lexer.tokens;


public record Span(
        long line,
        int begin,
        int end
) {

}