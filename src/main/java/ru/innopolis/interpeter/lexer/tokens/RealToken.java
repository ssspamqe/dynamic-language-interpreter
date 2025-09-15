package ru.innopolis.interpeter.lexer.tokens;

import ru.innopolis.interpeter.lexer.Code;

public class RealToken extends Token {
    private double value;

    public RealToken(Span span, double value) {
        super(span, Code.REAL_LITERAL, Double.toString(value));
        this.value = value;
    }
}
