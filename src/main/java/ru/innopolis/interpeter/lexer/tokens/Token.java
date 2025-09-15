package ru.innopolis.interpeter.lexer.tokens;

import ru.innopolis.interpeter.lexer.Code;

public class Token {
    private final Span span;
    private final Code code;
    private final String rawValue;

    public Token(Span span, Code code, String rawValue) {
        this.span = span;
        this.code = code;
        this.rawValue = rawValue;
    }

    public Span getSpan() {
        return span;
    }

    public Code getCode() {
        return code;
    }

    public String getRawValue() {
        return rawValue;
    }

    @Override
    public String toString() {
        return String.format("%s(%s)", code, rawValue);
    }
}
