package ru.innopolis.interpeter.lexer.tokens;

import lombok.Getter;
import ru.innopolis.interpeter.lexer.Code;

@Getter
public class IntegerToken extends Token {
    long value;

    public IntegerToken(Span span, long value) {
        super(span, Code.INT_LITERAL);
        this.value = value;
    }
}
