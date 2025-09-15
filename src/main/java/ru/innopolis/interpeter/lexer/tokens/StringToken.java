package ru.innopolis.interpeter.lexer.tokens;

import lombok.Getter;
import ru.innopolis.interpeter.lexer.Code;

@Getter
public class StringToken extends Token {

    private String value;

    public StringToken(Span span, String value) {
        super(span, Code.STRING_LITERAL, value);
        this.value = value;
    }
}
