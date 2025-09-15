package ru.innopolis.interpeter.lexer.tokens;

import ru.innopolis.interpeter.lexer.Code;

public class BooleanToken extends Token {

   private boolean value;

    public BooleanToken(Span span, boolean value) {
        super(span, value ? Code.TRUE : Code.FALSE, Boolean.toString(value));
        this.value = value;
    }

}
