package ru.innopolis.interpeter.lexer.tokens;

import ru.innopolis.interpeter.lexer.Code;

public class IdentifierToken extends Token {

    private String value;

    public IdentifierToken(Span span, String value){
        super(span, Code.IDENTIFIER, value);
        this.value = value;
    }
}
