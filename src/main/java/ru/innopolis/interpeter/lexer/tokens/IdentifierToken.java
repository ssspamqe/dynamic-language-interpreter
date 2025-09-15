package ru.innopolis.interpeter.lexer.tokens;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Value;
import ru.innopolis.interpeter.lexer.Code;

@Getter
public class IdentifierToken extends Token {

    private String value;

    public IdentifierToken(Span span, String value){
        super(span, Code.IDENTIFIER, value);
        this.value = value;
    }
}
