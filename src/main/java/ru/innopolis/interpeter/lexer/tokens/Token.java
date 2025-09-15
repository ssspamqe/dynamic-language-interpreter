package ru.innopolis.interpeter.lexer.tokens;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import ru.innopolis.interpeter.lexer.Code;

@Getter
@AllArgsConstructor
public class Token {
    Span span;
    Code code;
    String rawValue;

    @Override
    public String toString() {
        return String.format("%s(%s)", code, rawValue);
    }
}
