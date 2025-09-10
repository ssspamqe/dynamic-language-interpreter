package ru.innopolis.interpeter.lexer.tokens;

import lombok.EqualsAndHashCode;
import lombok.Value;

@Value
@EqualsAndHashCode(callSuper = true)
public class BooleanToken {

    boolean value;

}
