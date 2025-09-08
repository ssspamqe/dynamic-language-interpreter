package ru.innopolis.interpeter.lexer;

import lombok.EqualsAndHashCode;
import lombok.Value;

@Value
@EqualsAndHashCode(callSuper = true)
public class Identifier extends Token {
    String value;
}
