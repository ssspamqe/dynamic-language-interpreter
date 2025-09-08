package ru.innopolis.interpeter.lexer;

import lombok.EqualsAndHashCode;
import lombok.Value;

@Value
@EqualsAndHashCode(callSuper = true)
public class Real extends Token {
    double value;
}
