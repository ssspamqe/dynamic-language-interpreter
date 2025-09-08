package ru.innopolis.interpeter.lexer;

import lombok.EqualsAndHashCode;
import lombok.Value;

@Value
@EqualsAndHashCode(callSuper = true)
public class Integer extends Token{
    long value;
}
