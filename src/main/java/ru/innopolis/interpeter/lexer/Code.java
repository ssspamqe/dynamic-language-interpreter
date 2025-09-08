package ru.innopolis.interpeter.lexer;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum Code {
    IF("if"),
    THEN("then"),
    ELSE("else"),
    END("end"),

    LAMBDA("=>"),
    ASSIGNMENT(":="),
    DECLARATION("var"),

    WHILE("while"),
    FOR("for"),
    IN("in"),
    LOOP("loop"),
    EXIT("exit"),

    INT("int"),
    REAL("real"),
    BOOL("bool"),
    STRING("string"),
    NONE("none"),
    FUNC("func"),

    RETURN("return"),
    PRINT("print"),

    OR("or"),
    AND("and"),
    XOR("xor"),

    LESS("<"),
    LESS_OR_EQUAL("<="),
    MORE(">"),
    MORE_OR_EQUAL(">="),
    EQUAL("="),
    NOT_EQUAL("/="),

    PLUS("+"),
    MINUS("-"),
    MULTIPLICATION("*"),
    DIVISION("/"),

    DOT("."),
    COMMA(","),
    SEMICOLON(";"),

    IS("is"),
    NOT("not"),

    ROUND_BRACKET_OPEN("("),
    ROUND_BRACKET_CLOSE(")"),
    CURLY_BRACKET_OPEN("{"),
    CURLY_BRACKET_CLOSE("}"),
    SQUARE_BRACKET_OPEN("["),
    SQUARE_BRACKET_CLOSE("]");

    private final String stringRepresentation;
}
