package ru.innopolis.interpreter.lexer;

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
    RANGE(".."),

    INT("int"),
    REAL("real"),
    BOOL("bool"),
    STRING("string"),
    NONE("none"),
    FUNC("func"),

    RETURN("return"),
    PRINT("print"),

    TRUE("true"),
    FALSE("false"),
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

    ROUND_BRACKET_LEFT("("),
    ROUND_BRACKET_RIGHT(")"),
    CURLY_BRACKET_LEFT("{"),
    CURLY_BRACKET_RIGHT("}"),
    SQUARE_BRACKET_LEFT("["),
    SQUARE_BRACKET_RIGHT("]"),

    NEWLINE(null),
    IDENTIFIER(null),

    INT_LITERAL(null),
    REAL_LITERAL(null),
    STRING_LITERAL(null);

    private final String stringRepresentation;

    // todo: conflict lombok&scala
    Code(String stringRepresentation) {
        this.stringRepresentation = stringRepresentation;
    }

    public String getStringRepresentation() {
        return stringRepresentation;
    }
}
