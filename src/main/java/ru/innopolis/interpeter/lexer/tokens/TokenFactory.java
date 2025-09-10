package ru.innopolis.interpeter.lexer.tokens;

import ru.innopolis.interpeter.lexer.Code;

public class TokenFactory {

    public static Token getTokenByCode(Code code, String rawToken, Span span) {
        return switch (code) {
            case INT_LITERAL -> new IntegerToken(span, Long.parseLong(rawToken));
            case REAL_LITERAL -> new RealToken(span, Double.parseDouble(rawToken));
            case STRING_LITERAL -> new StringToken(span, rawToken);
            case IDENTIFIER -> new IdentifierToken(span, rawToken);
            default -> new Token(span, code);
        };
    }
}
