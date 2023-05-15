enum TokenType {
    Illegal,
    EOF,

    IDENT,
    INT,

    ASSIGN,
    PLUS,

    COMMA,
    SEMICOLON,

    LPAREN,
    RPARENT,
    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
}

struct Token {
    token_type: TokenType,
    literal: String,
}
