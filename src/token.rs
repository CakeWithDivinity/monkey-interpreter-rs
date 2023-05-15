#[derive(Debug, PartialEq)]
pub enum TokenType {
    Illegal,
    EOF,

    IDENT,
    INT,

    ASSIGN,
    PLUS,

    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
}

struct Token {
    token_type: TokenType,
    literal: String,
}
