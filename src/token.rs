#[derive(Debug, PartialEq)]
pub enum TokenType {
    ILLEGAL,
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

#[derive(Debug, PartialEq)]
pub struct Token {
    token_type: TokenType,
    literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Self {
            token_type,
            literal,
        }
    }
}

pub fn resolve_ident(ident: &String) -> TokenType {
    match ident.as_str() {
        "fn" => TokenType::FUNCTION,
        "let" => TokenType::LET,
        _ => TokenType::IDENT,
    }
}

