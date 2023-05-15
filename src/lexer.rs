use crate::token::TokenType;

struct Lexer {}

impl Lexer {
    fn next_token(&self) -> TokenType {
        TokenType::Illegal
    }
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use super::*;
    use crate::token::TokenType;

    #[test]
    fn calculates_next_token() {
        let input = "=+(){},;";

        let expected = [
            TokenType::ASSIGN,
            TokenType::PLUS,
            TokenType::LPAREN,
            TokenType::RPAREN,
            TokenType::LBRACE,
            TokenType::RBRACE,
            TokenType::COMMA,
            TokenType::SEMICOLON,
            TokenType::EOF,
        ];

        let lexer = Lexer {};

        for token in expected {
            assert_eq!(token, lexer.next_token());
        }
    }
}
