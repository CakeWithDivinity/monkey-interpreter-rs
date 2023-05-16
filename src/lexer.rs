use crate::token::{Token, TokenType};

struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    char: u8,
}

impl Lexer {
    fn new(input: String) -> Self {
        let position = 0;
        // TODO: error handling for empty input
        let first_char = input.as_bytes()[0];

        Lexer {
            input,
            position,
            read_position: position + 1,
            char: first_char,
        }
    }

    fn next_token(&mut self) -> Token {
        let (token_type, literal) = match self.char {
            b'=' => (TokenType::ASSIGN, self.char),
            b';' => (TokenType::SEMICOLON, self.char),
            b'(' => (TokenType::LPAREN, self.char),
            b')' => (TokenType::RPAREN, self.char),
            b',' => (TokenType::COMMA, self.char),
            b'+' => (TokenType::PLUS, self.char),
            b'{' => (TokenType::LBRACE, self.char),
            b'}' => (TokenType::RBRACE, self.char),
            0 => (TokenType::EOF, 0),
            _ => (TokenType::ILLEGAL, self.char),
        };

        self.read_char();
        Token::new(token_type, char::from(literal).to_string())
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.char = 0;
        } else {
            self.char = self.input.as_bytes()[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
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
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::EOF, "\0".to_string()),
        ];

        let mut lexer = Lexer::new(input.to_string());

        for token in expected {
            assert_eq!(token, lexer.next_token());
        }
    }
}
