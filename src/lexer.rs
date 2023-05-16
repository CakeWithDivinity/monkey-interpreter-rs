use crate::token::TokenType;

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

    fn next_token(&mut self) -> TokenType {
        let current_token = match self.char {
            b'=' => TokenType::ASSIGN,
            b';' => TokenType::SEMICOLON,
            b'(' => TokenType::LPAREN,
            b')' => TokenType::RPAREN,
            b',' => TokenType::COMMA,
            b'+' => TokenType::PLUS,
            b'{' => TokenType::LBRACE,
            b'}' => TokenType::RBRACE,
            0 => TokenType::EOF,
            _ => TokenType::ILLEGAL
        };
        
        self.read_char();
        current_token
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

        let mut lexer = Lexer::new(input.to_string());

        for token in expected {
            assert_eq!(token, lexer.next_token());
        }
    }
}
