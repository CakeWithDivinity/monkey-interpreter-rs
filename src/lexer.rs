use crate::token::{resolve_ident, Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    char: u8,
}

impl Lexer {
    pub fn new(input: String) -> Self {
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

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let literal_string = char::from(self.char).to_string();

        let (token_type, literal) = match self.char {
            b'=' => match self.peek_char() {
                b'=' => {
                    self.read_char();
                    let next_char = char::from(self.char);
                    (TokenType::EQ, format!("{}{}", literal_string, next_char))
                }
                _ => (TokenType::ASSIGN, literal_string),
            },
            b';' => (TokenType::SEMICOLON, literal_string),
            b'(' => (TokenType::LPAREN, literal_string),
            b')' => (TokenType::RPAREN, literal_string),
            b',' => (TokenType::COMMA, literal_string),
            b'+' => (TokenType::PLUS, literal_string),
            b'-' => (TokenType::MINUS, literal_string),
            b'/' => (TokenType::SLASH, literal_string),
            b'*' => (TokenType::ASTERISK, literal_string),
            b'<' => (TokenType::LT, literal_string),
            b'>' => (TokenType::GT, literal_string),
            b'!' => match self.peek_char() {
                b'=' => {
                    self.read_char();
                    let next_char = char::from(self.char);
                    (TokenType::NEQ, format!("{}{}", literal_string, next_char))
                }
                _ => (TokenType::BANG, literal_string),
            },
            b'{' => (TokenType::LBRACE, literal_string),
            b'}' => (TokenType::RBRACE, literal_string),
            b'"' => (TokenType::STRING, self.read_string()),
            0 => (TokenType::EOF, char::from(0).to_string()),
            x if is_letter(x) => {
                let ident = self.read_identifier();

                // early return, as `read_identifier` already moves the reading
                // position to the next char
                return Token::new(resolve_ident(&ident), ident);
            }
            x if is_digit(x) => {
                let number = self.read_number();

                // early return, as `read_identifier` already moves the reading
                // position to the next char
                return Token::new(TokenType::INT, number);
            }
            _ => (TokenType::ILLEGAL, literal_string),
        };

        self.read_char();
        Token::new(token_type, literal)
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

    fn read_identifier(&mut self) -> String {
        let position = self.position;

        while is_letter(self.char) {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;

        while is_digit(self.char) {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;

        self.read_char();

        while self.char != b'"' && self.char != 0 {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }

    fn skip_whitespace(&mut self) {
        while self.char == b' ' || self.char == b'\t' || self.char == b'\n' || self.char == b'\r' {
            self.read_char();
        }
    }
}

fn is_letter(char: u8) -> bool {
    (char >= b'a' && char <= b'z') || (char >= b'A' && char <= b'Z') || char == b'_'
}

fn is_digit(char: u8) -> bool {
    char >= b'0' && char <= b'9'
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use super::*;
    use crate::token::TokenType;

    #[test]
    fn calculates_next_token() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if(5 < 10) {
  return true;
} else {
  return false;
}

10 == 10;
10 != 9;
\"foobar\";
\"foo bar\";
";

        let expected = [
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::FUNCTION, "fn".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "result".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::BANG, "!".to_string()),
            Token::new(TokenType::MINUS, "-".to_string()),
            Token::new(TokenType::SLASH, "/".to_string()),
            Token::new(TokenType::ASTERISK, "*".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::LT, "<".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::GT, ">".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::IF, "if".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::LT, "<".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RETURN, "return".to_string()),
            Token::new(TokenType::TRUE, "true".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::ELSE, "else".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RETURN, "return".to_string()),
            Token::new(TokenType::FALSE, "false".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::EQ, "==".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::NEQ, "!=".to_string()),
            Token::new(TokenType::INT, "9".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::STRING, "foobar".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::STRING, "foo bar".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::EOF, "\0".to_string()),
        ];

        let mut lexer = Lexer::new(input.to_string());

        for token in expected {
            assert_eq!(token, lexer.next_token());
        }
    }
}
