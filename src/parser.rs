use crate::{
    ast::{Expression, Identifier, Let, Program, Return, Statement},
    lexer::Lexer,
    token::{Token, TokenType},
};

struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Self {
            lexer,
            current_token,
            peek_token,
            errors: vec![],
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.current_token.token_type != TokenType::EOF {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(err) => self.errors.push(err),
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match &self.current_token.token_type {
            TokenType::LET => Ok(Statement::LetStmt(self.parse_let_statement()?)),
            TokenType::RETURN => Ok(Statement::ReturnStmt(self.parse_return_statement()?)),
            token_type => Err(ParseError::UnexpectedToken(UnexpectedTokenError::new(
                TokenType::LET,
                token_type,
            ))),
        }
    }

    fn parse_return_statement(&mut self) -> Result<Return, ParseError> {
        while self.current_token.token_type != TokenType::SEMICOLON {
            self.next_token();
        }

        // TODO: evaluate expression
        Ok(Return {
            value: Expression::IdentifierExpr(Identifier {
                value: "foo".to_string(),
            }),
        })
    }

    fn parse_let_statement(&mut self) -> Result<Let, ParseError> {
        self.assert_peek_token(TokenType::IDENT)?;
        let name = Identifier {
            value: self.current_token.literal.to_owned(),
        };

        self.assert_peek_token(TokenType::ASSIGN)?;
        while self.current_token.token_type != TokenType::SEMICOLON {
            self.next_token()
        }

        // TODO: evaluate value of let expression
        return Ok(Let {
            name,
            value: Expression::IdentifierExpr(Identifier {
                value: "foo".to_string(),
            }),
        });
    }

    fn assert_peek_token(&mut self, expected_type: TokenType) -> Result<(), ParseError> {
        if self.peek_token.token_type == expected_type {
            self.next_token();
            return Ok(());
        }

        Err(ParseError::UnexpectedToken(UnexpectedTokenError::new(
            TokenType::IDENT,
            &self.peek_token.token_type,
        )))
    }
}

#[derive(Debug)]
enum ParseError {
    UnexpectedToken(UnexpectedTokenError),
}

#[derive(Debug)]
struct UnexpectedTokenError {
    expected: String,
    actual: String,
}

impl UnexpectedTokenError {
    fn new(expected: TokenType, actual: &TokenType) -> Self {
        Self {
            expected: format!("{:?}", expected),
            actual: format!("{:?}", actual),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use crate::{ast::Statement, lexer::Lexer};

    use super::Parser;

    #[test]
    fn parses_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 696969;
        "
        .to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(3, program.statements.len());
        assert_eq!(0, parser.errors.len());

        let expected = ["x", "y", "foobar"];

        for (actual, expected) in program.statements.iter().zip(expected) {
            match actual {
                Statement::LetStmt(statement) => {
                    assert_eq!(*expected, statement.name.value);
                }
                _ => panic!(
                    "Incorrect statement type in test. Expected: Let, Got: {:?}",
                    actual
                ),
            }
        }
    }

    #[test]
    fn parses_return_statements() {
        let input = "
return 5;
return 10;
return 69420;
        "
        .to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(0, parser.errors.len());
        assert_eq!(3, program.statements.len());

        let expected = ["5", "10", "69420"];

        for (actual, expected) in program.statements.iter().zip(expected) {
            match actual {
                Statement::ReturnStmt(statement) => {
                    // TODO: expression value
                }
                _ => panic!(
                    "Incorrect statement type in test. Expected: Let, Got: {:?}",
                    actual
                ),
            }
        }
    }

    #[test]
    fn reports_errors() {
        let input = "
let x 5;
        "
        .to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        parser.parse_program();

        assert_eq!(3, parser.errors.len());
    }
}
