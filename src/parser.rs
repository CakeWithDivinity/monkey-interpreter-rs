use std::any::Any;

use crate::{
    ast::{
        Expression, ExpressionStatement, Identifier, IntegerLiteral, Let, Program, Return,
        Statement,
    },
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
            _ => Ok(Statement::ExpressionStmt(self.parse_expression_statement())),
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

    fn parse_expression_statement(&mut self) -> ExpressionStatement {
        let expression = self.parse_expression(Precedence::Lowest).unwrap();

        if self.peek_token.token_type == TokenType::SEMICOLON {
            self.next_token();
        }

        ExpressionStatement { expression }
    }

    fn parse_expression(&self, precedence: Precedence) -> Option<Expression> {
        self.parse_prefix_expression(&self.current_token.token_type)
    }

    fn parse_prefix_expression(&self, token_type: &TokenType) -> Option<Expression> {
        match token_type {
            TokenType::IDENT => Some(Expression::IdentifierExpr(Identifier {
                value: self.current_token.literal.to_owned(),
            })),
            TokenType::INT => {
                let token = &self.current_token.literal;

                match token.parse::<isize>() {
                    Ok(number) => Some(Expression::IntegerLiteralExpr(IntegerLiteral {
                        value: number,
                    })),
                    // TODO: add error to parser
                    _ => None,
                }
            }
            _ => None,
        }
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

enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
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

    use crate::{
        ast::{Expression, ExpressionStatement, Statement},
        lexer::Lexer,
    };

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
    fn parses_identifier_expression() {
        let input = "foobar;".to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(0, parser.errors.len());

        assert_eq!(1, program.statements.len());

        let stmt = &program.statements[0];
        if let Statement::ExpressionStmt(ExpressionStatement {
            expression: Expression::IdentifierExpr(expr),
        }) = stmt
        {
            assert_eq!("foobar", expr.value);
        } else {
            panic!("Incorrect statement type in test. Expected: ExpressionStatement with Identifier expression. Got {:?}", stmt);
        }
    }

    #[test]
    fn parses_integer_expression() {
        let input = "5;".to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(0, parser.errors.len());

        assert_eq!(1, program.statements.len());

        let stmt = &program.statements[0];
        if let Statement::ExpressionStmt(ExpressionStatement {
            expression: Expression::IntegerLiteralExpr(expr),
        }) = stmt
        {
            assert_eq!(5, expr.value);
        } else {
            panic!("Incorrect statement type in test. Expected: ExpressionStatement with IntegerLiteralExpression. Got {:?}", stmt);
        }
    }
}
