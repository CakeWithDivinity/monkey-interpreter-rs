use crate::{
    ast::{
        Expression, ExpressionStatement, Identifier, Infix, IntegerLiteral, Let, Prefix, Program,
        Return, Statement,
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
            _ => {
                let expr_stmt = self.parse_expression_statement();
                match expr_stmt {
                    Some(stmt) => Ok(Statement::ExpressionStmt(stmt)),
                    None => Err(ParseError::ParseExpressionError),
                }
            }
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

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token.token_type == TokenType::SEMICOLON {
            self.next_token();
        }

        Some(ExpressionStatement { expression })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut left = self.parse_prefix_expression(&self.current_token.token_type.clone())?;

        while self.peek_token.token_type != TokenType::SEMICOLON
            && precedence < self.peek_precedence()
        {
            if !self.peek_token.token_type.is_infix_parseable() {
                return Some(left);
            }

            self.next_token();

            left = self.parse_infix_expression(left)?;
        }

        Some(left)
    }

    fn parse_prefix_expression(&mut self, token_type: &TokenType) -> Option<Expression> {
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
            TokenType::BANG | TokenType::MINUS => {
                let operator = self.current_token.literal.to_string();

                self.next_token();

                Some(Expression::PrefixExpr(Prefix {
                    operator,
                    right: Box::new(self.parse_expression(Precedence::Prefix)?),
                }))
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

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let operator = self.current_token.literal.to_owned();
        let precedence = self.current_precedence();

        self.next_token();

        Some(Expression::InfixExpr(Infix {
            left_side: Box::new(left),
            operator,
            right_side: Box::new(self.parse_expression(precedence)?),
        }))
    }

    fn peek_precedence(&self) -> Precedence {
        Precedence::from(&self.peek_token.token_type)
    }

    fn current_precedence(&self) -> Precedence {
        Precedence::from(&self.current_token.token_type)
    }
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

impl From<&TokenType> for Precedence {
    fn from(token: &TokenType) -> Self {
        match token {
            TokenType::EQ | TokenType::NEQ => Self::Equals,
            TokenType::LT | TokenType::GT => Self::LessGreater,
            TokenType::PLUS | TokenType::MINUS => Self::Sum,
            TokenType::SLASH | TokenType::ASTERISK => Self::Product,
            _ => Self::Lowest,
        }
    }
}

#[derive(Debug)]
enum ParseError {
    UnexpectedToken(UnexpectedTokenError),
    // TODO: add message
    ParseExpressionError,
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

    #[test]
    fn parses_bang_prefix_expression() {
        test_prefix_expression("!5;".to_string(), "!".to_string(), 5);
    }

    #[test]
    fn parses_minus_prefix_expression() {
        test_prefix_expression("-15;".to_string(), "-".to_string(), 15);
    }

    fn test_prefix_expression(
        expression: String,
        expected_operator: String,
        expected_right_value: isize,
    ) {
        let lexer = Lexer::new(expression);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(0, parser.errors.len());

        assert_eq!(1, program.statements.len());

        let stmt = &program.statements[0];
        if let Statement::ExpressionStmt(ExpressionStatement {
            expression: Expression::PrefixExpr(expr),
        }) = stmt
        {
            assert_eq!(expected_operator, expr.operator);
            test_integer_literal(&*expr.right, expected_right_value);
        } else {
            panic!("Incorrect statement type in test. Expected: ExpressionStatement with PrefixExpression. Got {:?}", stmt);
        }
    }

    #[test]
    fn parses_infix_expressions() {
        let test_cases = [
            ("5 + 5", 5, "+", 5),
            ("5 - 5", 5, "-", 5),
            ("5 * 5", 5, "*", 5),
            ("5 / 5", 5, "/", 5),
            ("5 > 5", 5, ">", 5),
            ("5 < 5", 5, "<", 5),
            ("5 == 5", 5, "==", 5),
            ("5 != 5", 5, "!=", 5),
        ];

        for (input, left, operator, right) in test_cases {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            assert_eq!(0, parser.errors.len());
            assert_eq!(1, program.statements.len());

            let stmt = program.statements.first().unwrap();

            if let Statement::ExpressionStmt(ExpressionStatement {
                expression: Expression::InfixExpr(expr),
            }) = stmt
            {
                test_integer_literal(&*expr.left_side, left);
                assert_eq!(operator, expr.operator);
                test_integer_literal(&*expr.right_side, right);
            } else {
                panic!("Incorrect statement type in test. Expected: ExpressionStatement with InfixExpression. Got {:?}", stmt);
            }
        }
    }

    fn test_integer_literal(expr: &Expression, expected_value: isize) {
        if let Expression::IntegerLiteralExpr(expr) = expr {
            assert_eq!(expected_value, expr.value);
        } else {
            panic!("Expected IntegerLiteralExpression. Got {:?}", expr);
        }
    }
}
