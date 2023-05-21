use crate::{
    ast::{
        BlockStatement, BooleanLiteral, Call, Expression, ExpressionStatement, Function,
        Identifier, If, Infix, IntegerLiteral, Let, Prefix, Program, Return, Statement,
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
        self.next_token();

        let return_val = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            _ => return Err(ParseError::ParseExpressionError),
        };

        if self.peek_token.token_type == TokenType::SEMICOLON {
            self.next_token();
        }

        Ok(Return { value: return_val })
    }

    fn parse_let_statement(&mut self) -> Result<Let, ParseError> {
        self.assert_peek_token(TokenType::IDENT)?;
        let name = Identifier {
            value: self.current_token.literal.to_owned(),
        };

        self.assert_peek_token(TokenType::ASSIGN)?;

        self.next_token();

        let value = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            _ => return Err(ParseError::ParseExpressionError),
        };

        if self.peek_token.token_type == TokenType::SEMICOLON {
            self.next_token();
        }

        // TODO: evaluate value of let expression
        return Ok(Let {
            name,
            value,
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
            TokenType::INT => self.parse_int_literal_expression(),
            TokenType::TRUE | TokenType::FALSE => self.parse_boolean_literal_expression(),
            TokenType::BANG | TokenType::MINUS => self.parse_operator_prefix_expression(),
            TokenType::LPAREN => self.parse_grouped_expression(),
            TokenType::IF => self.parse_if_expression(),
            TokenType::FUNCTION => self.parse_function_expression(),
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

    fn assert_peek_token_safely(&mut self, expected_type: TokenType) -> Option<()> {
        match self.assert_peek_token(expected_type) {
            Err(err) => {
                self.errors.push(err);
                None
            }
            _ => Some(()),
        }
    }

    fn parse_function_expression(&mut self) -> Option<Expression> {
        self.assert_peek_token_safely(TokenType::LPAREN)?;

        let params = self.parse_function_parameters()?;

        self.assert_peek_token_safely(TokenType::LBRACE)?;

        let body = self.parse_block_statement();

        Some(Expression::FunctionExpr(Function {
            body,
            parameters: params,
        }))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers = vec![];

        if self.peek_token.token_type == TokenType::RPAREN {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        identifiers.push(Identifier {
            value: self.current_token.literal.to_owned(),
        });

        while self.peek_token.token_type == TokenType::COMMA {
            self.next_token();
            self.next_token();

            identifiers.push(Identifier {
                value: self.current_token.literal.to_owned(),
            });
        }

        self.assert_peek_token_safely(TokenType::RPAREN)?;

        Some(identifiers)
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest);

        match self.peek_token.token_type {
            TokenType::RPAREN => {
                self.next_token();
                expr
            }
            _ => None,
        }
    }

    fn parse_operator_prefix_expression(&mut self) -> Option<Expression> {
        let operator = self.current_token.literal.to_string();

        self.next_token();

        Some(Expression::PrefixExpr(Prefix {
            operator,
            right: Box::new(self.parse_expression(Precedence::Prefix)?),
        }))
    }

    fn parse_int_literal_expression(&mut self) -> Option<Expression> {
        let token = &self.current_token.literal;

        match token.parse::<isize>() {
            Ok(number) => Some(Expression::IntegerLiteralExpr(IntegerLiteral {
                value: number,
            })),
            // TODO: add error to parser
            _ => None,
        }
    }

    fn parse_boolean_literal_expression(&mut self) -> Option<Expression> {
        let val = match self.current_token.token_type {
            TokenType::TRUE => true,
            TokenType::FALSE => false,
            _ => return None,
        };

        Some(Expression::BooleanLiteralExpr(BooleanLiteral {
            value: val,
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        // TODO: make this call cleaner
        if self.current_token.token_type == TokenType::LPAREN {
            return self.parse_call_expression(left);
        }

        let operator = self.current_token.literal.to_owned();
        let precedence = self.current_precedence();

        self.next_token();

        Some(Expression::InfixExpr(Infix {
            left_side: Box::new(left),
            operator,
            right_side: Box::new(self.parse_expression(precedence)?),
        }))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        Some(Expression::CallExpr(Call {
            function: Box::new(function),
            arguments: self.parse_call_arguments()?,
        }))
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Expression>> {
        let mut args = vec![];

        if self.peek_token.token_type == TokenType::RPAREN {
            self.next_token();
            return Some(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token.token_type == TokenType::COMMA {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.assert_peek_token_safely(TokenType::RPAREN)?;

        Some(args)
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        self.assert_peek_token_safely(TokenType::LPAREN)?;
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.assert_peek_token_safely(TokenType::RPAREN)?;
        self.assert_peek_token_safely(TokenType::LBRACE)?;

        let consequence = self.parse_block_statement();

        let alternative = match self.peek_token.token_type {
            TokenType::ELSE => {
                self.next_token();
                self.assert_peek_token_safely(TokenType::LBRACE)?;
                Some(self.parse_block_statement())
            }
            _ => None,
        };

        Some(Expression::IfExpr(If {
            condition: Box::new(condition),
            consequence,
            alternative,
        }))
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut statements: Vec<Statement> = vec![];

        self.next_token();

        while self.current_token.token_type != TokenType::RBRACE
            && self.current_token.token_type != TokenType::EOF
        {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => self.errors.push(err),
            };

            self.next_token()
        }

        BlockStatement { statements }
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
            TokenType::LPAREN => Self::Call,
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
    use std::{assert_eq, fmt::format};

    use crate::{
        ast::{Expression, ExpressionStatement, Prefix, Statement},
        lexer::Lexer,
    };

    use super::Parser;

    #[test]
    fn parses_let_statements() {
        let input = "
let x = 5;
let y = true;
let foobar = y;
        "
        .to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(3, program.statements.len());
        assert_eq!(0, parser.errors.len());

        let expected = [("x", "5"), ("y", "true"), ("foobar", "y")];

        for (actual, (expected_ident, expected_val)) in program.statements.iter().zip(expected) {
            match actual {
                Statement::LetStmt(statement) => {
                    assert_eq!(*expected_ident, statement.name.value);
                    assert_eq!(expected_val, format!("{}", statement.value));
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
return true;
return y;
        "
        .to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(0, parser.errors.len());
        assert_eq!(3, program.statements.len());

        let expected = ["5", "true", "y"];

        for (actual, expected) in program.statements.iter().zip(expected) {
            match actual {
                Statement::ReturnStmt(statement) => {
                    assert_eq!(expected, format!("{}", statement.value))
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

    #[test]
    fn parses_complex_infix_expression() {
        let test_cases = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            // grouped expressions:
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        for (input, expected) in test_cases {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            assert_eq!(0, parser.errors.len());

            assert_eq!(expected, format!("{}", program));
        }
    }

    fn test_integer_literal(expr: &Expression, expected_value: isize) {
        if let Expression::IntegerLiteralExpr(expr) = expr {
            assert_eq!(expected_value, expr.value);
        } else {
            panic!("Expected IntegerLiteralExpression. Got {:?}", expr);
        }
    }

    #[test]
    fn parses_boolean_expression() {
        let input = "true";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(0, parser.errors.len());
        assert_eq!(1, program.statements.len());
        assert_eq!("true", format!("{}", program));

        let stmt = program.statements.first().unwrap();

        if let Statement::ExpressionStmt(ExpressionStatement {
            expression: Expression::BooleanLiteralExpr(expr),
        }) = stmt
        {
            assert_eq!(true, expr.value);
        } else {
            panic!("Incorrect statement type in test. Expected: ExpressionStatement with InfixExpression. Got {:?}", stmt);
        }
    }

    #[test]
    fn parses_if_expression() {
        let input = "if(x < y) { x }".to_string();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(0, parser.errors.len());
        assert_eq!(1, program.statements.len());

        let stmt = program.statements.first().unwrap();
        let Statement::ExpressionStmt(stmt) = &stmt else {
            panic!("Expected ExpresionStmt. Got {:?}", stmt);
        };

        let Expression::IfExpr(expr) = &stmt.expression else {
            panic!("Expected IfExpr. Got {:?}", stmt.expression);
        };
        assert_eq!("(x < y)", format!("{}", expr.condition));
        assert_eq!(1, expr.consequence.statements.len());
        let consequence = expr.consequence.statements.first().unwrap();

        let Statement::ExpressionStmt(ExpressionStatement {
            expression: Expression::IdentifierExpr(ident),
        }) = consequence else {
            panic!(
                "Expected ExpressionStatement with IdentifierExpression. Got {:?}",
                stmt.expression
            );
        };
        assert_eq!("x", ident.value);
    }

    #[test]
    fn parses_if_else_expression() {
        let input = "if(x < y) { x } else { y }".to_string();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(0, parser.errors.len());
        assert_eq!(1, program.statements.len());

        let stmt = program.statements.first().unwrap();
        let Statement::ExpressionStmt(stmt) = &stmt else {
            panic!("Expected ExpresionStmt. Got {:?}", stmt);
        };

        let Expression::IfExpr(expr) = &stmt.expression else {
            panic!("Expected IfExpr. Got {:?}", stmt.expression);
        };
        assert_eq!("(x < y)", format!("{}", expr.condition));
        assert_eq!(1, expr.consequence.statements.len());
        let consequence = expr.consequence.statements.first().unwrap();

        let Statement::ExpressionStmt(ExpressionStatement {
            expression: Expression::IdentifierExpr(ident),
        }) = consequence else {
            panic!(
                "Expected ExpressionStatement with IdentifierExpression. Got {:?}",
                stmt.expression
            );
        };
        assert_eq!("x", ident.value);

        let Some(alt) = &expr.alternative else {
            panic!("No alternative");
        };
        assert_eq!(1, alt.statements.len());
        let alt_stmt = alt.statements.first().unwrap();

        let Statement::ExpressionStmt(ExpressionStatement {
            expression: Expression::IdentifierExpr(ident),
        }) = alt_stmt else {
            panic!(
                "Expected ExpressionStatement with IdentifierExpression. Got {:?}",
                stmt.expression
            );
        };
        assert_eq!("y", ident.value);
    }

    #[test]
    fn parses_function_literal() {
        let input = "fn(x, y) { x + y; }".to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(0, parser.errors.len());
        assert_eq!(1, program.statements.len());

        let stmt = program.statements.first().unwrap();

        let Statement::ExpressionStmt(expr) = stmt else {
            panic!("Expected ExpressionStmt. Got {:?}", stmt);
        };

        let Expression::FunctionExpr(expr) = &expr.expression else {
            panic!("Expected FunctionExpr. Got {:?}", expr);
        };

        assert_eq!(2, expr.parameters.len());
        assert_eq!("x", expr.parameters[0].value);
        assert_eq!("y", expr.parameters[1].value);

        assert_eq!(1, expr.body.statements.len());

        let stmt = expr.body.statements.first().unwrap();
        assert_eq!("(x + y)", format!("{}", stmt));
    }

    #[test]
    fn parses_function_parameters() {
        let test_cases = [
            ("fn () {};", vec![]),
            ("fn (x) {};", vec!["x"]),
            ("fn (x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for (input, expected) in test_cases {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            assert_eq!(0, parser.errors.len());

            let stmt = program.statements.first().unwrap();

            let Statement::ExpressionStmt(stmt) = stmt else {
                panic!("Expected ExpressionStmt. Got {:?}", stmt);
            };

            let Expression::FunctionExpr(func) = &stmt.expression else {
                panic!("Expected FunctionExpr. Got {:?}", stmt.expression);
            };

            assert_eq!(expected.len(), func.parameters.len());

            for (index, param) in expected.iter().enumerate() {
                assert_eq!(*param, func.parameters[index].value);
            }
        }
    }

    #[test]
    fn parses_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);".to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(0, parser.errors.len());
        assert_eq!(1, program.statements.len());

        let stmt = program.statements.first().unwrap();

        let Statement::ExpressionStmt(stmt) = stmt else {
                panic!("Expected ExpressionStmt. Got {:?}", stmt);
        };

        let Expression::CallExpr(call) = &stmt.expression else {
                panic!("Expected CallExpr. Got {:?}", stmt.expression);
        };

        assert_eq!("add", format!("{}", call.function));

        assert_eq!(3, call.arguments.len());
        assert_eq!("1", format!("{}", call.arguments[0]));
        assert_eq!("(2 * 3)", format!("{}", call.arguments[1]));
        assert_eq!("(4 + 5)", format!("{}", call.arguments[2]));
    }
}
