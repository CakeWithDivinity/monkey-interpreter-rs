use crate::{
    ast::{Identifier, Let, Program, Statement, Expression},
    lexer::Lexer,
    token::{Token, TokenType},
};

struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Self {
            lexer,
            current_token,
            peek_token,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.current_token.token_type != TokenType::EOF {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token.token_type {
            TokenType::LET => Some(Statement::LetStmt(self.parse_let_statement()?)),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Let> {
        if self.peek_token.token_type != TokenType::IDENT {
            return None;
        }
        self.next_token();

        let name = Identifier {
            value: self.current_token.literal.to_owned(),
        };

        if self.peek_token.token_type != TokenType::ASSIGN {
            return None;
        }
        self.next_token();

        while self.current_token.token_type != TokenType::SEMICOLON {
            self.next_token()
        }

        // TODO: evaluate value of let expression
        return Some(Let {
            name,
            value: Expression::IdentifierExpr(Identifier {
               value: "foo".to_string(), 
            }),
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::Statement, lexer::Lexer};

    use super::Parser;

    #[test]
    fn evaluates_let_statements() {
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

        let expected = ["x", "y", "foobar"];

        for (actual, expected) in program.statements.iter().zip(expected.iter()) {
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
}
