use std::fmt::{format, Display};

#[derive(Debug)]
pub enum Statement {
    LetStmt(Let),
    ReturnStmt(Return),
    ExpressionStmt(ExpressionStatement),
}

#[derive(Debug)]
pub struct Let {
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Return {
    pub value: Expression,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

#[derive(Debug)]
pub enum Expression {
    IdentifierExpr(Identifier),
    IntegerLiteralExpr(IntegerLiteral),
    BooleanLiteralExpr(BooleanLiteral),
    PrefixExpr(Prefix),
    InfixExpr(Infix),
}

#[derive(Debug)]
pub struct Identifier {
    pub value: String,
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub value: isize,
}

#[derive(Debug)]
pub struct BooleanLiteral {
    pub value: bool,
}

#[derive(Debug)]
pub struct Prefix {
    pub operator: String,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub struct Infix {
    pub left_side: Box<Expression>,
    pub operator: String,
    pub right_side: Box<Expression>,
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStmt(stmt) => write!(f, "let {} = {};", stmt.name, stmt.value),
            Statement::ReturnStmt(stmt) => write!(f, "return {};", stmt.value),
            Statement::ExpressionStmt(stmt) => write!(f, "{}", stmt.expression),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::IdentifierExpr(expr) => write!(f, "{}", expr),
            Expression::IntegerLiteralExpr(expr) => write!(f, "{}", expr.value),
            Expression::BooleanLiteralExpr(expr) => write!(f, "{}", expr.value),
            Expression::PrefixExpr(expr) => write!(f, "({}{})", expr.operator, expr.right),
            Expression::InfixExpr(expr) => write!(
                f,
                "({} {} {})",
                expr.left_side, expr.operator, expr.right_side
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Expression, Identifier, Let, Statement};

    #[test]
    fn displays_statement() {
        let statement = Statement::LetStmt(Let {
            name: Identifier {
                value: "myVar".to_string(),
            },
            value: Expression::IdentifierExpr(Identifier {
                value: "anotherVar".to_string(),
            }),
        });

        assert_eq!("let myVar = anotherVar;", format!("{}", statement));
    }
}
