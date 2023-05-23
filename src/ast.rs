use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Statement {
    LetStmt(Let),
    ReturnStmt(Return),
    ExpressionStmt(ExpressionStatement),
    BlockStmt(BlockStatement),
}

#[derive(Debug, Clone)]
pub struct Let {
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    IdentifierExpr(Identifier),
    IntegerLiteralExpr(IntegerLiteral),
    BooleanLiteralExpr(BooleanLiteral),
    PrefixExpr(Prefix),
    InfixExpr(Infix),
    IfExpr(If),
    FunctionExpr(Function),
    CallExpr(Call),
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub value: isize,
}

#[derive(Debug, Clone)]
pub struct BooleanLiteral {
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct Prefix {
    pub operator: String,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Infix {
    pub left_side: Box<Expression>,
    pub operator: String,
    pub right_side: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

pub struct Program {
    pub statements: Vec<Statement>,
}

pub enum Node {
    Stmt(Statement),
    Expr(Expression),
    Program(Program),
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }

        Ok(())
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }

        Ok(())
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStmt(stmt) => write!(f, "let {} = {};", stmt.name, stmt.value),
            Statement::ReturnStmt(stmt) => write!(f, "return {};", stmt.value),
            Statement::ExpressionStmt(stmt) => write!(f, "{}", stmt.expression),
            Statement::BlockStmt(stmt) => write!(f, "{}", stmt),
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
            Expression::IfExpr(expr) => {
                write!(f, "if {} {}", expr.condition, expr.consequence)?;

                if let Some(alt) = &expr.alternative {
                    write!(f, "else {}", alt)?;
                }

                Ok(())
            }
            Expression::FunctionExpr(expr) => {
                write!(f, "fn (")?;

                let mut params = expr.parameters.iter().peekable();

                while let Some(param) = params.next() {
                    if params.peek().is_some() {
                        write!(f, "{}, ", param)?;
                    } else {
                        write!(f, "{}", param)?;
                    }
                }

                write!(f, ") {}", expr.body)?;
                Ok(())
            }
            Expression::CallExpr(expr) => {
                write!(f, "{} (", expr.function)?;

                let mut args = expr.arguments.iter().peekable();

                while let Some(arg) = args.next() {
                    if args.peek().is_some() {
                        write!(f, "{}, ", arg)?;
                    } else {
                        write!(f, "{})", arg)?;
                    }
                }

                Ok(())
            }
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
