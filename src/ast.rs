use std::fmt::{Display, Pointer};

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
pub struct Prefix {
    pub operator: String,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub struct Infix {
    left_side: Box<Expression>,
}

pub struct Program {
    pub statements: Vec<Statement>,
}
