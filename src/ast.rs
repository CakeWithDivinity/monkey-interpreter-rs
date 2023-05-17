#[derive(Debug)]
pub enum Statement {
    LetStmt(Let),
}

#[derive(Debug)]
pub struct Let {
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug)]
pub enum Expression {
    IdentifierExpr(Identifier),
}

#[derive(Debug)]
pub struct Identifier {
    pub value: String,
}

pub struct Program {
    pub statements: Vec<Statement>,
}
