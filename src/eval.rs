use crate::{
    ast::{Expression, Node, Statement},
    object::Object,
};

pub fn eval(node: Node) -> Option<Object> {
    match node {
        Node::Expr(Expression::IntegerLiteralExpr(expr)) =>
            Some(Object::Integer(expr.value)),
        Node::Program(program) =>
            eval_statements(program.statements),
        Node::Stmt(Statement::ExpressionStmt(stmt)) =>
            eval(Node::Expr(stmt.expression)),
        _ => todo!(),
    }
}

fn eval_statements(stmts: Vec<Statement>) -> Option<Object> {
    let mut result = None;

    for stmt in stmts {
        result = eval(Node::Stmt(stmt))
    }

    result
}

#[cfg(test)]
mod tests {
    use crate::{ast::Node, lexer::Lexer, object::Object, parser::Parser};

    use super::eval;

    #[test]
    fn evaluates_integer_expr() {
        let test_cases = [("5", 5), ("10", 10)];

        for test in test_cases {
            let evaluated = test_eval(test.0);
            test_integer_obj(evaluated, test.1);
        }
    }

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        eval(Node::Program(program)).expect("Input created no output")
    }

    fn test_integer_obj(obj: Object, expected: isize) {
        let Object::Integer(int) = obj else {
            panic!("Expected Integer Object. Got {:?}", obj);
        };

        assert_eq!(expected, int);
    }
}
