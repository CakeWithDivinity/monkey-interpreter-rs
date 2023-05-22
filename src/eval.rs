use crate::{
    ast::{Expression, Node, Statement},
    object::Object,
};

pub fn eval(node: Node) -> Option<Object> {
    match node {
        Node::Expr(Expression::IntegerLiteralExpr(expr)) => Some(Object::Integer(expr.value)),
        Node::Expr(Expression::BooleanLiteralExpr(expr)) => Some(Object::Boolean(expr.value)),
        Node::Expr(Expression::PrefixExpr(expr)) => {
            let right = eval(Node::Expr(*expr.right))?;
            eval_prefix_expression(expr.operator, right)
        }
        Node::Expr(Expression::InfixExpr(expr)) => {
            let left = eval(Node::Expr(*expr.left_side))?;
            let right = eval(Node::Expr(*expr.right_side))?;

            eval_infix_expression(expr.operator, left, right)
        }
        Node::Program(program) => eval_statements(program.statements),
        Node::Stmt(Statement::ExpressionStmt(stmt)) => eval(Node::Expr(stmt.expression)),
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

fn eval_prefix_expression(operator: String, right: Object) -> Option<Object> {
    match operator.as_str() {
        "!" => Some(eval_bang_operator(right)),
        "-" => eval_minus_prefix_operator(right),
        _ => None,
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> Option<Object> {
    match (left, right, operator.as_str()) {
        (Object::Integer(left), Object::Integer(right), operator) => {
            eval_integer_infix_expression(operator, left, right)
        },
        (Object::Boolean(left), Object::Boolean(right), "==") => {
            Some(Object::Boolean(left == right))
        },
        (Object::Boolean(left), Object::Boolean(right), "!=") => {
            Some(Object::Boolean(left != right))
        },
        _ => None,
    }
}

fn eval_integer_infix_expression(operator: &str, left: isize, right: isize) -> Option<Object> {
    match operator {
        "+" => Some(Object::Integer(left + right)),
        "-" => Some(Object::Integer(left - right)),
        "*" => Some(Object::Integer(left * right)),
        "/" => Some(Object::Integer(left / right)),
        "<" => Some(Object::Boolean(left < right)),
        ">" => Some(Object::Boolean(left > right)),
        "==" => Some(Object::Boolean(left == right)),
        "!=" => Some(Object::Boolean(left != right)),
        _ => None,
    }
}

fn eval_bang_operator(right: Object) -> Object {
    match right {
        Object::Boolean(bool) => Object::Boolean(!bool),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_prefix_operator(right: Object) -> Option<Object> {
    let Object::Integer(int) = right else {
        return None;
    };

    Some(Object::Integer(-int))
}

#[cfg(test)]
mod tests {
    use crate::{ast::Node, lexer::Lexer, object::Object, parser::Parser};

    use super::eval;

    #[test]
    fn evaluates_integer_expr() {
        let test_cases = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for test in test_cases {
            let evaluated = test_eval(test.0);
            test_integer_obj(evaluated, test.1);
        }
    }

    fn test_integer_obj(obj: Object, expected: isize) {
        let Object::Integer(int) = obj else {
            panic!("Expected Integer Object. Got {:?}", obj);
        };

        assert_eq!(expected, int);
    }

    #[test]
    fn evaluates_boolean_expression() {
        let test_cases = [("true", true), ("false", false)];

        for test in test_cases {
            let evaluated = test_eval(test.0);
            test_boolean_obj(evaluated, test.1);
        }
    }

    #[test]
    fn evaluates_bang_operator() {
        let test_cases = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for test in test_cases {
            let evaluated = test_eval(test.0);
            test_boolean_obj(evaluated, test.1);
        }
    }

    fn test_boolean_obj(obj: Object, expected: bool) {
        let Object::Boolean(bool) = obj else {
            panic!("Expected Boolean Object. Got {:?}", obj);
        };

        assert_eq!(expected, bool);
    }

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        eval(Node::Program(program)).expect("Input created no output")
    }
}
