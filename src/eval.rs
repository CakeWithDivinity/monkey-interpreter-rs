use crate::{
    ast::{BlockStatement, Expression, If, Node, Program, Statement},
    object::Object,
};

pub fn eval(node: Node) -> Option<Object> {
    // TODO: if we change Object::Error to an actual error, we can use ? 
    // for early returns
    match node {
        Node::Expr(Expression::IntegerLiteralExpr(expr)) => Some(Object::Integer(expr.value)),
        Node::Expr(Expression::BooleanLiteralExpr(expr)) => Some(Object::Boolean(expr.value)),
        Node::Expr(Expression::PrefixExpr(expr)) => {
            let right = eval(Node::Expr(*expr.right))?;
            if right.is_error() {
                return Some(right);
            }
            
            Some(eval_prefix_expression(expr.operator, right))
        }
        Node::Expr(Expression::InfixExpr(expr)) => {
            let left = eval(Node::Expr(*expr.left_side))?;
            if left.is_error() {
                return Some(left);
            }

            let right = eval(Node::Expr(*expr.right_side))?;
            if right.is_error() {
                return Some(right);
            }

            Some(eval_infix_expression(expr.operator, left, right))
        }
        Node::Expr(Expression::IfExpr(expr)) => eval_if_expression(expr),
        Node::Program(program) => eval_program(program),
        Node::Stmt(Statement::ExpressionStmt(stmt)) => eval(Node::Expr(stmt.expression)),
        Node::Stmt(Statement::BlockStmt(stmt)) => eval_block_statement(stmt),
        Node::Stmt(Statement::ReturnStmt(stmt)) => {
            let value = eval(Node::Expr(stmt.value))?;
            if value.is_error() {
                return Some(value);
            }

            Some(Object::ReturnValue(Box::new(value)))
        }
        _ => todo!(),
    }
}

fn eval_program(program: Program) -> Option<Object> {
    let mut result = None;

    for stmt in program.statements {
        result = eval(Node::Stmt(stmt));

        if let Some(Object::ReturnValue(return_val)) = result {
            return Some(*return_val);
        }

        if let Some(Object::Error(_)) = result {
            return result;
        }
    }

    result
}

fn eval_block_statement(block: BlockStatement) -> Option<Object> {
    let mut result = None;

    for stmt in block.statements {
        result = eval(Node::Stmt(stmt));

        if let Some(Object::ReturnValue(_)) | Some(Object::Error(_)) = result {
            break;
        }
    }

    result
}

fn eval_prefix_expression(operator: String, right: Object) -> Object {
    match operator.as_str() {
        "!" => eval_bang_operator(right),
        "-" => eval_minus_prefix_operator(right),
        _ => Object::Error(format!("unknown operator: {}{}", operator, right)),
    }
}

fn eval_if_expression(expr: If) -> Option<Object> {
    let condition = eval(Node::Expr(*expr.condition))?;

    if condition.is_truthy() {
        eval(Node::Stmt(Statement::BlockStmt(expr.consequence)))
    } else if expr.alternative.is_some() {
        eval(Node::Stmt(Statement::BlockStmt(expr.alternative.unwrap())))
    } else {
        Some(Object::Null)
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> Object {
    match (left, right, operator.as_str()) {
        (Object::Integer(left), Object::Integer(right), operator) => {
            eval_integer_infix_expression(operator, left, right)
        }
        (Object::Boolean(left), Object::Boolean(right), "==") => Object::Boolean(left == right),
        (Object::Boolean(left), Object::Boolean(right), "!=") => Object::Boolean(left != right),
        (left, right, operator) => {
            if std::mem::discriminant(&left) == std::mem::discriminant(&right) {
                Object::Error(format!(
                    "unknown operator: {:?} {} {:?}",
                    left, operator, right
                ))
            } else {
                Object::Error(format!(
                    "type mismatch: {:?} {} {:?}",
                    left, operator, right
                ))
            }
        }
    }
}

fn eval_integer_infix_expression(operator: &str, left: isize, right: isize) -> Object {
    match operator {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => Object::Boolean(left < right),
        ">" => Object::Boolean(left > right),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => Object::Error(format!(
            "unknown operator: {:?} {} {:?}",
            left, operator, right
        )),
    }
}

fn eval_bang_operator(right: Object) -> Object {
    match right {
        Object::Boolean(bool) => Object::Boolean(!bool),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_prefix_operator(right: Object) -> Object {
    match right {
        Object::Integer(int) => Object::Integer(-int),
        _ => Object::Error(format!("unknown operator: -{:?}", right)),
    }
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

    #[test]
    fn evaluates_if_expressions() {
        let test_cases = [
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        for test in test_cases {
            let evaluated = test_eval(test.0);

            match test.1 {
                Some(int) => {
                    test_integer_obj(evaluated, int);
                }
                None => {
                    test_null_obj(evaluated);
                }
            }
        }
    }

    #[test]
    fn evaluates_return_statements() {
        let test_cases = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }",
                10,
            ),
        ];

        for (input, expected) in test_cases {
            let evaluated = test_eval(input);
            test_integer_obj(evaluated, expected);
        }
    }

    #[test]
    fn returns_errors() {
        let test_cases = [
            // TODO: we probably want an error type here
            // instead of literal string matching
            ("5 + true;", "type mismatch: Integer(5) + Boolean(true)"),
            ("5 + true; 5;", "type mismatch: Integer(5) + Boolean(true)"),
            ("-true", "unknown operator: -Boolean(true)"),
            (
                "true + false;",
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
            (
                "5; true + false; 5",
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }
                    return 1;
                ",
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
        ];

        for (input, expected) in test_cases {
            let evaluated = test_eval(input);

            let Object::Error(msg) = evaluated else {
                panic!("Expected Error Object. Got {:?}", evaluated);   
            };

            assert_eq!(expected, msg);
        }
    }

    fn test_null_obj(obj: Object) {
        let Object::Null = obj else {
            panic!("Expected null Object. Got {:?}", obj);
        };
    }

    fn test_integer_obj(obj: Object, expected: isize) {
        let Object::Integer(int) = obj else {
            panic!("Expected Integer Object. Got {:?}", obj);
        };

        assert_eq!(expected, int);
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

        eval(Node::Program(program)).expect("No output")
    }
}
