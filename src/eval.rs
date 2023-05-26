use crate::{
    ast::{BlockStatement, Expression, Identifier, If, Node, Program, Statement},
    built_in_function::BuiltInFunction,
    object::{Environment, FuncObject, Object},
};

pub fn eval(node: Node, env: &mut Environment) -> Option<Object> {
    // TODO: if we change Object::Error to an actual error, we can use ?
    // for early returns
    match node {
        Node::Expr(Expression::IntegerLiteralExpr(expr)) => Some(Object::Integer(expr.value)),
        Node::Expr(Expression::BooleanLiteralExpr(expr)) => Some(Object::Boolean(expr.value)),
        Node::Expr(Expression::StringLiteralExpr(expr)) => Some(Object::String(expr.value)),
        Node::Expr(Expression::PrefixExpr(expr)) => {
            let right = eval(Node::Expr(*expr.right), env)?;
            if right.is_error() {
                return Some(right);
            }

            Some(eval_prefix_expression(expr.operator, &right))
        }
        Node::Expr(Expression::InfixExpr(expr)) => {
            let left = eval(Node::Expr(*expr.left_side), env)?;
            if left.is_error() {
                return Some(left);
            }

            let right = eval(Node::Expr(*expr.right_side), env)?;
            if right.is_error() {
                return Some(right);
            }

            Some(eval_infix_expression(expr.operator, &left, &right))
        }
        Node::Expr(Expression::IfExpr(expr)) => eval_if_expression(expr, env),
        Node::Expr(Expression::IdentifierExpr(expr)) => Some(eval_identifier(expr, env)),
        // FIXME: recursive function calls do not work, because the environment
        // does not include the function that calls itself in it
        Node::Expr(Expression::FunctionExpr(func)) => Some(Object::Func(FuncObject {
            params: func.parameters,
            body: func.body,
            env: env.clone(),
        })),
        Node::Expr(Expression::CallExpr(expr)) => {
            let func = eval(Node::Expr(*expr.function), env)?;
            if func.is_error() {
                return Some(func);
            }

            let args = eval_expressions(expr.arguments, env)?;

            // TODO: find a way to not create a new Object here
            if let Some(Object::Error(err)) = args.first() {
                return Some(Object::Error(err.to_string()));
            }

            if let Object::Func(func) = func {
                return apply_function(func, args);
            };

            if let Object::BuiltInFunc(func) = func {
                return func.execute(args);
            }

            None
        },
        Node::Expr(Expression::ArrayLiteralExpr(arr)) => {
            todo!()
        },
        Node::Program(program) => eval_program(program, env),
        Node::Stmt(Statement::ExpressionStmt(stmt)) => eval(Node::Expr(stmt.expression), env),
        Node::Stmt(Statement::BlockStmt(stmt)) => eval_block_statement(stmt, env),
        Node::Stmt(Statement::ReturnStmt(stmt)) => {
            let value = eval(Node::Expr(stmt.value), env)?;
            if value.is_error() {
                return Some(value);
            }

            Some(Object::ReturnValue(Box::new(value.clone())))
        }
        Node::Stmt(Statement::LetStmt(stmt)) => {
            let value = eval(Node::Expr(stmt.value), env)?;

            if value.is_error() {
                return Some(value);
            }

            env.set(stmt.name.value, value.clone());
            None
        }
    }
}

fn eval_expressions(exprs: Vec<Expression>, env: &mut Environment) -> Option<Vec<Object>> {
    let mut result = vec![];

    for expr in exprs {
        let evaluated = eval(Node::Expr(expr), env)?;

        if evaluated.is_error() {
            return Some(vec![evaluated]);
        }

        result.push(evaluated);
    }

    Some(result)
}

fn apply_function(func: FuncObject, args: Vec<Object>) -> Option<Object> {
    let mut env = extend_func_env(func.env, func.params, args);
    let evaluated = eval(Node::Stmt(Statement::BlockStmt(func.body)), &mut env)?;

    Some(unwrap_return_val(evaluated))
}

fn extend_func_env(
    mut environment: Environment,
    params: Vec<Identifier>,
    args: Vec<Object>,
) -> Environment {
    for (param, arg) in params.iter().zip(args) {
        environment.set(param.value.to_owned(), arg)
    }

    environment
}

fn unwrap_return_val(obj: Object) -> Object {
    match obj {
        Object::ReturnValue(val) => *val,
        _ => obj,
    }
}

fn eval_program(program: Program, env: &mut Environment) -> Option<Object> {
    let mut result = None;

    for stmt in program.statements {
        result = eval(Node::Stmt(stmt), env);

        if let Some(Object::ReturnValue(return_val)) = result {
            return Some(*return_val);
        }

        if let Some(Object::Error(_)) = result {
            return result;
        }
    }

    result
}

fn eval_block_statement(block: BlockStatement, env: &mut Environment) -> Option<Object> {
    let mut result = None;

    for stmt in block.statements {
        result = eval(Node::Stmt(stmt), env);

        if let Some(Object::ReturnValue(_)) | Some(Object::Error(_)) = result {
            break;
        }
    }

    result
}

fn eval_prefix_expression(operator: String, right: &Object) -> Object {
    match operator.as_str() {
        "!" => eval_bang_operator(right),
        "-" => eval_minus_prefix_operator(right),
        _ => Object::Error(format!("unknown operator: {}{}", operator, right)),
    }
}

fn eval_if_expression(expr: If, env: &mut Environment) -> Option<Object> {
    let condition = eval(Node::Expr(*expr.condition), env)?;

    if condition.is_truthy() {
        eval(Node::Stmt(Statement::BlockStmt(expr.consequence)), env)
    } else if expr.alternative.is_some() {
        eval(
            Node::Stmt(Statement::BlockStmt(expr.alternative.unwrap())),
            env,
        )
    } else {
        Some(Object::Null)
    }
}

fn eval_infix_expression(operator: String, left: &Object, right: &Object) -> Object {
    match (left, right, operator.as_str()) {
        (Object::Integer(left), Object::Integer(right), operator) => {
            eval_integer_infix_expression(operator, left, right)
        }
        (Object::String(left), Object::String(right), "+") => {
            Object::String(left.to_owned() + right.as_str())
        }
        (Object::Boolean(left), Object::Boolean(right), "==") => Object::Boolean(left == right),
        (Object::Boolean(left), Object::Boolean(right), "!=") => Object::Boolean(left != right),
        (left, right, operator) => {
            if std::mem::discriminant(left) == std::mem::discriminant(right) {
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

fn eval_integer_infix_expression(operator: &str, left: &isize, right: &isize) -> Object {
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

fn eval_bang_operator(right: &Object) -> Object {
    match right {
        Object::Boolean(bool) => Object::Boolean(!bool),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_prefix_operator(right: &Object) -> Object {
    match right {
        Object::Integer(int) => Object::Integer(-int),
        _ => Object::Error(format!("unknown operator: -{:?}", right)),
    }
}

fn eval_identifier(ident: Identifier, env: &Environment) -> Object {
    let identifier = ident.value.to_owned();

    if let Some(expr) = env.get(ident.value) {
        return expr.clone();
    }

    if let Some(func) = BuiltInFunction::from_identifier(&identifier) {
        return Object::BuiltInFunc(func);
    }

    Object::Error(format!("identifier not found: {}", identifier))
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::Node,
        lexer::Lexer,
        object::{Environment, Object},
        parser::Parser,
    };

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
            ("foobar", "identifier not found: foobar"),
        ];

        for (input, expected) in test_cases {
            let evaluated = test_eval(input);

            let Object::Error(msg) = evaluated else {
                panic!("Expected Error Object. Got {:?}", evaluated);   
            };

            assert_eq!(expected, msg);
        }
    }

    #[test]
    fn evaluates_let_statements() {
        let test_cases = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in test_cases {
            let evaluated = test_eval(input);
            test_integer_obj(evaluated, expected);
        }
    }

    #[test]
    fn evaluates_function_objects() {
        let input = "fn(x) { x + 2 };";

        let evaluated = test_eval(input);

        let Object::Func(func) = evaluated else {
            panic!("Expected function obj. Got {:?}", evaluated); 
        };

        assert_eq!(1, func.params.len());
        assert_eq!("x", func.params.first().unwrap().value);
        assert_eq!("(x + 2)", format!("{}", func.body));
    }

    #[test]
    fn evalutes_func_call_expressions() {
        let test_cases = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in test_cases {
            let evaluated = test_eval(input);
            test_integer_obj(evaluated, expected);
        }
    }

    #[test]
    fn evaluates_string_literals() {
        let input = "\"Hello World\"";

        let evaluated = test_eval(input);

        let Object::String(string) = evaluated else {
            panic!("Expected string obj. Got {:?}", evaluated);
        };

        assert_eq!("Hello World", string);
    }

    #[test]
    fn evaluates_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World\"";

        let evaluated = test_eval(input);

        let Object::String(string) = evaluated else {
            panic!("Expected string obj. Got {:?}", evaluated);
        };

        assert_eq!("Hello World", string);
    }

    #[test]
    fn evaluates_built_in_functions() {
        let test_cases = [
            ("len(\"\")", 0),
            ("len(\"four\")", 4),
            ("len(\"Hello world\")", 11),
        ];

        for (input, expected) in test_cases {
            let evaluated = test_eval(input);
            test_integer_obj(evaluated, expected);
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

        let mut environment = Environment::new();

        eval(Node::Program(program), &mut environment).expect("No output")
    }
}
