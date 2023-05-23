use std::{
    io::{self, Write},
    println,
};

use crate::{
    ast::Node,
    eval::eval,
    lexer::Lexer,
    object::Environment,
    parser::{ParseError, Parser},
};

pub fn run_repl() -> io::Result<()> {
    let mut environment = Environment::new();

    loop {
        print!(">> ");
        std::io::stdout().flush()?;

        let mut input = String::new();

        io::stdin().read_line(&mut input)?;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            print_parser_errors(&parser.errors);
            continue;
        }

        let output = eval(Node::Program(program), &mut environment);

        match output {
            Some(output) => println!("{}", output),
            None => println!("**No output**"),
        };
    }
}

fn print_parser_errors(errors: &Vec<ParseError>) {
    // TODO: make errors better
    for error in errors {
        println!("{}", error);
    }
}
