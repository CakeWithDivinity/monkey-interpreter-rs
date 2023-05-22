use std::io::{self, Write};

use crate::{
    lexer::Lexer,
    parser::{ParseError, Parser},
};

pub fn run_repl() -> io::Result<()> {
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

        println!("{}", program);
    }
}

fn print_parser_errors(errors: &Vec<ParseError>) {
    // TODO: make errors better
    for error in errors {
        println!("{}", error);
    }
}
