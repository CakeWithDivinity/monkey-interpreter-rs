use std::io::{self, Write};

use crate::{lexer::Lexer, token::TokenType};

pub fn run_repl() -> io::Result<()> {
    loop {
        print!(">> ");
        std::io::stdout().flush()?;

        let mut input = String::new();

        io::stdin().read_line(&mut input)?;

        let mut lexer = Lexer::new(input);

        let mut token = lexer.next_token();

        while token.token_type != TokenType::EOF {
            println!("{:#?}", token);
            token = lexer.next_token();
        }
        println!();
    }
}
