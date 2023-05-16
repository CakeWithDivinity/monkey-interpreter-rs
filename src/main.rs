use monkey_interpreter::repl;

fn main() {
    println!("Welcome to the Monkey REPL");
    println!("Type your expression below to evaluate it:");

    repl::run_repl().expect("Error happened while executing REPL");
}
