use lib::{ast::Parser, lexer::Lexer};
use std::io::{self, Write};
fn main() {
    loop {
        print!(">>> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        if input.trim() == "exit" {
            break;
        }
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        for error in program.errors {
            eprintln!("{:?}", error);
        }
        for statement in program.statements {
            println!("{}", statement);
        }
    }
}
