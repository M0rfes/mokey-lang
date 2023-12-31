mod lib;
use std::io;

use lib::lexer::Lexer;

use crate::lib::eval;

const PROMPT: &str = ">> ";
fn main() {
    let mut user_input = String::new();
    let stdin = io::stdin();
    while let Ok(_) = stdin.read_line(&mut user_input) {
        print!("{PROMPT}");
        let lex = Lexer::new(user_input.clone());
        let mut p = lib::parser::Parser::new(lex);
        let p = p.parse_program();
        println!("{}", eval::eval_statements(&p.statement).inspect());
        println!("");
        user_input.clear();
    }
}
