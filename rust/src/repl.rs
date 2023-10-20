mod lib;
use std::io;

use lib::lexer::Lexer;
const PROMPT: &str = ">> ";
fn main() {
    let mut user_input = String::new();
    let stdin = io::stdin();
    while let Ok(_) = stdin.read_line(&mut user_input) {
        print!("{PROMPT}");
        let lex = Lexer::new(user_input.clone());
        let mut p = lib::parser::Parser::new(lex);
        let p = p.parse_program();
        for stmt in p.statement {
            println!("{}", stmt.to_string());
        }
        println!("");
        user_input.clear();
    }
}
