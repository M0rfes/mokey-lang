#[path = "../lib/mod.rs"]
mod lib;
use std::io;

use crate::lib::lexer::Lexer;

const PROMPT: &str = ">>";
fn main() {
    let mut user_input = String::new();
    let stdin = io::stdin();
    while let Ok(_) = stdin.read_line(&mut user_input) {
        print!("{PROMPT}");
        let lex = Lexer::new(&user_input);
        for t in lex {
            print!("{:?} ", t);
        }
        println!("");
        user_input.clear();
    }
}
