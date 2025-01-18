use std::io::{self, Write};
use lib::lexer::Lexer;
fn main(){
    loop {
        print!(">>> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        if input.trim() == "exit" {
            break;
        }
        let lexer = Lexer::new(&input);
        for token in lexer {
            println!("{:?}", token.unwrap());
        }
    }

}