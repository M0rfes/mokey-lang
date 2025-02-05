use lib::{ast::Parser, lexer::Lexer};
use std::{
    cell::RefCell, io::{self, Write}, rc::Rc
};
fn main() {
    let env = Rc::new(RefCell::new(lib::object::Environment::new()));
    println!("Monkey REPL");
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
        for error in &program.errors {
            eprintln!("{:?}", error);
        }
        if program.errors.len() > 0 {
            return;
        }
        let obj = lib::evaluator::eval_program(program, env.clone());
        println!("{}", obj);
    }
}
