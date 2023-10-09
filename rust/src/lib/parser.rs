use std::mem;

use super::ast;
use super::ast::Program;
use super::token;

use super::lexer;
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Priority {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}
#[derive(Default, Debug)]
pub struct Parser {
    l: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(l: lexer::Lexer) -> Self {
        let mut p = Parser {
            l,
            ..Default::default()
        };
        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.cur_token = mem::take(&mut self.peek_token);
        self.peek_token = self.l.next_token();
    }

    fn parse_program(&mut self) -> ast::Program {
        let mut program = Program::default();
        while self.cur_token != token::Token::EOF {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                program.statement.push(s);
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        use token::Token::*;
        match self.cur_token {
            LET => self.parse_let_statement(),
            RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        let peek = mem::take(&mut self.peek_token);
        if let token::Token::IDET(_) = peek {
            let stmt = ast::LetStatement {
                token: mem::take(&mut self.cur_token),
                name: ast::Identfier(peek),
                //value: None,
            };
            self.next_token();
            if !self.expect_peek(&token::Token::ASSIGN) {
                return None;
            }
            while !self.cur_token_is(&token::Token::SEMICOLON) {
                self.next_token();
            }
            Some(Box::new(stmt))
        } else {
            self.peek_error(&peek);
            None
        }
    }

    fn cur_token_is(&self, token: &token::Token) -> bool {
        self.cur_token == *token
    }

    fn peek_token_is(&self, token: &token::Token) -> bool {
        self.peek_token == *token
    }

    fn expect_peek(&mut self, token: &token::Token) -> bool {
        if self.peek_token_is(token) {
            self.next_token();
            true
        } else {
            self.peek_error(token);
            false
        }
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, token: &token::Token) {
        let msg = format!(
            "expected next token to be {:?}, got {:?}",
            token, self.peek_token
        );
        self.errors.push(msg);
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        let stmt = ast::ReturnStatement {
            token: mem::take(&mut self.cur_token),
            //return_value: None,
        };
        self.next_token();
        while !self.cur_token_is(&token::Token::SEMICOLON) {
            self.next_token();
        }
        Some(Box::new(stmt))
    }

    fn parse_identifier(&mut self) -> Box<dyn ast::Epression> {
        Box::new(ast::Identfier(mem::take(&mut self.cur_token)))
    }

    fn parse_expression(&mut self, precedence: Priority) -> Option<Box<dyn ast::Epression>> {
        use token::Token::*;
        let left = match self.cur_token {
            IDET(_) => Some(self.parse_identifier()),
            INT(_) => Some(self.parse_int_litral()),
            FLOAT(_) => Some(self.parse_float_litral()),
            TRUE | FALSE => Some(self.parse_boolean_litral()),
            STRING(_) => Some(self.parse_string_litral()),
            _ => None,
        };
        left
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        if let Some(exp) = self.parse_expression(Priority::LOWEST) {
            Some(Box::new(ast::ExpressionStatement {
                token: mem::take(&mut self.cur_token),
                expression: exp,
            }))
        } else {
            None
        }
    }

    fn parse_int_litral(&mut self) -> Box<dyn ast::Epression> {
        Box::new(ast::IntegerLitral(mem::take(&mut self.cur_token)))
    }
    fn parse_float_litral(&mut self) -> Box<dyn ast::Epression> {
        Box::new(ast::FloatLitral(mem::take(&mut self.cur_token)))
    }

    fn parse_boolean_litral(&mut self) -> Box<dyn ast::Epression> {
        Box::new(ast::BooleanLitral(mem::take(&mut self.cur_token)))
    }

    fn parse_string_litral(&mut self) -> Box<dyn ast::Epression> {
        Box::new(ast::StringLitral(mem::take(&mut self.cur_token)))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_let_staments() {
        let input = String::from(
            "let x = 5;
let y = 10;
let foobar = 838383; ",
        );
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(p.errors().len(), 0);
        assert_eq!(program.statement.len(), 3);
        let tests = ["x", "y", "foobar"];
        for (i, s) in program.statement.into_iter().enumerate() {
            assert_eq!(s.token_literal(), "LET");
            let let_stmt = s.into_let_statement().unwrap();
            assert_eq!(let_stmt.name.0.to_string(), tests[i]);
        }
    }

    #[test]
    fn test_error() {
        let input = String::from("let x y");
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        p.parse_program();
        assert_eq!(p.errors().len(), 1)
    }

    #[test]
    fn test_return_statement() {
        let input = String::from("return 5;return 10;return x;");
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(p.errors().len(), 0);
        assert_eq!(program.statement.len(), 3);
        let tests = ["5", "10", "x"];
        for (i, s) in program.statement.into_iter().enumerate() {
            assert_eq!(s.token_literal(), "RETURN");
            let return_stmt = s.into_return_statement().unwrap();
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(p.errors().len(), 0);
        assert_eq!(program.statement.len(), 1);
        let smt = program.statement[0].into_expresion_statement().unwrap();
        let ident = smt.expression.into_identifier().unwrap();
        assert_eq!(ident.0.to_string(), "foobar");
    }

    #[test]
    fn test_int_expression() {
        let input = "5;";
        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(p.errors().len(), 0);
        assert_eq!(program.statement.len(), 1);
        let smt = program.statement[0].into_expresion_statement().unwrap();
        let ident = smt.expression.into_int().unwrap();
        assert_eq!(ident.0.to_string(), "5");
    }

    #[test]
    fn test_float_expression() {
        let input = "5.5;";
        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(p.errors().len(), 0);
        assert_eq!(program.statement.len(), 1);
        let smt = program.statement[0].into_expresion_statement().unwrap();
        let ident = smt.expression.into_float().unwrap();
        assert_eq!(ident.0.to_string(), "5.5");
    }

    #[test]
    fn test_bool_expression() {
        let input = "true;false;";
        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(p.errors().len(), 0);
        assert_eq!(program.statement.len(), 2);
        let smt = program.statement[0].into_expresion_statement().unwrap();
        let ident = smt.expression.into_bool().unwrap();
        assert_eq!(ident.0.to_string(), "TRUE");
        let smt = program.statement[1].into_expresion_statement().unwrap();
        let ident = smt.expression.into_bool().unwrap();
        assert_eq!(ident.0.to_string(), "FALSE");
    }

    #[test]
    fn test_string_expression() {
        let input = "\"hello world\";";
        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(p.errors().len(), 0);
        assert_eq!(program.statement.len(), 1);
        let smt = program.statement[0].into_expresion_statement().unwrap();
        let ident = smt.expression.into_string().unwrap();
        assert_eq!(ident.0.to_string(), "hello world");
    }
}
