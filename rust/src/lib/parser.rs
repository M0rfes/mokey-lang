use super::{
    ast::{
        ExpresionStatement, Expression, Identifier, IntegerLiteral, LetStatement, PrefixExpression,
        Program, ReturnStatement, Statement,
    },
    lexer::Lexer,
    token::Token,
};
use std::collections::HashMap;

type PrepixParseFn = dyn Fn(&mut Parcer) -> Box<dyn Expression>;
type InfixParseFn = dyn Fn(&mut Parcer, Box<dyn Expression>) -> Box<dyn Expression>;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Priority {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Exp {
    Identifier,
    Int,
    BANG,
    MINUS,
}

pub struct Parcer<'a> {
    l: &'a mut Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prepix_parse_fns: HashMap<Exp, Box<PrepixParseFn>>,
    infix_parse_fns: HashMap<Exp, Box<InfixParseFn>>,
}

fn parse_identifier(p: &mut Parcer) -> Box<dyn Expression> {
    Box::new(Identifier::new(p.cur_token.clone(), p.cur_token.literal()))
}

fn parse_int(p: &mut Parcer) -> Box<dyn Expression> {
    Box::new(IntegerLiteral::new(
        p.cur_token.clone(),
        p.cur_token.literal().parse::<i64>().unwrap(),
    ))
}

fn parse_prefix(p: &mut Parcer) -> Box<dyn Expression> {
    let cur_token = p.cur_token.clone();
    let litral = cur_token.literal();
    p.next_token();
    let expression = PrefixExpression::new(
        cur_token,
        litral,
        p.parse_expression(Priority::PREFIX).unwrap(),
    );
    Box::new(expression)
}

impl<'a> Parcer<'a> {
    fn new<'b>(l: &'a mut Lexer<'a>) -> Self {
        let mut p = Parcer {
            l,
            cur_token: Token::EOF,
            peek_token: Token::EOF,
            errors: Vec::new(),
            prepix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        p.next_token();
        p.next_token();
        p.register_prefix(Exp::Identifier, Box::new(parse_identifier));
        p.register_prefix(Exp::Int, Box::new(parse_int));
        p.register_prefix(Exp::BANG, Box::new(parse_prefix));
        p.register_prefix(Exp::MINUS, Box::new(parse_prefix));
        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };
        while self.cur_token != Token::EOF {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        if let Token::IDENT(ref id) = self.peek_token.clone() {
            if !self.expect_peek(Token::IDENT(id.to_string())) {
                return None;
            }
            let name = Identifier::new(self.cur_token.clone(), id.to_string());
            if !self.expect_peek(Token::ASSIGN) {
                return None;
            }
            while !self.cur_token_is(Token::SEMICOLON) {
                self.next_token();
            }
            Some(Box::new(LetStatement::new(Token::LET, Box::new(name))))
        } else {
            return None;
        }
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let statement = ReturnStatement::new(self.cur_token.clone());
        self.next_token();
        while !self.cur_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        Some(Box::new(statement))
    }

    fn peek_token_is(&self, token: Token) -> bool {
        self.peek_token == token
    }

    fn cur_token_is(&self, token: Token) -> bool {
        self.cur_token == token
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token_is(token.clone()) {
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

    fn peek_error(&mut self, token: Token) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            token, self.peek_token
        ));
    }

    fn register_prefix(&mut self, token: Exp, func: Box<PrepixParseFn>) {
        self.prepix_parse_fns.insert(token, func);
    }

    fn register_infix(&mut self, token: Exp, func: Box<InfixParseFn>) {
        self.infix_parse_fns.insert(token, func);
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        if let Some(exp) = self.parse_expression(Priority::LOWEST) {
            let statement = ExpresionStatement::new(self.cur_token.clone(), exp);
            return Some(Box::new(statement));
        }
        None
    }

    fn parse_expression(&mut self, priority: Priority) -> Option<Box<dyn Expression>> {
        let exp = match self.cur_token {
            Token::IDENT(_) => Some(Exp::Identifier),
            Token::INT(_) => Some(Exp::Int),
            _ => None,
        };
        if exp.is_none() {
            return None;
        }
        let exp = &exp.unwrap();
        let prefix = self.prepix_parse_fns.get(exp).unwrap();
        let left_exp = prefix(&mut self);
        Some(left_exp)
    }
}

#[cfg(test)]
mod test {
    use crate::lib::ast::Node;

    use super::*;

    #[test]
    fn test_let_statements() {
        let input = &String::from("let x = 5; let y=10; let foobar=838383;");

        let mut l = Lexer::new(input);
        let mut p = Parcer::new(&mut l);
        let program = p.parse_program();
        check_parser_error(&mut p);
        assert_eq!(program.statements.len(), 3);
        let expected_names = ["x", "y", "foobar"];
        for (i, statement) in program.statements.iter().enumerate() {
            let exp = expected_names[i];
            test_let_statement(statement, exp.to_string());
        }
    }
    fn test_let_statement(statement: &Box<dyn Statement>, name: String) {
        let let_statement = statement.into_let().unwrap();
        assert_eq!(let_statement.token_literal().unwrap(), "LET");
        let id = &let_statement.name;
        assert_eq!(id.token_literal().unwrap(), name);
    }

    fn check_parser_error(p: &mut Parcer) {
        let errors = p.errors();
        if errors.len() == 0 {
            return;
        }
        println!("parser has {} errors", errors.len());
        for msg in errors {
            println!("parser error: {}", msg);
        }
    }

    #[test]
    fn test_parser_error() {
        let input = &String::from("let x = 5; let y=10; let foobar 838383;");

        let mut l = Lexer::new(input);
        let mut p = Parcer::new(&mut l);
        p.parse_program();
        check_parser_error(&mut p);
    }

    #[test]
    fn test_return_statements() {
        let input = &String::from("return 5; return 10; return 993322;");
        let mut l = Lexer::new(input);
        let mut p = Parcer::new(&mut l);
        let program = p.parse_program();
        check_parser_error(&mut p);
        assert_eq!(program.statements.len(), 3);
        for statement in program.statements.iter() {
            let return_statement = statement.into_return().unwrap();
            assert_eq!(return_statement.token_literal().unwrap(), "RETURN");
        }
    }

    #[test]
    fn test_identifier() {
        let input = String::from("foobar;");
        let mut l = Lexer::new(&input);
        let mut p = Parcer::new(&mut l);
        let program = p.parse_program();
        check_parser_error(&mut p);
        assert_eq!(program.statements.len(), 1);
        let expression = program
            .statements
            .first()
            .unwrap()
            .into_expression()
            .unwrap();
        let identifier = expression.into_identifier().unwrap();
        assert_eq!(identifier.value, "foobar");
        assert_eq!(identifier.token_literal().unwrap(), "foobar");
    }

    #[test]
    fn test_int() {
        let input = String::from("5;");
        let mut l = Lexer::new(&input);
        let mut p = Parcer::new(&mut l);
        let program = p.parse_program();
        check_parser_error(&mut p);
        assert_eq!(program.statements.len(), 1);
        let expression = program
            .statements
            .first()
            .unwrap()
            .into_expression()
            .unwrap();

        let int = expression.into_int().unwrap();

        assert_eq!(int.value, 5);
        assert_eq!(int.token_literal().unwrap(), "5");
    }

    #[test]
    fn test_prefix_expression() {
        let input = String::from("!5; -15;");
        let mut l = Lexer::new(&input);
        let mut p = Parcer::new(&mut l);
        let program = p.parse_program();
        check_parser_error(&mut p);
        assert_eq!(program.statements.len(), 2);
        let tests = [("!", 5), ("-", 15)];
        for (i, statement) in program.statements.iter().enumerate() {
            let expression = statement.into_expression().unwrap();
            println!("{:?}", expression);
            let prefix = expression.into_prefix().unwrap();
            assert_eq!(prefix.operator, tests[i].0);
            assert_eq!(
                prefix
                    .right
                    .token_literal()
                    .unwrap()
                    .parse::<i64>()
                    .ok()
                    .unwrap(),
                tests[i].1
            );
        }
    }
}
