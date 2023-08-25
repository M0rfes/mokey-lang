use super::{
    ast::{
        ExpresionStatement, Expression, Identifier, InfixExpression, IntegerLiteral, LetStatement,
        PrefixExpression, Program, ReturnStatement, Statement,
    },
    lexer::Lexer,
    token::Token,
};

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

pub struct Parcer<'a> {
    l: &'a mut Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parcer<'a> {
    fn new<'b>(l: &'a mut Lexer<'a>) -> Self {
        let mut p = Parcer {
            l,
            cur_token: Token::EOF,
            peek_token: Token::EOF,
            errors: Vec::new(),
        };
        p.next_token();
        p.next_token();
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
            if !self.expect_peek(&Token::IDENT(id.to_string())) {
                return None;
            }
            let name = Identifier::new(self.cur_token.clone(), id.to_string());
            if !self.expect_peek(&Token::ASSIGN) {
                return None;
            }
            while !self.cur_token_is(&Token::SEMICOLON) {
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
        while !self.cur_token_is(&Token::SEMICOLON) {
            self.next_token();
        }
        Some(Box::new(statement))
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        self.peek_token == *token
    }

    fn cur_token_is(&self, token: &Token) -> bool {
        self.cur_token == *token
    }

    fn expect_peek(&mut self, token: &Token) -> bool {
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

    fn peek_error(&mut self, token: &Token) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            *token, self.peek_token
        ));
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        if let Some(exp) = self.parse_expression(Priority::LOWEST) {
            let statement = ExpresionStatement::new(self.cur_token.clone(), exp);
            return Some(Box::new(statement));
        }
        None
    }

    fn parse_expression(&mut self, priority: Priority) -> Option<Box<dyn Expression>> {
        let left = match self.cur_token {
            Token::IDENT(_) => Some(self.parse_identifier()),
            Token::INT(_) => Some(self.parse_int()),
            Token::BANG | Token::MINUS => Some(self.parse_prefix()),
            _ => None,
        };
        let Some(mut left) = left else {
                            return None;
                        };
        while !self.peek_token_is(&Token::SEMICOLON) && priority < self.peek_precedence() {
            let is_infix = match self.peek_token {
                Token::PLUS | Token::MINUS | Token::SLASH | Token::ASTERISK => true,
                _ => false,
            };
            if !is_infix {
                break;
            }
            self.next_token();
            left = self.parse_infix_expression(left);
            println!("left: {:?}", left);
        }
        Some(left)
    }
    fn parse_identifier(&mut self) -> Box<dyn Expression> {
        Box::new(Identifier::new(
            self.cur_token.clone(),
            self.cur_token.literal(),
        ))
    }

    fn parse_int(&mut self) -> Box<dyn Expression> {
        Box::new(IntegerLiteral::new(
            self.cur_token.clone(),
            self.cur_token.literal().parse::<i64>().unwrap(),
        ))
    }

    fn parse_prefix(&mut self) -> Box<dyn Expression> {
        let cur_token = self.cur_token.clone();
        let litral = cur_token.literal();
        self.next_token();
        let expression = PrefixExpression::new(
            cur_token,
            litral,
            self.parse_expression(Priority::PREFIX).unwrap(),
        );
        Box::new(expression)
    }

    fn peek_precedence(&self) -> Priority {
        match self.peek_token {
            Token::EQ | Token::NOTEQ => Priority::EQUALS,
            Token::LT | Token::GT => Priority::LESSGREATER,
            Token::PLUS | Token::MINUS => Priority::SUM,
            Token::SLASH | Token::ASTERISK => Priority::PRODUCT,
            _ => Priority::LOWEST,
        }
    }

    fn cur_precedence(&self) -> Priority {
        match self.cur_token {
            Token::EQ | Token::NOTEQ => Priority::EQUALS,
            Token::LT | Token::GT => Priority::LESSGREATER,
            Token::PLUS | Token::MINUS => Priority::SUM,
            Token::SLASH | Token::ASTERISK => Priority::PRODUCT,
            _ => Priority::LOWEST,
        }
    }

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Box<dyn Expression> {
        let cur_token = self.cur_token.clone();
        let litral = cur_token.literal();
        let priority = self.cur_precedence();
        self.next_token();
        let expression = InfixExpression::new(
            cur_token,
            left,
            litral,
            self.parse_expression(priority).unwrap(),
        );
        Box::new(expression)
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
            println!("expression {:?}", expression);
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

    #[test]
    fn test_infix_expression() {
        let input = [
            ("5+5".to_string(), 5, "+".to_string(), 5),
            ("5-5".to_string(), 5, "-".to_string(), 5),
            ("5/5".to_string(), 5, "/".to_string(), 5),
            ("5*5".to_string(), 5, "*".to_string(), 5),
        ];

        for (input, left, operator, right) in input.iter() {
            let mut l = Lexer::new(input);
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
            let infix = expression.into_infix().unwrap();
            assert_eq!(
                infix.left.token_literal().unwrap().parse::<i64>().unwrap(),
                *left
            );
            assert_eq!(infix.operator, *operator);
            assert_eq!(
                infix.right.token_literal().unwrap().parse::<i64>().unwrap(),
                *right
            );
        }
    }
}
