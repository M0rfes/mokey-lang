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

impl Priority {
    fn precedence(token: &token::Token) -> Priority {
        use token::Token::*;
        match token {
            EQ => Priority::EQUALS,
            NOTEQ => Priority::EQUALS,
            LT => Priority::LESSGREATER,
            GT => Priority::LESSGREATER,
            GTEQ => Priority::LESSGREATER,
            LTEQ => Priority::LESSGREATER,
            PLUS => Priority::SUM,
            MINUS => Priority::SUM,
            SLASH => Priority::PRODUCT,
            ASTRISK => Priority::PRODUCT,
            MOD => Priority::SUM,
            LPAREN => Priority::CALL,
            _ => Priority::LOWEST,
        }
    }
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

    pub fn parse_program(&mut self) -> ast::Program {
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
            let token = mem::take(&mut self.cur_token);
            let name = ast::Identfier(peek);
            self.next_token();
            if !self.expect_peek(&token::Token::ASSIGN) {
                return None;
            }
            self.next_token();
            let Some(value) = self.parse_expression(Priority::LOWEST) else {
                return None;
            };
            Some(Box::new(ast::LetStatement { token, name, value }))
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
        let token = mem::take(&mut self.cur_token);
        self.next_token();
        let Some(value) = self.parse_expression(Priority::LOWEST) else {
            return None;
        };
        if self.peek_token_is(&token::Token::SEMICOLON) {
            self.next_token();
        }
        Some(Box::new(ast::ReturnStatement { token, value }))
    }

    fn parse_identifier(&mut self) -> Box<dyn ast::Expression> {
        Box::new(ast::Identfier(mem::take(&mut self.cur_token)))
    }

    fn parse_expression(&mut self, precedence: Priority) -> Option<Box<dyn ast::Expression>> {
        use token::Token::*;
        let mut left = match self.cur_token {
            IDET(_) => Some(self.parse_identifier()),
            INT(_) => Some(self.parse_int_litral()),
            FLOAT(_) => Some(self.parse_float_litral()),
            TRUE | FALSE => Some(self.parse_boolean_litral()),
            STRING(_) => Some(self.parse_string_litral()),
            BANG | MINUS => Some(self.parse_prefix_expresion()),
            LPAREN => self.parse_group_expression(),
            IF => self.parsr_if_expresssion(),
            FUNCTION => self.parse_function_expression(),
            _ => None,
        };
        if let Some(mut l) = left {
            while !self.peek_token_is(&token::Token::SEMICOLON)
                && precedence < self.peek_precedence()
            {
                l = match self.peek_token {
                    PLUS | MINUS | ASTRISK | SLASH | LT | GT | EQ | NOTEQ | GTEQ | LTEQ | MOD => {
                        self.next_token();
                        self.parse_infix_expression(l)
                    }
                    LPAREN => {
                        self.next_token();
                        self.pasrse_call_expression(l).unwrap()
                    }
                    _ => l,
                }
            }

            return Some(l);
        }

        None
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

    fn parse_int_litral(&mut self) -> Box<dyn ast::Expression> {
        Box::new(ast::IntegerLitral(mem::take(&mut self.cur_token)))
    }
    fn parse_float_litral(&mut self) -> Box<dyn ast::Expression> {
        Box::new(ast::FloatLitral(mem::take(&mut self.cur_token)))
    }

    fn parse_boolean_litral(&mut self) -> Box<dyn ast::Expression> {
        Box::new(ast::BooleanLitral(mem::take(&mut self.cur_token)))
    }

    fn parse_string_litral(&mut self) -> Box<dyn ast::Expression> {
        Box::new(ast::StringLitral(mem::take(&mut self.cur_token)))
    }

    fn parse_prefix_expresion(&mut self) -> Box<dyn ast::Expression> {
        let cur = mem::take(&mut self.cur_token);
        self.next_token();
        let right = self.parse_expression(Priority::PREFIX).unwrap();
        Box::new(ast::PrefixExpression { token: cur, right })
    }

    fn peek_precedence(&self) -> Priority {
        Priority::precedence(&self.peek_token)
    }

    fn cur_precedence(&self) -> Priority {
        Priority::precedence(&self.cur_token)
    }

    fn parse_infix_expression(
        &mut self,
        left: Box<dyn ast::Expression>,
    ) -> Box<dyn ast::Expression> {
        let precedence = self.cur_precedence();
        let cur = mem::take(&mut self.cur_token);
        self.next_token();
        let right = self.parse_expression(precedence).unwrap();
        Box::new(ast::InfixExpression {
            left,
            token: cur,
            right,
        })
    }

    fn parse_group_expression(&mut self) -> Option<Box<dyn ast::Expression>> {
        self.next_token();
        let exp = self.parse_expression(Priority::LOWEST);
        if !self.expect_peek(&token::Token::RPAREN) {
            return None;
        }
        exp
    }

    fn pasrse_block_statement(&mut self) -> ast::BlockStatement {
        let mut block = ast::BlockStatement {
            token: mem::take(&mut self.cur_token),
            statements: vec![],
        };
        self.next_token();

        while !self.cur_token_is(&token::Token::RBRACE) && !self.cur_token_is(&token::Token::EOF) {
            if let Some(smt) = self.parse_statement() {
                block.statements.push(smt);
            }
            self.next_token();
        }
        block
    }

    fn parsr_if_expresssion(&mut self) -> Option<Box<dyn ast::Expression>> {
        let cur = mem::take(&mut self.cur_token);
        if !self.expect_peek(&token::Token::LPAREN) {
            return None;
        }
        self.next_token();
        let Some(condition) = self.parse_expression(Priority::LOWEST) else {
            return None;
        };
        if !self.expect_peek(&token::Token::RPAREN) {
            return None;
        }
        if !self.expect_peek(&token::Token::LBRACE) {
            return None;
        }
        let consequence = self.pasrse_block_statement();
        let mut alternative = None;
        if self.peek_token_is(&token::Token::ELSE) {
            self.next_token();
            if !self.expect_peek(&token::Token::LBRACE) {
                return None;
            }
            alternative = Some(self.pasrse_block_statement());
        }
        Some(Box::new(ast::IfExpression {
            token: cur,
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_function_patameters(&mut self) -> Option<Vec<ast::Identfier>> {
        let mut identifiers = vec![];
        if self.peek_token_is(&token::Token::RPAREN) {
            self.next_token();
            return Some(identifiers);
        }
        self.next_token();
        identifiers.push(ast::Identfier(mem::take(&mut self.cur_token)));
        while self.peek_token_is(&token::Token::COMMA) {
            self.next_token();
            self.next_token();
            identifiers.push(ast::Identfier(mem::take(&mut self.cur_token)));
        }

        if !self.expect_peek(&token::Token::RPAREN) {
            return None;
        }
        Some(identifiers)
    }

    fn parse_function_expression(&mut self) -> Option<Box<dyn ast::Expression>> {
        let cur = mem::take(&mut self.cur_token);
        if !self.expect_peek(&token::Token::LPAREN) {
            return None;
        }
        if !self.expect_peek(&token::Token::LBRACE) {
            return None;
        }
        let Some(parameters) = self.parse_function_patameters() else {
            return None;
        };
        let body = self.pasrse_block_statement();
        Some(Box::new(ast::FunctioinLitral {
            token: cur,
            parameters,
            body,
        }))
    }

    fn pasrse_call_expression(
        &mut self,
        function: Box<dyn ast::Expression>,
    ) -> Option<Box<dyn ast::Expression>> {
        let token = mem::take(&mut self.cur_token);
        let Some(arguments) = self.parse_call_arguments() else {
            return None;
        };
        Some(Box::new(ast::CallExpression {
            token,
            function,
            arguments,
        }))
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Box<dyn ast::Expression>>> {
        let mut args = vec![];
        if self.peek_token_is(&token::Token::RPAREN) {
            self.next_token();
            return Some(args);
        }
        self.next_token();
        let Some(arg) = self.parse_expression(Priority::LOWEST) else {
            return None;
        };
        args.push(arg);

        while self.peek_token_is(&token::Token::COMMA) {
            self.next_token();
            self.next_token();
            let Some(arg) = self.parse_expression(Priority::LOWEST) else {
                return None;
            };
            args.push(arg);
        }

        if !self.expect_peek(&token::Token::RPAREN) {
            return None;
        }
        Some(args)
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
        let values = ["5", "10", "838383"];
        for (i, s) in program.statement.into_iter().enumerate() {
            assert_eq!(s.token_literal(), "LET");
            let let_stmt = s.into_let_statement().unwrap();
            assert_eq!(let_stmt.name.0.to_string(), tests[i]);
            assert_eq!(let_stmt.value.to_string(), values[i]);
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
            let return_stmt = s.into_return_statement().unwrap().value.to_string();
            assert_eq!(return_stmt, tests[i]);
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

    #[test]
    fn test_prefix_expression() {
        let input = "!5;-5;!5.5;-5.5;!true;!false;-true;-false;";
        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(p.errors().len(), 0);
        assert_eq!(program.statement.len(), 8);
        let test = [
            ("!", "5"),
            ("-", "5"),
            ("!", "5.5"),
            ("-", "5.5"),
            ("!", "TRUE"),
            ("!", "FALSE"),
            ("-", "TRUE"),
            ("-", "FALSE"),
        ];

        for (i, stmt) in program.statement.into_iter().enumerate() {
            let exp = stmt
                .into_expresion_statement()
                .unwrap()
                .expression
                .into_prefix_expression()
                .unwrap();
            assert_eq!(exp.token.to_string(), test[i].0);
            assert_eq!(exp.right.to_string(), test[i].1);
        }
    }

    #[test]
    fn test_infix_expression() {
        let input = "5+5;5-5;5*5;5/5;5>5;5<5;5==5;5!=5;5>=5;5<=5;5%5";
        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(p.errors().len(), 0);
        assert_eq!(program.statement.len(), 11);
        let test = [
            ("5", "+", "5"),
            ("5", "-", "5"),
            ("5", "*", "5"),
            ("5", "/", "5"),
            ("5", ">", "5"),
            ("5", "<", "5"),
            ("5", "==", "5"),
            ("5", "!=", "5"),
            ("5", ">=", "5"),
            ("5", "<=", "5"),
            ("5", "%", "5"),
        ];
        for (i, stmt) in program.statement.into_iter().enumerate() {
            let exp = stmt
                .into_expresion_statement()
                .unwrap()
                .expression
                .into_infix_expression()
                .unwrap();
            assert_eq!(exp.left.to_string(), test[i].0);
            assert_eq!(exp.token.to_string(), test[i].1);
            assert_eq!(exp.right.to_string(), test[i].2);
        }
    }

    #[test]
    fn test_oprator_precence_paring() {
        let cases = [
            ("1+(2+3)+4", "((1 + (2 + 3)) + 4)"),
            ("(5+5)*2", "((5 + 5) * 2)"),
            ("2/(5+5)", "(2 / (5 + 5))"),
            ("-(5+5)", "(-(5 + 5))"),
            ("!(true==true)", "(!(TRUE == TRUE))"),
        ];

        for case in cases {
            let l = lexer::Lexer::new(case.0.to_string());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert_eq!(p.errors().len(), 0);
            assert_eq!(program.to_string(), case.1);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if(x<y){x}";
        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(p.errors().len(), 0);
        assert_eq!(program.statement.len(), 1);
        for stmt in program.statement {
            let exp = stmt
                .into_expresion_statement()
                .unwrap()
                .expression
                .into_if_expresion()
                .unwrap();
            assert_eq!(exp.token, token::Token::IF);
            assert_eq!(exp.condition.to_string(), "(x < y)");
            assert_eq!(exp.consequence.to_string(), "x");
            assert!(exp.alternative.is_none());
        }
    }

    #[test]
    fn test_ifelse_expression() {
        let input = "if(x<y){x}else{y}";
        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(p.errors().len(), 0);
        assert_eq!(program.statement.len(), 1);
        for smt in program.statement {
            let exp = smt
                .into_expresion_statement()
                .unwrap()
                .expression
                .into_if_expresion()
                .unwrap();

            assert_eq!(exp.token, token::Token::IF);
            assert_eq!(exp.condition.to_string(), "(x < y)");
            assert_eq!(exp.consequence.to_string(), "x");
            assert!(exp.alternative.is_some());
        }
    }

    #[test]
    fn test_funtion_expression() {
        let input = "fn(x,y){x+y}";
        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(p.errors().len(), 0);
        assert_eq!(program.statement.len(), 1);
        for stmt in program.statement {
            let exp = stmt
                .into_expresion_statement()
                .unwrap()
                .expression
                .into_funtion_expression()
                .unwrap();
            assert_eq!(exp.token, token::Token::FUNCTION);
            assert_eq!(exp.parameters.len(), 2);
            assert_eq!(exp.parameters[0].to_string(), "x");
            assert_eq!(exp.parameters[1].to_string(), "y");
            assert_eq!(exp.body.to_string(), "(x + y)");
        }
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1,2*3,4+5)";
        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(p.errors().join("\n"), "");
        assert_eq!(p.errors().len(), 0);
        assert_eq!(program.statement.len(), 1);
    }
}
