use core::str;
use std::{any::Any, iter::Peekable};

use crate::{
    lexer::{Lexer, LexerError},
    token::{self, Token},
};

pub trait Node: Any {
    fn token_literal(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement: Node {}

pub trait Expression: Node {}

#[derive(Debug)]
enum ParseError {
    UnexpectedToken,
    UnexpectedEOF,
    LexerError(LexerError),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,      // For the lowest precedence (e.g., comma in expressions)
    Equals,      // ==, !=
    LessGreater, // <, >, <=, >=
    Sum,         // +, -
    Product,     // *, /, %
    Prefix,      // -a, !a (unary operators)
    Postfix,     // a++, a--, function calls like foo(), or a[0]
    Call,        // Highest precedence, typically parentheses grouping (e.g., (a + b))
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
    pub errors: Vec<ParseError>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.is_empty() {
            return String::new();
        }
        let strings: Vec<String> = self.statements.iter().map(|s| s.token_literal()).collect();
        strings.join(" ")
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

struct Identifier(String);

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.0.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Identifier {}

struct LetStatement {
    name: Identifier,
    value: Box<dyn Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        "let".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for LetStatement {}

struct ReturnStatement(Box<dyn Expression>);

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        "return".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ReturnStatement {}

struct ExpressionStatement(Box<dyn Expression>);

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.0.token_literal()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ExpressionStatement {}

struct Prefix {
    operator: Token,
    right: Box<dyn Expression>,
}

impl Node for Prefix {
    fn token_literal(&self) -> String {
        format!("({}{})", self.operator, self.right.token_literal())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

struct Postfix {
    left: Box<dyn Expression>,
    operator: Token,
}

impl Node for Postfix {
    fn token_literal(&self) -> String {
        format!("({}{})", self.left.token_literal(), self.operator)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Postfix {}

impl Expression for Prefix {}

struct Infix {
    left: Box<dyn Expression>,
    operator: Token,
    right: Box<dyn Expression>,
}

impl Node for Infix {
    fn token_literal(&self) -> String {
        format!(
            "({} {} {})",
            self.left.token_literal(),
            self.operator,
            self.right.token_literal()
        )
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Infix {}

struct Int(i64);

impl Node for Int {
    fn token_literal(&self) -> String {
        self.0.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Int {}

struct Float(f64);

impl Node for Float {
    fn token_literal(&self) -> String {
        self.0.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Float {}

struct Bool(bool);

impl Node for Bool {
    fn token_literal(&self) -> String {
        self.0.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Bool {}

struct StringLiteral(String);

impl Node for StringLiteral {
    fn token_literal(&self) -> String {
        self.0.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for StringLiteral {}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
        }
    }

    fn next(&mut self) -> Result<Token, ParseError> {
        self.lexer
            .next()
            .ok_or(ParseError::UnexpectedEOF)
            .and_then(|token| token.map_err(ParseError::LexerError))
    }

    fn peek(&mut self) -> Option<&Token> {
        let peek = self.lexer.peek();
        peek.map(|token| token.as_ref().ok()).flatten()
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
            errors: Vec::new(),
        };

        while let Some(token) = self.peek() {
            match token {
                Token::Let => match self.parse_let_statement() {
                    Ok(statement) => program.statements.push(Box::new(statement)),
                    Err(err) => program.errors.push(err),
                },
                Token::Return => match self.parse_return_statement() {
                    Ok(statement) => program.statements.push(Box::new(statement)),
                    Err(err) => program.errors.push(err),
                },
                _ => match self.parse_expression_statement() {
                    Ok(expression) => {
                        program.statements.push(Box::new(expression));
                    }
                    Err(err) => program.errors.push(err),
                },
            }
        }

        program
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, ParseError> {
        // reads the `let` token
        self.next()?;
        let name = self.parse_identifier()?;
        // reads the `=` token
        self.next()?;
        let value = self.parse_expression(Precedence::Lowest)?;
        // reads the `;` token
        let Token::Semicolon = self.next()? else {
            return Err(ParseError::UnexpectedToken);
        };
        Ok(LetStatement { name, value })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ParseError> {
        // reads the `return` token
        self.next()?;
        let value = self.parse_expression(Precedence::Lowest)?;
        // reads the `;` token
        let Token::Semicolon = self.next()? else {
            return Err(ParseError::UnexpectedToken);
        };
        Ok(ReturnStatement(value))
    }

    fn parse_identifier(&mut self) -> Result<Identifier, ParseError> {
        match self.next()? {
            Token::Ident(ident) => Ok(Identifier(ident)),
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, ParseError> {
        let expression = self.parse_expression(Precedence::Lowest)?;
        // reads the `;` token
        let Token::Semicolon = self.next()? else {
            return Err(ParseError::UnexpectedToken);
        };
        Ok(ExpressionStatement(expression))
    }

    fn parse_expression(
        &mut self,
        precedence: Precedence,
    ) -> Result<Box<dyn Expression>, ParseError> {
        let mut left = self.parse_prefix_expression()?;
        while let Some(token) = self.peek().cloned() {
            if precedence >= token.precedence() {
                break;
            }
            if token.precedence() == Precedence::Postfix {
                left = self.parse_postfix_expression(left)?;
                continue;
            }
            self.next()?;
            left = self.parse_infix_expression(left, &token)?;
        }
        Ok(left)
    }

    fn parse_prefix_expression(&mut self) -> Result<Box<dyn Expression>, ParseError> {
        match self.next()? {
            Token::Ident(ident) => Ok(Box::new(Identifier(ident))),
            Token::Int(n) => Ok(Box::new(Int(n))),
            Token::Float(n) => Ok(Box::new(Float(n))),
            Token::True => Ok(Box::new(Bool(true))),
            Token::False => Ok(Box::new(Bool(false))),
            Token::Str(s) => Ok(Box::new(StringLiteral(s))),
            Token::Sub | Token::Not | Token::Increment | Token::Decrement => {
                let operator = self.next()?;
                let right = self.parse_expression(Precedence::Prefix)?;
                Ok(Box::new(Prefix { operator, right }))
            }
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    fn parse_infix_expression(
        &mut self,
        left: Box<dyn Expression>,
        operator: &Token,
    ) -> Result<Box<dyn Expression>, ParseError> {
        let precedence = operator.precedence();
        let right = self.parse_expression(precedence)?;
        Ok(Box::new(Infix {
            left,
            operator: operator.clone(),
            right,
        }))
    }

    fn parse_postfix_expression(
        &mut self,
        left: Box<dyn Expression>,
    ) -> Result<Box<dyn Expression>, ParseError> {
        println!("parse_postfix_expression ------------> {:?}", self.lexer);
        let operator = self.next()?;
        Ok(Box::new(Postfix { left, operator }))
    }
}

impl Token {
    fn precedence(&self) -> Precedence {
        match self {
            Token::Equal | Token::NotEqual => Precedence::Equals,
            Token::Less | Token::LessEq | Token::Greater | Token::GreaterEq => {
                Precedence::LessGreater
            }
            Token::Add | Token::Sub => Precedence::Sum,
            Token::Mul | Token::Div | Token::Power => Precedence::Product,
            Token::LParen => Precedence::Call,
            Token::Increment | Token::Decrement => Precedence::Postfix,
            Token::Not => Precedence::Prefix,
            _ => Precedence::Lowest,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statement() {
        let input = "let x = 5;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);
        let statement = program.statements[0]
            .as_any()
            .downcast_ref::<LetStatement>();
        assert!(statement.is_some());
        let statement = statement.unwrap();
        assert_eq!(statement.name.0, "x");
    }

    #[test]
    fn test_postfix_expression() {
        let input = "x++;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);
        let statement = program.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>();
        assert!(statement.is_some());
        let statement = statement.unwrap();
        let postfix = statement.0.as_any().downcast_ref::<Postfix>();
        assert!(postfix.is_some());
    }
}
