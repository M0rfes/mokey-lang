use std::fmt::Debug;

use super::token::Token;

pub trait Node: Debug {
    fn token_literal(&self) -> Option<String>;
}

pub trait Expression: Node {
    // fn expression_node(&self) -> &dyn Node;
    fn into_identifier(&self) -> Option<&Identifier> {
        None
    }

    fn into_int(&self) -> Option<&IntegerLiteral> {
        None
    }

    fn into_prefix(&self) -> Option<&PrefixExpression> {
        None
    }

    fn into_infix(&self) -> Option<&InfixExpression> {
        println!("into_infix {:?}", self);
        None
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> Option<String> {
        if let Some(statement) = self.statements.first() {
            statement.token_literal()
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Box<Identifier>,
    //    value: Box<dyn Expression>,
}

pub trait Statement: Node {
    // fn statement_node(&self) -> &dyn Node;
    fn into_let(&self) -> Option<&LetStatement> {
        None
    }

    fn into_return(&self) -> Option<&ReturnStatement> {
        None
    }

    fn into_expression(&self) -> Option<&ExpresionStatement> {
        None
    }
}

impl LetStatement {
    pub fn new<'b>(
        token: Token,
        name: Box<Identifier>, /*value: Box<dyn Expression>*/
    ) -> Self {
        LetStatement {
            token,
            name, /*value*/
        }
    }
}

impl Statement for LetStatement {
    fn into_let(&self) -> Option<&LetStatement> {
        if self.token == Token::LET {
            Some(self)
        } else {
            None
        }
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> Option<String> {
        if self.token == Token::LET {
            Some(String::from("LET"))
        } else {
            None
        }
    }
}

// Identifier struct implementing the Expression trait
#[derive(Debug)]
pub struct Identifier {
    token: Token,
    pub value: String,
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        Identifier { token, value }
    }
}

impl Expression for Identifier {
    fn into_identifier(&self) -> Option<&Identifier> {
        if let Token::IDENT(_) = self.token {
            Some(self)
        } else {
            None
        }
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> Option<String> {
        if let Token::IDENT(ref id) = self.token {
            Some(id.to_string())
        } else {
            return None;
        }
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> Self {
        IntegerLiteral { token, value }
    }
}

impl Expression for IntegerLiteral {
    fn into_int(&self) -> Option<&IntegerLiteral> {
        if let Token::INT(_) = self.token {
            Some(self)
        } else {
            None
        }
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> Option<String> {
        if let Token::INT(ref id) = self.token {
            Some(id.to_string())
        } else {
            return None;
        }
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    token: Token,
    //    return_value: Box<dyn Expression>,
}

impl ReturnStatement {
    pub fn new(token: Token) -> Self {
        ReturnStatement { token }
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> Option<String> {
        if self.token == Token::RETURN {
            Some(String::from("RETURN"))
        } else {
            None
        }
    }
}

impl Statement for ReturnStatement {
    fn into_return(&self) -> Option<&ReturnStatement> {
        if self.token == Token::RETURN {
            Some(self)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct ExpresionStatement {
    pub token: Token,
    pub expression: Box<dyn Expression>,
}

impl ExpresionStatement {
    pub fn new(token: Token, expression: Box<dyn Expression>) -> Self {
        ExpresionStatement { token, expression }
    }
}

impl Node for ExpresionStatement {
    fn token_literal(&self) -> Option<String> {
        Some(format!("{:?}", self.token))
    }
}

impl Statement for ExpresionStatement {
    fn into_expression(&self) -> Option<&ExpresionStatement> {
        match self.token {
            Token::IDENT(_) => Some(self),
            Token::INT(_) => Some(self),
            _ => None,
        }
    }
}

impl Expression for ExpresionStatement {
    fn into_identifier(&self) -> Option<&Identifier> {
        if let Token::IDENT(_) = self.token {
            self.expression.into_identifier()
        } else {
            None
        }
    }

    fn into_int(&self) -> Option<&IntegerLiteral> {
        if let Token::INT(_) = self.token {
            self.expression.into_int()
        } else {
            None
        }
    }

    fn into_prefix(&self) -> Option<&PrefixExpression> {
        println!("into_prefix {:?}", self);
        self.expression.into_prefix()
    }

    fn into_infix(&self) -> Option<&InfixExpression> {
        self.expression.into_infix()
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String, right: Box<dyn Expression>) -> Self {
        PrefixExpression {
            token,
            operator,
            right,
        }
    }
}

impl Expression for PrefixExpression {
    fn into_prefix(&self) -> Option<&PrefixExpression> {
        match self.token {
            Token::BANG | Token::MINUS => Some(self),
            _ => None,
        }
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> Option<String> {
        Some(self.token.literal())
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl InfixExpression {
    pub fn new(
        token: Token,
        left: Box<dyn Expression>,
        operator: String,
        right: Box<dyn Expression>,
    ) -> InfixExpression {
        InfixExpression {
            token,
            left,
            operator,
            right,
        }
    }
}

impl Expression for InfixExpression {
    fn into_infix(&self) -> Option<&InfixExpression> {
        match self.token {
            Token::PLUS
            | Token::MINUS
            | Token::SLASH
            | Token::ASTERISK
            | Token::LT
            | Token::GT
            | Token::EQ
            | Token::NOTEQ => Some(self),
            _ => None,
        }
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> Option<String> {
        Some(self.token.literal())
    }
}
