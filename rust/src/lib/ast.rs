use std::fmt::Display;

use super::token;

pub trait Node: ToString {
    fn token_literal(&self) -> String;
    fn token(&self) -> &token::Token;
    fn into_program(&self) -> Option<&Program> {
        None
    }
    fn into_expression(&self) -> Option<&ExpressionStatement> {
        None
    }
}

pub trait Statement: Node {
    fn into_let_statement(&self) -> Option<&LetStatement> {
        None
    }

    fn into_return_statement(&self) -> Option<&ReturnStatement> {
        None
    }

    fn into_expresion_statement(&self) -> Option<&ExpressionStatement> {
        None
    }

    fn into_block_statement(&self) -> Option<&BlockStatement> {
        None
    }
}

pub trait Expression: Node {
    fn into_identifier(&self) -> Option<&Identfier> {
        None
    }

    fn into_int(&self) -> Option<&IntegerLitral> {
        None
    }

    fn into_float(&self) -> Option<&FloatLitral> {
        None
    }

    fn into_bool(&self) -> Option<&BooleanLitral> {
        None
    }

    fn into_string(&self) -> Option<&StringLitral> {
        None
    }

    fn into_prefix_expression(&self) -> Option<&PrefixExpression> {
        None
    }

    fn into_infix_expression(&self) -> Option<&InfixExpression> {
        None
    }

    fn into_if_expresion(&self) -> Option<&IfExpression> {
        None
    }

    fn into_funtion_expression(&self) -> Option<&FunctioinLitral> {
        None
    }

    fn into_call_expression(&self) -> Option<&CallExpression> {
        None
    }
}

#[derive(Default)]
pub struct Program {
    pub statement: Vec<Box<dyn Statement>>,
}

impl ToString for Program {
    fn to_string(&self) -> String {
        let mut s = String::new();
        for stmt in &self.statement {
            s = format!("{}{}", s, stmt.to_string());
        }
        s
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if !self.statement.is_empty() {
            self.statement[0].token_literal()
        } else {
            String::from("")
        }
    }

    fn token(&self) -> &token::Token {
        if !self.statement.is_empty() {
            self.statement[0].token()
        } else {
            &token::Token::EOF
        }
    }

    fn into_program(&self) -> Option<&Program> {
        Some(self)
    }
}

pub struct Identfier(pub token::Token);

impl ToString for Identfier {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

impl Node for Identfier {
    fn token_literal(&self) -> String {
        self.0.to_string()
    }

    fn token(&self) -> &token::Token {
        &self.0
    }
}

impl Expression for Identfier {
    fn into_identifier(&self) -> Option<&Identfier> {
        match self.0 {
            token::Token::IDET(_) => Some(self),
            _ => None,
        }
    }
}

pub struct LetStatement {
    pub token: token::Token,
    pub name: Identfier,
    pub value: Box<dyn Expression>,
}

impl ToString for LetStatement {
    fn to_string(&self) -> String {
        format!("let {} = {}", self.name.to_string(), self.value.to_string())
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }

    fn token(&self) -> &token::Token {
        &self.token
    }
}

impl Statement for LetStatement {
    fn into_let_statement(&self) -> Option<&LetStatement> {
        match self.token {
            token::Token::LET => Some(self),
            _ => None,
        }
    }
}

pub struct ReturnStatement {
    pub token: token::Token,
    pub value: Box<dyn Expression>,
}

impl ToString for ReturnStatement {
    fn to_string(&self) -> String {
        format!(
            "{return} {value}",
            return = self.token_literal(),
            value = self.value.to_string()
        )
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }

    fn token(&self) -> &token::Token {
        &self.token
    }
}

impl Statement for ReturnStatement {
    fn into_return_statement(&self) -> Option<&ReturnStatement> {
        match self.token {
            token::Token::RETURN => Some(self),
            _ => None,
        }
    }
}

pub struct ExpressionStatement {
    pub token: token::Token,
    pub expression: Box<dyn Expression>,
}

impl ToString for ExpressionStatement {
    fn to_string(&self) -> String {
        self.expression.to_string()
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }

    fn token(&self) -> &token::Token {
        &self.token
    }

    fn into_expression(&self) -> Option<&ExpressionStatement> {
        Some(self)
    }
}

impl Statement for ExpressionStatement {
    fn into_expresion_statement(&self) -> Option<&ExpressionStatement> {
        match self.token {
            token::Token::LET | token::Token::RETURN => None,
            _ => Some(self),
        }
    }
}

pub struct IntegerLitral(pub token::Token);

impl ToString for IntegerLitral {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

impl Node for IntegerLitral {
    fn token_literal(&self) -> String {
        self.0.to_string()
    }

    fn token(&self) -> &token::Token {
        &self.0
    }
}

impl Expression for IntegerLitral {
    fn into_int(&self) -> Option<&IntegerLitral> {
        match self.0 {
            token::Token::INT(_) => Some(self),
            _ => None,
        }
    }
}

pub struct FloatLitral(pub token::Token);

impl ToString for FloatLitral {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

impl Node for FloatLitral {
    fn token_literal(&self) -> String {
        self.0.to_string()
    }

    fn token(&self) -> &token::Token {
        &self.0
    }
}

impl Expression for FloatLitral {
    fn into_float(&self) -> Option<&FloatLitral> {
        match self.0 {
            token::Token::FLOAT(_) => Some(self),
            _ => None,
        }
    }
}

pub struct BooleanLitral(pub token::Token);

impl ToString for BooleanLitral {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

impl Node for BooleanLitral {
    fn token_literal(&self) -> String {
        self.0.to_string()
    }

    fn token(&self) -> &token::Token {
        &self.0
    }
}

impl Expression for BooleanLitral {
    fn into_bool(&self) -> Option<&BooleanLitral> {
        match self.0 {
            token::Token::TRUE | token::Token::FALSE => Some(self),
            _ => None,
        }
    }
}

pub struct StringLitral(pub token::Token);

impl ToString for StringLitral {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

impl Node for StringLitral {
    fn token_literal(&self) -> String {
        self.0.to_string()
    }

    fn token(&self) -> &token::Token {
        &self.0
    }
}

impl Expression for StringLitral {
    fn into_string(&self) -> Option<&StringLitral> {
        match self.0 {
            token::Token::STRING(_) => Some(self),
            _ => None,
        }
    }
}

pub struct PrefixExpression {
    pub token: token::Token,
    pub right: Box<dyn Expression>,
}

impl ToString for PrefixExpression {
    fn to_string(&self) -> String {
        format!("({}{})", self.token_literal(), self.right.to_string())
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }

    fn token(&self) -> &token::Token {
        &self.token
    }
}

impl Expression for PrefixExpression {
    fn into_prefix_expression(&self) -> Option<&PrefixExpression> {
        match self.token {
            token::Token::BANG | token::Token::MINUS => Some(self),
            _ => None,
        }
    }
}

pub struct InfixExpression {
    pub left: Box<dyn Expression>,
    pub token: token::Token,
    pub right: Box<dyn Expression>,
}

impl ToString for InfixExpression {
    fn to_string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.to_string(),
            self.token.to_string(),
            self.right.to_string()
        )
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }

    fn token(&self) -> &token::Token {
        &self.token
    }
}

impl Expression for InfixExpression {
    fn into_infix_expression(&self) -> Option<&InfixExpression> {
        use token::Token::*;
        match self.token {
            PLUS | MINUS | ASTRISK | SLASH | EQ | NOTEQ | LT | GT | LTEQ | GTEQ | MOD => Some(self),
            _ => None,
        }
    }
}

pub struct BlockStatement {
    pub token: token::Token,
    pub statements: Vec<Box<dyn Statement>>,
}

impl ToString for BlockStatement {
    fn to_string(&self) -> String {
        self.statements
            .as_slice()
            .iter()
            .map(|s| s.to_string())
            .collect()
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.to_string()
    }

    fn token(&self) -> &token::Token {
        &self.token
    }
}

impl Statement for BlockStatement {
    fn into_block_statement(&self) -> Option<&BlockStatement> {
        match self.token {
            token::Token::LPAREN => Some(self),
            _ => None,
        }
    }
}

pub struct IfExpression {
    pub token: token::Token,
    pub condition: Box<dyn Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl ToString for IfExpression {
    fn to_string(&self) -> String {
        let mut s = format!(
            "if{} {}",
            self.condition.to_string(),
            self.consequence.to_string()
        );

        if let Some(alt) = &self.alternative {
            s = format!("{}else {}", s, alt.to_string());
        }
        s
    }
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.to_string()
    }

    fn token(&self) -> &token::Token {
        &self.token
    }
}

impl Expression for IfExpression {
    fn into_if_expresion(&self) -> Option<&IfExpression> {
        match self.token {
            token::Token::IF => Some(self),
            _ => None,
        }
    }
}

pub struct FunctioinLitral {
    pub token: token::Token,
    pub parameters: Vec<Identfier>,
    pub body: BlockStatement,
}

impl ToString for FunctioinLitral {
    fn to_string(&self) -> String {
        let mut s = format!("{}(", self.token.to_string());
        for id in &self.parameters {
            s = format!("{} {},", s, id.to_string());
        }
        s.pop();
        s = format!("{}){{{}}}", s, self.body.to_string());
        s
    }
}

impl Node for FunctioinLitral {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }

    fn token(&self) -> &token::Token {
        &self.token
    }
}

impl Expression for FunctioinLitral {
    fn into_funtion_expression(&self) -> Option<&FunctioinLitral> {
        match self.token {
            token::Token::FUNCTION => Some(self),
            _ => None,
        }
    }
}

pub struct CallExpression {
    pub token: token::Token,
    pub function: Box<dyn Expression>,
    pub arguments: Vec<Box<dyn Expression>>,
}

impl ToString for CallExpression {
    fn to_string(&self) -> String {
        let mut s = format!("{}(", self.function.to_string());
        for args in &self.arguments {
            s = format!("{} {},", s, args.to_string());
        }
        s = format!("{})", s);
        s
    }
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }

    fn token(&self) -> &token::Token {
        &self.token
    }
}

impl Expression for CallExpression {
    fn into_call_expression(&self) -> Option<&CallExpression> {
        match self.token {
            token::Token::LPAREN => Some(self),
            _ => None,
        }
    }
}
