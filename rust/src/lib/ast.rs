use std::{any::Any, fmt, iter::Peekable};

use crate::{
    lexer::{Lexer, LexerError},
    token::Token,
};

// impl std::fmt::Display
pub trait Node: Any + std::fmt::Display {
    fn token_literal(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement: Node + std::fmt::Display {}

pub trait Expression: Node + std::fmt::Display {}

#[derive(Debug)]
pub enum ParseError {
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
        strings.join("\n")
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            writeln!(f, "{}", statement)?;
        }
        Ok(())
    }
}

pub struct Identifier(pub String);

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.0.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Identifier {}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct LetStatement {
   pub name: Identifier,
   pub value: Box<dyn Expression>,
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

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

pub struct ReturnStatement(pub Box<dyn Expression>);

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        "return".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ReturnStatement {}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "return {};", self.0)
    }
}

pub struct ExpressionStatement(pub Box<dyn Expression>);

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.0.token_literal()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ExpressionStatement {}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{};", self.0)
    }
}

pub struct Prefix {
    pub operator: Token,
    pub right: Box<dyn Expression>,
}

impl Node for Prefix {
    fn token_literal(&self) -> String {
        format!("({}{})", self.operator, self.right.token_literal())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Prefix {}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

pub struct Postfix {
    pub left: Box<dyn Expression>,
    pub operator: Token,
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

impl fmt::Display for Postfix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}{})", self.left, self.operator)
    }
}

pub struct Infix {
    pub left: Box<dyn Expression>,
    pub operator: Token,
    pub right: Box<dyn Expression>,
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

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

pub struct Int(pub i128);

impl Node for Int {
    fn token_literal(&self) -> String {
        self.0.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Int {}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct Float(pub f64);

impl Node for Float {
    fn token_literal(&self) -> String {
        self.0.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Expression for Float {}

pub struct Bool(pub bool);

impl Node for Bool {
    fn token_literal(&self) -> String {
        self.0.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Bool {}

impl fmt::Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
pub struct StringLiteral(pub String);

impl Node for StringLiteral {
    fn token_literal(&self) -> String {
        self.0.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for StringLiteral {}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\"", self.0)
    }
}

pub struct BlockStatement {
   pub statements: Vec<Box<dyn Statement>>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        if self.statements.is_empty() {
            return String::new();
        }
        let strings: Vec<String> = self.statements.iter().map(|s| s.token_literal()).collect();
        strings.join("\n")
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for BlockStatement {}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in &self.statements {
            writeln!(f, "{}\n", statement)?;
        }
        Ok(())
    }
}

pub struct IfExpression {
    condition: Box<dyn Expression>,
    consequence: BlockStatement,
    alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        "if".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for IfExpression {}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if{}{}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, "else {}", alt)?;
        }
        Ok(())
    }
}

struct FunctionLiteral {
    parameters: Vec<Identifier>,
    body: BlockStatement,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        "fn".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

struct CallExpression {
    function: Box<dyn Expression>,
    arguments: Vec<Box<dyn Expression>>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        "call".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for CallExpression {}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let args: Vec<String> = self.arguments.iter().map(|a| a.to_string()).collect();
        write!(f, "{}({})", self.function, args.join(", "))
    }
}

impl Expression for FunctionLiteral {}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        write!(f, "fn({}) {}", params.join(", "), self.body)
    }
}

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

        while let Some(_) = self.peek() {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(err) => {
                    program.errors.push(err);
                    self.next();
                }
            }
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Box<dyn Statement>, ParseError> {
        match self.peek() {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            Some(Token::LBrace) => Ok(Box::new(self.parse_block_statement()?)),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Box<dyn Statement>, ParseError> {
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
        Ok(Box::new(LetStatement { name, value }))
    }

    fn parse_return_statement(&mut self) -> Result<Box<dyn Statement>, ParseError> {
        // reads the `return` token
        self.next()?;
        let value = self.parse_expression(Precedence::Lowest)?;
        // reads the `;` token
        let Token::Semicolon = self.next()? else {
            return Err(ParseError::UnexpectedToken);
        };
        Ok(Box::new(ReturnStatement(value)))
    }

    fn parse_identifier(&mut self) -> Result<Identifier, ParseError> {
        match self.next()? {
            Token::Ident(ident) => Ok(Identifier(ident)),
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Box<dyn Statement>, ParseError> {
        let expression = self.parse_expression(Precedence::Lowest)?;
        // reads the `;` token
        if Token::Semicolon != self.next()? {
            return Err(ParseError::UnexpectedToken);
        }
        Ok(Box::new(ExpressionStatement(expression)))
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
            left = self.parse_infix_expression(left)?;
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
            Token::Function => {
                if Token::LParen != self.next()? {
                    return Err(ParseError::UnexpectedToken);
                }
                let parameters = self.parse_function_parameters()?;
                let body = self.parse_block_statement()?;
                let f = FunctionLiteral { parameters, body };
                Ok(Box::new(f))
            }
            operator @ (Token::Sub
            | Token::Not
            | Token::Increment
            | Token::Decrement
            | Token::BitwiseNot) => {
                let right = self.parse_expression(Precedence::Prefix)?;
                Ok(Box::new(Prefix { operator, right }))
            }
            Token::If => {
                let condition = self.parse_expression(Precedence::Lowest)?;
                let consequence = self.parse_block_statement()?;
                let alternative = if let Some(Token::Else) = self.peek() {
                    self.next()?;
                    Some(self.parse_block_statement()?)
                } else {
                    None
                };
                Ok(Box::new(IfExpression {
                    condition,
                    consequence,
                    alternative,
                }))
            }
            Token::LParen => {
                let expression = self.parse_expression(Precedence::Lowest)?;
                if self.next()? != Token::RParen {
                    return Err(ParseError::UnexpectedToken);
                }
                Ok(expression)
            }
            _ => Err(ParseError::UnexpectedToken),
        }
    }
    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        // reads the `{` token
        if self.next()? != Token::LBrace {
            return Err(ParseError::UnexpectedToken);
        }
        let mut statements = Vec::new();
        while let Some(token) = self.peek() {
            match token {
                Token::RBrace => break,
                _ => match self.parse_statement() {
                    Ok(statement) => statements.push(statement),
                    Err(err) => return Err(err),
                },
            }
        }
        // reads the `}` token
        if self.next()? != Token::RBrace {
            return Err(ParseError::UnexpectedToken);
        }
        Ok(BlockStatement { statements })
    }

    fn parse_infix_expression(
        &mut self,
        left: Box<dyn Expression>,
    ) -> Result<Box<dyn Expression>, ParseError> {
        let operator = self.next()?;
        if Token::LParen == operator {
            return self.parse_call_expression(left);
        }
        let precedence = operator.precedence();
        let right = self.parse_expression(precedence)?;
        Ok(Box::new(Infix {
            left,
            operator,
            right,
        }))
    }

    fn parse_postfix_expression(
        &mut self,
        left: Box<dyn Expression>,
    ) -> Result<Box<dyn Expression>, ParseError> {
        let operator = self.next()?;
        Ok(Box::new(Postfix { left, operator }))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, ParseError> {
        let mut parameters = Vec::new();

        while let Some(token) = self.peek() {
            match token {
                Token::Ident(ident) => {
                    parameters.push(Identifier(ident.clone()));
                    self.next()?;
                }
                Token::RParen => {
                    self.next()?;
                    break;
                }
                Token::Comma => {
                    self.next()?;
                }
                _ => return Err(ParseError::UnexpectedToken),
            }
        }

        Ok(parameters)
    }

    fn parse_call_expression(
        &mut self,
        function: Box<dyn Expression>,
    ) -> Result<Box<dyn Expression>, ParseError> {
        let arguments = self.parse_call_arguments()?;
        Ok(Box::new(CallExpression {
            function,
            arguments,
        }))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Box<dyn Expression>>, ParseError> {
        let mut arguments = Vec::new();

        while let Some(token) = self.peek() {
            match token {
                Token::RParen => {
                    self.next()?;
                    break;
                }
                _ => {
                    let argument = self.parse_expression(Precedence::Lowest)?;
                    arguments.push(argument);
                    if let Some(Token::Comma) = self.peek() {
                        self.next()?;
                    }
                }
            }
        }

        Ok(arguments)
    }
}

impl Token {
    fn precedence(&self) -> Precedence {
        match self {
            Token::Equal | Token::NotEqual => Precedence::Equals,
            Token::Less | Token::LessEq | Token::Greater | Token::GreaterEq => {
                Precedence::LessGreater
            }
            Token::Add
            | Token::Sub
            | Token::LogicalAnd
            | Token::LogicalOr
            | Token::LogicalXor
            | Token::ShiftLeft
            | Token::ShiftRight
            | Token::BitwiseAnd
            | Token::BitwiseOr
            | Token::BitwiseXor
            | Token::Mod
            | Token::Assign => Precedence::Sum,
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

    #[test]
    fn test_infix_expression() {
        let input = "x + 5;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);
        let statement = program.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>();
        assert!(statement.is_some());
        let statement = statement.unwrap();
        let infix = statement.0.as_any().downcast_ref::<Infix>();
        assert!(infix.is_some());
    }

    #[test]
    fn test_prefix_expression() {
        let input = "!x;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);
        let statement = program.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>();
        assert!(statement.is_some());
        let statement = statement.unwrap();
        let prefix = statement.0.as_any().downcast_ref::<Prefix>();
        assert!(prefix.is_some());
        let prefix = prefix.unwrap();
        assert_eq!(prefix.operator, Token::Not);
        let right = prefix.right.as_any().downcast_ref::<Identifier>();
        assert!(right.is_some());
        let right = right.unwrap();
        assert_eq!(right.0, "x");
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; };";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);
        let statement = program.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>();
        assert!(statement.is_some());
        let statement = statement.unwrap();
        let function = statement.0.as_any().downcast_ref::<FunctionLiteral>();
        assert!(function.is_some());
        let function = function.unwrap();
        assert_eq!(function.parameters.len(), 2);
        assert_eq!(function.parameters[0].0, "x");
        assert_eq!(function.parameters[1].0, "y");
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);
        let statement = program.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>();
        assert!(statement.is_some());
        let statement = statement.unwrap();
        let call = statement.0.as_any().downcast_ref::<CallExpression>();
        assert!(call.is_some());
        let call = call.unwrap();
        assert_eq!(call.arguments.len(), 3);
        let fun = call.function.as_any().downcast_ref::<Identifier>();
        assert!(fun.is_some());
        let fun = fun.unwrap();
        assert_eq!(fun.0, "add");
    }

    #[test]
    fn test_assignment() {
        let input = "x = 5;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);
        let statement = program.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>();
        assert!(statement.is_some());
        let statement = statement.unwrap();
        let infix = statement.0.as_any().downcast_ref::<Infix>();
        assert!(infix.is_some());
        let infix = infix.unwrap();
        assert_eq!(infix.operator, Token::Assign);
        let left = infix.left.as_any().downcast_ref::<Identifier>();
        assert!(left.is_some());
        let left = left.unwrap();
        assert_eq!(left.0, "x");
    }

    #[test]
    fn test_block_statement() {
        let input = "{ let x = 5; let y = 10; }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);
        let statement = program.statements[0]
            .as_any()
            .downcast_ref::<BlockStatement>();
        assert!(statement.is_some());
        let statement = statement.unwrap();
        let block = &statement.statements;
        assert_eq!(block.len(), 2);
        let let_statement1 = block[0]
            .as_any()
            .downcast_ref::<LetStatement>();
        assert!(let_statement1.is_some());
        let let_statement1 = let_statement1.unwrap();
        assert_eq!(let_statement1.name.0, "x");
        let let_statement2 = block[1]
            .as_any()
            .downcast_ref::<LetStatement>();
        assert!(let_statement2.is_some());
        let let_statement2 = let_statement2.unwrap();
        assert_eq!(let_statement2.name.0, "y");
    }

}
