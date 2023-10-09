use super::token;

pub trait Node: ToString {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    #[cfg(test)]
    fn into_let_statement(&self) -> Option<&LetStatement> {
        None
    }

    #[cfg(test)]
    fn into_return_statement(&self) -> Option<&ReturnStatement> {
        None
    }

    #[cfg(test)]
    fn into_expresion_statement(&self) -> Option<&ExpressionStatement> {
        None
    }
}

pub trait Epression: Node {
    #[cfg(test)]
    fn into_identifier(&self) -> Option<&Identfier> {
        None
    }

    #[cfg(test)]
    fn into_int(&self) -> Option<&IntegerLitral> {
        None
    }

    #[cfg(test)]
    fn into_float(&self) -> Option<&FloatLitral> {
        None
    }

    #[cfg(test)]
    fn into_bool(&self) -> Option<&BooleanLitral> {
        None
    }

    #[cfg(test)]
    fn into_string(&self) -> Option<&StringLitral> {
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
}

impl Epression for Identfier {
    #[cfg(test)]
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
    // pub value: Box<dyn Epression>,
}

impl ToString for LetStatement {
    fn to_string(&self) -> String {
        let mut s = String::new();
        // s = format!("{let} {name} = {value}", let=self.token_literal(), name=self.name.to_string(),value=self.value.to_string());
        s
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Statement for LetStatement {
    #[cfg(test)]
    fn into_let_statement(&self) -> Option<&LetStatement> {
        match self.token {
            token::Token::LET => Some(self),
            _ => None,
        }
    }
}

pub struct ReturnStatement {
    pub token: token::Token,
    // pub return_value: Box<dyn Epression>,
}

impl ToString for ReturnStatement {
    fn to_string(&self) -> String {
        let mut s = String::new();
        s = format!("{return} ", return = self.token_literal());
        s
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Statement for ReturnStatement {
    #[cfg(test)]
    fn into_return_statement(&self) -> Option<&ReturnStatement> {
        match self.token {
            token::Token::RETURN => Some(self),
            _ => None,
        }
    }
}

pub struct ExpressionStatement {
    pub token: token::Token,
    pub expression: Box<dyn Epression>,
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
}

impl Statement for ExpressionStatement {
    #[cfg(test)]
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
}

impl Epression for IntegerLitral {
    #[cfg(test)]
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
}

impl Epression for FloatLitral {
    #[cfg(test)]
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
}

impl Epression for BooleanLitral {
    #[cfg(test)]
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
}

impl Epression for StringLitral {
    #[cfg(test)]
    fn into_string(&self) -> Option<&StringLitral> {
        match self.0 {
            token::Token::STRING(_) => Some(self),
            _ => None,
        }
    }
}
