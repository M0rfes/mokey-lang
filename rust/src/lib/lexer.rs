use crate::token::Token;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

#[derive(Debug, Clone)]
pub enum LexerError {
    InvalidNumber(String),
    UnexpectedCharacter(char),
    InvalidStartOfIdentifier(char),
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
        }
    }

    pub(crate) fn peek(&mut self) -> Option<char> {
        self.input.peek().copied()
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if !ch.is_whitespace() {
                break;
            }
            self.input.next();
        }
    }

    fn read_identifier(&mut self, current: char) -> Result<String, LexerError> {
        if current.is_ascii_digit() {
            return Err(LexerError::InvalidStartOfIdentifier(current));
        }
        let mut identifier = String::new();
        identifier.push(current);

        while let Some(ch) = self.peek() {
            if !ch.is_alphabetic() && ch != '_' && !ch.is_ascii_digit() {
                break;
            }
            identifier.push(ch);
            self.input.next();
        }

        Ok(identifier)
    }

    fn read_number(&mut self, current: char) -> Result<Token, LexerError> {
        let mut number = String::new();
        number.push(current);
        let mut has_decimal = false;

        while let Some(ch) = self.peek() {
            if ch == '.' && !has_decimal {
                has_decimal = true;
                number.push(ch);
                self.input.next();
            } else if ch.is_ascii_digit() {
                number.push(ch);
                self.input.next();
            } else {
                break;
            }
        }

        if has_decimal {
            number
                .parse::<f64>()
                .map(Token::Float)
                .map_err(|_| LexerError::InvalidNumber(number))
        } else {
            number
                .parse::<i64>()
                .map(Token::Int)
                .map_err(|_| LexerError::InvalidNumber(number))
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        let token = match self.input.next()? {
            '=' if self.peek() == Some('=') => {
                self.input.next();
                Ok(Token::Equal)
            }
            '!' if self.peek() == Some('=') => {
                self.input.next();
                Ok(Token::NotEqual)
            }
            '<' if self.peek() == Some('=') => {
                self.input.next();
                Ok(Token::LessEq)
            }
            '>' if self.peek() == Some('=') => {
                self.input.next();
                Ok(Token::GreaterEq)
            }
            '|' if self.peek() == Some('|') => {
                self.input.next();
                Ok(Token::LogicalOr)
            }
            '&' if self.peek() == Some('&') => {
                self.input.next();
                Ok(Token::LogicalAnd)
            }
            '^' if self.peek() == Some('^') => {
                self.input.next();
                Ok(Token::LogicalXor)
            }
            '|' => Ok(Token::BitwiseOr),
            '&' => Ok(Token::BitwiseAnd),
            '~' => Ok(Token::BitwiseNot),
            '^' => Ok(Token::BitwiseXor),
            '<' if self.peek() == Some('<') => {
                self.input.next();
                Ok(Token::ShiftLeft)
            }
            '>' if self.peek() == Some('>') => {
                self.input.next();
                Ok(Token::ShiftRight)
            }
            '+' if self.peek() == Some('+') => {
                self.input.next();
                Ok(Token::Increment)
            }
            '-' if self.peek() == Some('-') => {
                self.input.next();
                Ok(Token::Decrement)
            }
            '=' => Ok(Token::Assign),
            '+' => Ok(Token::Add),
            '-' => Ok(Token::Sub),
            '!' => Ok(Token::Not),
            '*' if self.peek() == Some('*') => {
                self.input.next();
                Ok(Token::Power)
            }
            '*' => Ok(Token::Mul),
            '/' => Ok(Token::Div),
            ';' => Ok(Token::Semicolon),
            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            '{' => Ok(Token::LBrace),
            '}' => Ok(Token::RBrace),
            ',' => Ok(Token::Comma),
            '<' => Ok(Token::Less),
            '>' => Ok(Token::Greater),
            '%' => Ok(Token::Mod),
            '"' => {
                let mut string = String::new();
                while let Some(ch) = self.peek() {
                    if ch == '"' {
                        break;
                    }
                    string.push(ch);
                    self.input.next();
                }
                self.input.next(); // consume closing quote
                Ok(Token::Str(string))
            }
            ch if ch.is_alphabetic() || ch == '_' => match self.read_identifier(ch) {
                Ok(ident) => Ok(ident.into()),
                Err(err) => Err(err),
            },
            ch if ch.is_ascii_digit() => self.read_number(ch),
            ch => Err(LexerError::UnexpectedCharacter(ch)),
        };

        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use crate::token;

    use super::*;

    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            let is_less = five < ten;
            let is_greater = five > ten;
            let is2 = 2;
            let float = 3.14;
            let float2_3 = 0.1;
            let float3_t = 0.1;
            !-/*5;
            if (5 < 10) {
                return true;
            } else {
                return false;
            };
            2 ** 3;
            2 <= 3;
            2 >= 3;
            2 == 3;
            2 != 3;
            "Hello, World!";
            ++x;
            --x;
            x++;
            x--;
            ++add1;
            "#;

        let expected = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Add,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Let,
            Token::Ident("is_less".to_string()),
            Token::Assign,
            Token::Ident("five".to_string()),
            Token::Less,
            Token::Ident("ten".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("is_greater".to_string()),
            Token::Assign,
            Token::Ident("five".to_string()),
            Token::Greater,
            Token::Ident("ten".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("is2".to_string()),
            Token::Assign,
            Token::Int(2),
            Token::Semicolon,
            Token::Let,
            Token::Ident("float".to_string()),
            Token::Assign,
            Token::Float(3.14),
            Token::Semicolon,
            Token::Let,
            Token::Ident("float2_3".to_string()),
            Token::Assign,
            Token::Float(0.1),
            Token::Semicolon,
            Token::Let,
            Token::Ident("float3_t".to_string()),
            Token::Assign,
            Token::Float(0.1),
            Token::Semicolon,
            Token::Not,
            Token::Sub,
            Token::Div,
            Token::Mul,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::Less,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Int(2),
            Token::Power,
            Token::Int(3),
            Token::Semicolon,
            Token::Int(2),
            Token::LessEq,
            Token::Int(3),
            Token::Semicolon,
            Token::Int(2),
            Token::GreaterEq,
            Token::Int(3),
            Token::Semicolon,
            Token::Int(2),
            Token::Equal,
            Token::Int(3),
            Token::Semicolon,
            Token::Int(2),
            Token::NotEqual,
            Token::Int(3),
            Token::Semicolon,
            Token::Str("Hello, World!".to_string()),
            Token::Semicolon,
            Token::Increment,
            Token::Ident("x".to_string()),
            Token::Semicolon,
            Token::Decrement,
            Token::Ident("x".to_string()),
            Token::Semicolon,
            Token::Ident("x".to_string()),
            Token::Increment,
            Token::Semicolon,
            Token::Ident("x".to_string()),
            Token::Decrement,
            Token::Semicolon,
            Token::Increment,
            Token::Ident("add1".to_string()),
            Token::Semicolon,
            // ... rest of the tokens
        ];
        let lexer = Lexer::new(input);
        let mut i = 0;
        for (token, expected) in lexer.zip(expected.into_iter()) {
            println!("{:?}", token);
            let token = token.unwrap();
            assert_eq!(
                token, expected,
                "Expected {:?} but got {:?} at {}",
                expected, token, i
            );
            i += 1;
        }
    }
}
