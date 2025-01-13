use crate::token::Token;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    current: Option<char>,
}

#[derive(Debug)]
pub enum LexerError {
    InvalidNumber(String),
    UnexpectedCharacter(char),
    InvalidStartOfIdentifier(char),
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut chars = input.chars().peekable();
        let current = chars.next();
        Self {
            input: chars,
            current,
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.current = self.input.next();
        self.current
    }

    fn peek(&mut self) -> Option<char> {
        self.input.peek().copied()
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current {
            if !ch.is_whitespace() {
                break;
            }
            self.advance();
        }
    }

    fn read_identifier(&mut self) -> Result<String, LexerError> {
        let mut identifier = String::new();

        if let Some(ch) = self.current {
            if ch.is_ascii_digit() {
                return Err(LexerError::InvalidStartOfIdentifier(ch));
            }
            identifier.push(ch);
        }

        while let Some(ch) = self.peek() {
            if !ch.is_alphabetic() && ch != '_' && !ch.is_ascii_digit() {
                break;
            }
            identifier.push(ch);
            self.advance();
        }

        Ok(identifier)
    }

    fn read_number(&mut self) -> Result<Token, LexerError> {
        let mut number = String::new();
        let mut has_decimal = false;

        if let Some(ch) = self.current {
            number.push(ch);
        }

        while let Some(ch) = self.peek() {
            if ch == '.' && !has_decimal {
                has_decimal = true;
                number.push(ch);
                self.advance();
            } else if ch.is_ascii_digit() {
                number.push(ch);
                self.advance();
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

        let token = match self.current? {
            '=' => Ok(Token::Assign),
            '+' => Ok(Token::Add),
            '-' => Ok(Token::Sub),
            '!' => Ok(Token::Not),
            '*' => Ok(Token::Mul),
            '/' => Ok(Token::Div),
            ';' => Ok(Token::Semicolon),
            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            '{' => Ok(Token::LBrace),
            '}' => Ok(Token::RBrace),
            ',' => Ok(Token::Comma),
            ch if ch.is_alphabetic() || ch == '_' => {
                match self.read_identifier() {
                    Ok(ident) => Ok(Token::from_ident(&ident)),
                    Err(err) => Err(err),
                }
            }
            ch if ch.is_ascii_digit() => self.read_number(),
            ch => Err(LexerError::UnexpectedCharacter(ch)),
        };

        self.advance();
        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };"#;

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
            // ... rest of the tokens
        ];
        let lexer = Lexer::new(input);
        for (token, expected) in lexer.zip(expected.into_iter()) {
            assert_eq!(token.unwrap(), expected);
        }
    }
}
