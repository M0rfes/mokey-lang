use super::token::Token;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a String,
    position: usize,
    read_position: usize,
    ch: String,
}

impl Lexer<'_> {
    pub fn new(input: &String) -> Lexer {
        let mut lex = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0'.to_string(),
        };
        lex.read_char();
        lex
    }
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0'.to_string();
        } else {
            self.ch = self
                .input
                .chars()
                .nth(self.read_position)
                .unwrap_or('\0')
                .to_string();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token_type = match &self.ch[..] {
            "=" => {
                if self.peek_char() == '='.to_string() {
                    self.read_char();
                    self.read_char();
                    Token::EQ
                } else {
                    self.read_char();
                    Token::ASSIGN
                }
            }
            "+" => {
                self.read_char();
                Token::PLUS
            }
            "-" => {
                self.read_char();
                Token::MINUS
            }
            "!" => {
                if self.peek_char() == "=".to_string() {
                    self.read_char();
                    self.read_char();
                    Token::NOTEQ
                } else {
                    self.read_char();
                    Token::BANG
                }
            }
            "*" => {
                self.read_char();
                Token::ASTERISK
            }
            "/" => {
                self.read_char();
                Token::SLASH
            }
            "<" => {
                self.read_char();
                Token::LT
            }
            ">" => {
                self.read_char();
                Token::GT
            }
            "(" => {
                self.read_char();
                Token::LPAREM
            }
            ")" => {
                self.read_char();
                Token::RPAREM
            }
            "{" => {
                self.read_char();
                Token::LBRACE
            }
            "}" => {
                self.read_char();
                Token::RBRACE
            }
            "," => {
                self.read_char();
                Token::COMMA
            }
            ";" => {
                self.read_char();
                Token::SEMICOLON
            }
            "\0" => Token::EOF,
            _ => {
                if self.ch.chars().all(|c| c.is_ascii_digit()) {
                    Token::INT(self.read_number())
                } else if self.ch.chars().all(|c| c.is_ascii_alphabetic()) {
                    let token = self.read_identifier();
                    match &token[..] {
                        "fn" => Token::FUNCTION,
                        "let" => Token::LET,
                        "true" => Token::TRUE,
                        "false" => Token::FALSE,
                        "if" => Token::IF,
                        "else" => Token::ELSE,
                        "return" => Token::RETURN,
                        _ => Token::IDENT(token.to_string()),
                    }
                } else {
                    Token::ILLEGAL
                }
            }
        };
        token_type
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;

        while self.ch.chars().all(|c| c.is_ascii_alphabetic()) {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }
    fn read_number(&mut self) -> i32 {
        let position = self.position;

        while self.ch.chars().all(|c| c.is_ascii_digit()) {
            self.read_char();
        }

        let token = self.input[position..self.position].to_string();
        token.parse().unwrap()
    }

    fn skip_whitespace(&mut self) {
        while self.ch == " " || self.ch == "\t" || self.ch == "\n" || self.ch == "\r" {
            self.read_char();
        }
    }
    fn peek_char(&self) -> String {
        if self.read_position >= self.input.len() {
            "\0".to_string()
        } else {
            let peek = self
                .input
                .chars()
                .nth(self.read_position)
                .unwrap_or('\0')
                .to_string();
            peek
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.next_token() {
            Token::EOF => None,
            Token::ILLEGAL => None,
            token => Some(token),
        }
    }
}

#[cfg(test)]
mod tests {
    // this brings everything from parent's scope into this scope
    use super::*;

    #[test]
    fn it_gets_next_token() {
        let input = &String::from(
            "let five=5; let ten=10;
            let add=fn(x,y){ x+y; };
            let result=add(five,ten);
            !-/*5;
            5<10>5;
            if(5<10){ return true; }
            else{ return false; } 
            10==10; 
            10!=9;",
        );
        let mut lexer = Lexer::new(input);
        let expected_tokens = vec![
            Token::LET,
            Token::IDENT("five".to_string()),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".to_string()),
            Token::ASSIGN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".to_string()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREM,
            Token::IDENT("x".to_string()),
            Token::COMMA,
            Token::IDENT("y".to_string()),
            Token::RPAREM,
            Token::LBRACE,
            Token::IDENT("x".to_string()),
            Token::PLUS,
            Token::IDENT("y".to_string()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".to_string()),
            Token::ASSIGN,
            Token::IDENT("add".to_string()),
            Token::LPAREM,
            Token::IDENT("five".to_string()),
            Token::COMMA,
            Token::IDENT("ten".to_string()),
            Token::RPAREM,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT(5),
            Token::SEMICOLON,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::GT,
            Token::INT(5),
            Token::SEMICOLON,
            //   if(5<10){ return true; }else{ return false; } 10==10; 10!=9;
            Token::IF,
            Token::LPAREM,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::RPAREM,
            Token::LBRACE,
            Token::RETURN,
            Token::TRUE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::FALSE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::INT(10),
            Token::EQ,
            Token::INT(10),
            Token::SEMICOLON,
            Token::INT(10),
            Token::NOTEQ,
            Token::INT(9),
            Token::SEMICOLON,
        ];
        for expected_token in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token, expected_token);
        }
    }
}
