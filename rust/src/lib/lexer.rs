use super::token;

#[derive(Debug, Default)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input,
            ..Default::default()
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        self.ch = self.input.chars().nth(self.read_position).unwrap_or('\0');
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> token::Token {
        self.skip_white_space();
        let token = match self.ch {
            '=' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    token::Token::EQ
                } else {
                    token::Token::ASSIGN
                }
            }
            ';' => {
                self.read_char();
                token::Token::SEMICOLON
            }
            '(' => {
                self.read_char();
                token::Token::LPAREN
            }
            ')' => {
                self.read_char();
                token::Token::RPAREN
            }
            ',' => {
                self.read_char();
                token::Token::COMMA
            }
            '+' => {
                self.read_char();
                token::Token::PLUS
            }
            '{' => {
                self.read_char();
                token::Token::LBRACE
            }
            '}' => {
                self.read_char();
                token::Token::RBRACE
            }
            '-' => {
                self.read_char();
                token::Token::MINUS
            }
            '!' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    token::Token::NOTEQ
                } else {
                    token::Token::BANG
                }
            }
            '/' => {
                self.read_char();
                token::Token::SLASH
            }
            '*' => {
                self.read_char();
                token::Token::ASTRISK
            }
            '<' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    token::Token::LTEQ
                } else {
                    token::Token::LT
                }
            }
            '>' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    token::Token::GTEQ
                } else {
                    token::Token::GT
                }
            }
            '%' => {
                self.read_char();
                token::Token::MOD
            }
            '\0' => {
                self.read_char();
                token::Token::EOF
            }
            _ => {
                if self.ch.is_alphabetic() || self.ch == '_' {
                    self.read_identifier().into()
                } else if self.ch.is_ascii_digit() {
                    let (int, float) = self.read_number();
                    if let Some(i) = int {
                        token::Token::INT(i)
                    } else if let Some(f) = float {
                        token::Token::FLOAT(f)
                    } else {
                        token::Token::ILLEGAL
                    }
                } else if self.ch == '"' {
                    let string = self.read_str().to_string();
                    token::Token::STRING(string)
                } else {
                    self.read_char();
                    token::Token::ILLEGAL
                }
            }
        };

        token
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while self.ch.is_alphanumeric() || self.ch == '_' {
            self.read_char();
        }
        &self.input[position..self.position] as _
    }

    fn read_number(&mut self) -> (Option<i64>, Option<f64>) {
        let position = self.position;
        let mut saw_dot = 0;
        while (self.ch.is_ascii_digit() || self.ch == '.') && saw_dot <= 1 {
            self.read_char();
            if self.ch == '.' {
                saw_dot += 1;
            }
        }
        let num = &self.input[position..self.position];
        let mut int: Option<i64> = None;
        let mut float: Option<f64> = None;
        if saw_dot == 1 {
            float = Some(num.parse::<f64>().unwrap());
        } else {
            int = Some(num.parse::<i64>().unwrap());
        }
        (int, float)
    }

    fn read_str(&mut self) -> &str {
        self.read_char();
        let position = self.position;
        while self.ch != '"' {
            self.read_char();
        }
        self.read_char();
        &self.input[position..(self.position - 1)] as _
    }

    fn skip_white_space(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
}

impl Iterator for Lexer {
    type Item = token::Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position > self.input.len() {
            None
        } else {
            Some(self.next_token())
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use token::Token::*;
    #[test]
    fn test_next_token() {
        let input = String::from(
            "let five = 5;
        let ten_10 = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
        !-/*5; 5<10>5;
        if(5<10){
            return true;
        } else {
            return false;
        }
        10 == 10;
        10 != 10;
        \"hellow world\";
        1.42;
        4%2;
        4>=2;
        1<=2;
",
        );
        let l = Lexer::new(input);
        let expected = [
            LET,
            IDET("five".to_string()),
            ASSIGN,
            INT(5),
            SEMICOLON,
            LET,
            IDET("ten_10".to_string()),
            ASSIGN,
            INT(10),
            SEMICOLON,
            LET,
            IDET("add".to_string()),
            ASSIGN,
            FUNCTION,
            LPAREN,
            IDET("x".to_string()),
            COMMA,
            IDET("y".to_string()),
            RPAREN,
            LBRACE,
            IDET("x".to_string()),
            PLUS,
            IDET("y".to_string()),
            SEMICOLON,
            RBRACE,
            SEMICOLON,
            LET,
            IDET("result".to_string()),
            ASSIGN,
            IDET("add".to_string()),
            LPAREN,
            IDET("five".to_string()),
            COMMA,
            IDET("ten".to_string()),
            RPAREN,
            SEMICOLON,
            BANG,
            MINUS,
            SLASH,
            ASTRISK,
            INT(5),
            SEMICOLON,
            INT(5),
            LT,
            INT(10),
            GT,
            INT(5),
            SEMICOLON,
            IF,
            LPAREN,
            INT(5),
            LT,
            INT(10),
            RPAREN,
            LBRACE,
            RETURN,
            TRUE,
            SEMICOLON,
            RBRACE,
            ELSE,
            LBRACE,
            RETURN,
            FALSE,
            SEMICOLON,
            RBRACE,
            INT(10),
            EQ,
            INT(10),
            SEMICOLON,
            INT(10),
            NOTEQ,
            INT(10),
            SEMICOLON,
            STRING("hellow world".to_string()),
            SEMICOLON,
            FLOAT(1.42_f64),
            SEMICOLON,
            INT(4),
            MOD,
            INT(2),
            SEMICOLON,
            INT(4),
            GTEQ,
            INT(2),
            SEMICOLON,
            INT(1),
            LTEQ,
            INT(2),
            SEMICOLON,
            EOF,
        ];

        for (i, t) in l.enumerate() {
            assert_eq!(t, expected[i]);
        }
    }
}
