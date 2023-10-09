use std::default;

#[derive(Debug, PartialEq, Default)]
pub enum Token {
    ILLEGAL,
    #[default]
    EOF,
    IDET(String),
    INT(i64),
    FLOAT(f64),
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTRISK,
    SLASH,
    LT,
    GT,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    IF,
    ELSE,
    TRUE,
    FALSE,
    RETURN,
    EQ,
    NOTEQ,
    STRING(String),
    MOD,
    LTEQ,
    GTEQ,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            Token::ILLEGAL => "ILLEGAL".to_string(),
            Token::EOF => "EOF".to_string(),
            Token::IDET(s) => s.clone(),
            Token::INT(i) => i.to_string(),
            Token::FLOAT(f) => f.to_string(),
            Token::ASSIGN => "=".to_string(),
            Token::PLUS => "+".to_string(),
            Token::COMMA => ",".to_string(),
            Token::SEMICOLON => ";".to_string(),
            Token::LPAREN => "(".to_string(),
            Token::RPAREN => ")".to_string(),
            Token::LBRACE => "{".to_string(),
            Token::RBRACE => "}".to_string(),
            Token::FUNCTION => "FUNCTION".to_string(),
            Token::LET => "LET".to_string(),
            Token::MINUS => "-".to_string(),
            Token::BANG => "!".to_string(),
            Token::ASTRISK => "*".to_string(),
            Token::SLASH => "/".to_string(),
            Token::LT => "<".to_string(),
            Token::GT => ">".to_string(),
            Token::IF => "IF".to_string(),
            Token::ELSE => "ELSE".to_string(),
            Token::TRUE => "TRUE".to_string(),
            Token::FALSE => "FALSE".to_string(),
            Token::RETURN => "RETURN".to_string(),
            Token::EQ => "==".to_string(),
            Token::NOTEQ => "!=".to_string(),
            Token::STRING(s) => s.clone(),
            Token::MOD => "%".to_string(),
            Token::LTEQ => "<=".to_string(),
            Token::GTEQ => ">=".to_string(),
        }
    }
}

impl From<String> for Token {
    fn from(value: String) -> Self {
        match value.as_str() {
            "fn" => Token::FUNCTION,
            "let" => Token::LET,
            "if" => Token::IF,
            "else" => Token::ELSE,
            "true" => Token::TRUE,
            "false" => Token::FALSE,
            "return" => Token::RETURN,
            _ => Token::IDET(value),
        }
    }
}
