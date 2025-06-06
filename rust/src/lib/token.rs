/// Represents a token in the Monkey programming language
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    /// Identifiers and literals
    #[allow(missing_docs)]
    Ident(String),
    #[allow(missing_docs)]
    Int(i128),
    #[allow(missing_docs)]
    Float(f64),
    #[allow(missing_docs)]
    Str(String),

    /// Assignment operators
    Assign, // =

    /// Arithmetic operators
    Add, // +
    Sub,   // -
    Mul,   // *
    Div,   // /
    Power, // **
    Mod,  // %

    /// Unary operators
    Not, // !
    Increment, // ++
    Decrement, // --

    /// Logical operators
    LogicalOr, // ||
    LogicalAnd, // &&
    LogicalXor, // ^^

    /// Bitwise operators
    BitwiseOr, // |
    BitwiseAnd, // &
    BitwiseNot, // ~
    BitwiseXor, // ^
    ShiftLeft,  // <<
    ShiftRight, // >>

    /// Comparison operators
    Less, // <
    LessEq,    // <=
    Greater,   // >
    GreaterEq, // >=
    Equal,     // ==
    NotEqual,  // !=

    /// Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    /// Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
    /// Converts an identifier string into a keyword token if applicable,
    /// otherwise returns an Ident token
    fn from_ident(ident: &str) -> Token {
        match ident {
            "fn" => Self::Function,
            "let" => Self::Let,
            "true" => Self::True,
            "false" => Self::False,
            "if" => Self::If,
            "else" => Self::Else,
            "return" => Self::Return,
            _ => Self::Ident(ident.to_owned()),
        }
    }
}


impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(s) | Self::Str(s) => write!(f, "{}", s),
            Self::Int(n) => write!(f, "{}", n),
            Self::Float(n) => write!(f, "{}", n),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LBracket => write!(f, "["),
            Self::RBracket => write!(f, "]"),
            Self::Comma => write!(f, ","),
            Self::Semicolon => write!(f, ";"),
            Self::Assign => write!(f, "="),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Power => write!(f, "**"),
            Self::Mod => write!(f, "%"),
            Self::Not => write!(f, "!"),
            Self::Increment => write!(f, "++"),
            Self::Decrement => write!(f, "--"),
            Self::LogicalOr => write!(f, "||"),
            Self::LogicalAnd => write!(f, "&&"),
            Self::LogicalXor => write!(f, "^"),
            Self::BitwiseOr => write!(f, "|"),
            Self::BitwiseAnd => write!(f, "&"),
            Self::BitwiseNot => write!(f, "~"),
            Self::BitwiseXor => write!(f, "^"),
            Self::ShiftLeft => write!(f, "<<"),
            Self::ShiftRight => write!(f, ">>"),
            Self::Less => write!(f, "<"),
            Self::LessEq => write!(f, "<="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEq => write!(f, ">="),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::Function => write!(f, "fn"),
            Self::Let => write!(f, "let"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Return => write!(f, "return"),
            _ => write!(f, "{:?}", self),
        }
    }
}

impl<T: AsRef<str>> From<T> for Token {
    fn from(value: T) -> Self {
        Token::from_ident(value.as_ref())
    }
}
