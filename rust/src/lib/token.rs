/// Represents a token in the Monkey programming language
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    /// Identifiers and literals
    #[allow(missing_docs)]
    Ident(String),
    #[allow(missing_docs)]
    Int(i64),
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
            _ => write!(f, "{:?}", self),
        }
    }
}

impl<T: AsRef<str>> From<T> for Token {
    fn from(value: T) -> Self {
        Token::from_ident(value.as_ref())
    }
}
