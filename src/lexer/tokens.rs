#[derive(Debug, PartialEq, PartialOrd)]
pub enum Token {
    Illegal,

    // Ident + Literals
    Ident(String),
    Int(i64),

    // Operators
    Bang,

    Assign,
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,

    Equal,
    NotEqual,
    LessThan,
    GreaterThan,

    // Delimiters
    Comma,
    Semicolon,

    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,

    // Keywords
    True,
    False,

    Function,
    Let,
    If,
    Else,
    Return,
}
