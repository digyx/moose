#[derive(Debug, PartialEq, PartialOrd)]
pub enum Token {
    Illegal,

    // Ident + Literals
    Ident(String),
    Int(i64),

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,

    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,

    // Keywords
    Function,
    Let,
}
