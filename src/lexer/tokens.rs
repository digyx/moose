use std::iter::Peekable;
use std::vec::IntoIter;

use anyhow::{bail, Result};

pub type Tokens = Peekable<IntoIter<Token>>;

/// Each token is made up of various subtypes, such as Term, PrefixOperator, and
/// Keyword.  These are used to ensure that all patterns of a specific token are
/// handled during the parsing phase.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Token {
    Assign,

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,

    // Terms
    Ident(String),
    Int(i64),
    True,
    False,

    //Operators
    Bang,
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

    // Expression groupers
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Assign => write!(f, "="),
            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
            Token::Ident(tok) => write!(f, "{}", tok),
            Token::Int(tok) => write!(f, "{}", tok),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Bang => write!(f, "!"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Asterisk => write!(f, "*"),
            Token::ForwardSlash => write!(f, "/"),
            Token::Equal => write!(f, "="),
            Token::NotEqual => write!(f, "!="),
            Token::LessThan => write!(f, "<"),
            Token::GreaterThan => write!(f, ">"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::LeftParenthesis => write!(f, "("),
            Token::RightParenthesis => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
        }
    }
}

// ==================== Token Types ====================
// Terms
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Term {
    Ident(String),
    Int(i64),
    True,
    False,
}

impl Term {
    pub fn is(tok: &Token) -> bool {
        match tok {
            Token::Ident(_) | Token::Int(_) | Token::True | Token::False => true,
            _ => false,
        }
    }
}

impl TryFrom<Token> for Term {
    type Error = anyhow::Error;

    fn try_from(token: Token) -> Result<Self> {
        let term = match token {
            Token::Ident(val) => Term::Ident(val),
            Token::Int(val) => Term::Int(val),
            Token::True => Term::True,
            Token::False => Term::False,

            tok => bail!("invalid token: {} is not a term", tok),
        };

        Ok(term)
    }
}

// Prefix Operators
#[derive(Debug)]
pub enum PrefixOperator {
    Bang,
    Minus,
    If,
    Function,
}

impl TryFrom<&Token> for PrefixOperator {
    type Error = anyhow::Error;

    fn try_from(token: &Token) -> Result<Self> {
        let term = match token {
            Token::Bang => Self::Bang,
            Token::Minus => Self::Minus,
            Token::If => Self::If,
            Token::Function => Self::Function,

            tok => bail!("invalid token: {} is not a prefix operator", tok),
        };

        Ok(term)
    }
}

impl PrefixOperator {
    pub fn is(tok: &Token) -> bool {
        Self::try_from(tok).is_ok()
    }
}

// Infix Operators
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum InfixOperator {
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,

    Equal,
    NotEqual,
    LessThan,
    GreaterThan,

    Call,
}

impl InfixOperator {
    pub fn is(tok: &Token) -> bool {
        Self::try_from(tok).is_ok()
    }
}

impl TryFrom<&Token> for InfixOperator {
    type Error = anyhow::Error;

    fn try_from(token: &Token) -> Result<Self> {
        let term = match token {
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,
            Token::Asterisk => Self::Asterisk,
            Token::ForwardSlash => Self::ForwardSlash,

            Token::Equal => Self::Equal,
            Token::NotEqual => Self::NotEqual,
            Token::LessThan => Self::LessThan,
            Token::GreaterThan => Self::GreaterThan,

            Token::LeftParenthesis => Self::Call,

            tok => bail!("invalid token: {} is not an infix operator", tok),
        };

        Ok(term)
    }
}

// Keywords
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Keyword {
    Let,
    Return,
}

impl Keyword {
    pub fn is(tok: &Token) -> bool {
        Self::try_from(tok).is_ok()
    }
}

impl TryFrom<&Token> for Keyword {
    type Error = anyhow::Error;

    fn try_from(token: &Token) -> Result<Self> {
        let term = match token {
            Token::Let => Self::Let,
            Token::Return => Self::Return,

            tok => bail!("invalid token: {} is not a keyword", tok),
        };

        Ok(term)
    }
}
