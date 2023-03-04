use std::{iter::Peekable, vec::IntoIter};

use super::LexerError;

pub type Tokens = Peekable<IntoIter<Token>>;

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
    LessThanEqual,
    GreaterThanEqual,

    // Delimiters
    Comma,
    Semicolon,

    // Expression groupers
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
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
    type Error = LexerError;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        let term = match token {
            Token::Ident(val) => Term::Ident(val),
            Token::Int(val) => Term::Int(val),
            Token::True => Term::True,
            Token::False => Term::False,

            _ => return Err(LexerError::InvalidToken),
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
}

impl TryFrom<&Token> for PrefixOperator {
    type Error = LexerError;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        let term = match token {
            Token::Bang => Self::Bang,
            Token::Minus => Self::Minus,
            Token::If => Self::If,

            _ => return Err(LexerError::InvalidToken),
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
    LessThanEqual,
    GreaterThanEqual,
}

impl InfixOperator {
    pub fn is(tok: &Token) -> bool {
        Self::try_from(tok).is_ok()
    }
}

impl TryFrom<&Token> for InfixOperator {
    type Error = LexerError;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        let term = match token {
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,
            Token::Asterisk => Self::Asterisk,
            Token::ForwardSlash => Self::ForwardSlash,

            Token::Equal => Self::Equal,
            Token::NotEqual => Self::NotEqual,
            Token::LessThan => Self::LessThan,
            Token::LessThanEqual => Self::LessThanEqual,
            Token::GreaterThan => Self::GreaterThan,
            Token::GreaterThanEqual => Self::GreaterThanEqual,

            _ => return Err(LexerError::InvalidToken),
        };

        Ok(term)
    }
}

// Keywords
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Keyword {
    Let,
    Function,
    Return,
}

impl Keyword {
    pub fn is(tok: &Token) -> bool {
        Self::try_from(tok).is_ok()
    }
}

impl TryFrom<&Token> for Keyword {
    type Error = LexerError;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        let term = match token {
            Token::Let => Self::Let,
            Token::Function => Self::Function,
            Token::Return => Self::Return,

            _ => return Err(LexerError::InvalidToken),
        };

        Ok(term)
    }
}
