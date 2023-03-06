use std::fmt::Display;

use crate::lexer::Token;

#[derive(Debug)]
pub enum ParserError {
    EOF,
    UnexpectedToken(&'static str, Token),
    ExpectedExpression,
    ExpectedBoolean,
    ExpectedNumeric,
    ExpectedBlock,
    ExpectedLeftParenthesis,
    ExpectedRightParenthesis,
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::EOF => write!(f, "unexpected EOF"),
            ParserError::UnexpectedToken(expected, got) => {
                write!(f, "invalid token; expected {} got {:?}", expected, got)
            }
            ParserError::ExpectedExpression => write!(f, "expected expression"),
            ParserError::ExpectedBoolean => write!(f, "expected boolean expression"),
            ParserError::ExpectedNumeric => write!(f, "expected numeric expression"),
            ParserError::ExpectedBlock => write!(f, "expected block"),
            ParserError::ExpectedLeftParenthesis => write!(f, "expected left parenthesis"),
            ParserError::ExpectedRightParenthesis => write!(f, "expected right parenthesis"),
        }
    }
}

impl std::error::Error for ParserError {}
