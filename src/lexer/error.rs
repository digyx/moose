#[derive(Debug)]
pub enum LexerError {
    IllegalToken,
    InvalidToken,
}

impl std::error::Error for LexerError {}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::IllegalToken => write!(f, "illegal token found; only ascii is valid"),
            LexerError::InvalidToken => write!(f, "improper token type"),
        }
    }
}
