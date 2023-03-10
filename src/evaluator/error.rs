use std::fmt::Display;

use super::object::Object;

#[derive(Debug)]
pub enum EvalError {
    TypeError,
    UndefinedVariable(String),

    // This is not actually an error, but we will abuse it's position in being propegated to
    // return a value early.
    Return(Object),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            EvalError::TypeError => write!(f, "improper type"),
            EvalError::UndefinedVariable(ident) => write!(f, "undefined variable: {}", ident),

            EvalError::Return(val) => write!(f, "return: {}", val),
        }
    }
}

impl std::error::Error for EvalError {}
