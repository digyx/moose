use crate::lexer::InfixOperator;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    Ordering,
    Sum,
    Product,
    Prefix,
    Call,
}

pub(super) fn get_prescedence(tok: &InfixOperator) -> Precedence {
    match *tok {
        InfixOperator::Equal => Precedence::Equals,
        InfixOperator::NotEqual => Precedence::Equals,

        InfixOperator::LessThan => Precedence::Ordering,
        InfixOperator::GreaterThan => Precedence::Ordering,

        InfixOperator::Plus => Precedence::Sum,
        InfixOperator::Minus => Precedence::Sum,

        InfixOperator::Asterisk => Precedence::Product,
        InfixOperator::ForwardSlash => Precedence::Product,

        InfixOperator::Call => Precedence::Call,
    }
}
