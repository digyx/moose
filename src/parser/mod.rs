use crate::lexer::{InfixOperator, Keyword, PrefixOperator, Term, Token, Tokens};

mod ast;
mod error;
mod precedence;

pub use ast::{Expression, Node, Program};
pub use error::ParserError;

use self::precedence::{get_prescedence, Precedence};

pub fn parse(tokens: Tokens) -> Program {
    let mut tokens = tokens;
    let mut ast = Vec::new();
    while let Some(node) = next_node(&mut tokens) {
        match node {
            Ok(node) => ast.push(node),
            // TODO: Handle this more gracefully than a panic
            Err(err) => panic!("{}", err),
        }
    }

    Program::new(ast)
}

fn next_node<'a>(tokens: &mut Tokens) -> Option<Result<Node, ParserError>> {
    let node = match tokens.peek()? {
        tok if Keyword::is(tok) => {
            let keyword = Keyword::try_from(tok).unwrap();

            match keyword {
                Keyword::Let => parse_let_statement(tokens),
                Keyword::Return => parse_return_statement(tokens),
                _ => panic!("not implemented"),
            }
        }

        // Parse Valid expression
        tok if PrefixOperator::is(tok) || Term::is(tok) || tok == &Token::LeftParenthesis => {
            parse_expression_statement(tokens)
        }

        Token::LeftBrace => parse_block_statement(tokens),

        Token::Semicolon => {
            // Eat ;
            tokens.next();
            next_node(tokens)?
        }

        tok => panic!("not implemented: {:?}", tok),
    };

    Some(node)
}

// Statement parsing
fn parse_let_statement(tokens: &mut Tokens) -> Result<Node, ParserError> {
    // Get rid of `let`
    tokens.next();

    let ident = match tokens.next() {
        Some(Token::Ident(ident)) => ident,
        Some(tok) => return Err(ParserError::UnexpectedToken("identifier", tok)),
        None => return Err(ParserError::EOF),
    };

    // Ensure `=` follows the identifier
    match tokens.next() {
        Some(tok) if tok != Token::Assign => return Err(ParserError::UnexpectedToken("=", tok)),
        _ => {}
    }

    let val = parse_expression(tokens, None, Precedence::Lowest)?;

    Ok(Node::Let(ident, val))
}

fn parse_return_statement(tokens: &mut Tokens) -> Result<Node, ParserError> {
    // Get rid of `return`
    tokens.next();

    let val = parse_expression(tokens, None, Precedence::Lowest)?;
    Ok(Node::Return(val))
}

fn parse_expression_statement(tokens: &mut Tokens) -> Result<Node, ParserError> {
    let val = parse_expression(tokens, None, Precedence::Lowest)?;
    Ok(Node::Expression(val))
}

fn parse_block_statement(tokens: &mut Tokens) -> Result<Node, ParserError> {
    let mut statements: Vec<Node> = Vec::new();

    // Ensure block starts with {
    if tokens.next() != Some(Token::LeftBrace) {
        return Err(ParserError::ExpectedBlock);
    };

    while tokens.peek() != Some(&Token::RightBrace) {
        match next_node(tokens) {
            Some(Ok(stmt)) => statements.push(stmt),
            Some(Err(err)) => return Err(err),
            None => return Err(ParserError::EOF),
        }
    }

    // Eat }
    tokens.next();

    Ok(Node::Block(statements))
}

// Expression parsing
fn parse_expression(
    tokens: &mut Tokens,
    lhs: Option<Expression>,
    precedence: Precedence,
) -> Result<Expression, ParserError> {
    // If LHS exists, then unwrap it.  Otherwise, parse the next token to determine what LHS is
    let lhs = match lhs {
        Some(lhs) => lhs,
        None => match tokens.next() {
            // Prefix operators
            Some(operator) if PrefixOperator::is(&operator) => {
                parse_prefix_operator(tokens, PrefixOperator::try_from(&operator).unwrap())?
            }

            // Grouped expressions
            Some(Token::LeftParenthesis) => {
                let res = parse_expression(tokens, None, Precedence::Lowest)?;

                if tokens.next() != Some(Token::RightParenthesis) {
                    return Err(ParserError::ExpectedRightParenthesis);
                }

                res
            }

            // Parse terms
            Some(term) if Term::is(&term) => parse_term(term.try_into().unwrap())?,

            Some(_) => return Err(ParserError::ExpectedExpression),
            None => return Err(ParserError::EOF),
        },
    };

    let expr = match tokens.peek() {
        None
        | Some(Token::RightParenthesis)
        | Some(Token::LeftBrace)
        | Some(Token::RightBrace)
        | Some(Token::Semicolon) => return Ok(lhs),

        Some(tok) => match tok {
            // Infix Operator
            tok if InfixOperator::is(tok) => {
                let operator = InfixOperator::try_from(tok).unwrap();
                if precedence >= get_prescedence(&operator) {
                    return Ok(lhs);
                }

                parse_infix_operator(tokens, lhs)?
            }

            // Prefix Operator
            // Since `-` is a prefix and infix operator, we give way to InfixOperator::Minus first
            tok if PrefixOperator::is(tok) => {
                let operator = tok.try_into().unwrap();
                parse_prefix_operator(tokens, operator)?
            }

            // Term
            tok if Term::is(tok) => {
                let term = tok.clone().try_into().unwrap();
                parse_term(term)?
            }

            // Invalid tokens
            _ => return Err(ParserError::ExpectedExpression),
        },
    };

    parse_expression(tokens, Some(expr), precedence)
}

fn parse_term(token: Term) -> Result<Expression, ParserError> {
    let res = match token {
        // Variables, functions, etc.
        Term::Ident(val) => Expression::Identifier(val),

        // Literals
        Term::Int(val) => Expression::Integer(val),
        Term::True => Expression::Bool(true),
        Term::False => Expression::Bool(false),
    };

    Ok(res)
}

fn parse_prefix_operator(
    tokens: &mut Tokens,
    operator: PrefixOperator,
) -> Result<Expression, ParserError> {
    let expr = match operator {
        // Not
        PrefixOperator::Bang => match parse_expression(tokens, None, Precedence::Prefix)? {
            expr if expr.is_bool() => Expression::Not(Box::new(expr)),
            _ => return Err(ParserError::ExpectedBoolean),
        },

        // Negative
        PrefixOperator::Minus => match parse_expression(tokens, None, Precedence::Prefix)? {
            expr if expr.is_numeric() => {
                let val = Box::new(expr);
                Expression::Negative(val)
            }
            _ => return Err(ParserError::ExpectedNumeric),
        },

        PrefixOperator::If => {
            let condition = parse_expression(tokens, None, Precedence::Lowest)?;
            let consequence = parse_block_statement(tokens)?;

            let alternative = if tokens.peek() == Some(&Token::Else) {
                // Eat else
                tokens.next();

                Some(Box::new(parse_block_statement(tokens)?))
            } else {
                None
            };

            Expression::If {
                condition: Box::new(condition),
                consequence: Box::new(consequence),
                alternative,
            }
        }
    };

    Ok(expr)
}

fn parse_infix_operator(tokens: &mut Tokens, lhs: Expression) -> Result<Expression, ParserError> {
    let operator = match tokens.next() {
        Some(operator) if InfixOperator::is(&operator) => {
            InfixOperator::try_from(&operator).unwrap()
        }
        Some(tok) => return Err(ParserError::UnexpectedToken("infix operator", tok)),
        None => return Err(ParserError::EOF),
    };

    let lhs = Box::new(lhs);
    let rhs = parse_expression(tokens, None, get_prescedence(&operator))?;
    let rhs = Box::new(rhs);

    let res = match operator {
        InfixOperator::Plus => Expression::Add(lhs, rhs),
        InfixOperator::Minus => Expression::Subtract(lhs, rhs),

        InfixOperator::Asterisk => Expression::Multiply(lhs, rhs),
        InfixOperator::ForwardSlash => Expression::Divide(lhs, rhs),

        InfixOperator::Equal => Expression::Equal(lhs, rhs),
        InfixOperator::NotEqual => Expression::NotEqual(lhs, rhs),

        InfixOperator::GreaterThan => Expression::GreaterThan(lhs, rhs),
        InfixOperator::LessThan => Expression::LessThan(lhs, rhs),
        InfixOperator::GreaterThanEqual => Expression::GreaterThanEqual(lhs, rhs),
        InfixOperator::LessThanEqual => Expression::LessThanEqual(lhs, rhs),
    };

    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;
    use rstest::rstest;

    #[rstest]
    #[case("let int = 5", "let int be 5")]
    #[case("return 7", "returning 7")]
    #[case("let x = 5 + 6", "let x be (5 + 6)")]
    #[case("return 5 + 6", "returning (5 + 6)")]
    #[case("5 + 6; 7+3", "(5 + 6)\n(7 + 3)")]
    #[case("(5 + 5) * 3; 2 + 2", "((5 + 5) * 3)\n(2 + 2)")]
    fn test_parser<'a>(#[case] input: &str, #[case] expected: &str) {
        let tokens = lexer::tokenize(input).unwrap();
        let res = parse(tokens);

        assert_eq!(&res.to_string(), expected);
    }

    #[rstest]
    #[case("let x 7")]
    #[case("return")]
    #[case("let = 8")]
    #[should_panic]
    fn test_parser_failure(#[case] input: &str) {
        let tokens = lexer::tokenize(input).unwrap();
        parse(tokens);
    }

    #[rstest]
    // Terms
    #[case("5", "5")]
    #[case("uwu", "uwu")]
    #[case("true", "true")]
    #[case("false", "false")]
    // Prefix operators
    #[case("!true", "(!true)")]
    #[case("!false", "(!false)")]
    #[case("-5", "(-5)")]
    // Infix operators
    #[case("5 + 6", "(5 + 6)")]
    #[case("5 - 6", "(5 - 6)")]
    #[case("5 * 6", "(5 * 6)")]
    #[case("5 / 6", "(5 / 6)")]
    #[case("5 == 6", "(5 == 6)")]
    #[case("5 != 6", "(5 != 6)")]
    #[case("5 < 6", "(5 < 6)")]
    #[case("5 > 6", "(5 > 6)")]
    #[case("5 <= 6", "(5 <= 6)")]
    #[case("5 >= 6", "(5 >= 6)")]
    // Boolean and numeric operators
    #[case("3 < 5 == true", "((3 < 5) == true)")]
    // Operator associativity
    #[case("5 + 6 + 7", "((5 + 6) + 7)")]
    #[case("a + b - c", "((a + b) - c)")]
    // Operator Prescedence
    #[case("5 + 6 * 8", "(5 + (6 * 8))")]
    #[case("5 < 7 == 4 > 3", "((5 < 7) == (4 > 3))")]
    #[case("5 - 6 * 7 + 2", "((5 - (6 * 7)) + 2)")]
    #[case("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)")]
    #[case("(5 + 5) * 2", "((5 + 5) * 2)")]
    fn test_parse_expression(#[case] input: &str, #[case] expected: &str) {
        let mut tokens = lexer::tokenize(input).unwrap();
        let res = parse_expression(&mut tokens, None, Precedence::Lowest).unwrap();
        assert_eq!(&res.to_string(), expected);
    }

    #[rstest]
    #[case("if true { 5 + 5 };", "if true then { (5 + 5) } else N/A")]
    #[case("if x > y { x }", "if (x > y) then { x } else N/A")]
    #[case("if x > y { x } else { y }", "if (x > y) then { x } else { y }")]
    fn test_if_expression(#[case] input: &str, #[case] expected: &str) {
        let mut tokens = lexer::tokenize(input).unwrap();
        let res = parse_expression(&mut tokens, None, Precedence::Lowest).unwrap();
        assert_eq!(&res.to_string(), expected);
    }
}
