use crate::lexer::{InfixOperator, Keyword, PrefixOperator, Term, Token, Tokens};

mod ast;
mod error;
mod precedence;

pub use ast::{Expression, Node, Program};
pub use error::ParserError;

use self::precedence::{get_prescedence, Precedence};

// Entrypoint
pub fn parse(tokens: Tokens) -> Program {
    // Redefine tokens are mutable so it can be used as an iterator
    let mut tokens = tokens;

    // Our AST is defined as a vector of statements
    let mut ast = Vec::new();

    // next_node return None on an EOF, so we know to end then
    while let Some(node) = next_node(&mut tokens) {
        match node {
            Ok(node) => ast.push(node),
            // TODO: Handle this more gracefully than a panic
            Err(err) => panic!("{}", err),
        }
    }

    Program::new(ast)
}

// Get the next statement of our program
fn next_node(tokens: &mut Tokens) -> Option<Result<Node, ParserError>> {
    // We return None on an EOF
    let node = match tokens.peek()? {
        // This check and coerceis a common pattern in this module
        //
        // I don't know of a way to convert the token at the same time
        // as checking its type.  I'd love to find a better way
        tok if Keyword::is(tok) => {
            let keyword = Keyword::try_from(tok).unwrap();
            // Eat the keyword
            tokens.next();

            match keyword {
                Keyword::Let => parse_let_statement(tokens),
                Keyword::Return => parse_return_statement(tokens),
            }
        }

        // Parse Valid expression
        tok if PrefixOperator::is(tok) || Term::is(tok) || tok == &Token::LeftParenthesis => {
            parse_expression_statement(tokens)
        }

        Token::LeftBrace => parse_block_statement(tokens),

        Token::Semicolon => {
            // Eat the Semicolon token
            tokens.next();
            next_node(tokens)?
        }

        tok => panic!("not implemented: {:?}", tok),
    };

    Some(node)
}

// Statement parsing
fn parse_let_statement(tokens: &mut Tokens) -> Result<Node, ParserError> {
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

    // Get the value of the identifier
    let val = parse_expression(tokens, Precedence::Lowest)?;

    Ok(Node::Let(ident, val))
}

fn parse_return_statement(tokens: &mut Tokens) -> Result<Node, ParserError> {
    let val = parse_expression(tokens, Precedence::Lowest)?;
    Ok(Node::Return(val))
}

fn parse_expression_statement(tokens: &mut Tokens) -> Result<Node, ParserError> {
    let val = parse_expression(tokens, Precedence::Lowest)?;
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
//
// Implemented via Pratt Parsing
fn parse_expression(
    tokens: &mut Tokens,
    precedence: Precedence,
) -> Result<Expression, ParserError> {
    // Parse the expression to get the LHS of our expression
    let mut lhs = match tokens.next() {
        // Prefix operators
        Some(operator) if PrefixOperator::is(&operator) => {
            parse_prefix_operator(tokens, PrefixOperator::try_from(&operator).unwrap())?
        }

        // Grouped expressions
        Some(Token::LeftParenthesis) => {
            let res = parse_expression(tokens, Precedence::Lowest)?;

            if tokens.next() != Some(Token::RightParenthesis) {
                return Err(ParserError::ExpectedRightParenthesis);
            }

            res
        }

        // Singular terms such as integers
        Some(term) if Term::is(&term) => parse_term(term.try_into().unwrap())?,

        // We expect one of the above tokens to indicate that this is a valid expression
        Some(_) => return Err(ParserError::ExpectedExpression),

        // We do not expect the end of the file at this point
        None => return Err(ParserError::EOF),
    };

    // Parse the infix with our LHS
    loop {
        // Break the loop on terminators
        match tokens.peek() {
            None
            | Some(Token::RightParenthesis)
            | Some(Token::Semicolon)
            | Some(Token::LeftBrace) // Break used for if statements
            | Some(Token::RightBrace) => break,
            _ => (),
        };

        // Ensure next token is an infix operator
        let operator = match tokens.peek() {
            Some(tok) if InfixOperator::is(tok) => InfixOperator::try_from(tok).unwrap(),
            Some(tok) => return Err(ParserError::UnexpectedToken("infix operator", tok.clone())),
            None => return Err(ParserError::EOF),
        };

        // Break if the current precedence is stronger than the lower precedence.
        //
        // Matklad has a great in-depth explanation
        // https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html#From-Precedence-to-Binding-Power
        //
        // Here's my summary (but go read his article; he writes great stuff)
        //
        // Instead of precedence, think of it as binding power.  If we look at (1 + 2 * 3 + 4), we evaluate 1 as the root and then see
        // that + is the infix operator with a binding power of 4, which is greater than 1 (our base precedence).  Parsing this, we
        // head down to our infix expression which is (1 + stuff).  When parsing stuff, we pass in 4 as our precedence.  The new expression
        // (2 * 3 + 4) has * as the first infix operator.  It has a binding power of 5, so we continue with (2 * stuff), passing 5 as our
        // precedence.  When parsing (3 + 4), we see + is our operator.  Now, + has a binding power of 4, which is lower than *'s of 5.  We
        // return the current expression of 4 as our RHS.  Now we have a precedence of 4 with the lhs being (2 * 3).  Since we want to
        // be left-hand heavy to keep associativity (is. 1 - 2 - 3 should be ((1 - 2) - 3) instead of (1 - (2 - 3))), we return here.  Now our
        // lhs is (1 + (2 * 3)) with a precedence of 1, our base.  Parsing the final infix operator, we get ((1 + (2 * 3)) + 4).
        if precedence >= get_prescedence(&operator) {
            break;
        }

        // Eat operator
        tokens.next();

        // Parse rest of the expression
        lhs = parse_infix_operator(tokens, operator, lhs)?;
    }

    Ok(lhs)
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
        PrefixOperator::Bang => match parse_expression(tokens, Precedence::Prefix)? {
            expr if expr.is_bool() => Expression::Not(Box::new(expr)),
            _ => return Err(ParserError::ExpectedBoolean),
        },

        // Negative
        PrefixOperator::Minus => match parse_expression(tokens, Precedence::Prefix)? {
            expr if expr.is_numeric() => {
                let val = Box::new(expr);
                Expression::Negative(val)
            }
            _ => return Err(ParserError::ExpectedNumeric),
        },

        // It feels weird that 'if' is a prefix operator, but it is.  If operators on the
        // expression by converting the expressions output into something different.
        PrefixOperator::If => {
            let condition = parse_expression(tokens, Precedence::Lowest)?;

            // Conditions in if statements must evaluate to booleans
            if !condition.is_bool() {
                return Err(ParserError::ExpectedBoolean);
            }

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

        // Once again, strange to think of it as such, but definitely is a prefix operator.
        // We're not quite at LISP levels of "everything is an expression!!" but we're not
        // that far away
        PrefixOperator::Function => {
            if Some(Token::LeftParenthesis) != tokens.next() {
                return Err(ParserError::ExpectedLeftParenthesis);
            }

            let mut parameters = Vec::new();
            // Because of how we're looping, `fn(x,,,,y)` is completely valid and will parse
            // to Expression::Function{ parameters: ["x", "y"], body: ... } which is fine but
            // technically different than the Monkey spec.  We could fix this by simply checking
            // if the next token after each comma is an identifier
            loop {
                let ident = match tokens.next() {
                    Some(Token::Ident(ident)) => ident,
                    Some(Token::Comma) => continue,
                    Some(Token::RightParenthesis) => break,

                    // Unexpected tokens
                    Some(tok) => {
                        return Err(ParserError::UnexpectedToken(
                            "identifier, comma, or right parenthesis",
                            tok,
                        ))
                    }

                    // Unexpected EOF
                    None => return Err(ParserError::EOF),
                };

                parameters.push(ident);
            }

            let body = parse_block_statement(tokens)?;

            Expression::Function {
                parameters,
                body: Box::new(body),
            }
        }
    };

    Ok(expr)
}

fn parse_infix_operator(
    tokens: &mut Tokens,
    operator: InfixOperator,
    lhs: Expression,
) -> Result<Expression, ParserError> {
    let lhs = Box::new(lhs);
    let rhs = Box::new(parse_expression(tokens, get_prescedence(&operator))?);

    let expr = match operator {
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

    Ok(expr)
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
        let res = parse_expression(&mut tokens, Precedence::Lowest).unwrap();
        dbg!(&res);
        assert_eq!(&res.to_string(), expected);
    }

    #[rstest]
    #[case("if true { 5 + 5 };", "if true then { (5 + 5) } else N/A")]
    #[case("if x > y { x }", "if (x > y) then { x } else N/A")]
    #[case("if x > y { x } else { y }", "if (x > y) then { x } else { y }")]
    fn test_if_expression(#[case] input: &str, #[case] expected: &str) {
        let mut tokens = lexer::tokenize(input).unwrap();
        let res = parse_expression(&mut tokens, Precedence::Lowest).unwrap();
        assert_eq!(&res.to_string(), expected);
    }

    #[rstest]
    #[case("fn() {true}", "fn() { true }")]
    #[case("fn(x) {x + 3}", "fn(x) { (x + 3) }")]
    #[case("fn(x, y) {x + y}", "fn(x, y) { (x + y) }")]
    fn test_function_expression(#[case] input: &str, #[case] expected: &str) {
        let mut tokens = lexer::tokenize(input).unwrap();
        let res = parse_expression(&mut tokens, Precedence::Lowest).unwrap();
        assert_eq!(&res.to_string(), expected);
    }
}
