use std::collections::HashMap;

use crate::parser::{Expression, Node, Program};

mod error;
mod object;

use error::EvalError;
use object::Object;

pub type Environment = HashMap<String, Object>;

pub fn evaluate(ast: Program, env: &mut Environment) -> Result<Object, EvalError> {
    let mut result = Object::Null;

    for statement in ast.program() {
        match eval_statement(statement, env) {
            Ok(res) => result = res,
            Err(EvalError::Return(res)) => return Ok(res),
            Err(err) => return Err(err),
        };
    }

    Ok(result)
}

fn eval_statement(statement: Node, env: &mut Environment) -> Result<Object, EvalError> {
    match statement {
        Node::Let(ident, expr) => {
            let value = eval_expression(expr, env)?;
            env.insert(ident, value);
            Ok(Object::Null)
        }

        Node::Return(expr) => {
            let res = eval_expression(expr, env)?;
            return Err(EvalError::Return(res));
        }

        Node::Block(statements) => {
            let mut result = Object::Null;
            for statement in statements {
                result = eval_statement(statement, env)?;
            }

            Ok(result)
        }

        Node::Expression(expr) => eval_expression(expr, env),
    }
}

fn eval_expression(expr: Expression, env: &mut Environment) -> Result<Object, EvalError> {
    match expr {
        Expression::Identifier(ident) => {
            // TODO:  Is this necessary?  Is there a better way than cloning?
            env.get(&ident)
                .ok_or(EvalError::UndefinedVariable(ident))
                .cloned()
        }

        // Literals
        Expression::Integer(val) => Ok(Object::Integer(val)),
        Expression::Bool(val) => Ok(Object::Boolean(val)),

        // Prefix Operators
        Expression::Negative(expr) => match eval_expression(*expr, env)? {
            Object::Integer(val) => Ok(Object::Integer(-val)),
            _ => Err(EvalError::TypeError),
        },
        Expression::Not(expr) => match eval_expression(*expr, env)? {
            Object::Boolean(val) => Ok(Object::Boolean(!val)),
            _ => Err(EvalError::TypeError),
        },

        // Infix Numeric Operators
        Expression::Add(lhs, rhs) => {
            match (eval_expression(*lhs, env)?, eval_expression(*rhs, env)?) {
                (Object::Integer(lhs), Object::Integer(rhs)) => Ok(Object::Integer(lhs + rhs)),
                _ => Err(EvalError::TypeError),
            }
        }
        Expression::Subtract(lhs, rhs) => {
            match (eval_expression(*lhs, env)?, eval_expression(*rhs, env)?) {
                (Object::Integer(lhs), Object::Integer(rhs)) => Ok(Object::Integer(lhs - rhs)),
                _ => Err(EvalError::TypeError),
            }
        }
        Expression::Multiply(lhs, rhs) => {
            match (eval_expression(*lhs, env)?, eval_expression(*rhs, env)?) {
                (Object::Integer(lhs), Object::Integer(rhs)) => Ok(Object::Integer(lhs * rhs)),
                _ => Err(EvalError::TypeError),
            }
        }
        Expression::Divide(lhs, rhs) => {
            match (eval_expression(*lhs, env)?, eval_expression(*rhs, env)?) {
                (Object::Integer(lhs), Object::Integer(rhs)) => Ok(Object::Integer(lhs / rhs)),
                _ => Err(EvalError::TypeError),
            }
        }

        // Infix Boolean Operators
        Expression::Equal(lhs, rhs) => {
            match (eval_expression(*lhs, env)?, eval_expression(*rhs, env)?) {
                (Object::Boolean(lhs), Object::Boolean(rhs)) => Ok(Object::Boolean(lhs == rhs)),
                (Object::Integer(lhs), Object::Integer(rhs)) => Ok(Object::Boolean(lhs == rhs)),
                _ => Err(EvalError::TypeError),
            }
        }
        Expression::NotEqual(lhs, rhs) => {
            match (eval_expression(*lhs, env)?, eval_expression(*rhs, env)?) {
                (Object::Boolean(lhs), Object::Boolean(rhs)) => Ok(Object::Boolean(lhs != rhs)),
                (Object::Integer(lhs), Object::Integer(rhs)) => Ok(Object::Boolean(lhs != rhs)),
                _ => Err(EvalError::TypeError),
            }
        }
        Expression::LessThan(lhs, rhs) => {
            match (eval_expression(*lhs, env)?, eval_expression(*rhs, env)?) {
                (Object::Integer(lhs), Object::Integer(rhs)) => Ok(Object::Boolean(lhs < rhs)),
                _ => Err(EvalError::TypeError),
            }
        }
        Expression::GreaterThan(lhs, rhs) => {
            match (eval_expression(*lhs, env)?, eval_expression(*rhs, env)?) {
                (Object::Integer(lhs), Object::Integer(rhs)) => Ok(Object::Boolean(lhs > rhs)),
                _ => Err(EvalError::TypeError),
            }
        }

        Expression::If {
            condition,
            consequence,
            alternative,
        } => {
            let condition = match eval_expression(*condition, env)? {
                Object::Boolean(val) => val,
                _ => return Err(EvalError::TypeError),
            };

            if condition {
                eval_statement(*consequence, env)
            } else if let Some(alt) = alternative {
                eval_statement(*alt, env)
            } else {
                Ok(Object::Null)
            }
        }

        Expression::Function { parameters, body } => Ok(Object::Function { parameters, body }),

        Expression::Call { function, args } => {
            let (params, body) = match eval_expression(*function, env)? {
                Object::Function { parameters, body } => (parameters, body),
                _ => return Err(EvalError::TypeError),
            };

            let mut func_env = env.clone();

            args.into_iter()
                .map(|arg| eval_expression(arg, env))
                .zip(params.iter())
                .try_for_each(|(arg, ident)| -> Result<(), EvalError> {
                    func_env.insert(ident.to_owned(), arg?);
                    Ok(())
                })?;

            eval_statement(*body, &mut func_env)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::tokenize;
    use crate::parser::parse;

    use super::*;
    use rstest::rstest;

    // General test function used for evaluation
    fn test(input: &str, expected: &str) {
        let tokens = tokenize(input).unwrap();
        let ast = parse(tokens).unwrap();
        let mut env: Environment = HashMap::new();
        let res = evaluate(ast, &mut env).unwrap();
        assert_eq!(&res.to_string(), expected)
    }

    #[rstest]
    #[case("5", "5")]
    #[case("true", "true")]
    #[case("-5", "-5")]
    fn literal(#[case] input: &str, #[case] expected: &str) {
        test(input, expected)
    }

    #[rstest]
    #[case("--5", "5")]
    #[case("!true", "false")]
    #[case("!!true", "true")]
    fn prefix_expression(#[case] input: &str, #[case] expected: &str) {
        test(input, expected)
    }

    #[rstest]
    #[case("2+2", "4")]
    #[case("2-2", "0")]
    #[case("2*3", "6")]
    #[case("2/2", "1")]
    // Booleans
    #[case("true == true", "true")]
    #[case("true == false", "false")]
    #[case("5 == 5", "true")]
    #[case("5 == 6", "false")]
    #[case("true != false", "true")]
    #[case("true != true", "false")]
    #[case("5 != 5", "false")]
    #[case("5 != 6", "true")]
    #[case("1 < 2", "true")]
    #[case("3 < 2", "false")]
    #[case("1 > 2", "false")]
    #[case("3 > 2", "true")]
    // Complex
    #[case("(3 + 4) * 32 > 16 == 3 < 17 + 90/2", "true")]
    fn infix_expression(#[case] input: &str, #[case] expected: &str) {
        test(input, expected)
    }

    #[rstest]
    #[case("if true { 2+2 }", "4")]
    #[case("if false { 2+2 }", "null")]
    #[case("if 4 > 2 { 2 + 3 } else { 2 + 2 }", "5")]
    #[case("if 4 < 2 { 2 + 3 } else { 2 + 2 }", "4")]
    #[should_panic] // Test that only boolean expressions are valid conditions
    #[case("if 4 { 2 + 3 } else { 2 + 2 }", "4")]
    fn if_expression(#[case] input: &str, #[case] expected: &str) {
        test(input, expected)
    }

    #[rstest]
    #[case("return 2 + 2;", "4")]
    #[case("return 2 + 2; 2 + 3", "4")]
    #[case("if true { return 1; 2 + 2 };", "1")]
    #[case("if true { return 1 }; return 2", "1")]
    #[case("if true { if true { return 1 }; return 2 }", "1")]
    fn return_statement(#[case] input: &str, #[case] expected: &str) {
        test(input, expected)
    }

    #[rstest]
    #[case("let a = 5; a", "5")]
    #[case("let a = 2 + 2; a", "4")]
    #[case("let a = 2; let b = a; b", "2")]
    fn let_statement(#[case] input: &str, #[case] expected: &str) {
        test(input, expected)
    }

    #[rstest]
    #[case("fn(x) { x + 2 }", "fn(x) { (x + 2) }")]
    #[case("fn(x) { x + 2 }(2)", "4")]
    #[case("let addtwo = fn(x) { x + 2 }; addtwo(3)", "5")]
    fn function(#[case] input: &str, #[case] expected: &str) {
        test(input, expected)
    }
}
