mod tokens;

use std::{iter::Peekable, str::Chars};

use tokens::Token;

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut input = input.chars().into_iter().peekable();

    let mut toks = Vec::new();
    while let Some(tok) = next_token(&mut input) {
        toks.push(tok)
    }

    toks
}

fn next_token(input: &mut Peekable<Chars>) -> Option<Token> {
    let tok = match input.next()? {
        '+' => Token::Plus,
        '-' => Token::Minus,
        '*' => Token::Asterisk,
        '/' => Token::ForwardSlash,

        '!' => {
            if input.peek() == Some(&'=') {
                input.next();
                Token::NotEqual
            } else {
                Token::Bang
            }
        }
        '=' => {
            if input.peek() == Some(&'=') {
                input.next();
                Token::Equal
            } else {
                Token::Assign
            }
        }

        '<' => Token::LessThan,
        '>' => Token::GreaterThan,

        ',' => Token::Comma,
        ';' => Token::Semicolon,

        '(' => Token::LeftParenthesis,
        ')' => Token::RightParenthesis,
        '{' => Token::LeftBrace,
        '}' => Token::RightBrace,

        // Parse multicharacter tokens
        tok if tok.is_ascii_alphabetic() => read_ident(input, tok),
        tok if tok.is_ascii_digit() => read_int(input, tok),

        // Skip whitespace
        tok if tok.is_ascii_whitespace() => next_token(input)?,

        _ => Token::Illegal,
    };

    Some(tok)
}

fn read_ident(input: &mut Peekable<Chars>, first: char) -> Token {
    // Read the entire ident
    let mut toks = vec![first];
    while let Some(tok) = input.peek() {
        if !tok.is_ascii_alphabetic() {
            break;
        }

        let tok = input.next().unwrap();
        toks.push(tok);
    }

    // Check if our ident is a keyword
    let ident = toks.iter().cloned().collect::<String>();
    match ident.as_str() {
        "true" => Token::True,
        "false" => Token::False,
        "fn" => Token::Function,
        "let" => Token::Let,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,

        ident => Token::Ident(ident.to_owned()),
    }
}

fn read_int(input: &mut Peekable<Chars>, first: char) -> Token {
    let mut toks = vec![first];
    while let Some(tok) = input.peek() {
        if !tok.is_ascii_digit() {
            break;
        }

        let tok = input.next().unwrap();
        toks.push(tok);
    }

    let int = toks
        .iter()
        .cloned()
        .collect::<String>()
        .parse::<i64>()
        .unwrap();
    Token::Int(int)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        "=+(){},;",
        vec![
            Token::Assign,
            Token::Plus,
            Token::LeftParenthesis,
            Token::RightParenthesis,
            Token::LeftBrace,
            Token::RightBrace,
            Token::Comma,
            Token::Semicolon,
        ])]
    #[case(
        "!-/*5; 5 < 10 > 5;",
        vec![
            Token::Bang,
            Token::Minus,
            Token::ForwardSlash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::GreaterThan,
            Token::Int(5),
            Token::Semicolon,
        ])]
    #[case(
        "5 != 6",
        vec![
            Token::Int(5),
            Token::NotEqual,
            Token::Int(6),
    ])]
    #[case(
        "8 == 8",
        vec![
            Token::Int(8),
            Token::Equal,
            Token::Int(8),
    ])]
    #[case(
        "if (5 < 7) {
            return true
        } else {
            return false
        }",
        vec![
            Token::If,
            Token::LeftParenthesis,
            Token::Int(5),
            Token::LessThan,
            Token::Int(7),
            Token::RightParenthesis,
            Token::LeftBrace,
            Token::Return,
            Token::True,
            Token::RightBrace,
            Token::Else,
            Token::LeftBrace,
            Token::Return,
            Token::False,
            Token::RightBrace
        ])]
    #[case(
        "let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        }

        let result = add(five, ten);
        ",
        vec![
            Token::Let,
            Token::Ident("five".into()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".into()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".into()),
            Token::Assign,
            Token::Function,
            Token::LeftParenthesis,
            Token::Ident("x".into()),
            Token::Comma,
            Token::Ident("y".into()),
            Token::RightParenthesis,
            Token::LeftBrace,
            Token::Ident("x".into()),
            Token::Plus,
            Token::Ident("y".into()),
            Token::Semicolon,
            Token::RightBrace,
            Token::Let,
            Token::Ident("result".into()),
            Token::Assign,
            Token::Ident("add".into()),
            Token::LeftParenthesis,
            Token::Ident("five".into()),
            Token::Comma,
            Token::Ident("ten".into()),
            Token::RightParenthesis,
            Token::Semicolon,
        ])]
    fn test_next_token(#[case] input: &str, #[case] expected: Vec<Token>) {
        let res = tokenize(input);
        assert_eq!(res, expected);
    }
}
