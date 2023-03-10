use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};

use anyhow::{Context, Result};
use clap::{CommandFactory, Parser};

use crate::evaluator::Environment;

mod evaluator;
mod lexer;
mod parser;

#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    path: Option<String>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let cmd = Args::command();

    match args.path {
        Some(path) => {
            let input = fs::read_to_string(&path).context("could not open file")?;
            let mut env: Environment = HashMap::new();
            let res = eval(&input, &mut env)?;
            println!("{}", res);
        }

        None => {
            start_repl(cmd.get_version().unwrap());
        }
    };

    Ok(())
}

fn start_repl(version: &str) {
    let mut input = String::new();
    let mut env: Environment = HashMap::new();

    println!("Moose {} -- REPL", version);

    loop {
        print!("> ");
        // We flush immediately otherwise the `>` is not actually printed until after
        // the user has hit the return key.
        io::stdout().flush().expect("failed to write to stdout");

        let bytes_read = io::stdin()
            .read_line(&mut input)
            .expect("failed to read from stdin");

        // Check for Ctrl+D
        if bytes_read == 0 {
            break;
        }

        let res = eval(&input, &mut env).unwrap_or_else(|err| format!("{:?}", err));
        if res == String::from("null") {
            input.clear();
            continue;
        }

        println!("{}", res);

        input.clear();
    }
}

fn eval(input: &str, env: &mut Environment) -> Result<String> {
    let tokens = lexer::tokenize(&input).context("failed to tokenize input")?;
    let ast = parser::parse(tokens).context("failed to parse statement")?;
    let res = evaluator::evaluate(ast, env).context("failed to evaluate statement")?;

    Ok(res.to_string())
}
