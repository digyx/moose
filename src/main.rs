use std::fs;
use std::io::{self, Write};

use clap::{CommandFactory, Parser};

mod lexer;
mod parser;

#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    path: Option<String>,
}

fn main() {
    let args = Args::parse();
    let cmd = Args::command();

    match args.path {
        Some(path) => {
            let input = fs::read_to_string(&path).unwrap();
            let tokens = lexer::tokenize(&input).unwrap();
            let ast = parser::parse(tokens);
            println!("{}", ast);
        }

        None => start_repl(cmd.get_version().unwrap()),
    }
}

fn start_repl(version: &str) {
    println!("Moose {} REPL", version);
    let mut input = String::new();
    loop {
        print!("> ");
        io::stdout().flush().expect("failed to write to stdout");

        io::stdin()
            .read_line(&mut input)
            .expect("failed to read from stdin");

        let tokens = lexer::tokenize(&input).unwrap();
        let ast = parser::parse(tokens);
        println!("{}", ast);

        input.clear();
    }
}
