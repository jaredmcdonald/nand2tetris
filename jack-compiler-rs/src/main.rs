extern crate regex;

mod parse;

use std::env::args;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufReader};
use parse::{tokenize, TokenError};

#[derive(Debug)]
enum CliError {
    TokenError(TokenError),
    IoError(io::Error),
}

impl From<io::Error> for CliError {
    fn from(error: io::Error) -> Self {
        CliError::IoError(error)
    }
}

impl From<TokenError> for CliError {
    fn from(error: TokenError) -> Self {
        CliError::TokenError(error)
    }
}

fn tokenize_file() -> Result<(), CliError> {
    let target = args().nth(1).unwrap_or(".".to_string());
    let file = File::open(target)?;
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;
    let tokenized = tokenize(&contents)?;
    println!("<tokens>");
    for token in tokenized {
        println!("{}", token);
    }
    println!("</tokens>");
    Ok(())
}

fn main() {
    match tokenize_file() {
        Ok(_) => (),
        Err(e) => panic!("error!\n{:?}", e),
    }
}
