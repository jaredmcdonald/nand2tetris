#![feature(try_from)]

extern crate regex;
#[macro_use]
extern crate lazy_static;

mod tokenize;
mod parse;
mod symbols;

use std::env::args;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufReader};
use tokenize::{tokenize, TokenError};
use parse::{parse, ParseError};

#[derive(Debug)]
enum CliError {
    NoTargetSpecified,
    TokenError(TokenError),
    ParseError(ParseError),
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

impl From<ParseError> for CliError {
    fn from(error: ParseError) -> Self {
        CliError::ParseError(error)
    }
}

fn read_file_to_string() -> Result<String, CliError> {
    let target = args().nth(1).ok_or(CliError::NoTargetSpecified)?;
    let file = File::open(target)?;
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;
    Ok(contents)
}

fn tokenize_and_parse_file() -> Result<(), CliError> {
    let contents = read_file_to_string()?;
    let tokens = tokenize(&contents)?;
    let parsed = parse(&tokens)?;
    println!("{}", parsed);
    Ok(())
}

fn main() {
    match tokenize_and_parse_file() {
        Ok(_) => (),
        Err(e) => panic!("error!\n{:?}", e),
    }
}
