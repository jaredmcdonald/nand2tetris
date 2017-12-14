#![feature(try_from)]

#[macro_use]
extern crate lazy_static;
extern crate rand;
extern crate regex;

mod tokenize;
mod ast;
mod parse;
mod symbols;
mod generate;

use std::env::args;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufReader};
use tokenize::{tokenize, TokenError};
use parse::parse;
use ast::ParseError;
use generate::{generate, CodeGenError};

#[derive(Debug)]
enum CliError {
    NoTargetSpecified,
    IoError(io::Error),
    TokenError(TokenError),
    ParseError(ParseError),
    CodeGenError(CodeGenError),
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

impl From<CodeGenError> for CliError {
    fn from(error: CodeGenError) -> Self {
        CliError::CodeGenError(error)
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

fn compile() -> Result<(), CliError> {
    let contents = read_file_to_string()?;
    let tokens = tokenize(&contents)?;
    let parsed = parse(&tokens)?;
    for line in generate(&parsed)? {
        println!("{}", line);
    }
    Ok(())
}

fn main() {
    match compile() {
        Ok(_) => (),
        Err(e) => panic!("error!\n{:?}", e),
    }
}
