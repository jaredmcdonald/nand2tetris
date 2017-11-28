extern crate rand;
extern crate glob;

mod parse;
mod code_gen;

use parse::{parse, ParseError};
use code_gen::{generate, bootstrap};

use glob::{glob, PatternError, GlobError};
use std::path::{Path, PathBuf};
use std::env::args;
use std::fs::{File, metadata};
use std::io::{self, BufReader, BufRead};

// https://doc.rust-lang.org/std/convert/trait.From.html
#[derive(Debug)]
enum CliError {
    IoError(io::Error),
    PatternError(PatternError),
    GlobError(GlobError),
    ParseError(ParseError),
}

impl From<io::Error> for CliError {
    fn from(error: io::Error) -> Self {
        CliError::IoError(error)
    }
}

impl From<PatternError> for CliError {
    fn from(error: PatternError) -> Self {
        CliError::PatternError(error)
    }
}

impl From<GlobError> for CliError {
    fn from(error: GlobError) -> Self {
        CliError::GlobError(error)
    }
}

impl From<ParseError> for CliError {
    fn from(error: ParseError) -> Self {
        CliError::ParseError(error)
    }
}

fn compile_file(filename: &Path) -> Result<Vec<String>, CliError> {
    let file = File::open(&filename)?;
    let file_reader = BufReader::new(&file);
    let lines = file_reader.lines().collect::<Result<Vec<String>, _>>();
    let name = filename.file_stem().unwrap().to_str().unwrap();
    Ok(generate(
        parse(lines?.as_slice())?.as_slice(),
        name
    ))
}

fn compile_directory(directory_path: &str) -> Result<Vec<String>, CliError> {
    let filenames = glob(&format!("{}/*.vm", directory_path))?
        .collect::<Result<Vec<PathBuf>, _>>()?;

    let mut compiled = Vec::new();
    for result in filenames.iter().map(|path_buf| compile_file(path_buf.as_path())) {
        compiled.extend(result?);
    }
    Ok(compiled)
}

fn compile() -> Result<(), CliError> {
    // is there a more idiomatic way to concatenate `Vec`s than `let mut` + `extend`?
    let mut lines = bootstrap();
    let target = args().nth(1).unwrap_or(".".to_string());
    let fs_metadata = metadata(&target)?;
    let additional_lines = if fs_metadata.is_dir() {
        compile_directory(&target)?
    } else {
        compile_file(Path::new(&target))?
    };
    lines.extend(additional_lines);
    for line in lines {
        // just print assembly to stdout, let the user direct it where she prefers
        println!("{}", line)
    }
    Ok(())
}

fn main() {
    match compile() {
        Ok(_) => (),
        Err(e) => panic!("error!\n{:?}", e),
    }
}
