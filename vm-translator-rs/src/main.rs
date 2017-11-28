extern crate rand;
extern crate glob;

mod parse;
mod code_gen;

use parse::parse;
use code_gen::{generate, bootstrap};

use glob::glob;
use std::path::Path;
use std::env::args;
use std::fs::{File, metadata};
use std::io::{BufReader, BufRead, Result};

fn compile_file(filename: &str) -> Result<Vec<String>> {
    let f = File::open(&filename)?;
    let file = BufReader::new(&f);
    // todo: see if i can eliminate this unwrap            👇
    let lines: Vec<String> = file.lines().map(|line| line.unwrap()).collect();
    let name = Path::new(&filename).file_stem().unwrap().to_str().unwrap();
    Ok(generate(
        parse(lines.as_slice()).as_slice(),
        name
    ))
}

fn compile_directory(directory_path: &str) -> Vec<String> {
    glob(&format!("{}/*.vm", directory_path)).unwrap()
        .flat_map(|path| compile_file(path.unwrap().to_str().unwrap()).unwrap()) // what could possibly go wrong? 🙀
        .collect() // is there a way to return this without collecting?
}

fn compile() -> Result<()> {
    // is there a more idiomatic way to concatenate `Vec`s than `let mut` + `extend`?
    let mut lines = bootstrap();
    let target = args().nth(1).unwrap_or(".".to_string());
    let fs_metadata = metadata(&target)?;
    let additional_lines = if fs_metadata.is_dir() {
        compile_directory(&target)
    } else {
        compile_file(&target)?
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
        Err(e) => panic!("{}", e),
    }
}
