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
use std::io::{BufReader, BufRead};

fn compile_file(filename: &str) -> Vec<String> {
    match File::open(&filename) {
        Ok(f) => {
            let file = BufReader::new(&f);
            let lines: Vec<String> = file.lines().map(|line| line.unwrap()).collect();
            // haha, ugh - what's the idiomatic way to avoid all this 👇 ?
            let name = Path::new(&filename).file_stem().unwrap().to_str().unwrap();
            generate(
                parse(lines.as_slice()).as_slice(),
                name
            )
        },
        Err(e) => {
            panic!("Error opening file {}: {:?}", filename, e);
        },
    }
}

fn compile_directory(directory_path: &str) -> Vec<String> {
    glob(&format!("{}/*.vm", directory_path)).unwrap()
        .flat_map(|path| compile_file(path.unwrap().to_str().unwrap())) // what could possibly go wrong? 🙀
        .collect() // is there a way to return this without collecting?
}

fn main() {
    // is there a more idiomatic way to concatenate `Vec`s than `let mut` + `extend`?
    let mut lines = bootstrap();
    lines.extend(
        // how to avoid all this `if let` nesting when we have a bunch of dependent results/options/etc?
        if let Some(target) = args().nth(1) {
            if let Ok(fs_metadata) = metadata(&target) {
                if fs_metadata.is_dir() {
                    compile_directory(&target)
                } else {
                    compile_file(&target)
                }
            } else {
                panic!("error reading path {}", target);
            }
        } else {
            panic!("must specify a filename or directory target as the first argument");
        }
    );
    for line in lines {
        // just print assembly to stdout, let the user direct it where she prefers
        println!("{}", line)
    }
}
