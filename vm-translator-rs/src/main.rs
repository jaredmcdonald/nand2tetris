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

fn parse_file(filename: &str) -> Vec<String> {
    match File::open(&filename) {
        Ok(f) => {
            let mut lines = Vec::new();
            let file = BufReader::new(&f);
            for line in file.lines() {
                lines.push(line.unwrap());
            }
            // haha, ugh                                  👇        👇      👇
            let name = Path::new(&filename).file_stem().unwrap().to_str().unwrap();
            let parsed = parse(lines.as_slice());
            let generated = generate(parsed.as_slice(), name);
            generated
        },
        Err(e) => {
            panic!("Error opening file {}: {:?}", filename, e);
        },
    }
}

fn parse_directory(directory_path: &str) -> Vec<String> {
    glob(&format!("{}/*.vm", directory_path)).unwrap()
        .flat_map(|path| parse_file(path.unwrap().to_str().unwrap())) // what could possibly go wrong? 🙀
        .collect()
}

fn main() {
    let mut lines = bootstrap();
    lines.extend(
        if let Some(filename) = args().nth(1) {
            if let Ok(file_metadata) = metadata(&filename) {
                if file_metadata.is_dir() {
                    parse_directory(&filename)
                } else {
                    parse_file(&filename)
                }
            } else {
                panic!("error reading file {}", filename);
            }
        } else {
            panic!("must specify a filename as the first argument");
        }
    );
    for line in lines {
        println!("{}", line)
    }
}
