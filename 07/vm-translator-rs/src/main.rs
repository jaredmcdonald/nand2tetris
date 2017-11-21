extern crate rand;

mod parse;
mod code_gen;

use parse::parse;
use code_gen::generate;

use std::path::Path;
use std::env::args;
use std::fs::File;
use std::process;
use std::io::{BufReader, BufRead};

fn main() {
    let mut lines = Vec::new();
    if let Some(filename) = args().nth(1) {
        match File::open(&filename) {
            Ok(f) => {
                let file = BufReader::new(&f);
                for line in file.lines() {
                    lines.push(line.unwrap());
                }
                // haha, ugh 👇
                let name = Path::new(&filename).file_stem().unwrap().to_str().unwrap();
                let parsed = parse(lines.as_slice());
                let generated = generate(parsed.as_slice(), name);
                for instruction in generated {
                    // stdout
                    println!("{}", instruction);
                }
            },
            Err(e) => {
                println!("Error opening file {}: {:?}", filename, e);
                process::exit(1);
            },
        }
    } else {
        println!("must specify a filename as the first argument");
        process::exit(1);
    }
}
