mod parse;
mod code;
mod symbols;

use std::env::args;
use std::fs::File;
use std::process;
use std::io::{BufReader, BufRead};

use parse::parse_lines;
use code::generate;

fn main() {
    let mut lines = Vec::new();
    if let Some(filename) = args().nth(1) {
        match File::open(&filename) {
            Ok(f) => {
                let file = BufReader::new(&f);
                for line in file.lines() {
                    lines.push(line.unwrap());
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

    let parsed = parse_lines(lines.as_slice());
    let generated = generate(parsed);
    for line in generated {
        println!("{}", line);
    }
}
