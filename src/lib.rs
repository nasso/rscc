#[macro_use]
extern crate nom;

pub mod ast;
pub mod parser;

use std::fs::File;
use std::io::Read;

fn read_content(path: &str) -> std::io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;

    Ok(contents)
}

pub fn translate(unit: &str) -> std::io::Result<Vec<u8>> {
    let source = read_content(unit)?;

    Ok(source)
}
