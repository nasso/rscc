#[macro_use]
extern crate nom;

mod parser;

use std::fs::File;
use std::io::Read;

fn read_content(path: &str) -> std::io::Result<String> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    Ok(contents)
}

pub fn translate(unit: &str) -> &[u8] {
    &[]
}
