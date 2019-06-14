#[macro_use]
extern crate nom;

pub mod ast;
pub mod parser;
pub mod pre;

pub fn translate(unit: &str) -> std::io::Result<Vec<u8>> {
    let _preprocessed_source = pre::process(unit);

    Ok(vec![])
}
