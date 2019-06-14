pub mod expressions;
pub mod functions;
pub mod token;
pub mod types;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Return(expressions::RValue),
}
