pub mod expressions;
pub mod functions;
pub mod types;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Return(expressions::RValue),
}
