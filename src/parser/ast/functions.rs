#[derive(Debug, PartialEq)]
pub struct FunctionArgumentDefinition(pub super::types::Type, pub String);

#[derive(Debug, PartialEq)]
pub struct FunctionDefinition {
    pub name: String,
    pub return_type: super::types::Type,
    pub arguments: Vec<FunctionArgumentDefinition>,
    pub body: Vec<super::Statement>,
}
