#[derive(Debug, PartialEq)]
pub enum Keyword {
    Auto,
    Double,
    Int,
    Struct,
    Break,
    Else,
    Long,
    Switch,
    Case,
    Enum,
    Register,
    Typedef,
    Char,
    Extern,
    Return,
    Union,
    Const,
    Float,
    Short,
    Unsigned,
    Continue,
    For,
    Signed,
    Void,
    Default,
    Goto,
    Sizeof,
    Volatile,
    Do,
    If,
    Static,
    While,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Constant(),
}
