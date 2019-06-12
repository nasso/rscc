#[derive(Debug, PartialEq)]
pub enum PrimitiveType {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
}

#[derive(Debug, PartialEq)]
pub struct IntegerSuffix {
    pub unsigned: bool,
    pub long: bool,
}

#[derive(Debug, PartialEq)]
pub enum IntegerConstant {
    Int(i32),
    UnsignedInt(u32),
    LongInt(i64),
    UnsignedLongInt(u64),
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    PointerTo(Box<Type>),
    ArrayOf(Box<Type>, usize),
}

impl From<&[u8]> for PrimitiveType {
    fn from(s: &[u8]) -> PrimitiveType {
        match s {
            b"void" => PrimitiveType::Void,
            b"char" => PrimitiveType::Char,
            b"short" => PrimitiveType::Short,
            b"int" => PrimitiveType::Int,
            b"long" => PrimitiveType::Long,
            b"float" => PrimitiveType::Float,
            b"double" => PrimitiveType::Double,
            _ => unreachable!(),
        }
    }
}
