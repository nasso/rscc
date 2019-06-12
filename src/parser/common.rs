use crate::ast;

// matches all reserved keywords
named!(
    pub p_reserved,
    alt!(
        tag!("void")
            | tag!("bool")
            | tag!("char")
            | tag!("short")
            | tag!("int")
            | tag!("long")
            | tag!("float")
            | tag!("double")
    )
);

// matches primitive types
named!(
    pub p_primitive_type<ast::types::PrimitiveType>,
    map!(
        alt!(
            tag!("void")
                | tag!("bool")
                | tag!("char")
                | tag!("short")
                | tag!("int")
                | tag!("long")
                | tag!("float")
                | tag!("double")
        ),
        |r| r.into()
    )
);

// matches all types
named!(
    pub p_type<ast::types::Type>,
    alt!(map!(p_primitive_type, |p| ast::types::Type::Primitive(p)))
);

// matches all identifiers (e.g. variable names, function names, structures...)
named!(
    pub p_identifier<String>,
    // the result is mapped to a String
    map!(
        do_parse!(
            // don't allow reserved keyword
            not!(p_reserved)
                // must start with alphabetic, but can then contain any alphanumeric char, + the _
                >> res: recognize!(tuple!(
                    take_while1!(nom::is_alphabetic),
                    take_while!(|c: u8| c == b'_' || nom::is_alphanumeric(c))
                ))
                >> (res)
        ),
        |r| String::from_utf8_lossy(r).into()
    )
);

// matches function arguments definition: (<type> <identifier>, <type> <identifier>, ...)
named!(
    pub p_func_args_def<Vec<ast::functions::FunctionArgumentDefinition>>,
    // delimited (between parenthesis)
    delimited!(
        // open (
        tag!("("),
        // arguments separated with a comma
        separated_list!(
            // separator
            tag!(","),
            // build arguments (with whitespace between them allowed)
            map!(ws!(tuple!(p_type, p_identifier)), |(t, i)| {
                ast::functions::FunctionArgumentDefinition(t, i)
            })
        ),
        // close )
        tag!(")")
    )
);

// matches integer-suffixes
named!(
    pub p_integer_suffix<ast::types::IntegerSuffix>,
    map!(
        alt!(tag_no_case!("lu") | tag_no_case!("ul") | tag_no_case!("l") | tag_no_case!("u")),
        |s: &[u8]| ast::types::IntegerSuffix {
            long: s.contains(&b'l') || s.contains(&b'L'),
            unsigned: s.contains(&b'u') || s.contains(&b'U'),
        }
    )
);

// matches decimal constants
named!(
    pub p_decimal_constant,
    recognize!(tuple!(
        // starts with a non-zero decimal digit
        take_while1!(|c: u8| c != b'0' && nom::is_digit(c)),
        // zero or more decimal digits
        take_while!(nom::is_digit)
    ))
);

// matches all integer constant literals
named!(
    pub p_integer_constant<ast::types::IntegerConstant>,
    alt!(map!(
        tuple!(p_decimal_constant, opt!(p_integer_suffix)),
        |(d, s)| {
            let d = std::str::from_utf8(d).unwrap();

            match s.map(|s| (s.unsigned, s.long)) {
                Some((true, true)) => {
                    ast::types::IntegerConstant::UnsignedLongInt(u64::from_str_radix(d, 10).unwrap())
                }
                Some((false, true)) => {
                    ast::types::IntegerConstant::LongInt(i64::from_str_radix(d, 10).unwrap())
                }
                Some((true, false)) => {
                    ast::types::IntegerConstant::UnsignedInt(u32::from_str_radix(d, 10).unwrap())
                }
                _ => ast::types::IntegerConstant::Int(i32::from_str_radix(d, 10).unwrap()),
            }
        }
    ))
);

// matches all rvalues
named!(
    pub p_rvalue<ast::expressions::RValue>,

    alt!(
        map!(p_integer_constant, |i| ast::expressions::RValue::IntegerConstant(i))
    )
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_decimal_constant() {
        assert_eq!(
            p_integer_constant(b"123456 xxx"),
            Ok((&b" xxx"[..], ast::types::IntegerConstant::Int(123456)))
        );
    }

    #[test]
    fn parse_long_decimal_constant() {
        assert_eq!(
            p_integer_constant(b"123456l xxx"),
            Ok((&b" xxx"[..], ast::types::IntegerConstant::LongInt(123456)))
        );
    }

    #[test]
    fn parse_unsigned_decimal_constant() {
        assert_eq!(
            p_integer_constant(b"123456u xxx"),
            Ok((
                &b" xxx"[..],
                ast::types::IntegerConstant::UnsignedInt(123456)
            ))
        );

        assert_eq!(
            p_integer_constant(b"123456U xxx"),
            Ok((
                &b" xxx"[..],
                ast::types::IntegerConstant::UnsignedInt(123456)
            ))
        );
    }

    #[test]
    fn parse_unsigned_long_decimal_constant() {
        assert_eq!(
            p_integer_constant(b"123456ul xxx"),
            Ok((
                &b" xxx"[..],
                ast::types::IntegerConstant::UnsignedLongInt(123456)
            ))
        );

        assert_eq!(
            p_integer_constant(b"123456LU xxx"),
            Ok((
                &b" xxx"[..],
                ast::types::IntegerConstant::UnsignedLongInt(123456)
            ))
        );
    }

    #[test]
    fn parse_func_args_def() {
        assert_eq!(p_func_args_def(b"() xxx"), Ok((&b" xxx"[..], vec![])));
        assert_eq!(
            p_func_args_def(b"(  \t\n int \nargc \n   ) xxx"),
            Ok((
                &b" xxx"[..],
                vec![ast::functions::FunctionArgumentDefinition(
                    ast::types::Type::Primitive(ast::types::PrimitiveType::Int),
                    String::from("argc")
                )]
            ))
        );
        assert_eq!(
            p_func_args_def(b"(  \t\n int \nargc \n  ,   char   hi ) xxx"),
            Ok((
                &b" xxx"[..],
                vec![
                    ast::functions::FunctionArgumentDefinition(
                        ast::types::Type::Primitive(ast::types::PrimitiveType::Int),
                        String::from("argc")
                    ),
                    ast::functions::FunctionArgumentDefinition(
                        ast::types::Type::Primitive(ast::types::PrimitiveType::Char),
                        String::from("hi")
                    )
                ]
            ))
        );
    }

    #[test]
    #[should_panic]
    fn parse_func_args_def_panic_on_trailing_comma() {
        p_func_args_def(b"(  \t\n int \nargc \n  ,  ) xxx").unwrap();
    }

    #[test]
    fn parse_primitive_type() {
        assert_eq!(
            p_primitive_type(b"void xxx"),
            Ok((&b" xxx"[..], ast::types::PrimitiveType::Void))
        );
        assert_eq!(
            p_primitive_type(b"char xxx"),
            Ok((&b" xxx"[..], ast::types::PrimitiveType::Char))
        );
        assert_eq!(
            p_primitive_type(b"short xxx"),
            Ok((&b" xxx"[..], ast::types::PrimitiveType::Short))
        );
        assert_eq!(
            p_primitive_type(b"int xxx"),
            Ok((&b" xxx"[..], ast::types::PrimitiveType::Int))
        );
        assert_eq!(
            p_primitive_type(b"long xxx"),
            Ok((&b" xxx"[..], ast::types::PrimitiveType::Long))
        );
        assert_eq!(
            p_primitive_type(b"float xxx"),
            Ok((&b" xxx"[..], ast::types::PrimitiveType::Float))
        );
        assert_eq!(
            p_primitive_type(b"double xxx"),
            Ok((&b" xxx"[..], ast::types::PrimitiveType::Double))
        );
    }

    #[test]
    #[should_panic]
    fn parse_primitive_type_panic_on_invalid_type() {
        p_primitive_type(b"invalid xxx").unwrap();
    }

    #[test]
    fn parse_identifier() {
        assert_eq!(
            p_identifier(b"abcdef xxx"),
            Ok((&b" xxx"[..], String::from("abcdef")))
        );
        assert_eq!(
            p_identifier(b"abcdef xxx"),
            Ok((&b" xxx"[..], String::from("abcdef")))
        );
        assert_eq!(
            p_identifier(b"under_score xxx"),
            Ok((&b" xxx"[..], String::from("under_score")))
        );
        assert_eq!(
            p_identifier(b"abc1 xxx"),
            Ok((&b" xxx"[..], String::from("abc1")))
        );
        assert_eq!(
            p_identifier(b"no-dash xxx"),
            Ok((&b"-dash xxx"[..], String::from("no")))
        );
    }

    #[test]
    #[should_panic]
    fn parse_identifier_panic_when_starts_with_digit() {
        p_identifier(b"1abc xxx").unwrap();
    }

    #[test]
    #[should_panic]
    fn parse_identifier_panic_when_reserved() {
        p_identifier(b"char xxx").unwrap();
    }
}
