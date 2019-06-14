use nom::branch::*;
use nom::bytes::complete::*;
use nom::combinator::*;
use nom::error::ParseError;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

type HeaderName<'a> = &'a str;
type Identifier<'a> = &'a str;
type Number<'a> = &'a str;
type CharacterConstant<'a> = &'a str;
type StringLiteral<'a> = &'a str;
type Operator<'a> = &'a str;
type Punctuator<'a> = &'a str;

#[derive(Debug, PartialEq)]
pub enum PreprocessingToken<'a> {
    Whitespace,
    Newline,
    HeaderName(HeaderName<'a>),
    Identifier(Identifier<'a>),
    Number(Number<'a>),
    CharacterConstant(CharacterConstant<'a>),
    StringLiteral(StringLiteral<'a>),
    Operator(Operator<'a>),
    Punctuator(Punctuator<'a>),
    Other(&'a str),
}

type PreprocessingTokens<'a> = Vec<PreprocessingToken<'a>>;
type ReplacementList<'a> = Option<PreprocessingTokens<'a>>;

#[derive(Debug, PartialEq)]
pub enum ControlLine<'a> {
    Include(PreprocessingTokens<'a>),
    Define(Identifier<'a>, ReplacementList<'a>),
    DefineFunc(Identifier<'a>, Vec<Identifier<'a>>, ReplacementList<'a>),
    Undef(Identifier<'a>),
    Line(PreprocessingTokens<'a>),
    Error(Option<PreprocessingTokens<'a>>),
    Pragma(Option<PreprocessingTokens<'a>>),
    Newline,
}

#[derive(Debug, PartialEq)]
pub enum GroupPart<'a> {
    Tokens(PreprocessingTokens<'a>),
    IfSection,
    ControlLine(ControlLine<'a>),
}

#[derive(Debug, PartialEq)]
pub struct PreprocessingFile<'a> {
    groups: Vec<GroupPart<'a>>,
}

// white-space (without newline characters)
fn parse_whitespace<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, PreprocessingToken<'a>, E> {
    map(
        many1_count(alt((
            recognize(tuple((tag("//"), take_until("\n")))),
            recognize(tuple((tag("/*"), take_until("*/"), opt(tag("*/"))))),
            recognize(is_a(" \t\r")),
        ))),
        |_| PreprocessingToken::Whitespace,
    )(i)
}

// white-space (without newline characters)
fn parse_newline<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, PreprocessingToken<'a>, E> {
    map(tag("\n"), |_| PreprocessingToken::Newline)(i)
}

// header-name
fn parse_header_name<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, PreprocessingToken<'a>, E> {
    map(
        recognize(alt((
            delimited(tag("<"), take_while(|c| c != '\n' && c != '>'), tag(">")),
            delimited(tag("\""), take_while(|c| c != '\n' && c != '"'), tag("\"")),
        ))),
        |s| PreprocessingToken::HeaderName(s),
    )(i)
}

// identifier
fn parse_identifier<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, PreprocessingToken<'a>, E> {
    map(
        recognize(tuple((
            take_while1(|c: char| c.is_ascii_alphabetic() || c == '_'),
            take_while(|c: char| c.is_ascii_alphanumeric() || c == '_'),
        ))),
        |s| PreprocessingToken::Identifier(s),
    )(i)
}

// pp-number
fn parse_pp_number<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, PreprocessingToken<'a>, E> {
    map(
        recognize(tuple((
            take_while_m_n(1, 1, |c: char| c == '.' || c.is_ascii_digit()),
            opt(many0_count(alt((
                tag_no_case("e+"),
                tag_no_case("e-"),
                take_while_m_n(1, 1, |c: char| {
                    c.is_ascii_alphanumeric() || c == '_' || c == '.'
                }),
            )))),
        ))),
        |s| PreprocessingToken::Number(s),
    )(i)
}

// escape-sequence
fn parse_escape_sequence<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    alt((
        // simple escape sequence
        tag("\\'"),
        tag("\\\""),
        tag("\\?"),
        tag("\\\\"),
        tag("\\a"),
        tag("\\b"),
        tag("\\f"),
        tag("\\n"),
        tag("\\r"),
        tag("\\t"),
        tag("\\v"),
        // octal escape sequence
        recognize(tuple((
            tag("\\"),
            take_while_m_n(1, 3, |c: char| "01234567".contains(c)),
        ))),
        // hexadecimal escape sequence
        recognize(tuple((
            tag("\\x"),
            take_while1(|c: char| c.is_ascii_hexdigit()),
        ))),
    ))(i)
}

// character-constant
fn parse_character_constant<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, PreprocessingToken<'a>, E> {
    map(
        recognize(tuple((
            opt(tag("L")),
            tag("'"),
            many1_count(alt((
                take_while_m_n(1, 1, |c: char| !"'\\\n".contains(c)),
                parse_escape_sequence,
            ))),
            tag("'"),
        ))),
        |s| PreprocessingToken::CharacterConstant(s),
    )(i)
}

// string-literal
fn parse_string_literal<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, PreprocessingToken<'a>, E> {
    map(
        recognize(tuple((
            opt(tag("L")),
            tag("\""),
            opt(many0_count(alt((
                take_while_m_n(1, 1, |c: char| !"\"\\\n".contains(c)),
                parse_escape_sequence,
            )))),
            tag("\""),
        ))),
        |s| PreprocessingToken::StringLiteral(s),
    )(i)
}

// operator
fn parse_operator<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, PreprocessingToken<'a>, E> {
    // we can't use a single alt() function because of the limitation on tuple size (21)
    map(
        alt((
            alt((
                tag("["),
                tag("]"),
                tag("("),
                tag(")"),
                tag("."),
                tag("->"),
                tag("++"),
                tag("--"),
                tag("&"),
                tag("*"),
                tag("+"),
                tag("-"),
                tag("~"),
                tag("!"),
                tag("sizeof"),
                tag("/"),
                tag("%"),
                tag("<<"),
                tag(">>"),
                tag("<"),
                tag(">"),
            )),
            alt((
                tag("<="),
                tag(">="),
                tag("=="),
                tag("!="),
                tag("^"),
                tag("|"),
                tag("&&"),
                tag("||"),
                tag("?"),
                tag(":"),
                tag("="),
                tag("*="),
                tag("/="),
                tag("%="),
                tag("+="),
                tag("-="),
                tag("<<="),
                tag(">>="),
                tag("&="),
                tag("^="),
                tag("|="),
            )),
            tag(","),
            tag("#"),
            tag("##"),
        )),
        |s| PreprocessingToken::Operator(s),
    )(i)
}

// punctuator
fn parse_punctuator<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, PreprocessingToken<'a>, E> {
    map(
        alt((
            tag("["),
            tag("]"),
            tag("("),
            tag(")"),
            tag("{"),
            tag("}"),
            tag("*"),
            tag(","),
            tag(":"),
            tag("="),
            tag(";"),
            tag("..."),
            tag("#"),
        )),
        |s| PreprocessingToken::Punctuator(s),
    )(i)
}

// each non-white-space character that cannot be one of the above
fn parse_other<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, PreprocessingToken<'a>, E> {
    map(take(1usize), |s| PreprocessingToken::Other(s))(i)
}

// root
pub fn parse_preprocessing_tokens<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Vec<PreprocessingToken<'a>>, E> {
    many0(alt((
        parse_whitespace,
        parse_newline,
        parse_header_name,
        parse_identifier,
        parse_pp_number,
        parse_character_constant,
        parse_string_literal,
        parse_operator,
        parse_punctuator,
        parse_other,
    )))(i)
}

// tests
#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::ErrorKind;

    #[test]
    fn test_parse_header_name() {
        assert_eq!(
            parse_header_name::<(&str, ErrorKind)>("<hello world>"),
            Ok(("", PreprocessingToken::HeaderName("<hello world>")))
        );
    }

    #[test]
    fn test_parse_identifier() {
        assert_eq!(
            parse_identifier::<(&str, ErrorKind)>("hello"),
            Ok(("", PreprocessingToken::Identifier("hello")))
        );

        assert_eq!(
            parse_identifier::<(&str, ErrorKind)>("hello__"),
            Ok(("", PreprocessingToken::Identifier("hello__")))
        );

        assert_eq!(
            parse_identifier::<(&str, ErrorKind)>("hello__123456"),
            Ok(("", PreprocessingToken::Identifier("hello__123456")))
        );
    }

    #[test]
    #[should_panic]
    fn test_parse_identifier_fail() {
        parse_identifier::<(&str, ErrorKind)>("123hello").unwrap();
    }

    #[test]
    fn test_parse_pp_number() {
        assert_eq!(
            parse_pp_number::<(&str, ErrorKind)>("123456"),
            Ok(("", PreprocessingToken::Number("123456")))
        );

        assert_eq!(
            parse_pp_number::<(&str, ErrorKind)>("156.123456E+89898.4894"),
            Ok(("", PreprocessingToken::Number("156.123456E+89898.4894")))
        );
    }

    #[test]
    fn test_parse_character_constant() {
        assert_eq!(
            parse_character_constant::<(&str, ErrorKind)>("'a'"),
            Ok(("", PreprocessingToken::CharacterConstant("'a'")))
        );

        assert_eq!(
            parse_character_constant::<(&str, ErrorKind)>("'\\123'"),
            Ok(("", PreprocessingToken::CharacterConstant("'\\123'")))
        );

        assert_eq!(
            parse_character_constant::<(&str, ErrorKind)>("'\\xbabafefe123456'"),
            Ok((
                "",
                PreprocessingToken::CharacterConstant("'\\xbabafefe123456'")
            ))
        );
    }

    #[test]
    fn test_parse_string_literal() {
        assert_eq!(
            parse_string_literal::<(&str, ErrorKind)>("\"hello a hi\""),
            Ok(("", PreprocessingToken::StringLiteral("\"hello a hi\"")))
        );

        assert_eq!(
            parse_string_literal::<(&str, ErrorKind)>("\"hello \\123 hi\""),
            Ok(("", PreprocessingToken::StringLiteral("\"hello \\123 hi\"")))
        );

        assert_eq!(
            parse_string_literal::<(&str, ErrorKind)>("\"hello \\xbabafefe123456 hi\""),
            Ok((
                "",
                PreprocessingToken::StringLiteral("\"hello \\xbabafefe123456 hi\"")
            ))
        );
    }

    #[test]
    fn test_tokens() {
        assert_eq!(
            parse_preprocessing_tokens::<(&str, ErrorKind)>(
                "
                #include <stdio.h>

                // This is a comment!
                int main(void)
                {
                    printf(\"Hello World!\\n\");
                    return 0;

                    /*
                    Multi
                    Line
                    Comment
                    */
                }
                "
            ),
            Ok((
                "",
                vec![
                    PreprocessingToken::Newline,
                    PreprocessingToken::Whitespace,
                    PreprocessingToken::Punctuator("#"),
                    PreprocessingToken::Identifier("include"),
                    PreprocessingToken::Whitespace,
                    PreprocessingToken::HeaderName("<stdio.h>"),
                    PreprocessingToken::Newline,
                    PreprocessingToken::Newline,
                    PreprocessingToken::Whitespace,
                    PreprocessingToken::Newline,
                    PreprocessingToken::Whitespace,
                    PreprocessingToken::Identifier("int"),
                    PreprocessingToken::Whitespace,
                    PreprocessingToken::Identifier("main"),
                    PreprocessingToken::Punctuator("("),
                    PreprocessingToken::Identifier("void"),
                    PreprocessingToken::Punctuator(")"),
                    PreprocessingToken::Newline,
                    PreprocessingToken::Whitespace,
                    PreprocessingToken::Punctuator("{"),
                    PreprocessingToken::Newline,
                    PreprocessingToken::Whitespace,
                    PreprocessingToken::Identifier("printf"),
                    PreprocessingToken::Punctuator("("),
                    PreprocessingToken::StringLiteral("\"Hello World!\\n\""),
                    PreprocessingToken::Punctuator(")"),
                    PreprocessingToken::Punctuator(";"),
                    PreprocessingToken::Newline,
                    PreprocessingToken::Whitespace,
                    PreprocessingToken::Identifier("return"),
                    PreprocessingToken::Whitespace,
                    PreprocessingToken::Number("0"),
                    PreprocessingToken::Punctuator(";"),
                    PreprocessingToken::Newline,
                    PreprocessingToken::Newline,
                    PreprocessingToken::Whitespace,
                    PreprocessingToken::Newline,
                    PreprocessingToken::Whitespace,
                    PreprocessingToken::Punctuator("}"),
                    PreprocessingToken::Newline,
                    PreprocessingToken::Whitespace,
                ]
            ))
        )
    }
}
