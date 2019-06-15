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

// white-space (without newline characters)
fn parse_whitespace<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    map(is_a(" \t\r\x0B\x0C"), |_| ())(i)
}

// white-space (with comments and without newline characters)
fn parse_whitespace_and_comments<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, (), E> {
    map(
        many1_count(alt((
            recognize(tuple((tag("//"), take_until("\n")))),
            recognize(tuple((tag("/*"), take_until("*/"), opt(tag("*/"))))),
            recognize(parse_whitespace),
        ))),
        |_| (),
    )(i)
}

// header-name
fn parse_header_name<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, HeaderName<'a>, E> {
    recognize(alt((
        delimited(tag("<"), take_while(|c| c != '\n' && c != '>'), tag(">")),
        delimited(tag("\""), take_while(|c| c != '\n' && c != '"'), tag("\"")),
    )))(i)
}

// identifier
fn parse_identifier<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Identifier<'a>, E> {
    recognize(tuple((
        take_while1(|c: char| c.is_ascii_alphabetic() || c == '_'),
        take_while(|c: char| c.is_ascii_alphanumeric() || c == '_'),
    )))(i)
}

// pp-number
fn parse_pp_number<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Number<'a>, E> {
    recognize(tuple((
        take_while_m_n(1, 1, |c: char| c == '.' || c.is_ascii_digit()),
        opt(many0_count(alt((
            tag_no_case("e+"),
            tag_no_case("e-"),
            take_while_m_n(1, 1, |c: char| {
                c.is_ascii_alphanumeric() || c == '_' || c == '.'
            }),
        )))),
    )))(i)
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
) -> IResult<&'a str, CharacterConstant<'a>, E> {
    recognize(tuple((
        opt(tag("L")),
        tag("'"),
        many1_count(alt((
            take_while_m_n(1, 1, |c: char| !"'\\\n".contains(c)),
            parse_escape_sequence,
        ))),
        tag("'"),
    )))(i)
}

// string-literal
fn parse_string_literal<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, StringLiteral<'a>, E> {
    recognize(tuple((
        opt(tag("L")),
        tag("\""),
        opt(many0_count(alt((
            take_while_m_n(1, 1, |c: char| !"\"\\\n".contains(c)),
            parse_escape_sequence,
        )))),
        tag("\""),
    )))(i)
}

// operator
fn parse_operator<'a, E: ParseError<&'a str>>(
    ctx: ParsingContext,
) -> impl Fn(&'a str) -> IResult<&'a str, Operator<'a>, E> {
    // we can't use a single alt() function because of the limitation on tuple size (21)
    alt((
        map(
            verify(
                cond(
                    ctx.in_expression,
                    alt((tag("["), tag("]"), tag("("), tag(")"), tag("?"), tag(":"))),
                ),
                |r| r.is_some(),
            ),
            |s| s.unwrap(),
        ),
        alt((
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
            tag("<="),
            tag(">="),
            tag("=="),
            tag("!="),
        )),
        alt((
            tag("^"),
            tag("|"),
            tag("&&"),
            tag("||"),
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
            tag(","),
            tag("#"),
            tag("##"),
        )),
    ))
}

// punctuator
fn parse_punctuator<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Operator<'a>, E> {
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
    ))(i)
}

// each non-white-space character that cannot be one of the above
fn parse_other<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    take(1usize)(i)
}

#[derive(Debug, PartialEq)]
pub enum PreprocessingToken<'a> {
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ParsingContext {
    in_include_ppdir: bool,
    in_macro_def: bool,
    in_expression: bool,
}

impl Default for ParsingContext {
    fn default() -> ParsingContext {
        ParsingContext {
            in_include_ppdir: false,
            in_macro_def: false,
            in_expression: false,
        }
    }
}

// root for preprocessing tokens
fn parse_preprocessing_tokens<'a, E: ParseError<&'a str>>(
    ctx: ParsingContext,
) -> impl Fn(&'a str) -> IResult<&'a str, PreprocessingTokens<'a>, E> {
    move |i: &'a str| match many0(alt((
        map(parse_whitespace_and_comments, |_| None),
        map(
            verify(cond(ctx.in_include_ppdir, parse_header_name), |r| {
                r.is_some()
            }),
            |s| Some(PreprocessingToken::HeaderName(s.unwrap())),
        ),
        map(parse_identifier, |s| {
            Some(PreprocessingToken::Identifier(s))
        }),
        map(parse_pp_number, |s| Some(PreprocessingToken::Number(s))),
        map(parse_character_constant, |s| {
            Some(PreprocessingToken::CharacterConstant(s))
        }),
        map(parse_string_literal, |s| {
            Some(PreprocessingToken::StringLiteral(s))
        }),
        map(parse_operator(ctx), |s| {
            Some(PreprocessingToken::Operator(s))
        }),
        map(parse_punctuator, |s| {
            Some(PreprocessingToken::Punctuator(s))
        }),
        map(parse_other, |s| Some(PreprocessingToken::Other(s))),
    )))(i)
    {
        Ok((i, o)) => Ok((i, o.into_iter().filter_map(|i| i).collect())),
        Err(e) => Err(e),
    }
}

// Pre-processing file
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

fn parse_control_line<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ControlLine<'a>, E> {
    fn parse_replacement_list<'a, E: ParseError<&'a str>>(
        i: &'a str,
    ) -> IResult<&'a str, ReplacementList<'a>, E> {
        opt(parse_preprocessing_tokens(ParsingContext {
            in_macro_def: true,
            ..ParsingContext::default()
        }))(i)
    }

    fn parse_include<'a, E: ParseError<&'a str>>(
        i: &'a str,
    ) -> IResult<&'a str, ControlLine<'a>, E> {
        map(
            tuple((
                tag("#"),
                opt(is_a(" \t")),
                tag("include"),
                opt(is_a(" \t")),
                parse_preprocessing_tokens(ParsingContext {
                    in_include_ppdir: true,
                    ..ParsingContext::default()
                }),
            )),
            |(_, _, _, _, tokens)| ControlLine::Include(tokens),
        )(i)
    }

    fn parse_define<'a, E: ParseError<&'a str>>(
        i: &'a str,
    ) -> IResult<&'a str, ControlLine<'a>, E> {
        map(
            tuple((
                tag("#"),
                opt(is_a(" \t")),
                tag("define"),
                opt(is_a(" \t")),
                parse_identifier,
                opt(is_a(" \t")),
                parse_replacement_list,
            )),
            |(_, _, _, _, id, _, tokens)| ControlLine::Define(id, tokens),
        )(i)
    }

    fn parse_undef<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ControlLine<'a>, E> {
        map(
            tuple((
                tag("#"),
                opt(is_a(" \t")),
                tag("undef"),
                opt(is_a(" \t")),
                parse_identifier,
            )),
            |(_, _, _, _, id)| ControlLine::Undef(id),
        )(i)
    }

    map(
        tuple((
            opt(is_a(" \t\n")),
            alt((parse_include, parse_define, parse_undef)),
            opt(is_a(" \t\n")),
        )),
        |(_, l, _)| l,
    )(i)
}

#[derive(Debug, PartialEq)]
pub enum GroupPart<'a> {
    Tokens(PreprocessingTokens<'a>),
    IfSection,
    ControlLine(ControlLine<'a>),
}

type PreprocessingFile<'a> = Vec<GroupPart<'a>>;

fn parse_group_part<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, GroupPart<'a>, E> {
    alt((
        map(parse_control_line, |l| GroupPart::ControlLine(l)),
        map(
            tuple((
                parse_preprocessing_tokens(ParsingContext::default()),
                opt(tag("\n")),
            )),
            |(t, _)| GroupPart::Tokens(t),
        ),
    ))(i)
}

// root preprocessing files
pub fn parse_preprocessing_file<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, PreprocessingFile<'a>, E> {
    map(opt(many0(parse_group_part)), |s| match s {
        Some(g) => g,
        None => Vec::new(),
    })(i)
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
            Ok(("", ("<hello world>")))
        );
    }

    #[test]
    fn test_parse_identifier() {
        assert_eq!(
            parse_identifier::<(&str, ErrorKind)>("hello"),
            Ok(("", ("hello")))
        );

        assert_eq!(
            parse_identifier::<(&str, ErrorKind)>("hello__"),
            Ok(("", ("hello__")))
        );

        assert_eq!(
            parse_identifier::<(&str, ErrorKind)>("hello__123456"),
            Ok(("", ("hello__123456")))
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
            Ok(("", ("123456")))
        );

        assert_eq!(
            parse_pp_number::<(&str, ErrorKind)>("156.123456E+89898.4894"),
            Ok(("", ("156.123456E+89898.4894")))
        );
    }

    #[test]
    fn test_parse_character_constant() {
        assert_eq!(
            parse_character_constant::<(&str, ErrorKind)>("'a'"),
            Ok(("", ("'a'")))
        );

        assert_eq!(
            parse_character_constant::<(&str, ErrorKind)>("'\\123'"),
            Ok(("", ("'\\123'")))
        );

        assert_eq!(
            parse_character_constant::<(&str, ErrorKind)>("'\\xbabafefe123456'"),
            Ok(("", ("'\\xbabafefe123456'")))
        );
    }

    #[test]
    fn test_parse_string_literal() {
        assert_eq!(
            parse_string_literal::<(&str, ErrorKind)>("\"hello a hi\""),
            Ok(("", ("\"hello a hi\"")))
        );

        assert_eq!(
            parse_string_literal::<(&str, ErrorKind)>("\"hello \\123 hi\""),
            Ok(("", ("\"hello \\123 hi\"")))
        );

        assert_eq!(
            parse_string_literal::<(&str, ErrorKind)>("\"hello \\xbabafefe123456 hi\""),
            Ok(("", ("\"hello \\xbabafefe123456 hi\"")))
        );
    }

    #[test]
    fn test_parse_control_line() {
        assert_eq!(
            parse_control_line::<(&str, ErrorKind)>("#include <stdio.h>"),
            Ok((
                "",
                ControlLine::Include(vec![PreprocessingToken::HeaderName("<stdio.h>")])
            ))
        );
    }

    #[test]
    fn test_parse_control_line_whitespace() {
        assert_eq!(
            parse_control_line::<(&str, ErrorKind)>(" \t \n\n \t # \t include\t <stdio.h> \t"),
            Ok((
                "",
                ControlLine::Include(vec![PreprocessingToken::HeaderName("<stdio.h>")])
            ))
        );
    }

    #[test]
    #[should_panic]
    fn test_tokens() {
        let source = r#"
            #include <stdio.h>

            // This is a comment!
            int main(void)
            {
                printf("Hello World!\n");
                return 0;

                /*
                Multi
                Line
                Comment
                */
            }
            "#;

        assert_eq!(
            parse_preprocessing_file::<nom::error::VerboseError<&str>>(source).or_else(
                |e| match e {
                    nom::Err::Incomplete(_) => Err("incomplete!".to_string()),
                    nom::Err::Error(e) | nom::Err::Failure(e) => {
                        Err(nom::error::convert_error(source, e))
                    }
                }
            ),
            Ok((
                "",
                vec![
                    // include directive
                    GroupPart::ControlLine(ControlLine::Include(vec![
                        PreprocessingToken::HeaderName("<stdio.h>")
                    ])),
                    // rest of the program (tokens)
                    GroupPart::Tokens(vec![]),
                    GroupPart::Tokens(vec![]),
                    GroupPart::Tokens(vec![
                        PreprocessingToken::Identifier("int"),
                        PreprocessingToken::Identifier("main"),
                        PreprocessingToken::Punctuator("("),
                        PreprocessingToken::Identifier("void"),
                        PreprocessingToken::Punctuator(")"),
                    ]),
                    GroupPart::Tokens(vec![PreprocessingToken::Punctuator("{"),]),
                    GroupPart::Tokens(vec![
                        PreprocessingToken::Identifier("printf"),
                        PreprocessingToken::Punctuator("("),
                        PreprocessingToken::StringLiteral("\"Hello World!\\n\""),
                        PreprocessingToken::Punctuator(")"),
                        PreprocessingToken::Punctuator(";"),
                    ]),
                    GroupPart::Tokens(vec![
                        PreprocessingToken::Identifier("return"),
                        PreprocessingToken::Number("0"),
                        PreprocessingToken::Punctuator(";"),
                    ]),
                    GroupPart::Tokens(vec![]),
                    GroupPart::Tokens(vec![]),
                    GroupPart::Tokens(vec![PreprocessingToken::Punctuator("}"),]),
                    GroupPart::Tokens(vec![]),
                ]
            ))
        )
    }
}
