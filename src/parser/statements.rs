use crate::ast;

// matches return statements
named!(
    pub p_return_statement<ast::Statement>,
    map!(
        ws!(tuple!(tag!("return"), super::common::p_rvalue, tag!(";"))),
        |(_, v, _)| { ast::Statement::Return(v) }
    )
);

// matches all statements
named!(
    pub p_statement<ast::Statement>,
    alt!(p_return_statement)
);

// tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_return_statement() {
        assert_eq!(
            p_statement(b"\t\n  return \n\t  5    \t;\t  xxx"),
            Ok((
                &b"xxx"[..],
                ast::Statement::Return(ast::expressions::RValue::IntegerConstant(
                    ast::types::IntegerConstant::Int(5)
                ))
            ))
        );
    }
}
