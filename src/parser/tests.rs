use crate::identifier::Ident;
use crate::term::Term::*;
use crate::term::{BinaryOp, RichTerm, UnaryOp};

fn parse_without_pos(s: &str) -> RichTerm {
    let parser = super::grammar::TermParser::new();
    let mut result = parser.parse(s).unwrap();
    result.clean_pos();
    result
}

#[test]
fn numbers() {
    assert_eq!(parse_without_pos("22"), Num(22.0).into());
    assert_eq!(parse_without_pos("22.0"), Num(22.0).into());
    assert_eq!(parse_without_pos("22.22"), Num(22.22).into());
    assert_eq!(parse_without_pos("(22)"), Num(22.0).into());
    assert_eq!(parse_without_pos("((22))"), Num(22.0).into());
}

#[test]
fn strings() {
    assert_eq!(
        parse_without_pos("\"hello world\""),
        Str("hello world".to_string()).into()
    );
    assert_eq!(
        parse_without_pos("\"hello \nworld\""),
        Str("hello \nworld".to_string()).into()
    );
    assert_eq!(
        parse_without_pos("\"hello Dimension C-132!\""),
        Str("hello Dimension C-132!".to_string()).into()
    );

    assert_eq!(
        parse_without_pos("\"hello\" ++ \"World\" ++ \"!!\" "),
        Op2(
            BinaryOp::PlusStr(),
            Str("hello".to_string()).into(),
            Op2(
                BinaryOp::PlusStr(),
                Str("World".to_string()).into(),
                Str("!!".to_string()).into()
            )
            .into()
        )
        .into()
    )
}

#[test]
fn plus() {
    assert_eq!(
        parse_without_pos("3 + 4"),
        Op2(BinaryOp::Plus(), Num(3.0).into(), Num(4.).into()).into()
    );
    assert_eq!(
        parse_without_pos("(true + false) + 4"),
        Op2(
            BinaryOp::Plus(),
            Op2(BinaryOp::Plus(), Bool(true).into(), Bool(false).into()).into(),
            Num(4.).into()
        )
        .into()
    );
}

#[test]
fn booleans() {
    assert_eq!(parse_without_pos("true"), Bool(true).into());
    assert_eq!(parse_without_pos("false"), Bool(false).into());
}

#[test]
fn ite() {
    assert_eq!(
        parse_without_pos("if true then 3 else 4"),
        RichTerm::app(
            RichTerm::app(
                Op1(UnaryOp::Ite(), Bool(true).into()).into(),
                Num(3.0).into()
            ),
            Num(4.0).into()
        )
    );
}

#[test]
fn applications() {
    assert_eq!(
        parse_without_pos("1 true 2"),
        RichTerm::app(
            RichTerm::app(Num(1.0).into(), Bool(true).into()),
            Num(2.0).into()
        ),
    );
    assert_eq!(
        parse_without_pos("1 (2 3) 4"),
        RichTerm::app(
            RichTerm::app(
                Num(1.0).into(),
                RichTerm::app(Num(2.0).into(), Num(3.0).into())
            ),
            Num(4.0).into()
        ),
    );
}

#[test]
fn variables() {
    let parser = super::grammar::TermParser::new();
    assert!(parser.parse("x1-x-").is_ok()); // TODO controversial
    assert!(parser.parse("x1_x_").is_ok());
}

#[test]
fn functions() {
    assert_eq!(
        parse_without_pos("fun x => x"),
        Fun(Ident("x".into()), RichTerm::var("x".into())).into(),
    );
}

#[test]
fn lets() {
    let parser = super::grammar::TermParser::new();
    assert!(parser.parse("let x1 = x2 in x3").is_ok());
    assert!(parser.parse("x (let x1 = x2 in x3) y").is_ok());
}

#[test]
fn unary_op() {
    assert_eq!(
        parse_without_pos("isZero x"),
        Op1(UnaryOp::IsZero(), RichTerm::var("x".to_string())).into()
    );
    assert_eq!(
        parse_without_pos("isZero x y"),
        RichTerm::app(
            Op1(UnaryOp::IsZero(), RichTerm::var("x".to_string())).into(),
            RichTerm::var("y".to_string())
        )
    );
}
