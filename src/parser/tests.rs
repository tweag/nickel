use identifier::Ident;
use term::Term::*;
use term::{BinaryOp, RichTerm, UnaryOp};

#[test]
fn numbers() {
    let parser = super::grammar::TermParser::new();
    assert_eq!(parser.parse("22").unwrap(), Num(22.0).into());
    assert_eq!(parser.parse("22.0").unwrap(), Num(22.0).into());
    assert_eq!(parser.parse("22.22").unwrap(), Num(22.22).into());
    assert_eq!(parser.parse("(22)").unwrap(), Num(22.0).into());
    assert_eq!(parser.parse("((22))").unwrap(), Num(22.0).into());
}

#[test]
fn plus() {
    let parser = super::grammar::TermParser::new();
    assert_eq!(
        parser.parse("3 + 4").unwrap(),
        Op2(BinaryOp::Plus(), Num(3.0).into(), Num(4.).into()).into()
    );
    assert_eq!(
        parser.parse("(true + false) + 4").unwrap(),
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
    let parser = super::grammar::TermParser::new();
    assert_eq!(parser.parse("true").unwrap(), Bool(true).into());
    assert_eq!(parser.parse("false").unwrap(), Bool(false).into());
}

#[test]
fn ite() {
    let parser = super::grammar::TermParser::new();
    assert_eq!(
        parser.parse("if true then 3 else 4").unwrap(),
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
    let parser = super::grammar::TermParser::new();
    assert_eq!(
        parser.parse("1 true 2").unwrap(),
        RichTerm::app(
            RichTerm::app(Num(1.0).into(), Bool(true).into()),
            Num(2.0).into()
        ),
    );
    assert_eq!(
        parser.parse("1 (2 3) 4").unwrap(),
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
    let parser = super::grammar::TermParser::new();
    assert_eq!(
        parser.parse("fun x => x").unwrap(),
        Fun(Ident("x".into()), RichTerm::var("x".into())).into(),
    );
}

#[test]
fn lets() {
    let parser = super::grammar::TermParser::new();
    assert!(parser.parse("let x1 = x2 in x3").is_ok());
    assert!(parser.parse("x (let x1 = x2 in x3) y").is_ok());
}
