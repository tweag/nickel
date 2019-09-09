use continuation::{BinaryOp, UnaryOp};
use identifier::Ident;
use term::Term;
use term::Term::*;

fn app(t1: Term, t2: Term) -> Term {
    App(Box::new(t1), Box::new(t2))
}

#[test]
fn numbers() {
    let parser = super::grammar::TermParser::new();
    assert_eq!(parser.parse("22").unwrap(), Num(22.0));
    assert_eq!(parser.parse("22.0").unwrap(), Num(22.0));
    assert_eq!(parser.parse("22.22").unwrap(), Num(22.22));
    assert_eq!(parser.parse("(22)").unwrap(), Num(22.0));
    assert_eq!(parser.parse("((22))").unwrap(), Num(22.0));
}

#[test]
fn plus() {
    let parser = super::grammar::TermParser::new();
    assert_eq!(
        parser.parse("+ 3 4").unwrap(),
        Op2(BinaryOp::Plus(), Box::new(Num(3.0)), Box::new(Num(4.)))
    );
    assert_eq!(
        parser.parse("+ (+ true false) 4").unwrap(),
        Op2(
            BinaryOp::Plus(),
            Box::new(Op2(
                BinaryOp::Plus(),
                Box::new(Bool(true)),
                Box::new(Bool(false))
            )),
            Box::new(Num(4.))
        )
    );
}

#[test]
fn booleans() {
    let parser = super::grammar::TermParser::new();
    assert_eq!(parser.parse("true").unwrap(), Bool(true));
    assert_eq!(parser.parse("false").unwrap(), Bool(false));
}

#[test]
fn ite() {
    let parser = super::grammar::TermParser::new();
    assert_eq!(
        parser.parse("if true then 3 else 4").unwrap(),
        app(
            app(Op1(UnaryOp::Ite(), Box::new(Bool(true))), Num(3.0)),
            Num(4.0)
        )
    );
}

#[test]
fn applications() {
    let parser = super::grammar::TermParser::new();
    assert_eq!(
        parser.parse("1 true 2").unwrap(),
        app(app(Num(1.0), Bool(true)), Num(2.0)),
    );
    assert_eq!(
        parser.parse("1 (2 3) 4").unwrap(),
        app(app(Num(1.0), app(Num(2.0), Num(3.0))), Num(4.0)),
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
        Fun(vec![Ident("x".into())], Box::new(Var(Ident("x".into())))),
    );
    assert_eq!(
        parser.parse("fun x y => x").unwrap(),
        Fun(
            vec![Ident("x".into()), Ident("y".into())],
            Box::new(Var(Ident("x".into())))
        ),
    );
}

#[test]
fn lets() {
    let parser = super::grammar::TermParser::new();
    assert!(parser.parse("let x1 = x2 in x3").is_ok());
    assert!(parser.parse("x (let x1 = x2 in x3) y").is_ok());
}
