use super::lexer::{Lexer, LexicalError, Token};
use crate::identifier::Ident;
use crate::term::Term::*;
use crate::term::{BinaryOp, RichTerm, UnaryOp};
use codespan::Files;

fn parse(s: &str) -> Option<RichTerm> {
    let id = Files::new().add("<test>", String::from(s));

    super::grammar::TermParser::new()
        .parse(&id, 0, Lexer::new(&s))
        .ok()
}

fn parse_without_pos(s: &str) -> RichTerm {
    let mut result = parse(s).unwrap();
    result.clean_pos();
    result
}

fn lex(s: &str) -> Result<Vec<(usize, Token, usize)>, LexicalError> {
    Lexer::new(s).collect()
}

fn lex_without_pos(s: &str) -> Result<Vec<Token>, LexicalError> {
    lex(s).map(|v| v.into_iter().map(|(_, tok, _)| tok).collect())
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
    println!("1 true 2: {:?}", parse_without_pos("1 true 2"));

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
    assert!(parse("x1-x-").is_some()); // TODO controversial
    assert!(parse("x1_x_").is_some());
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
    assert!(parse("let x1 = x2 in x3").is_some());
    assert!(parse("x (let x1 = x2 in x3) y").is_some());
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

#[test]
fn enum_terms() {
    assert_eq!(
        parse_without_pos("`foo"),
        Enum(Ident("foo".to_string())).into(),
    );

    assert_eq!(
        parse_without_pos("switch { foo => true, bar => false, _ => 456, } 123"),
        Op1(
            UnaryOp::Switch(
                vec![
                    (Ident("foo".to_string()), Bool(true).into()),
                    (Ident("bar".to_string()), Bool(false).into())
                ]
                .into_iter()
                .collect(),
                Some(Num(456.).into())
            ),
            Num(123.).into()
        )
        .into()
    )
}

#[test]
fn record_terms() {
    assert_eq!(
        parse_without_pos("{ a = 1; b = 2; c = 3;}"),
        Record(
            vec![
                (Ident("a".to_string()), Num(1.).into()),
                (Ident("b".to_string()), Num(2.).into()),
                (Ident("c".to_string()), Num(3.).into())
            ]
            .into_iter()
            .collect()
        )
        .into()
    );

    assert_eq!(
        parse_without_pos("{ a = 1; $123 = (if 4 then 5 else 6); d = 42;}"),
        Op2(
            BinaryOp::DynExtend(
                App(
                    App(Op1(UnaryOp::Ite(), Num(4.).into()).into(), Num(5.).into()).into(),
                    Num(6.).into()
                )
                .into()
            ),
            Num(123.).into(),
            Record(
                vec![
                    (Ident("a".to_string()), Num(1.).into()),
                    (Ident("d".to_string()), Num(42.).into()),
                ]
                .into_iter()
                .collect()
            )
            .into()
        )
        .into()
    );
}

#[test]
fn string_lexing() {
    assert_eq!(
        lex_without_pos("\"Good\" \"strings\""),
        Ok(vec![
            Token::DoubleQuote,
            Token::StrLiteral(String::from("Good")),
            Token::DoubleQuote,
            Token::DoubleQuote,
            Token::StrLiteral(String::from("strings")),
            Token::DoubleQuote,
        ])
    );

    assert_eq!(
        lex_without_pos("\"Good\\nEscape\\t\\\"\""),
        Ok(vec![
            Token::DoubleQuote,
            Token::StrLiteral(String::from("Good\nEscape\t\"")),
            Token::DoubleQuote,
        ])
    );

    assert_eq!(
        lex_without_pos("\"1 + ${ 1 } + 2\""),
        Ok(vec![
            Token::DoubleQuote,
            Token::StrLiteral(String::from("1 + ")),
            Token::DollarBrace,
            Token::NumLiteral(1.0),
            Token::RBrace,
            Token::StrLiteral(String::from(" + 2")),
            Token::DoubleQuote,
        ])
    );

    assert_eq!(
        lex_without_pos("\"1 + ${ \"${ 1 }\" } + 2\""),
        Ok(vec![
            Token::DoubleQuote,
            Token::StrLiteral(String::from("1 + ")),
            Token::DollarBrace,
            Token::DoubleQuote,
            Token::DollarBrace,
            Token::NumLiteral(1.0),
            Token::RBrace,
            Token::DoubleQuote,
            Token::RBrace,
            Token::StrLiteral(String::from(" + 2")),
            Token::DoubleQuote,
        ])
    );
}

#[test]
fn str_escape() {
    assert!(parse("\"bad escape \\g\"").is_none());
    assert_eq!(
        parse_without_pos(r#""str\twith\nescapes""#),
        Str(String::from("str\twith\nescapes")).into(),
    );
    assert_eq!(
        parse_without_pos(r#""\$\${ }$""#),
        Str(String::from("$${ }$")).into(),
    );
}
