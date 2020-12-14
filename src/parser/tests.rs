use super::lexer::{Lexer, LexicalError, NormalToken, StringToken, Token};
use crate::identifier::Ident;
use crate::term::make as mk_term;
use crate::term::Term::*;
use crate::term::{BinaryOp, RichTerm, StrChunk, UnaryOp};
use crate::{mk_app, mk_switch};
use codespan::Files;

fn parse(s: &str) -> Option<RichTerm> {
    let id = Files::new().add("<test>", String::from(s));

    super::grammar::TermParser::new()
        .parse(id, Lexer::new(&s))
        .map_err(|err| println!("{:?}", err))
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

/// Wrap a single string literal in a `StrChunks`.
fn mk_single_chunk(s: &str) -> RichTerm {
    StrChunks(vec![StrChunk::Literal(String::from(s))]).into()
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
        mk_single_chunk("hello world"),
    );
    assert_eq!(
        parse_without_pos("\"hello \nworld\""),
        mk_single_chunk("hello \nworld")
    );
    assert_eq!(
        parse_without_pos("\"hello Dimension C-132!\""),
        mk_single_chunk("hello Dimension C-132!")
    );

    assert_eq!(
        parse_without_pos("\"hello\" ++ \"World\" ++ \"!!\" "),
        Op2(
            BinaryOp::PlusStr(),
            Op2(
                BinaryOp::PlusStr(),
                mk_single_chunk("hello"),
                mk_single_chunk("World"),
            )
            .into(),
            mk_single_chunk("!!")
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
        mk_app!(mk_term::op1(UnaryOp::Ite(), Bool(true)), Num(3.0), Num(4.0))
    );
}

#[test]
fn applications() {
    assert_eq!(
        parse_without_pos("1 true 2"),
        mk_app!(Num(1.0), Bool(true), Num(2.0))
    );

    assert_eq!(
        parse_without_pos("1 (2 3) 4"),
        mk_app!(Num(1.0), mk_app!(Num(2.0), Num(3.0)), Num(4.0))
    );
}

#[test]
fn variables() {
    assert!(parse("x1_x_").is_some());
}

#[test]
fn functions() {
    assert_eq!(parse_without_pos("fun x => x"), mk_term::id());
}

#[test]
fn lets() {
    assert!(parse("let x1 = x2 in x3").is_some());
    assert!(parse("x (let x1 = x2 in x3) y").is_some());
}

#[test]
fn unary_op() {
    assert_eq!(
        parse_without_pos("%isNum% x"),
        mk_term::op1(UnaryOp::IsNum(), mk_term::var("x"))
    );
    assert_eq!(
        parse_without_pos("%isNum% x y"),
        mk_app!(
            mk_term::op1(UnaryOp::IsNum(), mk_term::var("x")),
            mk_term::var("y")
        ),
    );
}

#[test]
fn enum_terms() {
    assert_eq!(parse_without_pos("`foo"), Enum(Ident::from("foo")).into(),);

    assert_eq!(
        parse_without_pos("switch { foo => true, bar => false, _ => 456, } 123"),
        mk_switch!(Num(123.), ("foo", Bool(true)), ("bar", Bool(false)) ; Num(456.))
    )
}

#[test]
fn record_terms() {
    assert_eq!(
        parse_without_pos("{ a = 1; b = 2; c = 3;}"),
        RecRecord(
            vec![
                (Ident::from("a"), Num(1.).into()),
                (Ident::from("b"), Num(2.).into()),
                (Ident::from("c"), Num(3.).into())
            ]
            .into_iter()
            .collect()
        )
        .into()
    );

    assert_eq!(
        parse_without_pos("{ a = 1; $123 = (if 4 then 5 else 6); d = 42;}"),
        mk_app!(
            mk_term::op2(
                BinaryOp::DynExtend(),
                Num(123.),
                RecRecord(
                    vec![
                        (Ident::from("a"), Num(1.).into()),
                        (Ident::from("d"), Num(42.).into()),
                    ]
                    .into_iter()
                    .collect()
                )
            ),
            mk_app!(mk_term::op1(UnaryOp::Ite(), Num(4.)), Num(5.), Num(6.))
        )
    );
}

#[test]
fn string_lexing() {
    assert_eq!(
        lex_without_pos("\"Good\" \"strings\""),
        Ok(vec![
            Token::Normal(NormalToken::DoubleQuote),
            Token::Str(StringToken::Literal("Good")),
            Token::Normal(NormalToken::DoubleQuote),
            Token::Normal(NormalToken::DoubleQuote),
            Token::Str(StringToken::Literal("strings")),
            Token::Normal(NormalToken::DoubleQuote),
        ])
    );

    assert_eq!(
        lex_without_pos("\"Good\\nEscape\\t\\\"\""),
        Ok(vec![
            Token::Normal(NormalToken::DoubleQuote),
            Token::Str(StringToken::Literal("Good")),
            Token::Str(StringToken::EscapedChar('\n')),
            Token::Str(StringToken::Literal("Escape")),
            Token::Str(StringToken::EscapedChar('\t')),
            Token::Str(StringToken::EscapedChar('\"')),
            Token::Normal(NormalToken::DoubleQuote),
        ])
    );

    assert_eq!(
        lex_without_pos("\"1 + #{ 1 } + 2\""),
        Ok(vec![
            Token::Normal(NormalToken::DoubleQuote),
            Token::Str(StringToken::Literal("1 + ")),
            Token::Str(StringToken::HashBrace),
            Token::Normal(NormalToken::NumLiteral(1.0)),
            Token::Normal(NormalToken::RBrace),
            Token::Str(StringToken::Literal(" + 2")),
            Token::Normal(NormalToken::DoubleQuote),
        ])
    );

    assert_eq!(
        lex_without_pos("\"1 + #{ \"#{ 1 }\" } + 2\""),
        Ok(vec![
            Token::Normal(NormalToken::DoubleQuote),
            Token::Str(StringToken::Literal("1 + ")),
            Token::Str(StringToken::HashBrace),
            Token::Normal(NormalToken::DoubleQuote),
            Token::Str(StringToken::HashBrace),
            Token::Normal(NormalToken::NumLiteral(1.0)),
            Token::Normal(NormalToken::RBrace),
            Token::Normal(NormalToken::DoubleQuote),
            Token::Normal(NormalToken::RBrace),
            Token::Str(StringToken::Literal(" + 2")),
            Token::Normal(NormalToken::DoubleQuote),
        ])
    );
}

#[test]
fn str_escape() {
    assert!(parse("\"bad escape \\g\"").is_none());
    assert_eq!(
        parse_without_pos(r#""str\twith\nescapes""#),
        mk_single_chunk("str\twith\nescapes"),
    );
    assert_eq!(
        parse_without_pos("\"\\#\\#{ }\\#\""),
        mk_single_chunk("##{ }#"),
    );
    assert_eq!(
        parse_without_pos("\"#a#b#c\\#{d#\""),
        mk_single_chunk("#a#b#c#{d#"),
    );
}

/// Regression test for [#230](https://github.com/tweag/nickel/issues/230).
#[test]
fn multiline_str_escape() {
    assert_eq!(
        parse_without_pos(r##"m#"#Hel##lo###"#m"##),
        mk_single_chunk("#Hel##lo###"),
    );
    assert_eq!(
        parse_without_pos(r##"m#"#Hel##{lo###{"#m"##),
        mk_single_chunk("#Hel##{lo###{"),
    );
}

#[test]
fn line_comments() {
    assert_eq!(
        parse_without_pos("// 1 +\n1 + 1// + 3\n//+ 2"),
        parse_without_pos("1 + 1")
    );
    assert_eq!(
        parse_without_pos(
            "{ // Some comment
            field = foo; // Some description
            } // Some other"
        ),
        parse_without_pos("{field = foo}")
    );
}
