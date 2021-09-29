use super::lexer::{Lexer, MultiStringToken, NormalToken, StringToken, Token};
use crate::error::ParseError;
use crate::identifier::Ident;
use crate::parser::error::ParseError as InternalParseError;
use crate::term::make as mk_term;
use crate::term::Term::*;
use crate::term::{BinaryOp, RichTerm, StrChunk, UnaryOp};
use crate::{mk_app, mk_switch};
use assert_matches::assert_matches;
use codespan::Files;

fn parse(s: &str) -> Result<RichTerm, ParseError> {
    let id = Files::new().add("<test>", String::from(s));

    super::grammar::TermParser::new()
        .parse(id, Lexer::new(&s))
        .map_err(|err| ParseError::from_lalrpop(err, id))
}

fn parse_without_pos(s: &str) -> RichTerm {
    let mut result = parse(s).unwrap();
    result.clean_pos();
    result
}

fn lex(s: &str) -> Result<Vec<(usize, Token, usize)>, InternalParseError> {
    Lexer::new(s).collect()
}

fn lex_without_pos(s: &str) -> Result<Vec<Token>, InternalParseError> {
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
            BinaryOp::StrConcat(),
            Op2(
                BinaryOp::StrConcat(),
                mk_single_chunk("hello"),
                mk_single_chunk("World"),
            )
            .into(),
            mk_single_chunk("!!"),
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
            Num(4.).into(),
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
    assert!(parse("x1_x_").is_ok());
}

#[test]
fn functions() {
    assert_eq!(parse_without_pos("fun x => x"), mk_term::id());
}

#[test]
fn lets() {
    assert_matches!(parse("let x1 = x2 in x3"), Ok(..));
    assert_matches!(parse("x (let x1 = x2 in x3) y"), Ok(..));
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
        parse_without_pos("{ a = 1, b = 2, c = 3}"),
        RecRecord(
            vec![
                (Ident::from("a"), Num(1.).into()),
                (Ident::from("b"), Num(2.).into()),
                (Ident::from("c"), Num(3.).into()),
            ]
            .into_iter()
            .collect(),
            Vec::new(),
            Default::default()
        )
        .into()
    );

    assert_eq!(
        parse_without_pos("{ a = 1, \"#{123}\" = (if 4 then 5 else 6), d = 42}"),
        RecRecord(
            vec![
                (Ident::from("a"), Num(1.).into()),
                (Ident::from("d"), Num(42.).into()),
            ]
            .into_iter()
            .collect(),
            vec![(
                StrChunks(vec![StrChunk::expr(RichTerm::from(Num(123.)))]).into(),
                mk_app!(mk_term::op1(UnaryOp::Ite(), Num(4.)), Num(5.), Num(6.))
            )],
            Default::default(),
        )
        .into()
    );

    assert_eq!(
        parse_without_pos("{ a = 1, \"\\\"#}#\" = 2}"),
        RecRecord(
            vec![
                (Ident::from("a"), Num(1.).into()),
                (Ident::from("\"#}#"), Num(2.).into()),
            ]
            .into_iter()
            .collect(),
            Vec::new(),
            Default::default(),
        )
        .into()
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

    assert_eq!(
        lex_without_pos(r##"m#""#"#m"##),
        Ok(vec![
            Token::Normal(NormalToken::MultiStringStart(3)),
            Token::MultiStr(MultiStringToken::Literal("\"")),
            Token::MultiStr(MultiStringToken::Literal("#")),
            Token::MultiStr(MultiStringToken::End),
        ])
    );
}

#[test]
fn str_escape() {
    assert_matches!(
        parse("\"bad escape \\g\""),
        Err(ParseError::InvalidEscapeSequence(..))
    );
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

#[test]
fn ascii_escape() {
    assert_matches!(
        parse("\"\\x[f\""),
        Err(ParseError::InvalidEscapeSequence(..))
    );
    assert_matches!(
        parse("\"\\x0\""),
        Err(ParseError::InvalidEscapeSequence(..))
    );
    assert_matches!(
        parse("\"\\x0z\""),
        Err(ParseError::InvalidEscapeSequence(..))
    );

    assert_matches!(
        parse("\"\\x80\""),
        Err(ParseError::InvalidAsciiEscapeCode(..))
    );
    assert_matches!(
        parse("\"\\xab\""),
        Err(ParseError::InvalidAsciiEscapeCode(..))
    );
    assert_matches!(
        parse("\"\\xFF\""),
        Err(ParseError::InvalidAsciiEscapeCode(..))
    );

    assert_eq!(parse_without_pos("\"\\x00\""), mk_single_chunk("\x00"));
    assert_eq!(parse_without_pos("\"\\x08\""), mk_single_chunk("\x08"));
    assert_eq!(parse_without_pos("\"\\x7F\""), mk_single_chunk("\x7F"));

    assert_eq!(parse_without_pos("m#\"\\x[f\"#m"), mk_single_chunk("\\x[f"));
    assert_eq!(parse_without_pos("m#\"\\x0\"#m"), mk_single_chunk("\\x0"));
    assert_eq!(parse_without_pos("m#\"\\x0z\"#m"), mk_single_chunk("\\x0z"));
    assert_eq!(parse_without_pos("m#\"\\x00\"#m"), mk_single_chunk("\\x00"));
    assert_eq!(parse_without_pos("m#\"\\x08\"#m"), mk_single_chunk("\\x08"));
    assert_eq!(parse_without_pos("m#\"\\x7F\"#m"), mk_single_chunk("\\x7F"));
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
            field = foo, // Some description
            } // Some other"
        ),
        parse_without_pos("{field = foo}")
    );
}

#[test]
fn unbound_type_variables() {
    // should fail, "a" is unbound
    assert_matches!(
        parse("1 | a"),
        Err(ParseError::UnboundTypeVariables(unbound_vars, _)) if (unbound_vars.contains(&Ident("a".into())) && unbound_vars.len() == 1)
    );

    // should fail, "d" is unbound
    assert_matches!(
        parse("null: forall a b c. a -> (b -> List c) -> {foo : List {_ : d}, bar: b | Dyn}"),
        Err(ParseError::UnboundTypeVariables(unbound_vars, _)) if (unbound_vars.contains(&Ident("d".into())) && unbound_vars.len() == 1)
    );

    // should fail, "e" is unbound
    assert_matches!(
        parse("null: forall a b c. a -> (b -> List c) -> {foo : List {_ : a}, bar: b | e}"),
        Err(ParseError::UnboundTypeVariables(unbound_vars, _)) if (unbound_vars.contains(&Ident("e".into())) && unbound_vars.len() == 1)
    );

    // should fail, "a" is unbound
    assert_matches!(
        parse("null: a -> (forall a. a -> a)"),
        Err(ParseError::UnboundTypeVariables(unbound_vars, _)) if (unbound_vars.contains(&Ident("a".into())) && unbound_vars.len() == 1)
    );
}
