use std::rc::Rc;

use super::lexer::{Lexer, MultiStringToken, NormalToken, StringToken, SymbolicStringStart, Token};
use super::utils::{build_record, FieldPathElem};
use crate::error::ParseError;
use crate::identifier::LocIdent;
use crate::parser::{error::ParseError as InternalParseError, ErrorTolerantParser};
use crate::term::array::Array;
use crate::term::Number;
use crate::term::Term::*;
use crate::term::{make as mk_term, Term};
use crate::term::{record, BinaryOp, RichTerm, StrChunk, UnaryOp};

use crate::mk_app;
use assert_matches::assert_matches;
use codespan::Files;

fn parse(s: &str) -> Result<RichTerm, ParseError> {
    let id = Files::new().add("<test>", String::from(s));

    super::grammar::TermParser::new()
        .parse_strict(id, Lexer::new(s))
        .map_err(|errs| errs.errors.first().unwrap().clone())
}

fn parse_without_pos(s: &str) -> RichTerm {
    parse(s).unwrap().without_pos()
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

fn mk_symbolic_single_chunk(prefix: &str, s: &str) -> RichTerm {
    use crate::term::record::Field;

    build_record(
        [
            (
                FieldPathElem::Ident("tag".into()),
                Field::from(RichTerm::from(Term::Enum("SymbolicString".into()))),
            ),
            (
                FieldPathElem::Ident("prefix".into()),
                Field::from(RichTerm::from(Term::Enum(prefix.into()))),
            ),
            (
                FieldPathElem::Ident("fragments".into()),
                Field::from(RichTerm::from(Array(
                    Array::new(Rc::new([mk_single_chunk(s)])),
                    Default::default(),
                ))),
            ),
        ],
        Default::default(),
    )
    .into()
}

#[test]
fn numbers() {
    assert_eq!(parse_without_pos("22"), mk_term::integer(22));
    assert_eq!(parse_without_pos("22.0"), mk_term::integer(22));
    assert_eq!(
        parse_without_pos("22.22"),
        Num(Number::try_from_float_simplest(22.22).unwrap()).into()
    );
    assert_eq!(parse_without_pos("(22)"), mk_term::integer(22));
    assert_eq!(parse_without_pos("((22))"), mk_term::integer(22));
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
            BinaryOp::StringConcat,
            Op2(
                BinaryOp::StringConcat,
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
fn symbolic_strings() {
    assert_eq!(
        parse_without_pos(r#"foo-s%"hello world"%"#),
        mk_symbolic_single_chunk("foo", "hello world"),
    );
}

#[test]
fn plus() {
    assert_eq!(
        parse_without_pos("3 + 4"),
        Op2(BinaryOp::Plus, mk_term::integer(3), mk_term::integer(4)).into()
    );
    assert_eq!(
        parse_without_pos("(true + false) + 4"),
        Op2(
            BinaryOp::Plus,
            Op2(BinaryOp::Plus, Bool(true).into(), Bool(false).into()).into(),
            mk_term::integer(4),
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
        mk_app!(
            mk_term::op1(UnaryOp::IfThenElse, Bool(true)),
            mk_term::integer(3),
            mk_term::integer(4)
        )
    );
}

#[test]
fn applications() {
    assert_eq!(
        parse_without_pos("1 true 2"),
        mk_app!(mk_term::integer(1), Bool(true), mk_term::integer(2))
    );

    assert_eq!(
        parse_without_pos("1 (2 3) 4"),
        mk_app!(
            mk_term::integer(1),
            mk_app!(mk_term::integer(2), mk_term::integer(3)),
            mk_term::integer(4)
        )
    );
}

#[test]
fn variables() {
    assert!(parse("x1_x_").is_ok());
}

#[test]
fn lets() {
    assert_matches!(parse("let x1 = x2 in x3"), Ok(..));
    assert_matches!(parse("x (let x1 = x2 in x3) y"), Ok(..));
}

#[test]
fn unary_op() {
    assert_eq!(
        parse_without_pos("%typeof% x"),
        mk_term::op1(UnaryOp::Typeof, mk_term::var("x"))
    );
    assert_eq!(
        parse_without_pos("%typeof% x y"),
        mk_app!(
            mk_term::op1(UnaryOp::Typeof, mk_term::var("x")),
            mk_term::var("y")
        ),
    );
}

#[test]
fn enum_terms() {
    let success_cases = [
        (
            "simple raw enum tag",
            "'foo",
            Enum(LocIdent::from("foo")).into(),
        ),
        (
            "raw enum tag with keyword ident",
            "'if",
            Enum(LocIdent::from("if")).into(),
        ),
        ("empty string tag", "'\"\"", Enum(LocIdent::from("")).into()),
        (
            "string tag with non-ident chars",
            "'\"foo:bar\"",
            Enum(LocIdent::from("foo:bar")).into(),
        ),
        (
            "string with spaces",
            "'\"this works!\"",
            Enum(LocIdent::from("this works!")).into(),
        ),
    ];

    for (name, input, expected) in success_cases {
        let actual = parse_without_pos(input);
        assert_eq!(actual, expected, "test case \"{name}\" failed",);
    }

    let failure_cases = [
        ("whitespace between backtick & identifier", "'     test"),
        ("invalid identifier", "'$s"),
        ("empty raw identifier", "'"),
        ("multiline string", "'m%\"words\"%"),
        ("interpolation", "'\"%{x}\""),
    ];

    for (name, input) in failure_cases {
        let actual = parse(input);
        assert_matches!(actual, Err(..), "test case \"{}\" failed", name);
    }
}

#[test]
fn record_terms() {
    use crate::term::record::Field;

    assert_eq!(
        parse_without_pos("{ a = 1, b = 2, c = 3}"),
        RecRecord(
            record::RecordData::with_field_values(
                vec![
                    (LocIdent::from("a"), mk_term::integer(1)),
                    (LocIdent::from("b"), mk_term::integer(2)),
                    (LocIdent::from("c"), mk_term::integer(3)),
                ]
                .into_iter()
            ),
            Vec::new(),
            None,
        )
        .into()
    );

    assert_eq!(
        parse_without_pos("{ a = 1, \"%{123}\" = (if 4 then 5 else 6), d = 42}"),
        RecRecord(
            record::RecordData::with_field_values(
                vec![
                    (LocIdent::from("a"), mk_term::integer(1)),
                    (LocIdent::from("d"), mk_term::integer(42)),
                ]
                .into_iter()
            ),
            vec![(
                StrChunks(vec![StrChunk::expr(mk_term::integer(123))]).into(),
                Field::from(mk_app!(
                    mk_term::op1(UnaryOp::IfThenElse, mk_term::integer(4)),
                    mk_term::integer(5),
                    mk_term::integer(6)
                ))
            )],
            None,
        )
        .into()
    );

    assert_eq!(
        parse_without_pos("{ a = 1, \"\\\"%}%\" = 2}"),
        RecRecord(
            record::RecordData::with_field_values(
                vec![
                    (LocIdent::from("a"), mk_term::integer(1)),
                    (LocIdent::from("\"%}%"), mk_term::integer(2)),
                ]
                .into_iter()
            ),
            Vec::new(),
            None,
        )
        .into()
    );
}

/// Regression test for [#876](https://github.com/tweag/nickel/issues/876)
#[test]
fn invalid_record_types() {
    assert_matches!(
        parse("let x | forall r. { n | Num; r } = {} in x"),
        Err(ParseError::InvalidRecordType { .. })
    );

    assert_matches!(
        parse("let x : forall r. { n = fun i => i; r } = {} in x"),
        Err(ParseError::InvalidRecordType { .. })
    );
}

#[test]
fn string_lexing() {
    for (name, input, expected) in [
        (
            "simple strings",
            r#""Good" "strings""#,
            vec![
                Token::Normal(NormalToken::DoubleQuote),
                Token::Str(StringToken::Literal("Good".to_owned())),
                Token::Normal(NormalToken::DoubleQuote),
                Token::Normal(NormalToken::DoubleQuote),
                Token::Str(StringToken::Literal("strings".to_owned())),
                Token::Normal(NormalToken::DoubleQuote),
            ],
        ),
        (
            "valid escape sequence",
            r#""Good\nEscape\t\"""#,
            vec![
                Token::Normal(NormalToken::DoubleQuote),
                Token::Str(StringToken::Literal("Good".to_owned())),
                Token::Str(StringToken::EscapedChar('\n')),
                Token::Str(StringToken::Literal("Escape".to_owned())),
                Token::Str(StringToken::EscapedChar('\t')),
                Token::Str(StringToken::EscapedChar('\"')),
                Token::Normal(NormalToken::DoubleQuote),
            ],
        ),
        (
            "simple interpolation",
            r#""1 + %{ 1 } + 2""#,
            vec![
                Token::Normal(NormalToken::DoubleQuote),
                Token::Str(StringToken::Literal("1 + ".to_owned())),
                Token::Str(StringToken::Interpolation),
                Token::Normal(NormalToken::DecNumLiteral(Number::from(1))),
                Token::Normal(NormalToken::RBrace),
                Token::Str(StringToken::Literal(" + 2".to_owned())),
                Token::Normal(NormalToken::DoubleQuote),
            ],
        ),
        (
            "nested interpolated strings",
            r#""1 + %{ "%{ 1 }" } + 2""#,
            vec![
                Token::Normal(NormalToken::DoubleQuote),
                Token::Str(StringToken::Literal("1 + ".to_owned())),
                Token::Str(StringToken::Interpolation),
                Token::Normal(NormalToken::DoubleQuote),
                Token::Str(StringToken::Interpolation),
                Token::Normal(NormalToken::DecNumLiteral(Number::from(1))),
                Token::Normal(NormalToken::RBrace),
                Token::Normal(NormalToken::DoubleQuote),
                Token::Normal(NormalToken::RBrace),
                Token::Str(StringToken::Literal(" + 2".to_owned())),
                Token::Normal(NormalToken::DoubleQuote),
            ],
        ),
        (
            "multiline strings only close on delmiter with correct number of %s",
            r#"m%%""%"%%"#,
            vec![
                Token::Normal(NormalToken::MultiStringStart(4)),
                Token::MultiStr(MultiStringToken::Literal("\"%".to_owned())),
                Token::MultiStr(MultiStringToken::End),
            ],
        ),
        (
            "empty symbolic string lexes like multi-line str",
            r#"foo-s%""%"#,
            vec![
                Token::Normal(NormalToken::SymbolicStringStart(SymbolicStringStart {
                    prefix: "foo",
                    length: 3,
                })),
                Token::MultiStr(MultiStringToken::End),
            ],
        ),
        (
            "symbolic string with interpolation",
            r#"foo-s%"text %{ 1 } etc."%"#,
            vec![
                Token::Normal(NormalToken::SymbolicStringStart(SymbolicStringStart {
                    prefix: "foo",
                    length: 3,
                })),
                Token::MultiStr(MultiStringToken::Literal("text ".to_owned())),
                Token::MultiStr(MultiStringToken::Interpolation),
                Token::Normal(NormalToken::DecNumLiteral(Number::from(1))),
                Token::Normal(NormalToken::RBrace),
                Token::MultiStr(MultiStringToken::Literal(" etc.".to_owned())),
                Token::MultiStr(MultiStringToken::End),
            ],
        ),
        (
            "empty symbolic string with tag",
            r#"tf-s%""%"#,
            vec![
                Token::Normal(NormalToken::SymbolicStringStart(SymbolicStringStart {
                    prefix: "tf",
                    length: 3,
                })),
                Token::MultiStr(MultiStringToken::End),
            ],
        ),
    ] {
        assert_eq!(lex_without_pos(input), Ok(expected), "Case failed: {name}")
    }
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
        parse_without_pos("\"\\%\\%{ }\\%\""),
        mk_single_chunk("%%{ }%"),
    );
    assert_eq!(
        parse_without_pos("\"%a%b%c\\%{d%\""),
        mk_single_chunk("%a%b%c%{d%"),
    );
}

#[test]
fn carriage_returns() {
    assert_eq!(parse_without_pos("\"\\r\""), mk_single_chunk("\r"),);
    assert_matches!(parse("foo\rbar"), Err(ParseError::UnexpectedToken(..)))
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

    assert_eq!(parse_without_pos("m%\"\\x[f\"%"), mk_single_chunk("\\x[f"));
    assert_eq!(parse_without_pos("m%\"\\x0\"%"), mk_single_chunk("\\x0"));
    assert_eq!(parse_without_pos("m%\"\\x0z\"%"), mk_single_chunk("\\x0z"));
    assert_eq!(parse_without_pos("m%\"\\x00\"%"), mk_single_chunk("\\x00"));
    assert_eq!(parse_without_pos("m%\"\\x08\"%"), mk_single_chunk("\\x08"));
    assert_eq!(parse_without_pos("m%\"\\x7F\"%"), mk_single_chunk("\\x7F"));
}

/// Regression test for [#230](https://github.com/tweag/nickel/issues/230).
#[test]
fn multiline_str_escape() {
    assert_eq!(
        parse_without_pos(r#"m%"%Hel%%lo%%%"%"#),
        mk_single_chunk("%Hel%%lo%%%"),
    );
}

#[test]
fn line_comments() {
    assert_eq!(
        parse_without_pos("# 1 +\n1 + 1# + 3\n#+ 2"),
        parse_without_pos("1 + 1")
    );
    assert_eq!(
        parse_without_pos(
            "{ # Some comment
            field = foo, # Some description
            } # Some other"
        ),
        parse_without_pos("{field = foo}")
    );
}

/// Regression test for [#942](https://github.com/tweag/nickel/issues/942).
#[test]
fn ty_var_kind_mismatch() {
    for (name, src) in [
        (
            "var used as both row and type var",
            r#"
                let f | forall r. { x: r; r } -> { x: r; r } = fun r => r in
                f { x = 1 }
            "#,
        ),
        (
            "row type as return value type",
            r#"
                let f | forall r. { ; r } -> r = fun r => r in
                f { x = 1, y = 2}
            "#,
        ),
        (
            "row var in both enum and record",
            r#"
                let f | forall r. { x : r; r } -> [| 'a; r |] = fun x => x in
                f { x = 1 }
            "#,
        ),
    ] {
        assert_matches!(
            parse(src),
            Err(ParseError::TypeVariableKindMismatch { .. }),
            "{}",
            name
        )
    }
}

#[test]
fn import() {
    assert_eq!(
        parse_without_pos("import \"file.ncl\""),
        mk_term::import("file.ncl")
    );
    assert_matches!(
        parse("import \"file.ncl\" some args"),
        Err(ParseError::UnexpectedToken(_, _))
    );
    assert_eq!(
        parse_without_pos("(import \"file.ncl\") some args"),
        mk_app!(
            mk_term::import("file.ncl"),
            mk_term::var("some"),
            mk_term::var("args")
        )
    );
}
