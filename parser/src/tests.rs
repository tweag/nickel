use crate::{
    ErrorTolerantParser,
    ast::{
        Ast, AstAlloc, InputFormat, Node, Number, StringChunk, builder,
        primop::PrimOp,
        record::{FieldDef, FieldMetadata, FieldPathElem, Record},
    },
    error::{LexicalError, ParseError},
    files::Files,
    grammar::TermParser,
    lexer::{Lexer, MultiStringToken, NormalToken, StringToken, SymbolicStringStart, Token},
    position::TermPos,
};

use pretty_assertions::assert_eq;

use assert_matches::assert_matches;

fn parse<'ast>(alloc: &'ast AstAlloc, s: &str) -> Result<Ast<'ast>, ParseError> {
    let id = Files::empty().add("<test>", String::from(s));

    TermParser::new()
        .parse_strict(alloc, id, Lexer::new(s))
        .map_err(|errs| errs.errors.first().unwrap().clone())
}

fn parse_without_pos<'ast>(alloc: &'ast AstAlloc, s: &str) -> Ast<'ast> {
    parse(alloc, s).unwrap().without_pos(alloc)
}

fn lex(s: &str) -> Result<Vec<(usize, Token<'_>, usize)>, LexicalError> {
    Lexer::new(s).collect()
}

fn lex_without_pos(s: &str) -> Result<Vec<Token<'_>>, LexicalError> {
    lex(s).map(|v| v.into_iter().map(|(_, tok, _)| tok).collect())
}

/// Wrap a single string literal in a `StrChunks`.
fn mk_single_chunk<'ast>(alloc: &'ast AstAlloc, s: &str) -> Ast<'ast> {
    alloc
        .string_chunks([StringChunk::Literal(String::from(s))])
        .into()
}

fn mk_int<'ast>(alloc: &'ast AstAlloc, i: i64) -> Ast<'ast> {
    alloc.number(Number::from(i)).into()
}

fn mk_var(s: &str) -> Ast<'static> {
    Node::Var(s.into()).into()
}

fn mk_symbolic_single_chunk<'ast>(alloc: &'ast AstAlloc, prefix: &str, s: &str) -> Ast<'ast> {
    builder::Record::new()
        .fields(
            alloc,
            [
                builder::Field::name("tag")
                    .value(alloc.enum_variant("SymbolicString".into(), None)),
                builder::Field::name("prefix").value(alloc.enum_variant(prefix.into(), None)),
                builder::Field::name("fragments").value(alloc.array([mk_single_chunk(alloc, s)])),
            ],
        )
        .build(alloc)
}

#[test]
fn numbers() {
    let alloc = AstAlloc::new();
    assert_eq!(parse_without_pos(&alloc, "22"), mk_int(&alloc, 22));
    assert_eq!(parse_without_pos(&alloc, "22.0"), mk_int(&alloc, 22));
    assert_eq!(
        parse_without_pos(&alloc, "22.22"),
        alloc
            .number(Number::try_from_float_simplest(22.22).unwrap())
            .into()
    );
    assert_eq!(parse_without_pos(&alloc, "(22)"), mk_int(&alloc, 22));
    assert_eq!(parse_without_pos(&alloc, "((22))"), mk_int(&alloc, 22));
}

#[test]
fn strings() {
    let alloc = AstAlloc::new();
    assert_eq!(
        parse_without_pos(&alloc, "\"hello world\""),
        mk_single_chunk(&alloc, "hello world"),
    );
    assert_eq!(
        parse_without_pos(&alloc, "\"hello \nworld\""),
        mk_single_chunk(&alloc, "hello \nworld")
    );
    assert_eq!(
        parse_without_pos(&alloc, "\"hello Dimension C-132!\""),
        mk_single_chunk(&alloc, "hello Dimension C-132!")
    );

    assert_eq!(
        parse_without_pos(&alloc, "\"hello\" ++ \"World\" ++ \"!!\" "),
        alloc
            .prim_op(
                PrimOp::StringConcat,
                [
                    alloc
                        .prim_op(
                            PrimOp::StringConcat,
                            [
                                mk_single_chunk(&alloc, "hello"),
                                mk_single_chunk(&alloc, "World"),
                            ]
                        )
                        .into(),
                    mk_single_chunk(&alloc, "!!"),
                ]
            )
            .into()
    )
}

#[test]
fn symbolic_strings() {
    let alloc = AstAlloc::new();
    assert_eq!(
        parse_without_pos(&alloc, r#"foo-s%"hello world"%"#),
        mk_symbolic_single_chunk(&alloc, "foo", "hello world"),
    );
}

#[test]
fn plus() {
    let alloc = AstAlloc::new();
    assert_eq!(
        parse_without_pos(&alloc, "3 + 4").node,
        alloc.prim_op(PrimOp::Plus, [mk_int(&alloc, 3), mk_int(&alloc, 4)])
    );
    assert_eq!(
        parse_without_pos(&alloc, "(true + false) + 4"),
        alloc
            .prim_op(
                PrimOp::Plus,
                [
                    alloc
                        .prim_op(
                            PrimOp::Plus,
                            [Node::Bool(true).into(), Node::Bool(false).into()]
                        )
                        .into(),
                    mk_int(&alloc, 4)
                ]
            )
            .into()
    );
}

#[test]
fn booleans() {
    let alloc = AstAlloc::new();
    assert_eq!(parse_without_pos(&alloc, "true"), Node::Bool(true).into());
    assert_eq!(parse_without_pos(&alloc, "false"), Node::Bool(false).into());
}

#[test]
fn ite() {
    let alloc = AstAlloc::new();
    assert_eq!(
        parse_without_pos(&alloc, "if true then 3 else 4"),
        alloc
            .if_then_else(
                Node::Bool(true).into(),
                mk_int(&alloc, 3),
                mk_int(&alloc, 4)
            )
            .into()
    );
}

#[test]
fn applications() {
    let alloc = AstAlloc::new();
    assert_eq!(
        parse_without_pos(&alloc, "1 true 2"),
        alloc
            .app(
                mk_int(&alloc, 1),
                [Node::Bool(true).into(), mk_int(&alloc, 2)]
            )
            .into()
    );

    assert_eq!(
        parse_without_pos(&alloc, "1 (2 3) 4"),
        alloc
            .app(
                mk_int(&alloc, 1),
                [
                    alloc.app(mk_int(&alloc, 2), [mk_int(&alloc, 3)]).into(),
                    mk_int(&alloc, 4)
                ]
            )
            .into()
    );
}

#[test]
fn variables() {
    let alloc = AstAlloc::new();
    assert!(parse(&alloc, "x1_x_").is_ok());
}

#[test]
fn lets() {
    let alloc = AstAlloc::new();
    assert_matches!(parse(&alloc, "let x1 = x2 in x3"), Ok(..));
    assert_matches!(parse(&alloc, "x (let x1 = x2 in x3) y"), Ok(..));
}

#[test]
fn unary_op() {
    let alloc = AstAlloc::new();
    assert_eq!(
        parse_without_pos(&alloc, "%typeof% x"),
        alloc.prim_op(PrimOp::Typeof, [mk_var("x")]).into()
    );
    assert_eq!(
        parse_without_pos(&alloc, "%typeof% x y"),
        alloc
            .app(
                alloc.prim_op(PrimOp::Typeof, [mk_var("x")]).into(),
                [mk_var("y")]
            )
            .into()
    );
}

#[test]
fn enum_terms() {
    let alloc = AstAlloc::new();
    let enm = |s: &str| alloc.enum_variant(s.into(), None).into();
    let success_cases = [
        ("simple raw enum tag", "'foo", enm("foo")),
        ("raw enum tag with keyword ident", "'if", enm("if")),
        ("empty string tag", "'\"\"", enm("")),
        (
            "string tag with non-ident chars",
            "'\"foo:bar\"",
            enm("foo:bar"),
        ),
        ("string with spaces", "'\"this works!\"", enm("this works!")),
    ];

    for (name, input, expected) in success_cases {
        let actual = parse_without_pos(&alloc, input);
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
        let actual = parse(&alloc, input);
        assert_matches!(actual, Err(..), "test case \"{}\" failed", name);
    }
}

#[test]
fn record_terms() {
    let alloc = AstAlloc::new();

    assert_eq!(
        parse_without_pos(&alloc, "{ a = 1, b = 2, c = 3}"),
        builder::Record::new()
            .fields(
                &alloc,
                [
                    builder::Field::name("a").value(mk_int(&alloc, 1)),
                    builder::Field::name("b").value(mk_int(&alloc, 2)),
                    builder::Field::name("c").value(mk_int(&alloc, 3)),
                ]
            )
            .build(&alloc)
    );

    assert_eq!(
        parse_without_pos(
            &alloc,
            "{ a = 1, \"%{123}\" = (if 4 then 5 else 6), d = 42}"
        ),
        // TODO: extend builder to allow interpolated field names?
        alloc
            .record(Record {
                includes: &[],
                field_defs: alloc.alloc_many([
                    FieldDef {
                        path: alloc.alloc_many([FieldPathElem::Ident("a".into())]),
                        metadata: FieldMetadata::default(),
                        value: Some(mk_int(&alloc, 1)),
                        pos: TermPos::None
                    },
                    FieldDef {
                        path: alloc.alloc_many([FieldPathElem::Expr(
                            alloc
                                .string_chunks([StringChunk::Expr(mk_int(&alloc, 123), 0)])
                                .into()
                        )]),
                        metadata: FieldMetadata::default(),
                        value: Some(
                            alloc
                                .if_then_else(
                                    mk_int(&alloc, 4),
                                    mk_int(&alloc, 5),
                                    mk_int(&alloc, 6)
                                )
                                .into()
                        ),
                        pos: TermPos::None
                    },
                    FieldDef {
                        path: alloc.alloc_many([FieldPathElem::Ident("d".into())]),
                        metadata: FieldMetadata::default(),
                        value: Some(mk_int(&alloc, 42)),
                        pos: TermPos::None
                    },
                ]),
                open: false
            })
            .into()
    );

    assert_eq!(
        parse_without_pos(&alloc, "{ a = 1, \"\\\"%}%\" = 2}"),
        builder::Record::new()
            .fields(
                &alloc,
                [
                    builder::Field::name("a").value(mk_int(&alloc, 1)),
                    builder::Field::name("\"%}%").value(mk_int(&alloc, 2)),
                ]
            )
            .build(&alloc)
    );
}

/// Regression test for [#876](https://github.com/tweag/nickel/issues/876)
#[test]
fn invalid_record_types() {
    let alloc = AstAlloc::new();

    assert_matches!(
        parse(&alloc, "let x | forall r. { n | Num; r } = {} in x"),
        Err(ParseError::InvalidRecordType { .. })
    );

    assert_matches!(
        parse(&alloc, "let x : forall r. { n = fun i => i; r } = {} in x"),
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
    let alloc = AstAlloc::new();
    assert_matches!(
        parse(&alloc, "\"bad escape \\g\""),
        Err(ParseError::InvalidEscapeSequence(..))
    );
    assert_eq!(
        parse_without_pos(&alloc, r#""str\twith\nescapes""#),
        mk_single_chunk(&alloc, "str\twith\nescapes"),
    );
    assert_eq!(
        parse_without_pos(&alloc, "\"\\%\\%{ }\\%\""),
        mk_single_chunk(&alloc, "%%{ }%"),
    );
    assert_eq!(
        parse_without_pos(&alloc, "\"%a%b%c\\%{d%\""),
        mk_single_chunk(&alloc, "%a%b%c%{d%"),
    );
}

#[test]
fn carriage_returns() {
    let alloc = AstAlloc::new();
    assert_eq!(
        parse_without_pos(&alloc, "\"\\r\""),
        mk_single_chunk(&alloc, "\r"),
    );
    assert_matches!(
        parse(&alloc, "foo\rbar"),
        Err(ParseError::UnexpectedToken(..))
    )
}

#[test]
fn ascii_escape() {
    let alloc = AstAlloc::new();
    assert_matches!(
        parse(&alloc, "\"\\x[f\""),
        Err(ParseError::InvalidEscapeSequence(..))
    );
    assert_matches!(
        parse(&alloc, "\"\\x0\""),
        Err(ParseError::InvalidEscapeSequence(..))
    );
    assert_matches!(
        parse(&alloc, "\"\\x0z\""),
        Err(ParseError::InvalidEscapeSequence(..))
    );

    assert_matches!(
        parse(&alloc, "\"\\x80\""),
        Err(ParseError::InvalidAsciiEscapeCode(..))
    );
    assert_matches!(
        parse(&alloc, "\"\\xab\""),
        Err(ParseError::InvalidAsciiEscapeCode(..))
    );
    assert_matches!(
        parse(&alloc, "\"\\xFF\""),
        Err(ParseError::InvalidAsciiEscapeCode(..))
    );

    assert_eq!(
        parse_without_pos(&alloc, "\"\\x00\""),
        mk_single_chunk(&alloc, "\x00")
    );
    assert_eq!(
        parse_without_pos(&alloc, "\"\\x08\""),
        mk_single_chunk(&alloc, "\x08")
    );
    assert_eq!(
        parse_without_pos(&alloc, "\"\\x7F\""),
        mk_single_chunk(&alloc, "\x7F")
    );

    assert_eq!(
        parse_without_pos(&alloc, "m%\"\\x[f\"%"),
        mk_single_chunk(&alloc, "\\x[f")
    );
    assert_eq!(
        parse_without_pos(&alloc, "m%\"\\x0\"%"),
        mk_single_chunk(&alloc, "\\x0")
    );
    assert_eq!(
        parse_without_pos(&alloc, "m%\"\\x0z\"%"),
        mk_single_chunk(&alloc, "\\x0z")
    );
    assert_eq!(
        parse_without_pos(&alloc, "m%\"\\x00\"%"),
        mk_single_chunk(&alloc, "\\x00")
    );
    assert_eq!(
        parse_without_pos(&alloc, "m%\"\\x08\"%"),
        mk_single_chunk(&alloc, "\\x08")
    );
    assert_eq!(
        parse_without_pos(&alloc, "m%\"\\x7F\"%"),
        mk_single_chunk(&alloc, "\\x7F")
    );
}

/// Regression test for [#230](https://github.com/tweag/nickel/issues/230).
#[test]
fn multiline_str_escape() {
    let alloc = AstAlloc::new();
    assert_eq!(
        parse_without_pos(&alloc, r#"m%"%Hel%%lo%%%"%"#),
        mk_single_chunk(&alloc, "%Hel%%lo%%%"),
    );
}

#[test]
fn line_comments() {
    let alloc = AstAlloc::new();
    assert_eq!(
        parse_without_pos(&alloc, "# 1 +\n1 + 1# + 3\n#+ 2"),
        parse_without_pos(&alloc, "1 + 1")
    );
    assert_eq!(
        parse_without_pos(
            &alloc,
            "{ # Some comment
            field = foo, # Some description
            } # Some other"
        ),
        parse_without_pos(&alloc, "{field = foo}")
    );
}

/// Regression test for [#942](https://github.com/tweag/nickel/issues/942).
#[test]
fn ty_var_kind_mismatch() {
    let alloc = AstAlloc::new();
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
            parse(&alloc, src),
            Err(ParseError::TypeVariableKindMismatch { .. }),
            "{}",
            name
        )
    }
}

#[test]
fn import() {
    let alloc = AstAlloc::new();
    assert_eq!(
        parse_without_pos(&alloc, "import \"file.ncl\""),
        alloc
            .import_path("file.ncl".into(), InputFormat::Nickel)
            .into()
    );
    assert_matches!(
        parse(&alloc, "import \"file.ncl\" some args"),
        Err(ParseError::UnexpectedToken(_, _))
    );
    assert_eq!(
        parse_without_pos(&alloc, "(import \"file.ncl\") some args"),
        alloc
            .app(
                alloc
                    .import_path("file.ncl".into(), InputFormat::Nickel)
                    .into(),
                [mk_var("some"), mk_var("args")]
            )
            .into()
    );
}
