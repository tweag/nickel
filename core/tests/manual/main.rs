use std::{fs::File, io::read_to_string, iter::once};

use codespan_reporting::term::termcolor::NoColor;
use comrak::{
    arena_tree::{Node, NodeEdge},
    nodes::{Ast, AstNode, NodeCodeBlock, NodeValue},
    parse_document, ComrakOptions,
};
use nickel_lang_core::{
    error,
    eval::cache::CacheImpl,
    repl::{EvalResult, Repl, ReplImpl},
};
use nickel_lang_utils::{project_root::project_root, test_program};
use pretty_assertions::assert_str_eq;
use test_generator::test_resources;
use typed_arena::Arena;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum CodeBlockType {
    /// A code block intended to be evaluated in its entirety
    Single,
    /// A code block containing multiple lines, each of which should be
    /// evaluated separately
    Lines,
    /// A code block that should only be parsed, not evaluated
    Parse,
    /// A code block containing a REPL session, e.g.
    /// ```nickel #repl
    /// > let foo =
    ///     fun x => x
    ///   in foo 5
    /// 5
    ///
    /// > std.function.id 5
    /// 5
    /// ```
    /// We start a separate REPL session for each such block and interpret lines
    /// starting with `> ` directly after an empty line and followed by indented
    /// lines (starting with at least one `' '`) as a single REPL input. In
    /// particular REPL inputs must be separated by an empty line.
    ///
    /// Any text following a REPL input directly, without containing an empty
    /// line, is interpreted as an expected result. If it starts with `error:`,
    /// we expect the evaluation to produce an error and match the error report
    /// with the expected result. A final `[...]` means that the expected
    /// result is merely a prefix of the error report. If it doesn't start with
    /// `error:`, the evaluation is expected to succeed.
    Repl,
}

#[derive(Debug, Clone)]
struct CodeBlock {
    typ: CodeBlockType,
    content: String,
}

impl CodeBlockType {
    fn from_info(info: impl AsRef<str>) -> Option<CodeBlockType> {
        match info.as_ref() {
            "nickel" => Some(CodeBlockType::Single),
            "nickel #lines" => Some(CodeBlockType::Lines),
            "nickel #repl" => Some(CodeBlockType::Repl),
            "nickel #parse" => Some(CodeBlockType::Parse),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
enum MessageExpectation {
    Full(String),
    Abridged(String),
}

#[derive(Debug, Clone)]
enum ReplResult {
    Value(String),
    Error(MessageExpectation),
    Empty,
}

fn extract_repl_piece(piece: impl AsRef<str>) -> (String, ReplResult) {
    let lines: Vec<&str> = piece.as_ref().split_inclusive('\n').collect();
    let split_index = lines
        .iter()
        .position(|l| !l.starts_with(' '))
        .unwrap_or(lines.len());
    let (program_lines, result_lines) = lines.split_at(split_index);

    let result_string = result_lines.concat();
    let result = if result_string.is_empty() {
        ReplResult::Empty
    } else if result_string.starts_with("error:") {
        if let Some((result_string, _)) = result_string.rsplit_once("[...]") {
            ReplResult::Error(MessageExpectation::Abridged(result_string.to_owned()))
        } else {
            ReplResult::Error(MessageExpectation::Full(result_string))
        }
    } else {
        ReplResult::Value(result_string)
    };

    (program_lines.concat(), result)
}

/// Assert that two strings are equal except for possible trailing whitespace.
/// The error reporting by `codespan` sometimes produces trailing whitespace
/// which will be removed from the documentation markdown files.
#[track_caller]
fn assert_str_eq_approx(actual: impl AsRef<str>, expected: impl AsRef<str>) {
    assert_str_eq!(
        actual
            .as_ref()
            .lines()
            .flat_map(|l| once(l.trim_end()).chain(once("\n")))
            .collect::<String>()
            .trim_end(),
        expected.as_ref().trim_end()
    );
}

#[track_caller]
fn assert_prefix(actual: impl AsRef<str>, prefix: impl AsRef<str>) {
    assert!(
        actual.as_ref().starts_with(prefix.as_ref()),
        "{} was expected to be a prefix of {}",
        prefix.as_ref(),
        actual.as_ref()
    );
}

fn check_error_report(actual: impl AsRef<str>, expected: MessageExpectation) {
    match expected {
        MessageExpectation::Full(expected) => assert_str_eq_approx(actual, expected),
        MessageExpectation::Abridged(prefix) => assert_prefix(actual, prefix),
    }
}

fn check_repl(content: String) {
    use error::report::{report_with, ErrorFormat};

    let mut repl = ReplImpl::<CacheImpl>::new(std::io::sink());
    repl.load_stdlib().unwrap();
    for piece in content.split("\n\n") {
        // We only process `piece`s starting with `>`. This way we can make the
        // testing code ignore unknown REPL statements, e.g. `:query`.
        if let Some(piece) = piece.strip_prefix('>') {
            let (input, result) = extract_repl_piece(piece);

            eprintln!(">{input}"); // Print the input to stderr to make tracking test failures easier
            match (repl.eval_full(&input), result) {
                (Ok(EvalResult::Evaluated(rt)), ReplResult::Value(expected)) => {
                    assert_str_eq_approx(format!("{rt}"), expected)
                }
                (Ok(EvalResult::Bound(_)), ReplResult::Empty) => (),
                (Err(e), ReplResult::Error(expected)) => {
                    let mut error = NoColor::new(Vec::<u8>::new());
                    let stdlib_ids = repl.cache_mut().get_all_stdlib_modules_file_id();
                    let files = repl.cache_mut().files_mut();
                    report_with(&mut error, files, stdlib_ids.as_ref(), e, ErrorFormat::Text);

                    check_error_report(String::from_utf8(error.into_inner()).unwrap(), expected);
                }
                (actual, expected) => {
                    panic!("Evaluation produced {actual:?}, expected {expected:?}");
                }
            }
        }
    }
}

fn check_eval(program: String) {
    eprintln!("{program}"); // Print the program to stderr to make tracking test failures easier
    test_program::eval(program).unwrap();
}

fn check_parse(program: String) {
    eprintln!("{program}"); // Print the program to stderr to make tracking test failures easier
    test_program::parse(&program).unwrap();
}

impl CodeBlock {
    fn check(self) {
        match self.typ {
            CodeBlockType::Single => check_eval(self.content),
            CodeBlockType::Lines => {
                for line in self.content.lines() {
                    check_eval(line.to_owned());
                }
            }
            CodeBlockType::Parse => check_parse(self.content),
            CodeBlockType::Repl => check_repl(self.content),
        }
    }
}

fn nickel_code_blocks<'a>(document: &'a AstNode<'a>) -> impl Iterator<Item = CodeBlock> + 'a {
    document.traverse().filter_map(|ne| match ne {
        NodeEdge::Start(Node { data, .. }) => match &*data.borrow() {
            Ast {
                value: NodeValue::CodeBlock(NodeCodeBlock { info, literal, .. }),
                ..
            } => Some(CodeBlock {
                typ: CodeBlockType::from_info(info)?,
                content: literal.clone(),
            }),
            _ => None,
        },
        _ => None,
    })
}

#[test_resources("doc/manual/*.md")]
fn check_manual_snippets(path: &str) {
    let contents = read_to_string(File::open(project_root().join(path)).unwrap()).unwrap();
    let arena = Arena::new();
    let snippets = nickel_code_blocks(parse_document(&arena, &contents, &ComrakOptions::default()));

    for code_block in snippets {
        code_block.check();
    }
}
