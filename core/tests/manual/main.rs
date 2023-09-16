use std::{fs::File, io::read_to_string};

use comrak::{
    arena_tree::{Node, NodeEdge},
    nodes::{Ast, AstNode, NodeCodeBlock, NodeValue},
    parse_document, ComrakOptions,
};
use nickel_lang_utils::{project_root::project_root, test_program};
use test_generator::test_resources;
use typed_arena::Arena;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum CodeBlockType {
    /// A code block intended to be evaluated in its entirety
    Single,
    /// A code block containing multiple lines, each of which should be
    /// evaluated separately
    Lines,
    /// A code block containing a REPL session, e.g.
    /// ```nickel repl
    /// > let foo =
    ///     fun x => x
    ///   in foo 5
    /// 5
    ///
    /// > std.function.id 5
    /// 5
    /// ```
    /// We interpret lines starting with `> ` and followed by indented lines
    /// (starting with at least one `' '`) as a single Nickel program and try
    /// to parse them. For now, we don't check evaluation. REPL inputs must be
    /// separated by an empty line.
    Repl,
}

#[derive(Debug, Clone)]
struct CodeBlock {
    typ: CodeBlockType,
    content: String,
}

#[derive(Debug, Clone)]
enum TestCase {
    Evaluate(String),
    ParseExtended(String),
}

impl CodeBlockType {
    fn from_info(info: impl AsRef<str>) -> Option<CodeBlockType> {
        match info.as_ref() {
            "nickel" => Some(CodeBlockType::Single),
            "nickel lines" => Some(CodeBlockType::Lines),
            "nickel repl" => Some(CodeBlockType::Repl),
            _ => None,
        }
    }
}

fn repl_parts(content: String) -> Vec<TestCase> {
    content
        .split("\n\n")
        .map(|piece| {
            let program: String = piece
                .strip_prefix('>')
                .unwrap()
                .lines()
                .take_while(|l| l.starts_with(' '))
                .collect();
            TestCase::ParseExtended(program)
        })
        .collect()
}

impl CodeBlock {
    fn split(self) -> Vec<TestCase> {
        match self.typ {
            CodeBlockType::Single => vec![TestCase::Evaluate(self.content)],
            CodeBlockType::Lines => self
                .content
                .lines()
                .map(|line| TestCase::Evaluate(line.to_owned()))
                .collect(),
            CodeBlockType::Repl => repl_parts(self.content),
        }
    }

    fn check(self) {
        for case in self.split() {
            case.check()
        }
    }
}

impl TestCase {
    fn check(self) {
        use TestCase::*;
        match self {
            Evaluate(program) => {
                eprintln!("{}", program);
                test_program::eval(program).unwrap();
            }
            ParseExtended(program) => {
                eprintln!("{}", program);
                test_program::parse_extended(&program).unwrap();
            }
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
    panic!();
}
