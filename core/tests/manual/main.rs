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

fn check_repl_parts(content: String) {
    for piece in content.split("\n\n") {
        let program: String = piece
            .strip_prefix('>')
            .unwrap()
            .split_inclusive('\n')
            .take_while(|l| l.starts_with(' '))
            .collect();
        check_parse_extended(program);
    }
}

fn check_eval(program: String) {
    eprintln!("{}", program);
    test_program::eval(program).unwrap();
}

fn check_parse_extended(program: String) {
    eprintln!("{}", program);
    test_program::parse_extended(&program).unwrap();
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
            CodeBlockType::Repl => check_repl_parts(self.content),
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
