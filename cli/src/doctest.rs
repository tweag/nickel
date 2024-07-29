use std::{
    borrow::Cow,
    fmt, fs,
    io::{stdout, Sink, Write as _},
    path::{Path, PathBuf},
};

use comrak::{arena_tree::NodeEdge, nodes::AstNode, Arena, ComrakOptions};
use nickel_lang_core::{
    cache::{Envs, ImportResolver as _, InputFormat, SourcePath},
    error::{Error, IOError},
    eval::{
        cache::{
            lazy::{CBNCache, Thunk},
            CacheImpl,
        },
        Closure, Environment,
    },
    position::TermPos,
    program::Program,
    term::{
        pattern::{FieldPattern, Pattern, PatternData, RecordPattern},
        RichTerm, Term, TypeAnnotation,
    },
    transform::desugar_destructuring::desugar_let,
};

use crate::{
    cli::GlobalOptions,
    customize::ExtractFieldOnly,
    error::{CliResult, ResultErrorExt},
    input::{InputOptions, Prepare},
};

#[derive(clap::Parser, Debug)]
pub struct TestCommand {
    #[command(flatten)]
    pub input: InputOptions<ExtractFieldOnly>,
}

impl TestCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.input.prepare(&global)?;

        self.run_tests(&mut program).report_with_program(program)
    }

    fn run_tests(self, program: &mut Program<CacheImpl>) -> Result<(), Error> {
        let doc = program.extract_doc()?;

        for (path, docstring) in doc.docstrings() {
            one_doc_tests(program, &path, docstring);
        }

        Ok(())
    }
}

fn one_doc_tests(main_program: &mut Program<CacheImpl>, path: &[&str], doc: &str) -> CliResult<()> {
    let arena = Arena::new();
    let snippets = nickel_code_blocks(comrak::parse_document(
        &arena,
        &doc,
        &ComrakOptions::default(),
    ));

    if let [snip] = &snippets[..] {
        print!("Testing {}...", path.join("."));
        stdout().flush()?;
        snip.run(main_program);
        println!();
    } else if !snippets.is_empty() {
        println!("Testing {}...", path.join("."));
        for (i, snip) in snippets.iter().enumerate() {
            // TODO
            print!("\t{i}:");
            stdout().flush()?;
            println!();
        }
    }

    Ok(())
}

struct CodeBlock {
    // TODO: some location data for error reporting
    code: String,
    expected: Option<String>,
}

impl CodeBlock {
    fn run(&self, main_program: &mut Program<CacheImpl>) -> Result<(), Error> {
        let src_id = main_program
            .vm
            .import_resolver
            // TODO: maybe a SourcePath::Snippet would be better, if we could figure out
            // which file the doctest is coming from. That way, imports would be resolved
            // relative to it (which I think is the intuitive behavior)
            .add_string(SourcePath::Generated("test".to_owned()), self.code.clone());

        let term = main_program.eval_record_spine()?;
        let data = match term.as_ref() {
            Term::Record(data, ..) | Term::RecRecord(data, ..) => data,
            _ => todo!(),
        };
        let env: Environment = data
            .fields
            .iter()
            .filter_map(|(id, field)| {
                field.value.as_ref().map(|value| {
                    let clos = Closure {
                        body: value.clone(),
                        env: Environment::new(),
                    };
                    (id.ident(), Thunk::new(clos))
                })
            })
            .collect();

        main_program.vm.reset();
        main_program
            .vm
            .import_resolver
            .parse(src_id, InputFormat::Nickel)
            .unwrap();
        main_program.vm.import_resolver.transform(src_id).unwrap();
        let test_term = main_program.vm.import_resolver().get(src_id).unwrap();
        let test_clos = Closure {
            body: test_term,
            env,
        };

        dbg!(main_program.vm.eval_closure(test_clos).unwrap());

        Ok(())
    }
}

fn nickel_code_blocks<'a>(document: &'a AstNode<'a>) -> Vec<CodeBlock> {
    use comrak::arena_tree::Node;
    use comrak::nodes::{Ast, NodeCodeBlock, NodeValue};
    document
        .traverse()
        .filter_map(|ne| match ne {
            NodeEdge::Start(Node { data, .. }) => match &*data.borrow() {
                Ast {
                    value: NodeValue::CodeBlock(NodeCodeBlock { info, literal, .. }),
                    ..
                } => info.strip_prefix("nickel").and_then(|tag| {
                    (tag.trim() != "ignore").then(|| {
                        let mut expected = None;
                        // If the last line of the code block looks like
                        // `# => <nickel_expr>`
                        // then we'll assert that the code block evaluates to <nickel_expr>.
                        if let Some(last_nonempty) = literal.lines().rfind(|line| !line.is_empty())
                        {
                            if let Some(commented) = last_nonempty.trim_start().strip_prefix('#') {
                                if let Some(arrowed) = commented.trim_start().strip_prefix("=>") {
                                    expected = Some(arrowed.to_owned());
                                }
                            }
                        }
                        CodeBlock {
                            code: literal.to_owned(),
                            expected,
                        }
                    })
                }),
                _ => None,
            },
            _ => None,
        })
        .collect()
}
