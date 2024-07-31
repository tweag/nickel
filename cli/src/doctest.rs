use std::{collections::HashMap, io::Write as _};

use comrak::{arena_tree::NodeEdge, nodes::AstNode, Arena, ComrakOptions};
use nickel_lang_core::{
    cache::{Cache, EntryState, SourcePath},
    error::{Error as CoreError, EvalError},
    eval::{cache::CacheImpl, Closure},
    identifier::{Ident, LocIdent},
    match_sharedterm, mk_app, mk_fun,
    program::{FieldPath, Program},
    term::{record::RecordData, RichTerm, Term, Traverse as _, TraverseOrder},
};

use crate::{
    cli::GlobalOptions,
    customize::ExtractFieldOnly,
    error::CliResult,
    input::{InputOptions, Prepare},
};

#[derive(clap::Parser, Debug)]
pub struct TestCommand {
    #[command(flatten)]
    pub input: InputOptions<ExtractFieldOnly>,
}

enum Expected {
    Value(String),
    Error(String),
    None,
}

impl Expected {
    fn extract(doctest: &str) -> Self {
        if let Some(last_nonempty) = doctest.lines().rfind(|line| !line.is_empty()) {
            if let Some(commented) = last_nonempty.trim_start().strip_prefix('#') {
                if let Some(arrowed) = commented.trim_start().strip_prefix("=>") {
                    if let Some(msg) = arrowed.trim_start().strip_prefix("error:") {
                        return Expected::Error(msg.trim().to_owned());
                    } else {
                        return Expected::Value(arrowed.trim().to_owned());
                    }
                }
            }
        }
        Expected::None
    }

    fn error(&self) -> Option<String> {
        match self {
            Expected::Error(s) => Some(s.clone()),
            _ => None,
        }
    }
}

struct DocTest {
    input: String,
    expected: Expected,
}

struct TestEntry {
    expected_error: Option<String>,
    field_name: LocIdent,
    test_idx: usize,
}

#[derive(Default)]
struct TestRegistry {
    tests: HashMap<Ident, TestEntry>,
}

impl DocTest {
    fn new(input: String) -> Self {
        let expected = Expected::extract(&input);
        DocTest { input, expected }
    }

    fn code(&self) -> String {
        match &self.expected {
            Expected::Value(v) => format!("(({}) | std.contract.Equal ({v}))", self.input),
            _ => self.input.clone(),
        }
    }
}

struct Error {
    path: Vec<LocIdent>,
    idx: usize,
    kind: ErrorKind,
}

enum ErrorKind {
    UnexpectedFailure {
        error: EvalError,
    },
    /// A doctest was expected to fail, but instead it succeeded.
    UnexpectedSuccess {
        result: RichTerm,
    },
    /// A doctest failed with an unexpected message.
    WrongTestFailure {
        msg: String,
        expected: String,
    },
}

// Go through the record spine, running tests one-by-one
fn run_tests(
    path: &mut Vec<LocIdent>,
    prog: &mut Program<CacheImpl>,
    errors: &mut Vec<Error>,
    registry: &TestRegistry,
    term: &RichTerm,
    spine: &RichTerm,
) {
    match spine.as_ref() {
        Term::Record(data) | Term::RecRecord(data, ..) => {
            for (id, field) in &data.fields {
                if let Some(entry) = registry.tests.get(&id.ident()) {
                    let mut clos_path = path.clone();
                    clos_path.push(*id);

                    path.push(entry.field_name);
                    let path_display: Vec<_> = path.iter().map(|id| id.label()).collect();

                    print!("testing {}/{}...", path_display.join("."), entry.test_idx);
                    let _ = std::io::stdout().flush();

                    // Undo the test's lazy wrapper.
                    let cl = prog
                        .vm
                        .extract_field_value_closure(
                            Closure::atomic_closure(term.clone()),
                            &FieldPath(clos_path),
                        )
                        .unwrap();
                    let result = prog.vm.eval_closure(Closure {
                        body: mk_app!(cl.body, Term::Null),
                        env: cl.env,
                    });

                    let err = match result {
                        Ok(v) => {
                            if entry.expected_error.is_some() {
                                Some(ErrorKind::UnexpectedSuccess { result: v.body })
                            } else {
                                None
                            }
                        }
                        Err(e) => {
                            if let Some(expected) = &entry.expected_error {
                                // TODO: maybe we should turn off color for this? Or maybe use IntoDiagnostics directly
                                // and only look at the main message instead of the notes?
                                let msg = prog.report_as_str(e);
                                if !msg.contains(expected) {
                                    Some(ErrorKind::WrongTestFailure {
                                        msg,
                                        expected: expected.clone(),
                                    })
                                } else {
                                    None
                                }
                            } else {
                                Some(ErrorKind::UnexpectedFailure { error: e })
                            }
                        }
                    };
                    if let Some(err) = err {
                        println!("FAILED");
                        errors.push(Error {
                            kind: err,
                            path: path.clone(),
                            idx: entry.test_idx,
                        });
                    } else {
                        println!("ok");
                    }
                    path.pop();
                } else if let Some(val) = field.value.as_ref() {
                    path.push(*id);
                    run_tests(path, prog, errors, registry, term, val);
                    path.pop();
                }
            }
        }
        _ => {}
    }
}

impl TestCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.input.prepare(&global)?;

        let (spine, term, registry) = match self.prepare_tests(&mut program) {
            Ok(x) => x,
            Err(error) => return Err(crate::error::Error::Program { program, error }),
        };

        let mut path = Vec::new();
        let mut errors = Vec::new();
        run_tests(
            &mut path,
            &mut program,
            &mut errors,
            &registry,
            &term,
            &spine,
        );

        let has_error = !errors.is_empty();
        for error in errors {
            let path_display: Vec<_> = error.path.iter().map(|id| id.label()).collect();
            let path_display = path_display.join(".");
            match error.kind {
                // TODO: add locations
                ErrorKind::UnexpectedSuccess { result } => {
                    println!(
                        "test {}/{} succeeded (evaluated to {result}), but it should have failed",
                        path_display, error.idx
                    );
                }
                ErrorKind::WrongTestFailure { msg, expected } => {
                    println!(
                        r#"test {}/{} failed with error "{msg}", but we were expecting "{expected}""#,
                        path_display, error.idx
                    );
                }
                ErrorKind::UnexpectedFailure { error } => {
                    program.report(error, nickel_lang_core::error::report::ErrorFormat::Text);
                }
            }
        }

        if has_error {
            Err(crate::error::Error::FailedTests)
        } else {
            Ok(())
        }
    }

    fn prepare_tests(
        self,
        program: &mut Program<CacheImpl>,
    ) -> Result<(RichTerm, RichTerm, TestRegistry), CoreError> {
        let mut registry = TestRegistry::default();
        let term = program.parse()?;
        let transformed = doctest_transform(&mut program.vm.import_resolver, &mut registry, term);
        let state = program
            .vm
            .import_resolver
            .set(program.main_id, transformed.clone())
            .unwrap();
        assert_eq!(state, EntryState::Parsed);
        Ok((program.eval_record_spine()?, transformed, registry))
    }
}

fn nickel_code_blocks<'a>(document: &'a AstNode<'a>) -> Vec<DocTest> {
    use comrak::arena_tree::Node;
    use comrak::nodes::{Ast, NodeCodeBlock, NodeValue};
    document
        .traverse()
        .filter_map(|ne| match ne {
            // Question: can we extract enough location information so that
            // we can munge the parsed AST to point into the doc comment?
            NodeEdge::Start(Node { data, .. }) => match &*data.borrow() {
                Ast {
                    value: NodeValue::CodeBlock(NodeCodeBlock { info, literal, .. }),
                    ..
                } => info.strip_prefix("nickel").and_then(|tag| {
                    (tag.trim() != "ignore").then(|| DocTest::new(literal.to_owned()))
                }),
                _ => None,
            },
            _ => None,
        })
        .collect()
}

fn doctest_transform(cache: &mut Cache, registry: &mut TestRegistry, rt: RichTerm) -> RichTerm {
    let mut record_with_doctests = |mut record_data: RecordData, dyn_fields| {
        let mut doc_fields: Vec<(Ident, RichTerm)> = Vec::new();
        for (id, field) in &record_data.fields {
            if let Some(doc) = &field.metadata.doc {
                let arena = Arena::new();
                let snippets = nickel_code_blocks(comrak::parse_document(
                    &arena,
                    doc,
                    &ComrakOptions::default(),
                ));

                for (i, snippet) in snippets.iter().enumerate() {
                    let src_id =
                        cache.add_string(SourcePath::Generated("test".to_owned()), snippet.code());
                    let test_term = cache.parse_nocache(src_id).unwrap().0;

                    // Make the test term lazy, so that the tests don't automatically get evaluated
                    // just by evaluating the record spine.
                    let test_term = mk_fun!(LocIdent::fresh(), test_term);
                    let test_id = LocIdent::fresh().ident();
                    let entry = TestEntry {
                        expected_error: snippet.expected.error(),
                        field_name: *id,
                        test_idx: i,
                    };
                    registry.tests.insert(test_id, entry);
                    doc_fields.push((test_id, test_term));
                }
            }
        }
        for (id, term) in doc_fields {
            record_data.fields.insert(id.into(), term.into());
        }
        Term::RecRecord(record_data, dyn_fields, None).into()
    };

    let mut traversal = |rt: RichTerm| -> Result<RichTerm, ()> {
        let term = match_sharedterm!(match (rt.term) {
            Term::RecRecord(record_data, dyn_fields, _deps) => {
                record_with_doctests(record_data, dyn_fields)
            }
            Term::Record(record_data) => {
                record_with_doctests(record_data, Vec::new())
            }
            _ => rt,
        });
        Ok(term)
    };
    rt.traverse(&mut traversal, TraverseOrder::TopDown).unwrap()
}
