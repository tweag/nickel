use std::{collections::HashMap, io::Write as _};

use comrak::{arena_tree::NodeEdge, nodes::AstNode, Arena, ComrakOptions};
use nickel_lang_core::{
    cache::{Cache, EntryState, SourcePath},
    error::{Error as CoreError, EvalError, IntoDiagnostics},
    eval::{cache::CacheImpl, Closure, Environment},
    identifier::{Ident, LocIdent},
    match_sharedterm, mk_app, mk_fun,
    program::Program,
    term::{record::RecordData, RichTerm, Term, Traverse as _, TraverseOrder},
};
use once_cell::sync::Lazy;
use regex::Regex;

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

#[derive(Debug)]
enum Expected {
    Value(String),
    Error(String),
    None,
}

impl Expected {
    /// Parse out an expected outcome from a doctest.
    ///
    /// After ignoring whitespace-only trailing lines, we look for the last comment block in the doctest.
    /// If that comment block has a line starting (modulo whitespace) with "=>", everything following
    /// the "=>" is the expected value of the doctest.
    ///
    /// There are two special cases for tests that are expected to fail:
    /// - if the "=>" line looks like "=> error: some text", the test is expected to exit with an error,
    ///   and the error message is supposed to contain "some text".
    /// - if the "=>" line looks like "=> error", the test is expected to exit with an error (but we
    ///   don't care what the message is.
    fn extract(doctest: &str) -> Self {
        let mut lines: Vec<&str> = doctest.lines().collect();

        // Throw away trailing empty lines.
        let last_non_empty = lines
            .iter()
            .rposition(|line| !line.trim().is_empty())
            .unwrap_or(0);
        lines.truncate(last_non_empty + 1);

        let mut expected = Vec::new();
        for line in lines.iter().rev() {
            // If we encounter an uncommented line before we find a "=>", there's no expected value
            // for this test.
            let Some(commented) = line.trim_start().strip_prefix('#') else {
                break;
            };

            if let Some(arrowed) = commented.trim_start().strip_prefix("=>") {
                // We've found an expected value for the test.
                if let Some(msg) = arrowed.trim_start().strip_prefix("error:") {
                    expected.push(msg.trim());
                    expected.reverse();
                    return Expected::Error(expected.join("\n"));
                } else if arrowed.trim() == "error" {
                    return Expected::Error(String::new());
                } else {
                    expected.push(arrowed);
                    expected.reverse();
                    return Expected::Value(expected.join("\n"));
                }
            } else {
                expected.push(commented);
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

#[derive(Debug)]
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
            Expected::Value(v) => format!("(({}\n)| std.contract.Equal ({v}))", self.input),
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
        messages: Vec<String>,
        expected: String,
    },
}

// Go through the record spine, running tests one-by-one.
//
// `spine` is the already-evaluated record spine, which we use only for finding the path to
// each test. We don't use `spine` for evaluating the test because `RecRecord`s have already
// been evaluated to `Record`s so it's challenging to reconstruct the environment.
fn run_tests(
    path: &mut Vec<LocIdent>,
    prog: &mut Program<CacheImpl>,
    errors: &mut Vec<Error>,
    registry: &TestRegistry,
    spine: &RichTerm,
) {
    match spine.as_ref() {
        Term::Record(data) | Term::RecRecord(data, ..) => {
            for (id, field) in &data.fields {
                if let Some(entry) = registry.tests.get(&id.ident()) {
                    let Some(val) = field.value.as_ref() else {
                        continue;
                    };

                    path.push(entry.field_name);
                    let path_display: Vec<_> = path.iter().map(|id| id.label()).collect();

                    print!("testing {}/{}...", path_display.join("."), entry.test_idx);
                    let _ = std::io::stdout().flush();

                    prog.vm.reset();
                    // Undo the test's lazy wrapper.
                    let result = prog.vm.eval_closure(Closure {
                        body: mk_app!(val.clone(), Term::Null),
                        env: Environment::new(),
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
                                let diags = e.into_diagnostics(
                                    prog.vm.import_resolver_mut().files_mut(),
                                    None,
                                );
                                if !diags.iter().any(|diag| diag.message.contains(expected)) {
                                    Some(ErrorKind::WrongTestFailure {
                                        messages: diags.into_iter().map(|d| d.message).collect(),
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
                    run_tests(path, prog, errors, registry, val);
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

        let (spine, registry) = match self.prepare_tests(&mut program) {
            Ok(x) => x,
            Err(error) => return Err(crate::error::Error::Program { program, error }),
        };

        let mut path = Vec::new();
        let mut errors = Vec::new();
        run_tests(&mut path, &mut program, &mut errors, &registry, &spine);

        let num_errors = errors.len();
        for e in errors {
            let path_display: Vec<_> = e.path.iter().map(|id| id.label()).collect();
            let path_display = path_display.join(".");
            match e.kind {
                ErrorKind::UnexpectedSuccess { result } => {
                    println!(
                        "test {}/{} succeeded (evaluated to {result}), but it should have failed",
                        path_display, e.idx
                    );
                }
                ErrorKind::WrongTestFailure { messages, expected } => {
                    println!(
                        r#"test {}/{} failed with error "{}", but we were expecting "{expected}""#,
                        path_display,
                        e.idx,
                        messages.join(", "),
                    );
                }
                ErrorKind::UnexpectedFailure { error } => {
                    println!("test {}/{} failed", path_display, e.idx);
                    program.report(error, nickel_lang_core::error::report::ErrorFormat::Text);
                }
            }
        }

        if num_errors > 0 {
            println!("{num_errors} failures");
            Err(crate::error::Error::FailedTests)
        } else {
            Ok(())
        }
    }

    fn prepare_tests(
        self,
        program: &mut Program<CacheImpl>,
    ) -> Result<(RichTerm, TestRegistry), CoreError> {
        let mut registry = TestRegistry::default();
        let term = program.parse()?;
        let cache = &mut program.vm.import_resolver;
        let transformed = doctest_transform(cache, &mut registry, term);
        cache.set(program.main_id, transformed.clone()?, EntryState::Parsed);
        Ok((program.eval_record_spine()?, registry))
    }
}

fn nickel_code_blocks<'a>(document: &'a AstNode<'a>) -> Vec<DocTest> {
    use comrak::arena_tree::Node;
    use comrak::nodes::{Ast, NodeCodeBlock, NodeValue};
    document
        .traverse()
        .flat_map(|ne| match ne {
            // Question: can we extract enough location information so that
            // we can munge the parsed AST to point into the doc comment?
            NodeEdge::Start(Node { data, .. }) => match &*data.borrow() {
                Ast {
                    value: NodeValue::CodeBlock(NodeCodeBlock { info, literal, .. }),
                    ..
                } => info
                    .strip_prefix("nickel")
                    .map(|tag| match tag.trim() {
                        "ignore" => Vec::new(),
                        "multiline" => {
                            static BLANK_LINE: Lazy<Regex> =
                                Lazy::new(|| Regex::new("\n\\s*\n").unwrap());
                            BLANK_LINE
                                .split(literal)
                                .filter_map(|chunk| {
                                    if !chunk.trim().is_empty() {
                                        Some(DocTest::new(chunk.to_owned()))
                                    } else {
                                        None
                                    }
                                })
                                .collect()
                        }
                        _ => vec![DocTest::new(literal.to_owned())],
                    })
                    .unwrap_or_default(),
                _ => vec![],
            },
            _ => vec![],
        })
        .collect()
}

// Transform a term by taking all its doctests and inserting them into the record next
// to the field that they're annotating.
//
// For example,
// {
//   field | doc m%"
//     ```nickel
//       1 + 1
//     ```
//   "%
// }
// becomes
// {
//   field | doc m%"
//     ```nickel
//       1 + 1
//     ```
//   "%,
//   %0 = fun %1 => 1 + 1,
// }
//
// The idea is for the test to be evaluated in the same environment as the
// field that declares it. We wrap the test in a function so that it doesn't get
// evaluated too soon.
//
// One disadvantage with this traversal approach is that any parse errors in
// the test will be encountered as soon as we explore the record spine. We might
// prefer to delay parsing the tests until it's time to evaluate them.
// The main advantage of this approach is that it makes it easy to have the test
// evaluated in the right environment.
fn doctest_transform(
    cache: &mut Cache,
    registry: &mut TestRegistry,
    rt: RichTerm,
) -> Result<RichTerm, CoreError> {
    let mut record_with_doctests =
        |mut record_data: RecordData, dyn_fields, pos| -> Result<_, CoreError> {
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
                        let src_id = cache
                            .add_string(SourcePath::Generated("test".to_owned()), snippet.code());
                        let (test_term, errors) = cache.parse_nocache(src_id)?;
                        if !errors.errors.is_empty() {
                            return Err(errors.into());
                        }

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
            Ok(RichTerm::from(Term::RecRecord(record_data, dyn_fields, None)).with_pos(pos))
        };

    let mut traversal = |rt: RichTerm| -> Result<RichTerm, CoreError> {
        let term = match_sharedterm!(match (rt.term) {
            Term::RecRecord(record_data, dyn_fields, _deps) => {
                record_with_doctests(record_data, dyn_fields, rt.pos)?
            }
            Term::Record(record_data) => {
                record_with_doctests(record_data, Vec::new(), rt.pos)?
            }
            _ => rt,
        });
        Ok(term)
    };
    rt.traverse(&mut traversal, TraverseOrder::TopDown)
}
