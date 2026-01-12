//! The `nickel test` command.
//!
//! Extracts tests from docstrings and evaluates them, printing out any failures.

use std::{collections::HashMap, io::Write as _, path::PathBuf, rc::Rc};

use comrak::{Arena, arena_tree::NodeEdge, nodes::AstNode};
use nickel_lang_core::{
    cache::{CacheHub, ImportResolver, InputFormat, SourcePath},
    error::{
        Error as CoreError, EvalErrorData, Reporter as _,
        report::{ColorOpt, report_as_str, report_to_stdout},
    },
    eval::{
        Closure, Environment,
        cache::{CacheImpl, lazy::CBNCache},
        value::{Container, NickelValue, ValueContent, ValueContentRef, lens::TermContent},
    },
    identifier::{Ident, LocIdent},
    label::Label,
    mk_app, mk_fun,
    position::PosTable,
    program::Program,
    term::{LabeledType, Term, TypeAnnotation, make, record::RecordData},
    traverse::{Traverse as _, TraverseOrder},
    typ::{Type, TypeF},
    typecheck::TypecheckMode,
};
use once_cell::sync::Lazy;
use regex::Regex;

use crate::{
    color_opt_from_clap,
    customize::ExtractFieldOnly,
    global::GlobalContext,
    input::{InputOptions, NickelOnly},
};

#[derive(clap::Parser, Debug)]
pub struct TestCommand {
    #[command(flatten)]
    pub input: InputOptions<ExtractFieldOnly, NickelOnly>,
}

/// The expected outcome of a test.
#[derive(Debug)]
enum Expected {
    /// The test is expected to evaluate (without errors) to a specific value.
    ///
    /// The string here will be parsed into a nickel term, and then wrapped in a `std.contract.Equal`
    /// contract to provide a nice error message.
    Value(String),
    /// The test is expected to raise an error, and the error message is expected to contain
    /// this string as a substring.
    Error(String),
    /// The test is expected to evaluate without errors, but we don't care what it evaluates to.
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
}

struct Error {
    /// The record path to the field whose doctest triggered this error.
    path: Vec<LocIdent>,
    /// The field whose doctest triggered this error might have multiple tests in its
    /// doc metadata. This is the index of the failing test.
    idx: usize,
    kind: ErrorKind,
}

enum ErrorKind {
    /// A doctest was expected to succeed, but it failed.
    UnexpectedFailure { error: Box<EvalErrorData> },
    /// A doctest was expected to fail, but instead it succeeded.
    UnexpectedSuccess { result: NickelValue },
    /// A doctest failed with an unexpected message.
    WrongTestFailure { message: String, expected: String },
}

// Go through the record spine, running tests one-by-one.
//
// `spine` is the already-evaluated record spine. It was previously transformed
// with [`doctest_transform`], so all the tests are present in the record spine.
// They've already been closurized with the correct environment.
fn run_tests(
    path: &mut Vec<LocIdent>,
    prog: &mut Program<CacheImpl>,
    errors: &mut Vec<Error>,
    registry: &TestRegistry,
    spine: &NickelValue,
    color: ColorOpt,
) {
    let mut run_record_tests = |record: &RecordData| {
        for (id, field) in &record.fields {
            if let Some(entry) = registry.tests.get(&id.ident()) {
                let Some(val) = field.value.as_ref() else {
                    continue;
                };

                path.push(entry.field_name);
                let path_display: Vec<_> = path.iter().map(|id| id.label()).collect();

                print!("testing {}/{}...", path_display.join("."), entry.test_idx);
                let _ = std::io::stdout().flush();

                // Undo the test's lazy wrapper.
                let result = prog.eval_deep_closure(Closure {
                    value: mk_app!(val.clone(), NickelValue::null()),
                    env: Environment::new(),
                });

                let err = match result {
                    Ok(v) => {
                        if entry.expected_error.is_some() {
                            Some(ErrorKind::UnexpectedSuccess { result: v })
                        } else {
                            None
                        }
                    }
                    Err(e) => {
                        if let Some(expected) = &entry.expected_error {
                            let message = report_as_str(&mut prog.files(), e, color);
                            if !message.contains(expected) {
                                Some(ErrorKind::WrongTestFailure {
                                    message,
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
                run_tests(path, prog, errors, registry, val, color);
                path.pop();
            }
        }
    };

    match spine.content_ref() {
        ValueContentRef::Record(Container::Alloc(record)) => run_record_tests(record),
        ValueContentRef::Term(Term::RecRecord(data)) => run_record_tests(&data.record),
        _ => {}
    }
}

impl TestCommand {
    pub fn run(self, ctxt: &mut GlobalContext) {
        let color: ColorOpt = color_opt_from_clap(ctxt.opts.color);
        let num_errors = ctxt
            .with_program(&self.input, |prog| self.run_tests(prog, color))
            .unwrap_or(0);

        if num_errors > 0 {
            eprintln!("{num_errors} failures");
            ctxt.reporter.report(crate::error::Error::FailedTests);
        }
    }

    /// Returns the number of test failures.
    fn run_tests(
        &self,
        program: &mut Program<CBNCache>,
        color: ColorOpt,
    ) -> Result<usize, CoreError> {
        let (spine, registry) = self.prepare_tests(program)?;

        let mut path = Vec::new();
        let mut errors = Vec::new();
        run_tests(&mut path, program, &mut errors, &registry, &spine, color);

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
                ErrorKind::WrongTestFailure { message, expected } => {
                    println!(
                        "test {}/{} failed, but the error didn't contain \"{expected}\". Actual error:\n{}",
                        path_display, e.idx, message,
                    );
                }
                ErrorKind::UnexpectedFailure { error } => {
                    println!("test {}/{} failed", path_display, e.idx);
                    report_to_stdout(
                        &mut program.files(),
                        *error,
                        nickel_lang_core::error::report::ErrorFormat::Text,
                        color,
                    );
                }
            }
        }

        Ok(num_errors)
    }

    fn prepare_tests(
        &self,
        program: &mut Program<CacheImpl>,
    ) -> Result<(NickelValue, TestRegistry), CoreError> {
        let mut registry = TestRegistry::default();
        program.typecheck(TypecheckMode::Walk)?;
        program.compile()?;
        program
            .custom_transform(0, |cache, pos_table, rt| {
                doctest_transform(pos_table, cache, &mut registry, rt)
            })
            .map_err(|e| e.unwrap_error("transforming doctest"))?;
        Ok((program.eval_closurized_record_spine()?, registry))
    }
}

/// Extract all the nickel code blocks from a single doc comment.
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
                    value: NodeValue::CodeBlock(block),
                    ..
                } => {
                    let NodeCodeBlock { info, literal, .. } = &**block;
                    info.strip_prefix("nickel")
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
                        .unwrap_or_default()
                }
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
// The generated test field ids (i.e. `%0` in the example above) are collected
// in `registry` so that a later pass can go through and evaluate them.
//
// One disadvantage with this traversal approach is that any parse errors in
// the test will be encountered as soon as we explore the record spine. We might
// prefer to delay parsing the tests until it's time to evaluate them.
// The main advantage of this approach is that it makes it easy to have the test
// evaluated in the right environment.
fn doctest_transform(
    pos_table: &mut PosTable,
    cache: &mut CacheHub,
    registry: &mut TestRegistry,
    value: NickelValue,
) -> Result<NickelValue, CoreError> {
    // Get the path that of the current term, so we can pretend that test snippets
    // came from the same path. This allows imports to work.
    let path = value
        .pos(pos_table)
        .as_opt_ref()
        .and_then(|sp| cache.get_path(sp.src_id))
        .map(PathBuf::from);

    let source_path = match path {
        Some(p) => SourcePath::Snippet(p),
        None => SourcePath::Generated("test".to_owned()),
    };

    // Prepare a test snippet. Skips typechecking and transformations, because
    // the returned term will get inserted into a bigger term that will be
    // typechecked and transformed.
    fn prepare(
        pos_table: &mut PosTable,
        cache: &mut CacheHub,
        input: &str,
        source_path: &SourcePath,
    ) -> Result<NickelValue, CoreError> {
        let src_id = cache
            .sources
            .add_string(source_path.clone(), input.to_owned());
        cache.parse_to_term(pos_table, src_id, InputFormat::Nickel)?;
        // unwrap(): we just populated it
        Ok(cache.get(src_id).unwrap())
    }

    let mut record_with_doctests = |mut record_data: RecordData,
                                    rec_stuff: Option<(Vec<_>, Vec<_>)>,
                                    pos|
     -> Result<_, CoreError> {
        let mut doc_fields: Vec<(Ident, NickelValue)> = Vec::new();

        for (id, field) in &record_data.fields {
            if let Some(doc) = field.metadata.doc() {
                let arena = Arena::new();
                let snippets = nickel_code_blocks(comrak::parse_document(
                    &arena,
                    doc,
                    &comrak::Options::default(),
                ));

                for (i, snippet) in snippets.iter().enumerate() {
                    let mut test_term = prepare(pos_table, cache, &snippet.input, &source_path)?;

                    if let Expected::Value(s) = &snippet.expected {
                        // Create the contract `std.contract.Equal <expected>` and apply it to the
                        // test term.
                        let expected_term = prepare(pos_table, cache, s, &source_path)?;
                        // unwrap: we just parsed it, so it will have a span
                        let expected_span = expected_term.pos_idx();

                        let eq = make::static_access(
                            NickelValue::term_posless(Term::Var("std".into())),
                            ["contract", "Equal"],
                        );
                        let eq = mk_app!(eq, expected_term);
                        let eq_ty = Type::from(TypeF::Contract(eq));
                        test_term = Term::annotated(
                            TypeAnnotation {
                                typ: None,
                                contracts: vec![LabeledType {
                                    typ: eq_ty.clone(),
                                    label: Label {
                                        typ: Rc::new(eq_ty),
                                        span: expected_span,
                                        ..Default::default()
                                    },
                                }],
                            },
                            test_term,
                        )
                        .into();
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

        // We have to be careful about turning Records into RecRecords, because
        // some places (e.g. compiled match expressions) assume that they have
        // Records.
        let value = if let Some((includes, dyn_fields)) = rec_stuff {
            NickelValue::term(
                Term::rec_record(record_data, includes, dyn_fields, None, false),
                pos,
            )
        } else {
            NickelValue::record(record_data, pos)
        };

        Ok(value)
    };

    let mut traversal = |value: NickelValue| -> Result<NickelValue, CoreError> {
        let pos_idx = value.pos_idx();

        let value = match value.content() {
            ValueContent::Term(TermContent::RecRecord(lens)) => {
                let data = lens.take();
                record_with_doctests(data.record, Some((data.includes, data.dyn_fields)), pos_idx)?
            }
            ValueContent::Record(lens) => {
                record_with_doctests(lens.take().unwrap_or_alloc(), None, pos_idx)?
            }
            lens => lens.restore(),
        };

        Ok(value)
    };
    value.traverse(&mut traversal, TraverseOrder::TopDown)
}
