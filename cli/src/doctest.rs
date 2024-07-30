use std::collections::HashMap;

use comrak::{arena_tree::NodeEdge, nodes::AstNode, Arena, ComrakOptions};
use nickel_lang_core::{
    cache::{Cache, EntryState, SourcePath},
    closurize::Closurize as _,
    error::Error,
    eval::{cache::CacheImpl, Closure},
    identifier::Ident,
    match_sharedterm, mk_app,
    program::Program,
    term::{record::RecordData, RichTerm, Term, Traverse as _, TraverseOrder},
};
use sha2::Digest as _;

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
}

struct DocTest {
    input: String,
    expected: Expected,
}

const ASSERT: &str = r#"
    %contract/custom%
      (
        fun _label value =>
          if value then
            'Ok value
          else
            'Error {}
      )
"#;

impl DocTest {
    fn new(input: String) -> Self {
        let expected = Expected::extract(&input);
        DocTest { input, expected }
    }

    fn code(&self) -> String {
        match &self.expected {
            Expected::Value(v) => format!("({}) == ({}) | ({ASSERT})", self.input, v),
            // If we expect an error, wrap the value in a function so that it doesn't get automatically
            // evaluated. (We'll call `eval_record_spine`, so putting it in a record doesn't help.)
            Expected::Error(_) => format!("fun _ => {}", self.input),
            Expected::None => self.input.clone(),
        }
    }

    fn name(&self, field_name: &str, idx: usize) -> Ident {
        let mut hasher = sha2::Sha256::new();
        hasher.update(&self.input);
        let hash = hasher.finalize();
        let hashstr = hex::encode(&hash[..16]);
        let header = match &self.expected {
            Expected::Error(_) => "errtest",
            _ => "test",
        };
        Ident::new(format!("__{header}_{field_name}_{hashstr}_{idx}"))
    }
}

// Go through the record spine looking for tests that are supposed to error.
fn run_error_tests(
    prog: &mut Program<CacheImpl>,
    expected_errors: &HashMap<Ident, String>,
    term: &RichTerm,
) -> CliResult<()> {
    match term.as_ref() {
        Term::Record(data) | Term::RecRecord(data, ..) => {
            for (id, field) in &data.fields {
                if let Some(expected) = expected_errors.get(&id.ident()) {
                    if let Some(val) = &field.value {
                        let result = prog.vm.eval_closure(Closure {
                            body: mk_app!(val.clone(), Term::Null),
                            env: Default::default(),
                        });
                        match result {
                            Ok(v) => {
                                return Err(crate::error::Error::ExpectedTestFailure {
                                    result: v.body,
                                    // TODO: store the expected error somewhere
                                    expected: expected.clone(),
                                });
                            }
                            Err(e) => {
                                // TODO: maybe we should turn off color for this? Or maybe use IntoDiagnostics directly
                                // and only look at the main message instead of the notes?
                                let msg = prog.report_as_str(e);
                                if !msg.contains(expected) {
                                    return Err(crate::error::Error::WrongTestFailure {
                                        msg,
                                        expected: expected.clone(),
                                    });
                                }
                            }
                        }
                    }
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

impl TestCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.input.prepare(&global)?;

        let (term, expected_errors) = match self.run_success_tests(&mut program) {
            Ok(x) => x,
            Err(error) => return Err(crate::error::Error::Program { program, error }),
        };

        run_error_tests(&mut program, &expected_errors, &term)?;

        Ok(())
    }

    fn run_success_tests(
        self,
        program: &mut Program<CacheImpl>,
    ) -> Result<(RichTerm, HashMap<Ident, String>), Error> {
        let mut expected_errors = HashMap::new();
        let term = program.parse()?;
        let transformed =
            doctest_transform(&mut program.vm.import_resolver, &mut expected_errors, term);
        println!("{transformed}");
        let state = program
            .vm
            .import_resolver
            .set(program.main_id, transformed)
            .unwrap();
        assert_eq!(state, EntryState::Parsed);
        Ok((program.eval_record_spine()?, expected_errors))
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

fn doctest_transform(
    cache: &mut Cache,
    expected_errors: &mut HashMap<Ident, String>,
    rt: RichTerm,
) -> RichTerm {
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
                    let test_id = snippet.name(id.as_ref(), i);
                    if let Expected::Error(s) = &snippet.expected {
                        expected_errors.insert(test_id, s.clone());
                    }
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
