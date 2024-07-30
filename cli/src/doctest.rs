use comrak::{arena_tree::NodeEdge, nodes::AstNode, Arena, ComrakOptions};
use nickel_lang_core::{
    cache::{Cache, SourcePath},
    error::Error,
    eval::cache::CacheImpl,
    identifier::Ident,
    match_sharedterm,
    program::Program,
    term::{record::RecordData, RichTerm, Term, Traverse as _, TraverseOrder},
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
        let term = program.parse()?;
        let transformed = doctest_transform(&mut program.vm.import_resolver, term);
        println!("{transformed}");
        let state = program
            .vm
            .import_resolver
            .set(program.main_id, transformed)
            .unwrap();
        dbg!(state);

        program.eval_record_spine().map(|_| ())
    }
}

fn nickel_code_blocks<'a>(document: &'a AstNode<'a>) -> Vec<String> {
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
                } => info
                    .strip_prefix("nickel")
                    .and_then(|tag| (tag.trim() != "ignore").then(|| literal.to_owned())),
                _ => None,
            },
            _ => None,
        })
        .collect()
}

fn doctest_transform(cache: &mut Cache, rt: RichTerm) -> RichTerm {
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
                        cache.add_string(SourcePath::Generated("test".to_owned()), snippet.clone());
                    let test_term = cache.parse_nocache(src_id).unwrap().0;
                    let test_id = Ident::new(format!("__test_{id}_{i}"));
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
