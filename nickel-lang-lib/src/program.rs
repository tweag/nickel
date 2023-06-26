//! Program handling, from file reading to evaluation.
//!
//! A program is Nickel source code loaded from an input. This module offers an interface to load a
//! program source, parse it, evaluate it and report errors.
//!
//! # Standard library
//!
//! Some essential functions required for evaluation, such as builtin contracts, are written in
//! pure Nickel. Standard library files must be record literals:
//!
//! ```text
//! {
//!     val1 = ...
//!     val2 = ...
//! }
//! ```
//!
//! These .ncl file are not actually distributed as files, instead they are embedded, as plain
//! text, in the Nickel executable. The embedding is done by way of the [crate::stdlib], which
//! exposes the standard library files as strings. The embedded strings are then parsed by the
//! functions in [`crate::cache`] (see [`crate::cache::Cache::mk_eval_env`]).
//! Each such value is added to the initial environment before the evaluation of the program.
use crate::cache::*;
use crate::error::{Error, IntoDiagnostics, ParseError};
use crate::eval;
use crate::eval::cache::Cache as EvalCache;
use crate::eval::VirtualMachine;
use crate::identifier::Ident;
use crate::term::{record::Field, RichTerm};
use codespan::FileId;
use codespan_reporting::term::termcolor::{Ansi, ColorChoice, StandardStream};
use std::ffi::OsString;
use std::io::{self, Cursor, Read, Write};
use std::result::Result;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ColorOpt(pub(crate) clap::ColorChoice);

impl From<clap::ColorChoice> for ColorOpt {
    fn from(color_choice: clap::ColorChoice) -> Self {
        Self(color_choice)
    }
}

/// Attribute path provided when querying metadata.
#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub struct QueryPath(pub Vec<Ident>);

impl QueryPath {
    pub fn new() -> Self {
        Self::default()
    }

    /// Parse a string as a query path. A query path is a sequence of dot-separated identifiers.
    /// Identifiers can be enclosed by double quotes when they contain characters that aren't
    /// allowed inside bare identifiers. The accepted grammar is the same as a sequence of record
    /// accesses in Nickel, although string interpolation is forbidden.
    pub fn parse(cache: &mut Cache, input: String) -> Result<Self, ParseError> {
        use crate::parser::{
            grammar::FieldPathParser, lexer::Lexer, utils::FieldPathElem, ErrorTolerantParser,
        };

        let format_name = "query-path";
        let input_id = cache.replace_string(format_name, input);
        let s = cache.source(input_id);

        let parser = FieldPathParser::new();
        let field_path = parser
            .parse_strict(input_id, Lexer::new(s))
            // We just need to report an error here
            .map_err(|mut errs| {
                errs.errors.pop().expect(
                    "because parsing of the query path failed, the error \
                                              list must be non-empty, put .pop() failed",
                )
            })?;

        let path_as_idents: Result<Vec<Ident>, ParseError> = field_path
            .into_iter()
            .map(|elem| match elem {
                FieldPathElem::Ident(ident) => Ok(ident),
                FieldPathElem::Expr(expr) => {
                    let as_string = expr.as_ref().try_str_chunk_as_static_str().ok_or(
                        ParseError::InterpolationInQuery {
                            input: s.into(),
                            pos_path_elem: expr.pos,
                        },
                    )?;
                    Ok(Ident::from(as_string))
                }
            })
            .collect();

        path_as_idents.map(QueryPath)
    }

    /// As [`Self::parse`], but accepts an `Option` to accomodate for the absence of path. If the
    /// input is `None`, `Ok(QueryPath::default())` is returned (that is, an empty query path).
    pub fn parse_opt(cache: &mut Cache, input: Option<String>) -> Result<Self, ParseError> {
        Ok(input
            .map(|path| Self::parse(cache, path))
            .transpose()?
            .unwrap_or_default())
    }
}

/// A Nickel program.
///
/// Manage a file database, which stores the original source code of the program and eventually the
/// code of imported expressions, and a dictionary which stores corresponding parsed terms.
pub struct Program<EC: EvalCache> {
    /// The id of the program source in the file database.
    main_id: FileId,
    /// The state of the Nickel virtual machine.
    vm: VirtualMachine<Cache, EC>,
    /// The color option to use when reporting errors.
    color_opt: ColorOpt,
}

impl<EC: EvalCache> Program<EC> {
    /// Create a program by reading it from the standard input.
    pub fn new_from_stdin(trace: impl Write + 'static) -> std::io::Result<Self> {
        Program::new_from_source(io::stdin(), "<stdin>", trace)
    }

    pub fn new_from_file(
        path: impl Into<OsString>,
        trace: impl Write + 'static,
    ) -> std::io::Result<Self> {
        let mut cache = Cache::new(ErrorTolerance::Strict);
        let main_id = cache.add_file(path)?;
        let vm = VirtualMachine::new(cache, trace);

        Ok(Self {
            main_id,
            vm,
            color_opt: clap::ColorChoice::Auto.into(),
        })
    }

    /// Create a program by reading it from a generic source.
    pub fn new_from_source<T, S>(
        source: T,
        source_name: S,
        trace: impl Write + 'static,
    ) -> std::io::Result<Self>
    where
        T: Read,
        S: Into<OsString> + Clone,
    {
        let mut cache = Cache::new(ErrorTolerance::Strict);
        let main_id = cache.add_source(source_name, source)?;
        let vm = VirtualMachine::new(cache, trace);

        Ok(Self {
            main_id,
            vm,
            color_opt: clap::ColorChoice::Auto.into(),
        })
    }

    /// Retrieve the parsed term and typecheck it, and generate a fresh initial environment. Return
    /// both.
    fn prepare_eval(&mut self) -> Result<(RichTerm, eval::Environment), Error> {
        self.vm.prepare_eval(self.main_id)
    }

    /// Parse if necessary, typecheck and then evaluate the program.
    pub fn eval(&mut self) -> Result<RichTerm, Error> {
        let (t, initial_env) = self.prepare_eval()?;
        self.vm.reset();
        self.vm.eval(t, &initial_env).map_err(|e| e.into())
    }

    /// Same as `eval`, but proceeds to a full evaluation.
    pub fn eval_full(&mut self) -> Result<RichTerm, Error> {
        let (t, initial_env) = self.prepare_eval()?;
        self.vm.reset();
        self.vm.eval_full(t, &initial_env).map_err(|e| e.into())
    }

    /// Same as `eval`, but proceeds to a full evaluation.
    /// Skips record fields marked `not_exported`
    pub fn eval_full_for_export(&mut self) -> Result<RichTerm, Error> {
        let (t, initial_env) = self.prepare_eval()?;
        self.vm.reset();
        self.vm
            .eval_full_for_export(t, &initial_env)
            .map_err(|e| e.into())
    }

    /// Same as `eval_full`, but does not substitute all variables.
    pub fn eval_deep(&mut self) -> Result<RichTerm, Error> {
        let (t, initial_env) = self.prepare_eval()?;
        self.vm.reset();
        self.vm.eval_deep(t, &initial_env).map_err(|e| e.into())
    }

    /// Wrapper for [`query`].
    pub fn query(&mut self, path: Option<String>) -> Result<Field, Error> {
        let initial_env = self.vm.prepare_stdlib()?;
        let query_path = QueryPath::parse_opt(self.vm.import_resolver_mut(), path)?;
        query(&mut self.vm, self.main_id, &initial_env, query_path)
    }

    /// Load, parse, and typecheck the program and the standard library, if not already done.
    pub fn typecheck(&mut self) -> Result<(), Error> {
        self.vm.import_resolver_mut().parse(self.main_id)?;
        self.vm.import_resolver_mut().load_stdlib()?;
        let initial_env = self.vm.import_resolver().mk_type_ctxt().expect("program::typecheck(): stdlib has been loaded but was not found in cache on mk_types_env()");
        self.vm
            .import_resolver_mut()
            .resolve_imports(self.main_id)
            .map_err(|cache_err| {
                cache_err.unwrap_error("program::typecheck(): expected source to be parsed")
            })?;
        self.vm
            .import_resolver_mut()
            .typecheck(self.main_id, &initial_env)
            .map_err(|cache_err| {
                cache_err.unwrap_error("program::typecheck(): expected source to be parsed")
            })?;
        Ok(())
    }

    /// Wrapper for [`report`].
    pub fn report<E>(&mut self, error: E)
    where
        E: IntoDiagnostics<FileId>,
    {
        report(self.vm.import_resolver_mut(), error, self.color_opt)
    }

    /// Build an error report as a string and return it.
    pub fn report_as_str<E>(&mut self, error: E) -> String
    where
        E: IntoDiagnostics<FileId>,
    {
        let cache = self.vm.import_resolver_mut();
        let stdlib_ids = cache.get_all_stdlib_modules_file_id();
        let diagnostics = error.into_diagnostics(cache.files_mut(), stdlib_ids.as_ref());
        let mut buffer = Ansi::new(Cursor::new(Vec::new()));
        let config = codespan_reporting::term::Config::default();
        // write to `buffer`
        diagnostics
            .iter()
            .try_for_each(|d| {
                codespan_reporting::term::emit(&mut buffer, &config, cache.files_mut(), d)
            })
            // safe because writing to a cursor in memory
            .unwrap();
        // unwrap(): emit() should only print valid utf8 to the the buffer
        String::from_utf8(buffer.into_inner().into_inner()).unwrap()
    }

    /// Extract documentation from the program
    #[cfg(feature = "doc")]
    pub fn extract_doc(&mut self) -> Result<doc::ExtractedDocumentation, Error> {
        use crate::error::ExportError;

        self.vm.import_resolver_mut().parse(self.main_id)?;
        let term = self
            .vm
            .import_resolver()
            .get_ref(self.main_id)
            .expect("The file has been parsed and must therefore be in the cache");
        doc::ExtractedDocumentation::extract_from_term(term).ok_or(Error::ExportError(
            ExportError::NoDocumentation(term.clone()),
        ))
    }

    #[cfg(debug_assertions)]
    pub fn set_skip_stdlib(&mut self) {
        self.vm.import_resolver_mut().skip_stdlib = true;
    }

    pub fn set_color(&mut self, c: ColorOpt) {
        self.color_opt = c;
    }

    pub fn pprint_ast(
        &mut self,
        out: &mut std::io::BufWriter<Box<dyn std::io::Write>>,
        apply_transforms: bool,
    ) -> Result<(), Error> {
        use crate::pretty::*;
        use pretty::BoxAllocator;

        let Program {
            ref main_id, vm, ..
        } = self;
        let allocator = BoxAllocator;

        let rt = vm.import_resolver().parse_nocache(*main_id)?.0;
        let rt = if apply_transforms {
            crate::transform::transform(rt, None).unwrap()
        } else {
            rt
        };
        let doc: DocBuilder<_, ()> = rt.pretty(&allocator);
        doc.render(80, out).unwrap();
        Ok(())
    }
}

/// Query the metadata of a path of a term in the cache.
///
/// The path is a list of dot separated identifiers. For example, querying `{a = {b  = ..}}` (call
/// it `exp`) with path `a.b` will evaluate `exp.a` and retrieve the `b` field. `b` is forced as
/// well, in order to print its value (note that forced just means evaluated to a WHNF, it isn't
/// deeply - or recursively - evaluated).
//TODO: also gather type information, such that `query a.b.c <<< '{ ... } : {a: {b: {c: Num}}}`
//would additionally report `type: Num` for example. Maybe use the LSP infrastructure?
//TODO: not sure where this should go. It seems to embed too much logic to be in `Cache`, but is
//common to both `Program` and `Repl`. Leaving it here as a stand-alone function for now
pub fn query<EC: EvalCache>(
    vm: &mut VirtualMachine<Cache, EC>,
    file_id: FileId,
    initial_env: &Envs,
    path: QueryPath,
) -> Result<Field, Error> {
    vm.import_resolver_mut()
        .prepare(file_id, &initial_env.type_ctxt)?;

    let rt = vm.import_resolver().get_owned(file_id).unwrap();
    Ok(vm.query(rt, path, &initial_env.eval_env)?)
}

/// Pretty-print an error.
///
/// This function is located here in `Program` because errors need a reference to `files` in order
/// to produce a diagnostic (see `crate::error::label_alt`).
//TODO: not sure where this should go. It seems to embed too much logic to be in `Cache`, but is
//common to both `Program` and `Repl`. Leaving it here as a stand-alone function for now
pub fn report<E>(cache: &mut Cache, error: E, color_opt: ColorOpt)
where
    E: IntoDiagnostics<FileId>,
{
    let writer = StandardStream::stderr(color_opt.into());
    let config = codespan_reporting::term::Config::default();
    let stdlib_ids = cache.get_all_stdlib_modules_file_id();
    let diagnostics = error.into_diagnostics(cache.files_mut(), stdlib_ids.as_ref());

    let result = diagnostics.iter().try_for_each(|d| {
        codespan_reporting::term::emit(&mut writer.lock(), &config, cache.files_mut(), d)
    });
    match result {
        Ok(()) => (),
        Err(err) => panic!("Program::report: could not print an error on stderr: {err}"),
    };
}

impl From<ColorOpt> for ColorChoice {
    fn from(c: ColorOpt) -> Self {
        match c.0 {
            clap::ColorChoice::Auto => ColorChoice::Auto,
            clap::ColorChoice::Always => ColorChoice::Always,
            clap::ColorChoice::Never => ColorChoice::Never,
        }
    }
}

#[cfg(feature = "doc")]
mod doc {
    use crate::error::{Error, ExportError, IOError};
    use crate::term::{RichTerm, Term};
    use comrak::arena_tree::NodeEdge;
    use comrak::nodes::{
        Ast, AstNode, ListDelimType, ListType, NodeCode, NodeHeading, NodeList, NodeValue,
    };
    use comrak::{format_commonmark, parse_document, Arena, ComrakOptions};
    use serde::{Deserialize, Serialize};
    use std::collections::HashMap;
    use std::io::Write;

    #[derive(Clone, Debug, Serialize, Deserialize)]
    #[serde(transparent)]
    pub struct ExtractedDocumentation {
        fields: HashMap<String, DocumentationField>,
    }

    #[derive(Clone, Debug, Serialize, Deserialize)]
    struct DocumentationField {
        /// Field value [`ExtractedDocumentation`], if any
        fields: Option<ExtractedDocumentation>,
        /// Rendered type annotation, if any
        types: Option<String>,
        /// Rendered contract annotations
        contracts: Vec<String>,
        /// Rendered documentation, if any
        documentation: Option<String>,
    }

    impl ExtractedDocumentation {
        pub fn extract_from_term(rt: &RichTerm) -> Option<Self> {
            match rt.term.as_ref() {
                Term::Record(record) | Term::RecRecord(record, _, _) => {
                    let fields = record
                        .fields
                        .iter()
                        .map(|(ident, field)| {
                            let fields = field.value.as_ref().and_then(Self::extract_from_term);

                            let types = field
                                .metadata
                                .annotation
                                .types
                                .as_ref()
                                .map(|lt| lt.types.to_string());

                            let contracts = field
                                .metadata
                                .annotation
                                .contracts
                                .iter()
                                .map(|lt| lt.types.to_string())
                                .collect();

                            let documentation = field.metadata.doc.clone();

                            (
                                ident.label().to_owned(),
                                DocumentationField {
                                    fields,
                                    types,
                                    contracts,
                                    documentation,
                                },
                            )
                        })
                        .collect();
                    Some(Self { fields })
                }
                _ => None,
            }
        }

        pub fn write_json(&self, out: &mut dyn Write) -> Result<(), Error> {
            serde_json::to_writer(out, self)
                .map_err(|e| Error::ExportError(ExportError::Other(e.to_string())))
        }

        pub fn write_markdown(&self, out: &mut dyn Write) -> Result<(), Error> {
            let document = AstNode::from(NodeValue::Document);

            // Our nodes in the Markdown document are owned by this arena
            let arena = Arena::new();

            // The default ComrakOptions disables all extensions (essentially reducing to CommonMark)
            let options = ComrakOptions::default();

            self.markdown_append(0, &arena, &document, &options);
            format_commonmark(&document, &options, out)
                .map_err(|e| Error::IOError(IOError(e.to_string())))?;

            Ok(())
        }

        /// Recursively walk the given `DocOutput`, recursing into fields, looking for documentation.
        /// This documentation is then appended to the provided document.
        fn markdown_append<'a>(
            &'a self,
            header_level: u8,
            arena: &'a Arena<AstNode<'a>>,
            document: &'a AstNode<'a>,
            options: &ComrakOptions,
        ) {
            let mut entries: Vec<(_, _)> = self.fields.iter().collect();
            entries.sort_by_key(|(k, _)| *k);

            for (ident, field) in entries {
                let header = mk_header(ident, header_level + 1, arena);
                document.append(header);

                if field.types.is_some() || !field.contracts.is_empty() {
                    document.append(mk_types_and_contracts(
                        ident,
                        arena,
                        field.types.as_deref(),
                        field.contracts.as_ref(),
                    ))
                }

                if let Some(ref doc) = field.documentation {
                    document.append(parse_markdown_string(header_level + 1, arena, doc, options));
                }

                if let Some(ref subfields) = field.fields {
                    subfields.markdown_append(header_level + 1, arena, document, options);
                }
            }
        }
    }

    /// Parses a string into markdown and increases any headers in the markdown by the specified level.
    /// This allows having headers in documentation without clashing with the structure of the document.
    fn parse_markdown_string<'a>(
        header_level: u8,
        arena: &'a Arena<AstNode<'a>>,
        md: &str,
        options: &ComrakOptions,
    ) -> &'a AstNode<'a> {
        let node = parse_document(arena, md, options);

        // Increase header level of every header
        for edge in node.traverse() {
            if let NodeEdge::Start(n) = edge {
                n.data
                    .replace_with(|ast| increase_header_level(header_level, ast).clone());
            }
        }
        node
    }

    fn increase_header_level(header_level: u8, ast: &mut Ast) -> &Ast {
        if let NodeValue::Heading(NodeHeading { level, setext }) = ast.value {
            ast.value = NodeValue::Heading(NodeHeading {
                level: header_level + level,
                setext,
            });
        }
        ast
    }

    /// Creates a codespan header of the provided string with the provided header level.
    fn mk_header<'a>(
        ident: &str,
        header_level: u8,
        arena: &'a Arena<AstNode<'a>>,
    ) -> &'a AstNode<'a> {
        let res = arena.alloc(AstNode::from(NodeValue::Heading(NodeHeading {
            level: header_level,
            setext: false,
        })));

        let code = arena.alloc(AstNode::from(NodeValue::Code(NodeCode {
            num_backticks: 1,
            literal: ident.into(),
        })));

        res.append(code);

        res
    }

    fn mk_types_and_contracts<'a>(
        ident: &str,
        arena: &'a Arena<AstNode<'a>>,
        types: Option<&'a str>,
        contracts: &'a [String],
    ) -> &'a AstNode<'a> {
        let list = arena.alloc(AstNode::from(NodeValue::List(NodeList {
            list_type: ListType::Bullet,
            marker_offset: 1,
            padding: 0,
            start: 0,
            delimiter: ListDelimType::Period,
            bullet_char: b'*',
            tight: true,
        })));

        if let Some(t) = types {
            list.append(mk_type(ident, ':', t, arena));
        }

        for contract in contracts {
            list.append(mk_type(ident, '|', contract, arena));
        }

        list
    }

    fn mk_type<'a>(
        ident: &str,
        separator: char,
        types: &str,
        arena: &'a Arena<AstNode<'a>>,
    ) -> &'a AstNode<'a> {
        let list_item = arena.alloc(AstNode::from(NodeValue::Item(NodeList {
            list_type: ListType::Bullet,
            marker_offset: 1,
            padding: 0,
            start: 0,
            delimiter: ListDelimType::Period,
            bullet_char: b'*',
            tight: true,
        })));

        list_item.append(arena.alloc(AstNode::from(NodeValue::Code(NodeCode {
            literal: format!("{ident} {separator} {types}"),
            num_backticks: 1,
        }))));

        list_item
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::EvalError;
    use crate::eval::cache::CacheImpl;
    use crate::position::TermPos;
    use crate::term::array::ArrayAttrs;
    use assert_matches::assert_matches;
    use std::io::Cursor;

    fn eval_full(s: &str) -> Result<RichTerm, Error> {
        let src = Cursor::new(s);

        let mut p: Program<CacheImpl> = Program::new_from_source(src, "<test>", std::io::sink())
            .map_err(|io_err| {
                Error::EvalError(EvalError::Other(
                    format!("IO error: {io_err}"),
                    TermPos::None,
                ))
            })?;
        p.eval_full()
    }

    fn typecheck(s: &str) -> Result<(), Error> {
        let src = Cursor::new(s);

        let mut p: Program<CacheImpl> = Program::new_from_source(src, "<test>", std::io::sink())
            .map_err(|io_err| {
                Error::EvalError(EvalError::Other(
                    format!("IO error: {io_err}"),
                    TermPos::None,
                ))
            })?;
        p.typecheck()
    }

    #[test]
    fn evaluation_full() {
        use crate::{
            mk_array, mk_record,
            term::{make as mk_term, Term},
        };

        let t = eval_full("[(1 + 1), (\"a\" ++ \"b\"), ([ 1, [1 + 2] ])]").unwrap();

        // [2, "ab", [1, [3]]]
        let expd = mk_array!(
            mk_term::integer(2),
            Term::Str("ab".into()),
            mk_array!(
                mk_term::integer(1),
                mk_array!(mk_term::integer(3); ArrayAttrs::new().closurized());
                ArrayAttrs::new().closurized()
            );
            ArrayAttrs::new().closurized()
        );

        assert_eq!(t.without_pos(), expd.without_pos());

        let t = eval_full("let x = 1 in let y = 1 + x in let z = {foo.bar.baz = y} in z").unwrap();
        // Records are parsed as RecRecords, so we need to build one by hand
        let expd = mk_record!((
            "foo",
            mk_record!(("bar", mk_record!(("baz", mk_term::integer(2)))))
        ));
        assert_eq!(t.without_pos(), expd);

        // /!\ [MAY OVERFLOW STACK]
        // Check that substitution do not replace bound variables. Before the fixing commit, this
        // example would go into an infinite loop, and stack overflow. If it does, this just means
        // that this test fails.
        eval_full("{y = fun x => x, x = fun y => y}").unwrap();
    }

    #[test]
    // Regression test for issue 715 (https://github.com/tweag/nickel/issues/715)
    // Check that program::typecheck() fail on parse error
    fn typecheck_invalid_input() {
        assert_matches!(
            typecheck("{foo = 1 + `, bar : Str = \"a\"}"),
            Err(Error::ParseErrors(_))
        );
    }
}
