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
use crate::{
    cache::*,
    error::{report, ColorOpt, Error, EvalError, IOError, IntoDiagnostics, ParseError},
    eval,
    eval::{cache::Cache as EvalCache, VirtualMachine},
    identifier::LocIdent,
    label::Label,
    term::{
        make as mk_term, make::builder, record::Field, BinaryOp, MergePriority, RichTerm, Term,
    },
};

use codespan::FileId;
use codespan_reporting::term::termcolor::Ansi;
use std::path::PathBuf;

use std::{
    ffi::OsString,
    io::{self, Cursor, Read, Write},
    result::Result,
};

/// Attribute path provided when querying metadata.
#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub struct QueryPath(pub Vec<LocIdent>);

impl QueryPath {
    pub fn new() -> Self {
        Self::default()
    }

    /// Parse a string as a query path. A query path is a sequence of dot-separated identifiers.
    /// Identifiers can be enclosed by double quotes when they contain characters that aren't
    /// allowed inside bare identifiers. The accepted grammar is the same as a sequence of record
    /// accesses in Nickel, although string interpolation is forbidden.
    ///
    /// # Post-conditions
    ///
    /// If this function succeeds and returns `Ok(query_path)`, then `query_path.0` is non empty.
    /// Indeed, there's no such thing as a valid empty field path. If `input` is empty, or consists
    /// only of spaces, `parse` returns a parse error.
    pub fn parse(cache: &mut Cache, input: String) -> Result<Self, ParseError> {
        use crate::parser::{
            grammar::FieldPathParser, lexer::Lexer, utils::FieldPathElem, ErrorTolerantParser,
        };

        let input_id = cache.replace_string(SourcePath::Query, input);
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

        let path_as_idents: Result<Vec<LocIdent>, ParseError> = field_path
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
                    Ok(LocIdent::from(as_string))
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

/// Several CLI commands accept additional overrides specified directly on the command line. They
/// are represented by this structure.
#[derive(Clone)]
pub struct FieldOverride {
    /// The field path identifying the (potentially nested) field to override.
    pub path: Vec<String>,
    /// The overriding value.
    pub value: String,
    /// The priority associated with this override.
    pub priority: MergePriority,
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
    pub color_opt: ColorOpt,
    /// A list of [`FieldOverride`]s. During [`prepare_eval`], each
    /// override is imported in a separate in-memory source, for complete isolation (this way,
    /// overrides can't accidentally or intentionally capture other fields of the configuration).
    /// A stub record is then built, which has all fields defined by `overrides`, and values are
    /// an import referring to the corresponding isolated value. This stub is finally merged with
    /// the current program before being evaluated for import.
    overrides: Vec<FieldOverride>,
}

impl<EC: EvalCache> Program<EC> {
    /// Create a program by reading it from the standard input.
    pub fn new_from_stdin(trace: impl Write + 'static) -> std::io::Result<Self> {
        Program::new_from_source(io::stdin(), "<stdin>", trace)
    }

    /// Create program from possibly multiple files. Each input `path` is
    /// turned into a [`Term::Import`] and the main program will be the
    /// [`BinaryOp::Merge`] of all the inputs.
    pub fn new_from_files<I, P>(paths: I, trace: impl Write + 'static) -> std::io::Result<Self>
    where
        I: IntoIterator<Item = P>,
        P: Into<OsString>,
    {
        let mut cache = Cache::new(ErrorTolerance::Strict);

        let merge_term = paths
            .into_iter()
            .map(|f| RichTerm::from(Term::Import(f.into())))
            .reduce(|acc, f| mk_term::op2(BinaryOp::Merge(Label::default().into()), acc, f))
            .unwrap();
        let main_id = cache.add_string(
            SourcePath::Generated("main".into()),
            format!("{merge_term}"),
        );

        let vm = VirtualMachine::new(cache, trace);

        Ok(Self {
            main_id,
            vm,
            color_opt: clap::ColorChoice::Auto.into(),
            overrides: Vec::new(),
        })
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
            overrides: Vec::new(),
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
        let path = PathBuf::from(source_name.into());
        let main_id = cache.add_source(SourcePath::Path(path), source)?;
        let vm = VirtualMachine::new(cache, trace);

        Ok(Self {
            main_id,
            vm,
            color_opt: clap::ColorChoice::Auto.into(),
            overrides: Vec::new(),
        })
    }

    pub fn add_overrides(&mut self, overrides: impl IntoIterator<Item = FieldOverride>) {
        self.overrides.extend(overrides);
    }

    /// Only parse the program, don't typecheck or evaluate. returns the [`RichTerm`] AST
    pub fn parse(&mut self) -> Result<RichTerm, Error> {
        self.vm
            .import_resolver_mut()
            .parse(self.main_id)
            .map_err(Error::ParseErrors)?;
        Ok(self
            .vm
            .import_resolver()
            .terms()
            .get(&self.main_id)
            .expect("File parsed and then immediately accessed doesn't exist")
            .term
            .clone())
    }

    /// Retrieve the parsed term and typecheck it, and generate a fresh initial environment. Return
    /// both.
    fn prepare_eval(&mut self) -> Result<(RichTerm, eval::Environment), Error> {
        // If there are no overrides, we avoid the boilerplate of creating an empty record and
        // merging it with the current program
        if self.overrides.is_empty() {
            return self.vm.prepare_eval(self.main_id);
        }

        let mut record = builder::Record::new();

        for ovd in self.overrides.iter().cloned() {
            let value_file_id = self
                .vm
                .import_resolver_mut()
                .add_string(SourcePath::Override(ovd.path.clone()), ovd.value);
            self.vm.prepare_eval(value_file_id)?;
            record = record
                .path(ovd.path)
                .priority(ovd.priority)
                .value(Term::ResolvedImport(value_file_id));
        }

        let (t, initial_env) = self.vm.prepare_eval(self.main_id)?;
        let built_record = record.build();
        // For now, we can't do much better than using `Label::default`, but this is
        // hazardous. `Label::default` was originally written for tests, and although it
        // doesn't happen in practice as of today, it could theoretically generate invalid
        // codespan file ids (because it creates a new file database on the spot just to
        // generate a dummy file id).
        // We'll have to adapt `Label` and `MergeLabel` to be generated programmatically,
        // without referring to any source position.
        let wrapper = mk_term::op2(BinaryOp::Merge(Label::default().into()), t, built_record);
        Ok((wrapper, initial_env))
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

    /// Same as `eval`, but proceeds to a full evaluation. Optionally take a set of overrides that
    /// are to be applied to the term (in practice, to be merged with).
    ///
    /// Skips record fields marked `not_exported`.
    ///
    /// # Arguments
    ///
    /// - `override` is a list of overrides in the form of an iterator of [`FieldOverride`]s. Each
    ///   override is imported in a separate in-memory source, for complete isolation (this way,
    ///   overrides can't accidentally or intentionally capture other fields of the configuration).
    ///   A stub record is then built, which has all fields defined by `overrides`, and values are
    ///   an import referring to the corresponding isolated value. This stub is finally merged with
    ///   the current program before being evaluated for import.
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
        let initial_env = self.vm.import_resolver().mk_type_ctxt().expect(
            "program::typecheck(): \
            stdlib has been loaded but was not found in cache on mk_type_ctxt()",
        );
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

    /// Evaluate a program into a record spine, a form suitable for extracting the general
    /// structure of a configuration, and in particular its interface (fields that might need to be
    /// filled).
    ///
    /// This form is used to extract documentation through `nickel doc`, for example.
    ///
    /// ## Record spine
    ///
    /// By record spine, we mean that the result is a tree of evaluated nested records, and leafs
    /// are either non-record values in WHNF or partial expressions left
    /// unevaluated[^missing-field-def]. For example, the record spine of:
    ///
    /// ```nickel
    /// {
    ///   foo = {bar = 1 + 1} & {baz.subbaz = [some_func "some_arg"] @ ["snd" ++ "_elt"]},
    ///   input,
    ///   depdt = input & {extension = 2},
    /// }
    /// ```
    ///
    /// is
    ///
    /// ```nickel
    /// {
    ///   foo = {
    ///     bar = 2,
    ///     baz = {
    ///       subbaz = [some_func "some_arg", "snd" ++ "_elt"],
    ///     },
    ///   },
    ///   input,
    ///   depdt = input & {extension = 2},
    /// }
    /// ```
    ///
    /// To evaluate a term to a record spine, we first evaluate it to a WHNF and then:
    /// - If the result is a record, we recursively evaluate subfields to record spines
    /// - If the result isn't a record, it is returned as it is
    /// - If the evaluation fails with [crate::error::EvalError::MissingFieldDef], the original
    /// term is returned unevaluated[^missing-field-def]
    /// - If any other error occurs, the evaluation fails and returns the error.
    ///
    /// [^missing-field-def]: Because we want to handle partial configurations as well,
    /// [crate::error::EvalError::MissingFieldDef] errors are _ignored_: if this is encountered
    /// when evaluating a field, this field is just left as it is and the evaluation proceeds.
    #[cfg(feature = "doc")]
    pub fn eval_record_spine(&mut self) -> Result<RichTerm, Error> {
        use crate::eval::{Closure, Environment};
        use crate::match_sharedterm;
        use crate::term::{record::RecordData, RuntimeContract};

        let (t, initial_env) = self.prepare_eval()?;

        // Eval pending contracts as well, in order to extract more information from potential
        // record contract fields.
        fn eval_contracts<EC: EvalCache>(
            vm: &mut VirtualMachine<Cache, EC>,
            mut pending_contracts: Vec<RuntimeContract>,
            current_env: Environment,
            initial_env: &Environment,
        ) -> Result<Vec<RuntimeContract>, Error> {
            vm.reset();

            for ctr in pending_contracts.iter_mut() {
                let rt = ctr.contract.clone();
                ctr.contract = do_eval(vm, rt, current_env.clone(), initial_env)?;
            }

            Ok(pending_contracts)
        }

        fn do_eval<EC: EvalCache>(
            vm: &mut VirtualMachine<Cache, EC>,
            t: RichTerm,
            current_env: Environment,
            initial_env: &Environment,
        ) -> Result<RichTerm, Error> {
            vm.reset();
            let result = vm.eval_closure(
                Closure {
                    body: t.clone(),
                    env: current_env,
                },
                initial_env,
            );

            // We expect to hit `MissingFieldDef` errors. When a configuration
            // contains undefined record fields they most likely will be used
            // recursively in the definition of some other fields. So instead of
            // bubbling up an evaluation error in this case we just leave fields
            // that depend on as yet undefined fields unevaluated; we wouldn't
            // be able to extract dcoumentation from their values anyways. All
            // other evaluation errors should however be reported to the user
            // instead of resulting in documentation being silently skipped.

            let (rt, env) = match result {
                Err(EvalError::MissingFieldDef { .. }) => return Ok(t),
                _ => result,
            }?;

            match_sharedterm! {rt.term, with {
                    Term::Record(data) => {
                        let fields = data
                            .fields
                            .into_iter()
                            .map(|(id, field)| -> Result<_, Error> {
                                Ok((
                                    id,
                                    Field {
                                        value: field
                                            .value
                                            .map(|rt| do_eval(vm, rt, env.clone(), initial_env))
                                            .transpose()?,
                                        pending_contracts: eval_contracts(
                                            vm,
                                            field.pending_contracts,
                                            env.clone(),
                                            initial_env
                                        )?,
                                        ..field
                                    },
                                ))
                            })
                            .collect::<Result<_, Error>>()?;
                        Ok(RichTerm::new(
                            Term::Record(RecordData { fields, ..data }),
                            rt.pos,
                        ))
                    }
                } else Ok(rt)
            }
        }

        do_eval(&mut self.vm, t, Environment::new(), &initial_env)
    }

    /// Extract documentation from the program
    #[cfg(feature = "doc")]
    pub fn extract_doc(&mut self) -> Result<doc::ExtractedDocumentation, Error> {
        use crate::error::ExportError;

        let term = self.eval_record_spine()?;
        doc::ExtractedDocumentation::extract_from_term(&term).ok_or(Error::ExportError(
            ExportError::NoDocumentation(term.clone()),
        ))
    }

    #[cfg(debug_assertions)]
    pub fn set_skip_stdlib(&mut self) {
        self.vm.import_resolver_mut().skip_stdlib = true;
    }

    pub fn pprint_ast(
        &mut self,
        out: &mut impl std::io::Write,
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
            crate::transform::transform(rt, None).map_err(EvalError::from)?
        } else {
            rt
        };
        let doc: DocBuilder<_, ()> = rt.pretty(&allocator);
        doc.render(80, out).map_err(IOError::from)?;
        writeln!(out).map_err(IOError::from)?;
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
        #[serde(rename = "type")]
        typ: Option<String>,
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

                            // We use the original user-written type stored
                            // in the label. Using `lt.typ` instead is often
                            // unreadable, since we evaluate terms to a record
                            // spine before extracting documentation
                            let typ = field
                                .metadata
                                .annotation
                                .typ
                                .as_ref()
                                .map(|lt| lt.label.typ.to_string());

                            let contracts = field
                                .metadata
                                .annotation
                                .contracts
                                .iter()
                                .map(|lt| lt.label.typ.to_string())
                                .collect();

                            let documentation = field.metadata.doc.clone();

                            (
                                ident.label().to_owned(),
                                DocumentationField {
                                    fields,
                                    typ,
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

            // The default ComrakOptions disables all extensions (essentially reducing to
            // CommonMark)
            let options = ComrakOptions::default();

            self.markdown_append(0, &arena, &document, &options);
            format_commonmark(&document, &options, out)
                .map_err(|e| Error::IOError(IOError(e.to_string())))?;

            Ok(())
        }

        /// Recursively walk the given `DocOutput`, recursing into fields, looking for
        /// documentation. This documentation is then appended to the provided document.
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

                if field.typ.is_some() || !field.contracts.is_empty() {
                    document.append(mk_types_and_contracts(
                        ident,
                        arena,
                        field.typ.as_deref(),
                        field.contracts.as_ref(),
                    ))
                }

                document.append(arena.alloc(AstNode::from(NodeValue::LineBreak)));

                if let Some(ref doc) = field.documentation {
                    document.append(parse_markdown_string(header_level + 1, arena, doc, options));
                }

                if let Some(ref subfields) = field.fields {
                    subfields.markdown_append(header_level + 1, arena, document, options);
                }
            }
        }
    }

    /// Parses a string into markdown and increases any headers in the markdown by the specified
    /// level. This allows having headers in documentation without clashing with the structure of
    /// the document.
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
        typ: Option<&'a str>,
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

        if let Some(t) = typ {
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
        typ: &str,
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
            literal: format!("{ident} {separator} {typ}"),
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
    use crate::identifier::LocIdent;
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
