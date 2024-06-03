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
    error::{
        report::{report, ColorOpt, ErrorFormat},
        Error, EvalError, IOError, IntoDiagnostics, ParseError,
    },
    eval::{cache::Cache as EvalCache, Closure, VirtualMachine},
    identifier::LocIdent,
    label::Label,
    metrics::increment,
    term::{
        make as mk_term, make::builder, record::Field, BinaryOp, MergePriority, RichTerm, Term,
    },
};

use codespan::FileId;
use codespan_reporting::term::termcolor::Ansi;
use std::path::PathBuf;

use std::{
    ffi::OsString,
    fmt,
    io::{self, Cursor, Read, Write},
    result::Result,
};

/// A path of fields, that is a list, locating this field from the root of the configuration.
#[derive(Clone, Default, PartialEq, Eq, Debug, Hash)]
pub struct FieldPath(pub Vec<LocIdent>);

impl FieldPath {
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
    /// If this function succeeds and returns `Ok(field_path)`, then `field_path.0` is non empty.
    /// Indeed, there's no such thing as a valid empty field path (at least from the parsing point
    /// of view): if `input` is empty, or consists only of spaces, `parse` returns a parse error.
    pub fn parse(cache: &mut Cache, input: String) -> Result<Self, ParseError> {
        use crate::parser::{grammar::StaticFieldPathParser, lexer::Lexer, ErrorTolerantParser};

        let input_id = cache.replace_string(SourcePath::Query, input);
        let s = cache.source(input_id);

        let parser = StaticFieldPathParser::new();
        let field_path = parser
            .parse_strict(input_id, Lexer::new(s))
            // We just need to report an error here
            .map_err(|mut errs| {
                errs.errors.pop().expect(
                    "because parsing of the query path failed, the error \
                    list must be non-empty, put .pop() failed",
                )
            })?;

        Ok(FieldPath(field_path))
    }

    /// As [`Self::parse`], but accepts an `Option` to accomodate for the absence of path. If the
    /// input is `None`, `Ok(FieldPath::default())` is returned (that is, an empty field path).
    pub fn parse_opt(cache: &mut Cache, input: Option<String>) -> Result<Self, ParseError> {
        Ok(input
            .map(|path| Self::parse(cache, path))
            .transpose()?
            .unwrap_or_default())
    }
}

impl fmt::Display for FieldPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use crate::pretty::ident_quoted;

        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(ident_quoted)
                .collect::<Vec<_>>()
                .join(".")
        )
    }
}

/// Several CLI commands accept additional overrides specified directly on the command line. They
/// are represented by this structure.
#[derive(Clone)]
pub struct FieldOverride {
    /// The field path identifying the (potentially nested) field to override.
    pub path: FieldPath,
    /// The overriding value.
    pub value: String,
    /// The priority associated with this override.
    pub priority: MergePriority,
}

impl FieldOverride {
    /// Parse an assignment `path.to.field=value` to a field override, with the priority given as a
    /// separate argument.
    ///
    /// Internally, the parser entirely parses the `value` part to a [crate::term::RichTerm] (have
    /// it accept anything after the equal sign is in fact harder than actually parsing it), but
    /// what we need at this point is just a string. Thus, `parse` uses the span to extract back
    /// the `value` part of the input string.
    ///
    /// Theoretically, this means we parse two times the same string (the value part of an
    /// assignment). In practice, we expect this cost to be completly neglectible.
    pub fn parse(
        cache: &mut Cache,
        assignment: String,
        priority: MergePriority,
    ) -> Result<Self, ParseError> {
        use crate::parser::{grammar::CliFieldAssignmentParser, lexer::Lexer, ErrorTolerantParser};

        let input_id = cache.replace_string(SourcePath::CliFieldAssignment, assignment);
        let s = cache.source(input_id);

        let parser = CliFieldAssignmentParser::new();
        let (path, _, span_value) = parser
            .parse_strict(input_id, Lexer::new(s))
            // We just need to report an error here
            .map_err(|mut errs| {
                errs.errors.pop().expect(
                    "because parsing of the field assignment failed, the error \
                    list must be non-empty, put .pop() failed",
                )
            })?;

        let value = cache
            .files()
            .source_slice(span_value.src_id, span_value)
            .expect("the span coming from the parser must be valid");

        Ok(FieldOverride {
            path: FieldPath(path),
            value: value.to_owned(),
            priority,
        })
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
    pub color_opt: ColorOpt,
    /// A list of [`FieldOverride`]s. During [`prepare_eval`], each
    /// override is imported in a separate in-memory source, for complete isolation (this way,
    /// overrides can't accidentally or intentionally capture other fields of the configuration).
    /// A stub record is then built, which has all fields defined by `overrides`, and values are
    /// an import referring to the corresponding isolated value. This stub is finally merged with
    /// the current program before being evaluated for import.
    overrides: Vec<FieldOverride>,
    /// A specific field to act on. It is empty by default, which means that the whole program will
    /// be evaluated, but it can be set by the user (for example by the `--field` argument of the
    /// CLI) to evaluate only a specific field.
    pub field: FieldPath,
}

/// The Possible Input Sources, anything that a Nickel program can be created from
pub enum Input<T, S> {
    /// A filepath
    Path(S),
    /// The source is anything that can be Read from, the second argument is the name the source should have in the cache.
    Source(T, S),
}

impl<EC: EvalCache> Program<EC> {
    /// Create a program by reading it from the standard input.
    pub fn new_from_stdin(trace: impl Write + 'static) -> std::io::Result<Self> {
        Program::new_from_source(io::stdin(), "<stdin>", trace)
    }

    /// Contructor that abstracts over the Input type (file, string, etc.). Used by
    /// the other constructors. Published for those that need abstraction over the kind of Input.
    pub fn new_from_input<T, S>(
        input: Input<T, S>,
        trace: impl Write + 'static,
    ) -> std::io::Result<Self>
    where
        T: Read,
        S: Into<OsString>,
    {
        increment!("Program::new");
        let mut cache = Cache::new(ErrorTolerance::Strict);

        let main_id = match input {
            Input::Path(path) => cache.add_file(path)?,
            Input::Source(source, name) => {
                let path = PathBuf::from(name.into());
                cache.add_source(SourcePath::Path(path), source)?
            }
        };

        let vm = VirtualMachine::new(cache, trace);
        Ok(Self {
            main_id,
            vm,
            color_opt: clap::ColorChoice::Auto.into(),
            overrides: Vec::new(),
            field: FieldPath::new(),
        })
    }

    /// Constructor that abstracts over an iterator of Inputs (file, strings,
    /// etc). Published for those that need abstraction over the kind of Input
    /// or want to mix multiple different kinds of Input.
    pub fn new_from_inputs<I, T, S>(inputs: I, trace: impl Write + 'static) -> std::io::Result<Self>
    where
        I: IntoIterator<Item = Input<T, S>>,
        T: Read,
        S: Into<OsString>,
    {
        increment!("Program::new");
        let mut cache = Cache::new(ErrorTolerance::Strict);

        let merge_term = inputs
            .into_iter()
            .map(|input| match input {
                Input::Path(path) => RichTerm::from(Term::Import(path.into())),
                Input::Source(source, name) => {
                    let path = PathBuf::from(name.into());
                    cache
                        .add_source(SourcePath::Path(path.clone()), source)
                        .unwrap();
                    RichTerm::from(Term::Import(path.into()))
                }
            })
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
            field: FieldPath::new(),
        })
    }

    /// Create program from possibly multiple files. Each input `path` is
    /// turned into a [`Term::Import`] and the main program will be the
    /// [`BinaryOp::Merge`] of all the inputs.
    pub fn new_from_files<I, P>(paths: I, trace: impl Write + 'static) -> std::io::Result<Self>
    where
        I: IntoIterator<Item = P>,
        P: Into<OsString>,
    {
        // The File type parameter is a dummy type and not used.
        // It just needed to be something that implements Read, and File seemed fitting.
        Self::new_from_inputs(
            paths.into_iter().map(Input::<std::fs::File, _>::Path),
            trace,
        )
    }

    pub fn new_from_file(
        path: impl Into<OsString>,
        trace: impl Write + 'static,
    ) -> std::io::Result<Self> {
        // The File type parameter is a dummy type and not used.
        // It just needed to be something that implements Read, and File seemed fitting.
        Self::new_from_input(Input::<std::fs::File, _>::Path(path), trace)
    }

    /// Create a program by reading it from a generic source.
    pub fn new_from_source<T, S>(
        source: T,
        source_name: S,
        trace: impl Write + 'static,
    ) -> std::io::Result<Self>
    where
        T: Read,
        S: Into<OsString>,
    {
        Self::new_from_input(Input::Source(source, source_name), trace)
    }

    /// Create program from possibly multiple sources. The main program will be
    /// the [`BinaryOp::Merge`] of all the inputs.
    pub fn new_from_sources<I, T, S>(
        sources: I,
        trace: impl Write + 'static,
    ) -> std::io::Result<Self>
    where
        I: IntoIterator<Item = (T, S)>,
        T: Read,
        S: Into<OsString>,
    {
        let inputs = sources.into_iter().map(|(s, n)| Input::Source(s, n));
        Self::new_from_inputs(inputs, trace)
    }

    /// Parse an assignment of the form `path.to_field=value` as an override, with the provided
    /// merge priority. Assignments are typically provided by the user on the command line, as part
    /// of the customize mode.
    ///
    /// This method simply calls [FieldOverride::parse] with the [crate::cache::Cache] of the
    /// current program.
    pub fn parse_override(
        &mut self,
        assignment: String,
        priority: MergePriority,
    ) -> Result<FieldOverride, ParseError> {
        FieldOverride::parse(self.vm.import_resolver_mut(), assignment, priority)
    }

    /// Parse a dot-separated field path of the form `path.to.field`.
    ///
    /// This method simply calls [FieldPath::parse] with the [crate::cache::Cache] of the current
    /// program.
    pub fn parse_field_path(&mut self, path: String) -> Result<FieldPath, ParseError> {
        FieldPath::parse(self.vm.import_resolver_mut(), path)
    }

    pub fn add_overrides(&mut self, overrides: impl IntoIterator<Item = FieldOverride>) {
        self.overrides.extend(overrides);
    }

    /// Adds import paths to the end of the list.
    pub fn add_import_paths<P>(&mut self, paths: impl Iterator<Item = P>)
    where
        PathBuf: From<P>,
    {
        self.vm.import_resolver_mut().add_import_paths(paths);
    }

    /// Only parse the program, don't typecheck or evaluate. returns the [`RichTerm`] AST
    pub fn parse(&mut self) -> Result<RichTerm, Error> {
        self.vm
            .import_resolver_mut()
            .parse(self.main_id, InputFormat::Nickel)
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

    /// Retrieve the parsed term, typecheck it, and generate a fresh initial environment. If
    /// `self.overrides` isn't empty, generate the required merge parts and return a merge
    /// expression including the overrides. Extract the field corresponding to `self.field`, if not
    /// empty.
    fn prepare_eval(&mut self) -> Result<Closure, Error> {
        self.prepare_eval_impl(false)
    }

    /// Retrieve the parsed term, typecheck it, and generate a fresh initial environment. If
    /// `self.overrides` isn't empty, generate the required merge parts and return a merge
    /// expression including the overrides. DO NOT extract the field corresponding to `self.field`,
    /// because query does it itself. Otherwise, we would lose the associated metadata.
    fn prepare_query(&mut self) -> Result<Closure, Error> {
        self.prepare_eval_impl(true)
    }

    fn prepare_eval_impl(&mut self, for_query: bool) -> Result<Closure, Error> {
        // If there are no overrides, we avoid the boilerplate of creating an empty record and
        // merging it with the current program
        let prepared_body = if self.overrides.is_empty() {
            self.vm.prepare_eval(self.main_id)?
        } else {
            let mut record = builder::Record::new();

            for ovd in self.overrides.iter().cloned() {
                let value_file_id = self
                    .vm
                    .import_resolver_mut()
                    .add_string(SourcePath::Override(ovd.path.clone()), ovd.value);
                self.vm.prepare_eval(value_file_id)?;
                record = record
                    .path(ovd.path.0)
                    .priority(ovd.priority)
                    .value(Term::ResolvedImport(value_file_id));
            }

            let t = self.vm.prepare_eval(self.main_id)?;
            let built_record = record.build();
            // For now, we can't do much better than using `Label::default`, but this is
            // hazardous. `Label::default` was originally written for tests, and although it
            // doesn't happen in practice as of today, it could theoretically generate invalid
            // codespan file ids (because it creates a new file database on the spot just to
            // generate a dummy file id).
            // We'll have to adapt `Label` and `MergeLabel` to be generated programmatically,
            // without referring to any source position.
            mk_term::op2(BinaryOp::Merge(Label::default().into()), t, built_record)
        };

        let prepared = Closure::atomic_closure(prepared_body);

        let result = if for_query {
            prepared
        } else {
            self.vm.extract_field_value_closure(prepared, &self.field)?
        };

        Ok(result)
    }

    /// Parse if necessary, typecheck and then evaluate the program.
    pub fn eval(&mut self) -> Result<RichTerm, Error> {
        let prepared = self.prepare_eval()?;

        self.vm.reset();
        Ok(self.vm.eval_closure(prepared)?.body)
    }

    /// Same as `eval`, but proceeds to a full evaluation.
    pub fn eval_full(&mut self) -> Result<RichTerm, Error> {
        let prepared = self.prepare_eval()?;

        self.vm.reset();
        Ok(self.vm.eval_full_closure(prepared)?.body)
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
        let prepared = self.prepare_eval()?;

        self.vm.reset();
        Ok(self.vm.eval_full_for_export_closure(prepared)?)
    }

    /// Same as `eval_full`, but does not substitute all variables.
    pub fn eval_deep(&mut self) -> Result<RichTerm, Error> {
        let prepared = self.prepare_eval()?;

        self.vm.reset();
        Ok(self.vm.eval_deep_closure(prepared)?)
    }

    /// Prepare for evaluation, then fetch the metadata of `self.field`, or list the fields of the
    /// whole program if `self.field` is empty.
    pub fn query(&mut self) -> Result<Field, Error> {
        let prepared = self.prepare_query()?;

        Ok(self.vm.query_closure(prepared, &self.field)?)
    }

    /// Load, parse, and typecheck the program and the standard library, if not already done.
    pub fn typecheck(&mut self) -> Result<(), Error> {
        self.vm
            .import_resolver_mut()
            .parse(self.main_id, InputFormat::Nickel)?;
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
    pub fn report<E>(&mut self, error: E, format: ErrorFormat)
    where
        E: IntoDiagnostics<FileId>,
    {
        report(self.vm.import_resolver_mut(), error, format, self.color_opt)
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
    pub fn eval_record_spine(&mut self) -> Result<RichTerm, Error> {
        use crate::eval::Environment;
        use crate::match_sharedterm;
        use crate::term::{record::RecordData, RuntimeContract};

        let prepared = self.prepare_eval()?;

        // Eval pending contracts as well, in order to extract more information from potential
        // record contract fields.
        fn eval_contracts<EC: EvalCache>(
            vm: &mut VirtualMachine<Cache, EC>,
            mut pending_contracts: Vec<RuntimeContract>,
            current_env: Environment,
        ) -> Result<Vec<RuntimeContract>, Error> {
            vm.reset();

            for ctr in pending_contracts.iter_mut() {
                let rt = ctr.contract.clone();
                ctr.contract = do_eval(vm, rt, current_env.clone())?;
            }

            Ok(pending_contracts)
        }

        fn do_eval<EC: EvalCache>(
            vm: &mut VirtualMachine<Cache, EC>,
            term: RichTerm,
            env: Environment,
        ) -> Result<RichTerm, Error> {
            vm.reset();
            let result = vm.eval_closure(Closure {
                body: term.clone(),
                env,
            });

            // We expect to hit `MissingFieldDef` errors. When a configuration
            // contains undefined record fields they most likely will be used
            // recursively in the definition of some other fields. So instead of
            // bubbling up an evaluation error in this case we just leave fields
            // that depend on as yet undefined fields unevaluated; we wouldn't
            // be able to extract dcoumentation from their values anyways. All
            // other evaluation errors should however be reported to the user
            // instead of resulting in documentation being silently skipped.

            let result = match result {
                Err(EvalError::MissingFieldDef { .. }) => return Ok(term),
                _ => result,
            }?;

            match_sharedterm!(match (result.body.term) {
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
                                        .map(|value| do_eval(vm, value, result.env.clone()))
                                        .transpose()?,
                                    pending_contracts: eval_contracts(
                                        vm,
                                        field.pending_contracts,
                                        result.env.clone(),
                                    )?,
                                    ..field
                                },
                            ))
                        })
                        .collect::<Result<_, Error>>()?;

                    Ok(RichTerm::new(
                        Term::Record(RecordData { fields, ..data }),
                        result.body.pos,
                    ))
                }
                _ => Ok(result.body),
            })
        }

        do_eval(&mut self.vm, prepared.body, prepared.env)
    }

    /// Extract documentation from the program
    #[cfg(feature = "doc")]
    pub fn extract_doc(&mut self) -> Result<doc::ExtractedDocumentation, Error> {
        use crate::error::ExportErrorData;

        let term = self.eval_record_spine()?;
        doc::ExtractedDocumentation::extract_from_term(&term).ok_or(Error::ExportError(
            ExportErrorData::NoDocumentation(term.clone()).into(),
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
        use crate::{pretty::*, transform::transform};
        use pretty::BoxAllocator;

        let Program {
            ref main_id, vm, ..
        } = self;
        let allocator = BoxAllocator;

        let rt = vm.import_resolver().parse_nocache(*main_id)?.0;
        let rt = if apply_transforms {
            transform(rt, None).map_err(EvalError::from)?
        } else {
            rt
        };
        let doc: DocBuilder<_, ()> = rt.pretty(&allocator);
        doc.render(80, out).map_err(IOError::from)?;
        writeln!(out).map_err(IOError::from)?;

        Ok(())
    }
}

#[cfg(feature = "doc")]
mod doc {
    use crate::error::{Error, ExportErrorData, IOError};
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
                .map_err(|e| Error::ExportError(ExportErrorData::Other(e.to_string()).into()))
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

        // We have to wrap the content of the list item into a paragraph, otherwise the list won't
        // be properly separated from the next block coming after it, producing invalid output (for
        // example, the beginning of the documenantation of the current field might be merged with
        // the last type or contract item).
        //
        // We probably shouldn't have to, but afer diving into comrak's rendering engine, it seems
        // that some subtle interactions make things work correctly for parsed markdown (as opposed to
        // this one being programmatically generated) just because list items are always parsed as
        // paragraphs. We thus mimic this unspoken invariant here.
        let paragraph = arena.alloc(AstNode::from(NodeValue::Paragraph));

        paragraph.append(arena.alloc(AstNode::from(NodeValue::Code(NodeCode {
            literal: format!("{ident} {separator} {typ}"),
            num_backticks: 1,
        }))));
        list_item.append(paragraph);

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
            mk_array!(mk_term::integer(1), mk_array!(mk_term::integer(3)))
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
