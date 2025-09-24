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
//! functions in [`crate::cache`] (see [`crate::cache::CacheHub::mk_eval_env`]).
//! Each such value is added to the initial environment before the evaluation of the program.
use crate::{
    bytecode::ast::{compat::ToMainline, AstAlloc},
    cache::*,
    closurize::Closurize as _,
    error::{warning::Warning, Error, EvalError, IOError, ParseError, Reporter},
    eval::{cache::Cache as EvalCache, Closure, VirtualMachine},
    files::{FileId, Files},
    identifier::LocIdent,
    label::Label,
    metrics::{increment, measure_runtime},
    package::PackageMap,
    position::{RawSpan, TermPos},
    term::{
        make::{self as mk_term, builder},
        record::Field,
        BinaryOp, Import, MergePriority, RichTerm, RuntimeContract, Term,
    },
    typecheck::TypecheckMode,
};

use std::{
    ffi::OsString,
    fmt,
    io::{self, Read, Write},
    path::PathBuf,
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
    pub fn parse(caches: &mut CacheHub, input: String) -> Result<Self, ParseError> {
        use crate::parser::{
            grammar::StaticFieldPathParser, lexer::Lexer, ErrorTolerantParserCompat,
        };

        let input_id = caches.replace_string(SourcePath::Query, input);
        let s = caches.sources.source(input_id);

        let parser = StaticFieldPathParser::new();
        let field_path = parser
            .parse_strict_compat(input_id, Lexer::new(s))
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
    pub fn parse_opt(cache: &mut CacheHub, input: Option<String>) -> Result<Self, ParseError> {
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
    /// assignment). In practice, we expect this cost to be completely negligible.
    ///
    /// # Selectors
    ///
    /// The value part accepts special selectors starting with a leading `@` that aren't part of
    /// the core Nickel syntax. This list is subject to extensions.
    ///
    /// - `foo.bar=@env:<var>` will extract a string value from the environment variable `<var>`
    ///   and put it in `foo.bar`.
    pub fn parse(
        cache: &mut CacheHub,
        assignment: String,
        priority: MergePriority,
    ) -> Result<Self, ParseError> {
        use crate::parser::{
            grammar::{CliFieldAssignmentParser, StaticFieldPathParser},
            lexer::{Lexer, NormalToken, Token},
            ErrorTolerantParserCompat,
        };

        let input_id = cache.replace_string(SourcePath::CliFieldAssignment, assignment);
        let s = cache.sources.source(input_id);

        // We first look for a possible sigil `@` immediately following the (first not-in-a-string)
        // equal sign. This can't be valid Nickel, so we always consider that this is a special CLI
        // expression like `@env:VAR`.
        let mut lexer = Lexer::new(s);
        let equal_sign =
            lexer.find(|t| matches!(t, Ok((_, Token::Normal(NormalToken::Equals), _))));
        let after_equal = lexer.next();

        match (equal_sign, after_equal) {
            (
                Some(Ok((start_eq, _, end_eq))),
                Some(Ok((start_at, Token::Normal(NormalToken::At), _))),
            ) if end_eq == start_at => {
                let path = StaticFieldPathParser::new()
                    .parse_strict_compat(input_id, Lexer::new(&s[..start_eq]))
                    // We just need to report one error here
                    .map_err(|mut errs| {
                        errs.errors.pop().expect(
                            "because parsing of the field assignment failed, the error \
                        list must be non-empty, put .pop() failed",
                        )
                    })?;
                let value = s[start_at..].to_owned();

                Ok(FieldOverride {
                    path: FieldPath(path),
                    value: value.to_owned(),
                    priority,
                })
            }
            _ => {
                let (path, _, span_value) = CliFieldAssignmentParser::new()
                    .parse_strict_compat(input_id, Lexer::new(s))
                    // We just need to report one error here
                    .map_err(|mut errs| {
                        errs.errors.pop().expect(
                            "because parsing of the field assignment failed, the error \
                        list must be non-empty, put .pop() failed",
                        )
                    })?;

                let value = cache.files().source_slice(span_value);

                Ok(FieldOverride {
                    path: FieldPath(path),
                    value: value.to_owned(),
                    priority,
                })
            }
        }
    }
}

/// Additional contracts to apply to the main program.
pub enum ProgramContract {
    /// Contract specified directly as a term. Typically used for contracts generated or at least
    /// wrapped programmatically.
    Term(RuntimeContract),
    /// Contract specified as a source. They will be parsed and typechecked alongside the rest of
    /// the program. Typically coming from the CLI `--apply-contract` argument.
    Source(FileId),
}

/// A Nickel program.
///
/// Manage a file database, which stores the original source code of the program and eventually the
/// code of imported expressions, and a dictionary which stores corresponding parsed terms.
pub struct Program<EC: EvalCache> {
    /// The id of the program source in the file database.
    main_id: FileId,
    /// The state of the Nickel virtual machine.
    vm: VirtualMachine<CacheHub, EC>,
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
    /// Extra contracts to apply to the main program source. Note that the contract is applied to
    /// the whole value before fields are extracted.
    pub contracts: Vec<ProgramContract>,
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
    pub fn new_from_stdin(
        trace: impl Write + 'static,
        reporter: impl Reporter<(Warning, Files)> + 'static,
    ) -> std::io::Result<Self> {
        Program::new_from_source(io::stdin(), "<stdin>", trace, reporter)
    }

    /// Contructor that abstracts over the Input type (file, string, etc.). Used by
    /// the other constructors. Published for those that need abstraction over the kind of Input.
    ///
    /// The format of the input is Nickel by default. However, for [Input::Path]s, the format is
    /// determined from the file extension. This is useful to merge Nickel and non-Nickel files, or
    /// to apply extra contracts to non-Nickel configurations.
    pub fn new_from_input<T, S>(
        input: Input<T, S>,
        trace: impl Write + 'static,
        reporter: impl Reporter<(Warning, Files)> + 'static,
    ) -> std::io::Result<Self>
    where
        T: Read,
        S: Into<OsString>,
    {
        increment!("Program::new");
        let mut cache = CacheHub::new();

        let main_id = match input {
            Input::Path(path) => {
                let path = path.into();
                let format = InputFormat::from_path(&path).unwrap_or_default();
                cache.sources.add_file(path, format)?
            }
            Input::Source(source, name) => {
                let path = PathBuf::from(name.into());
                cache
                    .sources
                    .add_source(SourcePath::Path(path, InputFormat::Nickel), source)?
            }
        };

        let vm = VirtualMachine::new(cache, trace, reporter);
        Ok(Self {
            main_id,
            vm,
            overrides: Vec::new(),
            field: FieldPath::new(),
            contracts: Vec::new(),
        })
    }

    /// Constructor that abstracts over an iterator of Inputs (file, strings,
    /// etc). Published for those that need abstraction over the kind of Input
    /// or want to mix multiple different kinds of Input.
    ///
    /// The format of each input is Nickel by default. However, for [Input::Path]s, the format is
    /// determined from the file extension. This is useful to merge Nickel and non-Nickel files, or
    /// to apply extra contracts to non-Nickel configurations.
    pub fn new_from_inputs<I, T, S>(
        inputs: I,
        trace: impl Write + 'static,
        reporter: impl Reporter<(Warning, Files)> + 'static,
    ) -> std::io::Result<Self>
    where
        I: IntoIterator<Item = Input<T, S>>,
        T: Read,
        S: Into<OsString>,
    {
        increment!("Program::new");
        let mut cache = CacheHub::new();

        let merge_term = inputs
            .into_iter()
            .map(|input| match input {
                Input::Path(path) => {
                    let path = path.into();
                    let format = InputFormat::from_path(&path).unwrap_or_default();

                    RichTerm::from(Term::Import(Import::Path { path, format }))
                }
                Input::Source(source, name) => {
                    let path = PathBuf::from(name.into());
                    cache
                        .sources
                        .add_source(SourcePath::Path(path.clone(), InputFormat::Nickel), source)
                        .unwrap();
                    RichTerm::from(Term::Import(Import::Path {
                        path: path.into(),
                        format: InputFormat::Nickel,
                    }))
                }
            })
            .reduce(|acc, f| mk_term::op2(BinaryOp::Merge(Label::default().into()), acc, f))
            .unwrap();

        let main_id = cache.sources.add_string(
            SourcePath::Generated("main".into()),
            format!("{merge_term}"),
        );

        let vm = VirtualMachine::new(cache, trace, reporter);

        Ok(Self {
            main_id,
            vm,
            overrides: Vec::new(),
            field: FieldPath::new(),
            contracts: Vec::new(),
        })
    }

    /// Create program from possibly multiple files. Each input `path` is
    /// turned into a [`Term::Import`] and the main program will be the
    /// [`BinaryOp::Merge`] of all the inputs.
    pub fn new_from_files<I, P>(
        paths: I,
        trace: impl Write + 'static,
        reporter: impl Reporter<(Warning, Files)> + 'static,
    ) -> std::io::Result<Self>
    where
        I: IntoIterator<Item = P>,
        P: Into<OsString>,
    {
        // The File type parameter is a dummy type and not used.
        // It just needed to be something that implements Read, and File seemed fitting.
        Self::new_from_inputs(
            paths.into_iter().map(Input::<std::fs::File, _>::Path),
            trace,
            reporter,
        )
    }

    pub fn new_from_file(
        path: impl Into<OsString>,
        trace: impl Write + 'static,
        reporter: impl Reporter<(Warning, Files)> + 'static,
    ) -> std::io::Result<Self> {
        // The File type parameter is a dummy type and not used.
        // It just needed to be something that implements Read, and File seemed fitting.
        Self::new_from_input(Input::<std::fs::File, _>::Path(path), trace, reporter)
    }

    /// Create a program by reading it from a generic source.
    pub fn new_from_source<T, S>(
        source: T,
        source_name: S,
        trace: impl Write + 'static,
        reporter: impl Reporter<(Warning, Files)> + 'static,
    ) -> std::io::Result<Self>
    where
        T: Read,
        S: Into<OsString>,
    {
        Self::new_from_input(Input::Source(source, source_name), trace, reporter)
    }

    /// Create program from possibly multiple sources. The main program will be
    /// the [`BinaryOp::Merge`] of all the inputs.
    pub fn new_from_sources<I, T, S>(
        sources: I,
        trace: impl Write + 'static,
        reporter: impl Reporter<(Warning, Files)> + 'static,
    ) -> std::io::Result<Self>
    where
        I: IntoIterator<Item = (T, S)>,
        T: Read,
        S: Into<OsString>,
    {
        let inputs = sources.into_iter().map(|(s, n)| Input::Source(s, n));
        Self::new_from_inputs(inputs, trace, reporter)
    }

    /// Parse an assignment of the form `path.to_field=value` as an override, with the provided
    /// merge priority. Assignments are typically provided by the user on the command line, as part
    /// of the customize mode.
    ///
    /// This method simply calls [FieldOverride::parse] with the [crate::cache::CacheHub] of the
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
    /// This method simply calls [FieldPath::parse] with the [crate::cache::CacheHub] of the current
    /// program.
    pub fn parse_field_path(&mut self, path: String) -> Result<FieldPath, ParseError> {
        FieldPath::parse(self.vm.import_resolver_mut(), path)
    }

    pub fn add_overrides(&mut self, overrides: impl IntoIterator<Item = FieldOverride>) {
        self.overrides.extend(overrides);
    }

    /// Adds a contract to be applied to the final program.
    pub fn add_contract(&mut self, contract: ProgramContract) {
        self.contracts.push(contract);
    }

    /// Adds a list of contracts to be applied to the final program, specified as file paths. Those
    /// contracts will be parsed, typechecked and further processed together with the rest of the
    /// program.
    pub fn add_contract_paths<P>(
        &mut self,
        contract_files: impl IntoIterator<Item = P>,
    ) -> Result<(), Error>
    where
        OsString: From<P>,
    {
        let prog_contracts: Result<Vec<_>, _> = contract_files
            .into_iter()
            .map(|file| -> Result<_, Error> {
                let file: OsString = file.into();
                let file_str = file.to_string_lossy().into_owned();

                let file_id = self
                    .vm
                    .import_resolver_mut()
                    .sources
                    .add_file(file, InputFormat::Nickel)
                    .map_err(|err| {
                        Error::IOError(IOError(format!(
                            "when opening contract file `{}`: {}",
                            &file_str, err
                        )))
                    })?;

                Ok(ProgramContract::Source(file_id))
            })
            .collect();

        self.contracts.extend(prog_contracts?);
        Ok(())
    }

    /// Adds import paths to the end of the list.
    pub fn add_import_paths<P>(&mut self, paths: impl Iterator<Item = P>)
    where
        PathBuf: From<P>,
    {
        self.vm
            .import_resolver_mut()
            .sources
            .add_import_paths(paths);
    }

    pub fn set_package_map(&mut self, map: PackageMap) {
        self.vm.import_resolver_mut().sources.set_package_map(map)
    }

    /// Only parse the program (and any additional attached contracts), don't typecheck or
    /// evaluate. Returns the [`RichTerm`] AST
    pub fn parse(&mut self) -> Result<RichTerm, Error> {
        self.vm
            .import_resolver_mut()
            .parse_to_ast(self.main_id)
            .map_err(Error::ParseErrors)?;

        for source in self.contracts.iter() {
            match source {
                ProgramContract::Term(_) => (),
                ProgramContract::Source(file_id) => {
                    self.vm
                        .import_resolver_mut()
                        .parse_to_ast(*file_id)
                        .map_err(Error::ParseErrors)?;
                }
            }
        }

        Ok(self
            .vm
            .import_resolver()
            .terms
            .get_owned(self.main_id)
            .expect("File parsed and then immediately accessed doesn't exist"))
    }

    /// Applies a custom transformation to the main term, assuming that it has been parsed but not
    /// yet transformed.
    ///
    /// If multiple invocations of `custom_transform` are needed, each subsequent invocation must supply
    /// `transform_id` with with a number higher than that of all previous invocations.
    pub fn custom_transform<E, F>(
        &mut self,
        transform_id: usize,
        mut transform: F,
    ) -> Result<(), CacheError<E>>
    where
        F: FnMut(&mut CacheHub, RichTerm) -> Result<RichTerm, E>,
    {
        self.vm
            .import_resolver_mut()
            .custom_transform(self.main_id, transform_id, &mut transform)
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
        let mut prepared_body = if self.overrides.is_empty() {
            self.vm.prepare_eval(self.main_id)?
        } else {
            let mut record = builder::Record::new();

            for ovd in self.overrides.iter().cloned() {
                let value_file_id = self
                    .vm
                    .import_resolver_mut()
                    .sources
                    .add_string(SourcePath::Override(ovd.path.clone()), ovd.value);
                let value_unparsed = self.vm.import_resolver().sources.source(value_file_id);

                if let Some('@') = value_unparsed.chars().next() {
                    // We parse the sigil expression, which has the general form `@xxx/yyy:value` where
                    // `/yyy` is optional.
                    let value_sep = value_unparsed.find(':').ok_or_else(|| {
                        ParseError::SigilExprMissingColon(RawSpan::from_range(
                            value_file_id,
                            0..value_unparsed.len(),
                        ))
                    })?;
                    let attr_sep = value_unparsed[..value_sep].find('/');

                    let attr = attr_sep.map(|attr_sep| &value_unparsed[attr_sep + 1..value_sep]);
                    let selector = &value_unparsed[1..attr_sep.unwrap_or(value_sep)];
                    let value = value_unparsed[value_sep + 1..].to_owned();

                    match (selector, attr) {
                        ("env", None) => match std::env::var(&value) {
                            Ok(env_var) => {
                                record = record.path(ovd.path.0).priority(ovd.priority).value(
                                    RichTerm::new(
                                        Term::Str(env_var.into()),
                                        RawSpan::from_range(
                                            value_file_id,
                                            value_sep + 1..value_unparsed.len(),
                                        )
                                        .into(),
                                    ),
                                );
                                Ok(())
                            }
                            Err(std::env::VarError::NotPresent) => Err(Error::IOError(IOError(
                                format!("environment variable `{value}` not found"),
                            ))),
                            Err(std::env::VarError::NotUnicode(..)) => {
                                Err(Error::IOError(IOError(format!(
                                    "environment variable `{value}` has non-unicode content"
                                ))))
                            }
                        },
                        ("env", Some(attr)) => {
                            Err(Error::ParseErrors(
                                ParseError::UnknownSigilAttribute {
                                    // unwrap(): if `attr` is `Some`, then `attr_sep` must be `Some`
                                    selector: selector.to_owned(),
                                    span: RawSpan::from_range(
                                        value_file_id,
                                        attr_sep.unwrap() + 1..value_sep,
                                    ),
                                    attribute: attr.to_owned(),
                                }
                                .into(),
                            ))
                        }
                        (selector, _) => Err(Error::ParseErrors(
                            ParseError::UnknownSigilSelector {
                                span: RawSpan::from_range(
                                    value_file_id,
                                    1..attr_sep.unwrap_or(value_sep),
                                ),
                                selector: selector.to_owned(),
                            }
                            .into(),
                        )),
                    }?;
                } else {
                    self.vm.prepare_eval(value_file_id)?;
                    record = record
                        .path(ovd.path.0)
                        .priority(ovd.priority)
                        .value(Term::ResolvedImport(value_file_id));
                }
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

        let runtime_contracts: Result<Vec<_>, _> = self
            .contracts
            .iter()
            .map(|contract| -> Result<_, Error> {
                match contract {
                    ProgramContract::Term(contract) => Ok(contract.clone()),
                    ProgramContract::Source(file_id) => {
                        let cache = self.vm.import_resolver_mut();
                        cache.prepare(*file_id)?;

                        // unwrap(): we just prepared the file above, so it must be in the cache.
                        let term = cache.terms.get_owned(*file_id).unwrap();

                        // The label needs a position to show where the contract application is coming from.
                        // Since it's not really coming from source code, we reconstruct the CLI argument
                        // somewhere in the source cache.
                        let pos = term.pos;
                        let typ = crate::typ::Type {
                            typ: crate::typ::TypeF::Contract(term.clone()),
                            pos,
                        };

                        let source_name = cache.sources.name(*file_id).to_string_lossy();
                        let arg_id = cache.sources.add_string(
                            SourcePath::CliFieldAssignment,
                            format!("--apply-contract {source_name}"),
                        );

                        let span = cache.sources.files().source_span(arg_id);

                        Ok(RuntimeContract::new(
                            term,
                            Label {
                                typ: std::rc::Rc::new(typ),
                                span: Some(span),
                                ..Default::default()
                            },
                        ))
                    }
                }
            })
            .collect();

        prepared_body =
            RuntimeContract::apply_all(prepared_body, runtime_contracts?, TermPos::None);

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

    /// Evaluate a closure using the same virtual machine (and import resolver)
    /// as the main term. The closure should already have been prepared for
    /// evaluation, with imports resolved and any necessary transformations
    /// applied.
    pub fn eval_closure(&mut self, closure: Closure) -> Result<RichTerm, EvalError> {
        self.vm.reset();
        Ok(self.vm.eval_closure(closure)?.body)
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

    /// Same as `eval_closure`, but does a full evaluation and does not substitute all variables.
    ///
    /// (Or, same as `eval_deep` but takes a closure.)
    pub fn eval_deep_closure(&mut self, closure: Closure) -> Result<RichTerm, EvalError> {
        self.vm.reset();
        self.vm.eval_deep_closure(closure)
    }

    /// Prepare for evaluation, then fetch the metadata of `self.field`, or list the fields of the
    /// whole program if `self.field` is empty.
    pub fn query(&mut self) -> Result<Field, Error> {
        let prepared = self.prepare_query()?;

        Ok(self.vm.query_closure(prepared, &self.field)?)
    }

    /// Load, parse, and typecheck the program (together with additional contracts) and the
    /// standard library, if not already done.
    pub fn typecheck(&mut self, initial_mode: TypecheckMode) -> Result<(), Error> {
        // If the main file is known to not be Nickel, we don't bother parsing it into an AST
        // (`cache.typecheck()` will ignore it anyway)
        let is_nickel = !matches!(
            self.vm.import_resolver().input_format(self.main_id),
            None | Some(InputFormat::Nickel)
        );

        if is_nickel {
            self.vm.import_resolver_mut().parse_to_ast(self.main_id)?;
        }

        for source in self.contracts.iter() {
            match source {
                ProgramContract::Term(_) => (),
                ProgramContract::Source(file_id) => {
                    self.vm.import_resolver_mut().parse_to_ast(*file_id)?;
                }
            }
        }

        self.vm.import_resolver_mut().load_stdlib()?;
        self.vm
            .import_resolver_mut()
            .typecheck(self.main_id, initial_mode)
            .map_err(|cache_err| {
                cache_err.unwrap_error("program::typecheck(): expected source to be parsed")
            })?;

        for source in self.contracts.iter() {
            match source {
                ProgramContract::Term(_) => (),
                ProgramContract::Source(file_id) => {
                    self.vm
                        .import_resolver_mut()
                        .typecheck(*file_id, initial_mode)
                        .map_err(|cache_err| {
                            cache_err.unwrap_error(
                                "program::typecheck(): expected contract to be parsed",
                            )
                        })?;
                }
            }
        }

        Ok(())
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
    ///   term is returned unevaluated[^missing-field-def]
    /// - If any other error occurs, the evaluation fails and returns the error.
    ///
    /// [^missing-field-def]: Because we want to handle partial configurations as well,
    /// [crate::error::EvalError::MissingFieldDef] errors are _ignored_: if this is encountered
    /// when evaluating a field, this field is just left as it is and the evaluation proceeds.
    pub fn eval_record_spine(&mut self) -> Result<RichTerm, Error> {
        self.maybe_closurized_eval_record_spine(false)
    }

    /// Evaluate a program into a record spine, while closurizing all the
    /// non-record "leaves" in the spine.
    ///
    /// To understand the difference between this function and
    /// [`Program::eval_record_spine`], consider a term like
    ///
    /// ```nickel
    /// let foo = 1 in { bar = [foo] }
    /// ```
    ///
    /// `eval_record_spine` will evaluate this into a record containing the
    /// field `bar`, and the value of that field will be a `Term::Array`
    /// containing a `Term::Var("foo")`. In contrast, `eval_closurized` will
    /// still evaluate the term into a record contining `bar`, but the value of
    /// that field will be a `Term::Closure` containing that same `Term::Array`,
    /// together with an `Environment` defining the variable "foo". In
    /// particular, the closurized version is more useful if you intend to
    /// further evaluate any record fields, while the non-closurized version is
    /// more useful if you intend to do further static analysis.
    pub fn eval_closurized_record_spine(&mut self) -> Result<RichTerm, Error> {
        self.maybe_closurized_eval_record_spine(true)
    }

    fn maybe_closurized_eval_record_spine(&mut self, closurize: bool) -> Result<RichTerm, Error> {
        use crate::{
            eval::Environment,
            match_sharedterm,
            term::{record::RecordData, RuntimeContract},
        };

        let prepared = self.prepare_eval()?;

        // Naively evaluating some legit recursive structures might lead to an infinite loop. Take
        // for example this simple contract definition:
        //
        // ```nickel
        // {
        //   Tree = {
        //     data | Number,
        //     left | Tree | optional,
        //     right | Tree | optional,
        //   }
        // }
        // ```
        //
        // Here, we don't want to unfold the occurrences of `Tree` appearing in `left` and `right`,
        // or we will just go on indefinitely (until a stack overflow in practice). To avoid this,
        // we store the cache index (thunk) corresponding to the content of the `Tree` field before
        // evaluating it. After we have successfully evaluated it to a record, we mark it (lock),
        // and if we come across the same thunk while evaluating one of its children, here `left`
        // for example, we don't evaluate it further.

        // Eval pending contracts as well, in order to extract more information from potential
        // record contract fields.
        fn eval_contracts<EC: EvalCache>(
            vm: &mut VirtualMachine<CacheHub, EC>,
            mut pending_contracts: Vec<RuntimeContract>,
            current_env: Environment,
            closurize: bool,
        ) -> Result<Vec<RuntimeContract>, Error> {
            vm.reset();

            for ctr in pending_contracts.iter_mut() {
                let rt = ctr.contract.clone();
                // Note that contracts can't be referred to recursively, as they aren't binding
                // anything. Only fields are. This is why we pass `None` for `self_idx`: there is
                // no locking required here.
                ctr.contract = eval_guarded(vm, rt, current_env.clone(), closurize)?;
            }

            Ok(pending_contracts)
        }

        // Handles thunk locking (and unlocking upon errors) to detect infinite recursion, but
        // hands over the meat of the work to `do_eval`.
        fn eval_guarded<EC: EvalCache>(
            vm: &mut VirtualMachine<CacheHub, EC>,
            term: RichTerm,
            env: Environment,
            closurize: bool,
        ) -> Result<RichTerm, Error> {
            vm.reset();

            let curr_thunk = term.as_ref().try_as_closure();

            if let Some(thunk) = curr_thunk.as_ref() {
                // If the thunk is already locked, it's the thunk of some parent field, and we stop
                // here to avoid infinite recursion.
                if !thunk.lock() {
                    return Ok(term);
                }
            }

            let result = do_eval(vm, term.clone(), env, closurize);

            // Once we're done evaluating all the children, or if there was an error, we unlock the
            // current thunk
            if let Some(thunk) = curr_thunk.as_ref() {
                thunk.unlock();
            }

            // We expect to hit `MissingFieldDef` errors. When a configuration
            // contains undefined record fields they most likely will be used
            // recursively in the definition of some other fields. So instead of
            // bubbling up an evaluation error in this case we just leave fields
            // that depend on as yet undefined fields unevaluated; we wouldn't
            // be able to extract dcoumentation from their values anyways. All
            // other evaluation errors should however be reported to the user
            // instead of resulting in documentation being silently skipped.
            if matches!(
                result,
                Err(Error::EvalError(EvalError::MissingFieldDef { .. }))
            ) {
                return Ok(term);
            }

            result
        }

        // Evaluates the closure, and if it's a record, recursively evaluate its fields and their
        // contracts.
        fn do_eval<EC: EvalCache>(
            vm: &mut VirtualMachine<CacheHub, EC>,
            term: RichTerm,
            env: Environment,
            closurize: bool,
        ) -> Result<RichTerm, Error> {
            let evaled = vm.eval_closure(Closure { body: term, env })?;

            match_sharedterm!(match (evaled.body.term) {
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
                                        .map(|value| {
                                            eval_guarded(vm, value, evaled.env.clone(), closurize)
                                        })
                                        .transpose()?,
                                    pending_contracts: eval_contracts(
                                        vm,
                                        field.pending_contracts,
                                        evaled.env.clone(),
                                        closurize,
                                    )?,
                                    ..field
                                },
                            ))
                        })
                        .collect::<Result<_, Error>>()?;

                    Ok(RichTerm::new(
                        Term::Record(RecordData { fields, ..data }),
                        evaled.body.pos,
                    ))
                }
                _ =>
                    if closurize {
                        Ok(evaled.body.closurize(&mut vm.cache, evaled.env))
                    } else {
                        Ok(evaled.body)
                    },
            })
        }

        eval_guarded(&mut self.vm, prepared.body, prepared.env, closurize)
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

        let Program {
            ref main_id, vm, ..
        } = self;
        let allocator = Allocator::default();

        let ast_alloc = AstAlloc::new();
        let ast = vm
            .import_resolver()
            .sources
            .parse_nickel(&ast_alloc, *main_id)?;
        let rt = measure_runtime!("runtime:ast_conversion", ast.to_mainline());
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

    /// Returns a copy of the program's current `Files` database. This doesn't actually clone the content of the source files, see [crate::files::Files].
    pub fn files(&self) -> Files {
        self.vm.import_resolver().files().clone()
    }
}

#[cfg(feature = "doc")]
mod doc {
    use crate::error::{Error, ExportErrorData, IOError};
    use crate::term::{RichTerm, Term};
    use comrak::arena_tree::{Children, NodeEdge};
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

    fn ast_node<'a>(val: NodeValue) -> AstNode<'a> {
        // comrak allows for ast nodes to be tagged with source location. This location
        // isn't need for rendering; it seems to be mainly for plugins to use. Since our
        // markdown is generated anyway, we just stick in a dummy value.
        let pos = comrak::nodes::LineColumn::from((0, 0));
        AstNode::new(std::cell::RefCell::new(Ast::new(val, pos)))
    }

    impl ExtractedDocumentation {
        pub fn extract_from_term(rt: &RichTerm) -> Option<Self> {
            match rt.term.as_ref() {
                Term::Record(record) | Term::RecRecord(record, ..) => {
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
            let document = ast_node(NodeValue::Document);

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
                    for child in parse_markdown_string(header_level + 1, arena, doc, options) {
                        document.append(child);
                    }
                }

                if let Some(ref subfields) = field.fields {
                    subfields.markdown_append(header_level + 1, arena, document, options);
                }
            }
        }

        pub fn docstrings(&self) -> Vec<(Vec<&str>, &str)> {
            fn collect<'a>(
                slf: &'a ExtractedDocumentation,
                path: &[&'a str],
                acc: &mut Vec<(Vec<&'a str>, &'a str)>,
            ) {
                for (name, field) in &slf.fields {
                    let mut path = path.to_owned();
                    path.push(name);

                    if let Some(fields) = &field.fields {
                        collect(fields, &path, acc);
                    }

                    if let Some(doc) = &field.documentation {
                        acc.push((path, doc));
                    }
                }
            }

            let mut ret = Vec::new();
            collect(self, &[], &mut ret);
            ret
        }
    }

    /// Parses a string into markdown and increases any headers in the markdown by the specified
    /// level. This allows having headers in documentation without clashing with the structure of
    /// the document.
    ///
    /// Since this markdown chunk is going to be inserted into another document, we can't return a
    /// document node (a document within another document is considered ill-formed by `comrak`).
    /// Instead, we strip the root document node off, and return its children.
    fn parse_markdown_string<'a>(
        header_level: u8,
        arena: &'a Arena<AstNode<'a>>,
        md: &str,
        options: &ComrakOptions,
    ) -> Children<'a, std::cell::RefCell<Ast>> {
        let node = parse_document(arena, md, options);

        // Increase header level of every header
        for edge in node.traverse() {
            if let NodeEdge::Start(n) = edge {
                n.data
                    .replace_with(|ast| increase_header_level(header_level, ast).clone());
            }
        }

        debug_assert!(node.data.borrow().value == NodeValue::Document);
        node.children()
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
        let res = arena.alloc(ast_node(NodeValue::Heading(NodeHeading {
            level: header_level,
            setext: false,
        })));

        let code = arena.alloc(ast_node(NodeValue::Code(NodeCode {
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
        let list = arena.alloc(ast_node(NodeValue::List(NodeList {
            list_type: ListType::Bullet,
            marker_offset: 1,
            padding: 0,
            start: 0,
            delimiter: ListDelimType::Period,
            bullet_char: b'*',
            tight: true,
            is_task_list: false,
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
        let list_item = arena.alloc(ast_node(NodeValue::Item(NodeList {
            list_type: ListType::Bullet,
            marker_offset: 1,
            padding: 0,
            start: 0,
            delimiter: ListDelimType::Period,
            bullet_char: b'*',
            tight: true,
            is_task_list: false,
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
        let paragraph = arena.alloc(ast_node(NodeValue::Paragraph));

        paragraph.append(arena.alloc(ast_node(NodeValue::Code(NodeCode {
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
    use crate::error::{EvalError, NullReporter};
    use crate::eval::cache::CacheImpl;
    use crate::identifier::LocIdent;
    use crate::position::TermPos;
    use crate::term::array::ArrayAttrs;
    use assert_matches::assert_matches;
    use std::io::Cursor;

    fn eval_full(s: &str) -> Result<RichTerm, Error> {
        let src = Cursor::new(s);

        let mut p: Program<CacheImpl> =
            Program::new_from_source(src, "<test>", std::io::sink(), NullReporter {}).map_err(
                |io_err| {
                    Error::EvalError(EvalError::Other(
                        format!("IO error: {io_err}"),
                        TermPos::None,
                    ))
                },
            )?;
        p.eval_full()
    }

    fn typecheck(s: &str) -> Result<(), Error> {
        let src = Cursor::new(s);

        let mut p: Program<CacheImpl> =
            Program::new_from_source(src, "<test>", std::io::sink(), NullReporter {}).map_err(
                |io_err| {
                    Error::EvalError(EvalError::Other(
                        format!("IO error: {io_err}"),
                        TermPos::None,
                    ))
                },
            )?;
        p.typecheck(TypecheckMode::Walk)
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
