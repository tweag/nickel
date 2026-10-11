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
    ast::{AstAlloc, compat::ToMainline},
    cache::*,
    closurize::Closurize as _,
    error::{
        Error, EvalError, EvalErrorKind, IOError, NullReporter, ParseError, ParseErrors, Reporter,
        warning::Warning,
    },
    eval::{
        Closure, VirtualMachine, VmContext,
        cache::Cache as EvalCache,
        value::{Container, NickelValue, ValueContent},
    },
    files::{FileId, Files},
    identifier::{Ident, LocIdent},
    label::Label,
    metrics::{increment, measure_runtime},
    package::PackageMap,
    position::{PosIdx, PosTable, RawSpan},
    term::{
        BinaryOp, Import, MergePriority, RuntimeContract, Term,
        make::{self as mk_term, builder},
        record::Field,
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
            ErrorTolerantParserCompat, grammar::StaticFieldPathParser, lexer::Lexer,
        };

        let input_id = caches.replace_string(SourcePath::Query, input);
        let s = caches.sources.source(input_id);

        let parser = StaticFieldPathParser::new();
        let field_path = parser
            // This doesn't use the position table at all, since `LocIdent` currently stores a
            // TermPos directly
            .parse_strict_compat(&mut PosTable::new(), input_id, Lexer::new(s))
            // We just need to report an error here
            .map_err(|mut errs| {
                errs.errors.pop().expect(
                    "because parsing of the query path failed, the error \
                    list must be non-empty, put .pop() failed",
                )
            })?;

        Ok(FieldPath(field_path))
    }

    /// As [`Self::parse`], but accepts an `Option` to accommodate for the absence of path. If the
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
    /// Internally, the parser entirely parses the `value` part to a [NickelValue] (have it accept
    /// anything after the equal sign is in fact harder than actually parsing it), but what we need
    /// at this point is just a string. Thus, `parse` uses the span to extract back the `value`
    /// part of the input string.
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
            ErrorTolerantParserCompat,
            grammar::{CliFieldAssignmentParser, StaticFieldPathParser},
            lexer::{Lexer, NormalToken, Token},
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
                    // we don't use the position table for pure field paths
                    .parse_strict_compat(&mut PosTable::new(), input_id, Lexer::new(&s[..start_eq]))
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
                    // once again, we ditch the value, so no PosIdx leaks outside of
                    // `parse_strict_compat` and we can thus ignore the position table entirely
                    .parse_strict_compat(&mut PosTable::new(), input_id, Lexer::new(s))
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
    /// The context/persistent state of the Nickel virtual machine.
    vm_ctxt: VmContext<CacheHub, EC>,
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

impl<EC: EvalCache> Program<EC> {
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
        FieldOverride::parse(&mut self.vm_ctxt.import_resolver, assignment, priority)
    }

    /// Parse a dot-separated field path of the form `path.to.field`.
    ///
    /// This method simply calls [FieldPath::parse] with the [crate::cache::CacheHub] of the current
    /// program.
    pub fn parse_field_path(&mut self, path: String) -> Result<FieldPath, ParseError> {
        FieldPath::parse(&mut self.vm_ctxt.import_resolver, path)
    }

    pub fn add_overrides(&mut self, overrides: impl IntoIterator<Item = FieldOverride>) {
        self.overrides.extend(overrides);
    }

    /// Adds a contract to be applied to the final program.
    pub fn add_contract(&mut self, contract: ProgramContract) {
        self.contracts.push(contract);
    }

    /// Only parse the program (and any additional attached contracts), don't typecheck or
    /// evaluate. Returns the [`NickelValue`] AST
    pub fn parse(&mut self) -> Result<NickelValue, Error> {
        self.vm_ctxt
            .import_resolver
            .parse_to_ast(self.main_id)
            .map_err(Error::ParseErrors)?;

        for source in self.contracts.iter() {
            match source {
                ProgramContract::Term(_) => (),
                ProgramContract::Source(file_id) => {
                    self.vm_ctxt
                        .import_resolver
                        .parse_to_ast(*file_id)
                        .map_err(Error::ParseErrors)?;
                }
            }
        }

        Ok(self
            .vm_ctxt
            .import_resolver
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
    ) -> Result<(), TermCacheError<E>>
    where
        F: FnMut(&mut CacheHub, &mut PosTable, NickelValue) -> Result<NickelValue, E>,
    {
        self.vm_ctxt.import_resolver.custom_transform(
            self.main_id,
            transform_id,
            &mut |cache, value| transform(cache, &mut self.vm_ctxt.pos_table, value),
        )
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
            self.vm_ctxt.prepare_eval(self.main_id)?
        } else {
            let mut record = builder::Record::new();

            for ovd in self.overrides.iter().cloned() {
                let value_file_id = self
                    .vm_ctxt
                    .import_resolver
                    .sources
                    .add_string(SourcePath::Override(ovd.path.clone()), ovd.value);
                let value_unparsed = self.vm_ctxt.import_resolver.sources.source(value_file_id);

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
                                    NickelValue::string(
                                        env_var,
                                        self.vm_ctxt.pos_table.push(
                                            RawSpan::from_range(
                                                value_file_id,
                                                value_sep + 1..value_unparsed.len(),
                                            )
                                            .into(),
                                        ),
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
                    self.vm_ctxt.prepare_eval(value_file_id)?;
                    record = record
                        .path(ovd.path.0)
                        .priority(ovd.priority)
                        .value(Term::ResolvedImport(value_file_id));
                }
            }

            let t = self.vm_ctxt.prepare_eval(self.main_id)?;
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
                        let cache = &mut self.vm_ctxt.import_resolver;
                        cache.prepare(&mut self.vm_ctxt.pos_table, *file_id)?;

                        // unwrap(): we just prepared the file above, so it must be in the cache.
                        let value = cache.terms.get_owned(*file_id).unwrap();

                        // The label needs a position to show where the contract application is coming from.
                        // Since it's not really coming from source code, we reconstruct the CLI argument
                        // somewhere in the source cache.
                        let pos = value.pos_idx();
                        let typ = crate::typ::Type {
                            typ: crate::typ::TypeF::Contract(value.clone()),
                            pos: self.vm_ctxt.pos_table.get(pos),
                        };

                        let source_name = cache.sources.name(*file_id).to_string_lossy();
                        let arg_id = cache.sources.add_string(
                            SourcePath::CliFieldAssignment,
                            format!("--apply-contract {source_name}"),
                        );

                        let span = cache.sources.files().source_span(arg_id);

                        Ok(RuntimeContract::new(
                            value,
                            Label {
                                typ: std::rc::Rc::new(typ),
                                span: self.vm_ctxt.pos_table.push(span.into()),
                                ..Default::default()
                            },
                        ))
                    }
                }
            })
            .collect();

        prepared_body = RuntimeContract::apply_all(prepared_body, runtime_contracts?, PosIdx::NONE);

        let prepared: Closure = prepared_body.into();

        let result = if for_query {
            prepared
        } else {
            VirtualMachine::new(&mut self.vm_ctxt)
                .extract_field_value_closure(prepared, &self.field)?
        };

        Ok(result)
    }

    /// Creates an new VM instance borrowing from [Self::vm_ctxt].
    fn new_vm(&mut self) -> VirtualMachine<'_, CacheHub, EC> {
        VirtualMachine::new(&mut self.vm_ctxt)
    }

    /// Parse if necessary, typecheck and then evaluate the program.
    pub fn eval(&mut self) -> Result<NickelValue, Error> {
        let prepared = self.prepare_eval()?;
        Ok(self.new_vm().eval_closure(prepared)?.value)
    }

    /// Evaluate a closure using the same virtual machine (and import resolver)
    /// as the main term. The closure should already have been prepared for
    /// evaluation, with imports resolved and any necessary transformations
    /// applied.
    pub fn eval_closure(&mut self, closure: Closure) -> Result<NickelValue, EvalError> {
        Ok(self.new_vm().eval_closure(closure)?.value)
    }

    /// Same as `eval`, but proceeds to a full evaluation.
    pub fn eval_full(&mut self) -> Result<NickelValue, Error> {
        let prepared = self.prepare_eval()?;

        Ok(self.new_vm().eval_full_closure(prepared)?.value)
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
    pub fn eval_full_for_export(&mut self) -> Result<NickelValue, Error> {
        let prepared = self.prepare_eval()?;

        Ok(self.new_vm().eval_full_for_export_closure(prepared)?)
    }

    /// Same as `eval_full`, but does not substitute all variables.
    pub fn eval_deep(&mut self) -> Result<NickelValue, Error> {
        let prepared = self.prepare_eval()?;

        Ok(self.new_vm().eval_deep_closure(prepared)?)
    }

    /// Same as `eval_closure`, but does a full evaluation and does not substitute all variables.
    ///
    /// (Or, same as `eval_deep` but takes a closure.)
    pub fn eval_deep_closure(&mut self, closure: Closure) -> Result<NickelValue, EvalError> {
        self.new_vm().eval_deep_closure(closure)
    }

    /// Prepare for evaluation, then fetch the metadata of `self.field`, or list the fields of the
    /// whole program if `self.field` is empty.
    pub fn query(&mut self) -> Result<Field, Error> {
        let prepared = self.prepare_query()?;

        // We have to inline `new_vm()` to get the borrow checker to understand that we can both
        // borrow `vm_ctxt` mutably and `field` immutably at the same time.
        Ok(VirtualMachine::new(&mut self.vm_ctxt).query_closure(prepared, &self.field)?)
    }

    /// Load, parse, and typecheck the program (together with additional contracts) and the
    /// standard library, if not already done.
    pub fn typecheck(&mut self, initial_mode: TypecheckMode) -> Result<(), Error> {
        // If the main file is known to not be Nickel, we don't bother parsing it into an AST, and
        // there is nothing to typecheck for it: non-Nickel inputs aren't typechecked.
        let is_nickel = matches!(
            self.vm_ctxt.import_resolver.input_format(self.main_id),
            None | Some(InputFormat::Nickel)
        );

        if is_nickel {
            self.vm_ctxt.import_resolver.parse_to_ast(self.main_id)?;
        }

        for source in self.contracts.iter() {
            match source {
                ProgramContract::Term(_) => (),
                ProgramContract::Source(file_id) => {
                    self.vm_ctxt.import_resolver.parse_to_ast(*file_id)?;
                }
            }
        }

        self.vm_ctxt.import_resolver.load_stdlib()?;
        self.vm_ctxt.register_extend_env_for_typecheck();

        if is_nickel {
            self.vm_ctxt
                .import_resolver
                .typecheck(self.main_id, initial_mode)
                .map_err(|cache_err| {
                    cache_err.unwrap_error("program::typecheck(): expected source to be parsed")
                })?;
        } else {
            eprintln!(
                "Warning: skipping {} because it is not a Nickel source file (JSON-like data is always well typed)",
                self.vm_ctxt
                    .import_resolver
                    .sources
                    .name(self.main_id)
                    .to_string_lossy()
            );
        }

        for source in self.contracts.iter() {
            match source {
                ProgramContract::Term(_) => (),
                ProgramContract::Source(file_id) => {
                    self.vm_ctxt
                        .import_resolver
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

    /// Parse and compile the stdlib and the program to the runtime representation. This is usually
    /// done as part of the various `prepare_xxx` methods, but for some specific workflows (such as
    /// `nickel test`), compilation might need to be performed explicitly.
    pub fn compile(&mut self) -> Result<(), Error> {
        let cache = &mut self.vm_ctxt.import_resolver;

        cache.load_stdlib()?;
        cache.parse_to_ast(self.main_id)?;
        // unwrap(): We just loaded the stdlib, so it should be there
        cache.compile_stdlib(&mut self.vm_ctxt.pos_table).unwrap();
        cache
            .compile(&mut self.vm_ctxt.pos_table, self.main_id)
            .map_err(|cache_err| {
                cache_err.unwrap_error("program::compile(): we just parsed the program")
            })?;

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
    /// By record spine, we mean that the result is a tree of evaluated nested records, and leaves
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
    /// - If the evaluation fails with [EvalErrorKind::MissingFieldDef], the original
    ///   term is returned unevaluated[^missing-field-def]
    /// - If any other error occurs, the evaluation fails and returns the error.
    ///
    /// [^missing-field-def]: Because we want to handle partial configurations as well,
    /// [EvalErrorKind::MissingFieldDef] errors are _ignored_: if this is encountered when
    /// evaluating a field, this field is just left as it is and the evaluation proceeds.
    pub fn eval_record_spine(&mut self) -> Result<NickelValue, Error> {
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
    /// still evaluate the term into a record containing `bar`, but the value of
    /// that field will be a `Term::Closure` containing that same `Term::Array`,
    /// together with an `Environment` defining the variable "foo". In
    /// particular, the closurized version is more useful if you intend to
    /// further evaluate any record fields, while the non-closurized version is
    /// more useful if you intend to do further static analysis.
    pub fn eval_closurized_record_spine(&mut self) -> Result<NickelValue, Error> {
        self.maybe_closurized_eval_record_spine(true)
    }

    fn maybe_closurized_eval_record_spine(
        &mut self,
        closurize: bool,
    ) -> Result<NickelValue, Error> {
        use crate::{
            eval::Environment,
            term::{RuntimeContract, record::RecordData},
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
            vm_ctxt: &mut VmContext<CacheHub, EC>,
            mut pending_contracts: Vec<RuntimeContract>,
            current_env: Environment,
            closurize: bool,
        ) -> Result<Vec<RuntimeContract>, Error> {
            for ctr in pending_contracts.iter_mut() {
                let rt = ctr.contract.clone();
                // Note that contracts can't be referred to recursively, as they aren't binding
                // anything. Only fields are. This is why we pass `None` for `self_idx`: there is
                // no locking required here.
                ctr.contract = eval_guarded(vm_ctxt, rt, current_env.clone(), closurize)?;
            }

            Ok(pending_contracts)
        }

        // Handles thunk locking (and unlocking upon errors) to detect infinite recursion, but
        // hands over the meat of the work to `do_eval`.
        fn eval_guarded<EC: EvalCache>(
            vm_ctxt: &mut VmContext<CacheHub, EC>,
            term: NickelValue,
            env: Environment,
            closurize: bool,
        ) -> Result<NickelValue, Error> {
            let curr_thunk = term.as_thunk();

            if let Some(thunk) = curr_thunk {
                // If the thunk is already locked, it's the thunk of some parent field, and we stop
                // here to avoid infinite recursion.
                if !thunk.lock() {
                    return Ok(term);
                }
            }

            let result = do_eval(vm_ctxt, term.clone(), env, closurize);

            // Once we're done evaluating all the children, or if there was an error, we unlock the
            // current thunk
            if let Some(thunk) = curr_thunk {
                thunk.unlock();
            }

            // We expect to hit `MissingFieldDef` errors. When a configuration
            // contains undefined record fields they most likely will be used
            // recursively in the definition of some other fields. So instead of
            // bubbling up an evaluation error in this case we just leave fields
            // that depend on as yet undefined fields unevaluated; we wouldn't
            // be able to extract documentation from their values anyways. All
            // other evaluation errors should however be reported to the user
            // instead of resulting in documentation being silently skipped.
            if let Err(Error::EvalError(err_data)) = &result
                && let EvalErrorKind::MissingFieldDef { .. } = &err_data.error
            {
                return Ok(term);
            }

            result
        }

        // Evaluates the closure, and if it's a record, recursively evaluate its fields and their
        // contracts.
        fn do_eval<EC: EvalCache>(
            vm_ctxt: &mut VmContext<CacheHub, EC>,
            term: NickelValue,
            env: Environment,
            closurize: bool,
        ) -> Result<NickelValue, Error> {
            let evaled = VirtualMachine::new(vm_ctxt).eval_closure(Closure { value: term, env })?;
            let pos_idx = evaled.value.pos_idx();

            match evaled.value.content() {
                ValueContent::Record(lens) => {
                    let Container::Alloc(data) = lens.take() else {
                        //unwrap(): will go away
                        return Ok(NickelValue::empty_record().with_pos_idx(pos_idx));
                    };

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
                                            eval_guarded(
                                                vm_ctxt,
                                                value,
                                                evaled.env.clone(),
                                                closurize,
                                            )
                                        })
                                        .transpose()?,
                                    pending_contracts: eval_contracts(
                                        vm_ctxt,
                                        field.pending_contracts,
                                        evaled.env.clone(),
                                        closurize,
                                    )?,
                                    ..field
                                },
                            ))
                        })
                        .collect::<Result<_, Error>>()?;

                    Ok(NickelValue::record(RecordData { fields, ..data }, pos_idx))
                }
                lens => {
                    let value = lens.restore();

                    if closurize {
                        Ok(value.closurize(&mut vm_ctxt.cache, evaled.env))
                    } else {
                        Ok(value)
                    }
                }
            }
        }

        eval_guarded(&mut self.vm_ctxt, prepared.value, prepared.env, closurize)
    }

    /// Extract documentation from the program
    #[cfg(feature = "doc")]
    pub fn extract_doc(&mut self) -> Result<doc::ExtractedDocumentation, Error> {
        use crate::error::ExportErrorKind;

        let term = self.eval_record_spine()?;
        doc::ExtractedDocumentation::extract_from_term(&term).ok_or(Error::export_error(
            self.vm_ctxt.pos_table.clone(),
            ExportErrorKind::NoDocumentation(term.clone()),
        ))
    }

    /// Skip loading the standard library on the next preparation step. Only available in debug
    /// builds, since the underlying field on [`crate::cache::CacheHub`] is debug-gated.
    #[cfg(debug_assertions)]
    pub fn set_skip_stdlib(&mut self) {
        self.vm_ctxt.import_resolver.skip_stdlib = true;
    }

    pub fn pprint_ast(
        &mut self,
        out: &mut impl std::io::Write,
        apply_transforms: bool,
    ) -> Result<(), Error> {
        use crate::{pretty::*, transform::transform};

        let ast_alloc = AstAlloc::new();
        let ast = self
            .vm_ctxt
            .import_resolver
            .sources
            .parse_nickel(&ast_alloc, self.main_id)?;
        if apply_transforms {
            let allocator = Allocator::default();
            let rt = measure_runtime!(
                "runtime:ast_conversion",
                ast.to_mainline(&mut self.vm_ctxt.pos_table)
            );
            let rt = transform(&mut self.vm_ctxt.pos_table, rt, None)
                .map_err(|uvar_err| Error::ParseErrors(ParseErrors::from(uvar_err)))?;
            let doc: DocBuilder<_, ()> = rt.pretty(&allocator);
            doc.render(80, out).map_err(IOError::from)?;
            writeln!(out).map_err(IOError::from)?;
        } else {
            let allocator = crate::ast::pretty::Allocator::default();
            let doc: DocBuilder<_, ()> = ast.pretty(&allocator);
            doc.render(80, out).map_err(IOError::from)?;
            writeln!(out).map_err(IOError::from)?;
        }

        Ok(())
    }

    /// Returns a copy of the program's current `Files` database. This doesn't actually clone the content of the source files, see [crate::files::Files].
    pub fn files(&self) -> Files {
        self.vm_ctxt.import_resolver.files().clone()
    }

    /// Returns a reference to the position table.
    pub fn pos_table(&self) -> &PosTable {
        &self.vm_ctxt.pos_table
    }
}

/// An error that occurred during a call to [`ProgramBuilder::build`].
#[derive(Debug)]
pub enum BuilderError {
    /// No nickel code was provided to build the program.
    NoInputs,
    /// An I/O error occurred reading an input file.
    Io {
        path: Option<PathBuf>,
        error: std::io::Error,
    },
}

impl std::error::Error for BuilderError {}

impl std::fmt::Display for BuilderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io { path, error } => {
                if let Some(path) = path {
                    write!(f, "{}: {error}", path.display())
                } else {
                    error.fmt(f)
                }
            }
            Self::NoInputs => write!(f, "ProgramBuilder::build: no inputs were added"),
        }
    }
}

/// An input source held by a [`ProgramBuilder`].
///
/// The concrete `Read` implementation is erased to `Box<dyn Read>` so that builders can mix paths
/// and heterogeneous in-memory sources without per-source generics.
enum BuilderInput {
    /// A filesystem path. The format is inferred from the path's extension.
    Path(OsString),
    /// An in-memory source. Parsed as `format` and identified by `name` in the source cache.
    Source {
        source: Box<dyn Read>,
        name: OsString,
        format: InputFormat,
    },
}

/// Builder for [`Program`].
///
/// Accumulates inputs and configuration, then produces a [`Program`] via [`Self::build`]. All I/O
/// is deferred to `build`, so the configuration methods themselves are infallible.
///
/// # Generic parameters
///
/// - `R` — the warning reporter type, defaulting to [`NullReporter`].
/// - `W` — the trace writer type, defaulting to [`io::Sink`].
///
/// Defaults can be replaced with [`Self::with_reporter`] and [`Self::with_trace`]; both consume
/// `self` and return a builder with the new generic parameter.
///
/// # Example
///
/// ```no_run
/// use nickel_lang_core::{eval::cache::CacheImpl, program::{Program, ProgramBuilder}};
///
/// let program: Program<CacheImpl> = ProgramBuilder::new()
///     .add_path("config.ncl")
///     .add_import_paths(["./vendor"])
///     .build()
///     .unwrap();
/// ```
pub struct ProgramBuilder<R = NullReporter, W = io::Sink> {
    inputs: Vec<BuilderInput>,
    overrides: Vec<FieldOverride>,
    field: FieldPath,
    contracts: Vec<ProgramContract>,
    contract_paths: Vec<OsString>,
    import_paths: Vec<PathBuf>,
    package_map: Option<PackageMap>,
    extra_env: Vec<(Ident, NickelValue)>,
    #[cfg(feature = "incremental-experimental")]
    enable_incremental_evaluation: bool,
    trace: W,
    reporter: R,
}

impl Default for ProgramBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl ProgramBuilder {
    /// Create an empty builder with the default reporter ([`NullReporter`]) and trace writer
    /// ([`io::sink`]). Use [`Self::with_reporter`] and [`Self::with_trace`] to override them.
    pub fn new() -> Self {
        ProgramBuilder {
            inputs: Vec::new(),
            overrides: Vec::new(),
            field: FieldPath::new(),
            contracts: Vec::new(),
            contract_paths: Vec::new(),
            import_paths: Vec::new(),
            package_map: None,
            extra_env: Vec::new(),
            #[cfg(feature = "incremental-experimental")]
            enable_incremental_evaluation: false,
            trace: io::sink(),
            reporter: NullReporter {},
        }
    }
}

impl<R, W> ProgramBuilder<R, W> {
    /// Add a single filesystem path as an input. The format is inferred from the path's extension.
    pub fn add_path(mut self, path: impl Into<OsString>) -> Self {
        self.inputs.push(BuilderInput::Path(path.into()));
        self
    }

    /// Add multiple filesystem paths as inputs.
    pub fn add_paths<I, P>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<OsString>,
    {
        self.inputs
            .extend(paths.into_iter().map(|p| BuilderInput::Path(p.into())));
        self
    }

    /// Add an in-memory source as an input, parsed as Nickel.
    pub fn add_source(self, source: impl Read + 'static, name: impl Into<OsString>) -> Self {
        self.add_source_with_format(source, name, InputFormat::Nickel)
    }

    /// Add the given string as an in-memory Nickel source.
    pub fn add_source_string<S: ToOwned>(self, source: S, name: impl Into<OsString>) -> Self
    where
        S::Owned: Into<String>,
    {
        self.add_source_with_format(
            std::io::Cursor::new(source.to_owned().into()),
            name,
            InputFormat::Nickel,
        )
    }

    /// Add an in-memory source as an input, with an explicit format.
    pub fn add_source_with_format(
        mut self,
        source: impl Read + 'static,
        name: impl Into<OsString>,
        format: InputFormat,
    ) -> Self {
        self.inputs.push(BuilderInput::Source {
            source: Box::new(source),
            name: name.into(),
            format,
        });
        self
    }

    /// Add standard input as an input source, with the given format.
    pub fn add_stdin(self, format: InputFormat) -> Self {
        self.add_source_with_format(io::stdin(), "<stdin>", format)
    }

    /// Append overrides to be applied to the program at evaluation time.
    pub fn add_overrides(mut self, overrides: impl IntoIterator<Item = FieldOverride>) -> Self {
        self.overrides.extend(overrides);
        self
    }

    /// Append a contract to be applied to the final program.
    pub fn add_contract(mut self, contract: ProgramContract) -> Self {
        self.contracts.push(contract);
        self
    }

    /// Append contract files to be applied to the final program. The files are read at
    /// [`Self::build`] time.
    pub fn add_contract_paths<I, P>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<OsString>,
    {
        self.contract_paths
            .extend(paths.into_iter().map(Into::into));
        self
    }

    /// Append import paths, searched (in order) when resolving relative imports.
    pub fn add_import_paths<I, P>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        self.import_paths.extend(paths.into_iter().map(Into::into));
        self
    }

    /// Set the field path the program will operate on.
    pub fn with_field_path(mut self, field: FieldPath) -> Self {
        self.field = field;
        self
    }

    /// Set the package map used to resolve package imports.
    pub fn with_package_map(mut self, map: PackageMap) -> Self {
        self.package_map = Some(map);
        self
    }

    /// Replace the trace writer (where output from the `trace` builtin goes).
    pub fn with_trace<W2>(self, trace: W2) -> ProgramBuilder<R, W2>
    where
        W2: Write + 'static,
    {
        ProgramBuilder {
            inputs: self.inputs,
            overrides: self.overrides,
            field: self.field,
            contracts: self.contracts,
            contract_paths: self.contract_paths,
            import_paths: self.import_paths,
            package_map: self.package_map,
            extra_env: self.extra_env,
            #[cfg(feature = "incremental-experimental")]
            enable_incremental_evaluation: self.enable_incremental_evaluation,
            trace,
            reporter: self.reporter,
        }
    }

    /// Replace the warning reporter.
    pub fn with_reporter<R2>(self, reporter: R2) -> ProgramBuilder<R2, W>
    where
        R2: Reporter<(Warning, Files)> + 'static,
    {
        ProgramBuilder {
            inputs: self.inputs,
            overrides: self.overrides,
            field: self.field,
            contracts: self.contracts,
            contract_paths: self.contract_paths,
            import_paths: self.import_paths,
            package_map: self.package_map,
            extra_env: self.extra_env,
            #[cfg(feature = "incremental-experimental")]
            enable_incremental_evaluation: self.enable_incremental_evaluation,
            trace: self.trace,
            reporter,
        }
    }

    /// Layer additional bindings in addition to the standard library in the program's initial
    /// environment.
    ///
    /// Calling this method multiple times is additive: bindings accumulate, with later
    /// calls overriding earlier ones on collision.
    ///
    /// ```
    /// use nickel_lang_core::{
    ///     eval::value::NickelValue,
    ///     identifier::Ident,
    /// };
    ///
    /// let extras: Vec<(Ident, NickelValue)> = vec![
    ///     (Ident::from("hostname"), NickelValue::string_posless("example.com")),
    /// ];
    /// // `extras` can now be passed to ProgramBuilder::extend_initial_env
    /// ```
    pub fn extend_initial_env(mut self, env: Vec<(Ident, NickelValue)>) -> Self {
        self.extra_env.extend(env);
        self
    }

    /// Enable incremental evaluation (experimental). Disabled by default.
    #[cfg(feature = "incremental-experimental")]
    pub fn with_incremental_evaluation(mut self) -> Self {
        self.enable_incremental_evaluation = true;
        self
    }
}

impl<R, W> ProgramBuilder<R, W>
where
    R: Reporter<(Warning, Files)> + 'static,
    W: Write + 'static,
{
    /// Consume the builder and produce a [`Program`].
    ///
    /// Performs all deferred I/O: opens path inputs, registers in-memory sources, and loads
    /// contract files. Returns an error if any of these fail, or if no input has been added.
    pub fn build<EC: EvalCache>(self) -> Result<Program<EC>, BuilderError> {
        increment!("Program::new");

        let ProgramBuilder {
            inputs,
            overrides,
            field,
            mut contracts,
            contract_paths,
            import_paths,
            package_map,
            extra_env,
            #[cfg(feature = "incremental-experimental")]
            enable_incremental_evaluation,
            trace,
            reporter,
        } = self;

        if inputs.is_empty() {
            return Err(BuilderError::NoInputs);
        }

        let mut cache = CacheHub::new();

        let main_id = if inputs.len() == 1 {
            // Single-input fast path: register it directly and use its file id as the main id.
            let input = inputs.into_iter().next().expect("len == 1");
            register_single_input(&mut cache, input)?
        } else {
            // Multi-input: synthesize a `Merge` over imports of each input, then use that as the
            // main id.
            let mut iter = inputs.into_iter();
            let first = iter.next().expect("len > 1");
            let first_term = register_input_as_import(&mut cache, first)?;

            let merge_term = iter.try_fold(
                first_term,
                |acc, input| -> Result<NickelValue, BuilderError> {
                    let term = register_input_as_import(&mut cache, input)?;
                    Ok(mk_term::op2(
                        BinaryOp::Merge(Label::default().into()),
                        acc,
                        term,
                    ))
                },
            )?;

            cache.sources.add_string(
                SourcePath::Generated("main".into()),
                format!("{merge_term}"),
            )
        };

        cache.sources.add_import_paths(import_paths.into_iter());
        if let Some(map) = package_map {
            cache.sources.set_package_map(map);
        }

        for file in contract_paths {
            let file_id = cache
                .sources
                .add_file(file.clone(), InputFormat::Nickel)
                .map_err(|err| BuilderError::Io {
                    path: Some(file.into()),
                    error: err,
                })?;
            contracts.push(ProgramContract::Source(file_id));
        }

        #[allow(unused_mut)]
        let mut vm_ctxt = VmContext::new(cache, trace, reporter).with_extend_env(extra_env);

        #[cfg(feature = "incremental-experimental")]
        if enable_incremental_evaluation {
            vm_ctxt = vm_ctxt.with_incremental_evaluation();
        }

        Ok(Program {
            main_id,
            vm_ctxt,
            overrides,
            field,
            contracts,
        })
    }
}

/// Register a single [`BuilderInput`] in the cache and return its [`FileId`]. Used for the
/// single-input path of [`ProgramBuilder::build`], which doesn't need to synthesize a `Merge`
/// term.
fn register_single_input(
    cache: &mut CacheHub,
    input: BuilderInput,
) -> Result<crate::files::FileId, BuilderError> {
    match input {
        BuilderInput::Path(path) => {
            let format = InputFormat::from_path(&path).unwrap_or_default();
            cache
                .sources
                .add_file(path.clone(), format)
                .map_err(|err| BuilderError::Io {
                    path: Some(path.into()),
                    error: err,
                })
        }
        BuilderInput::Source {
            source,
            name,
            format,
        } => {
            let path = PathBuf::from(name);
            cache
                .sources
                .add_source(SourcePath::Path(path.clone(), format), source)
                .map_err(|err| BuilderError::Io {
                    path: Some(path),
                    error: err,
                })
        }
    }
}

/// Register a [`BuilderInput`] in the cache and return a [`Term::Import`] referring to it. Used
/// for the multi-input path of [`ProgramBuilder::build`].
fn register_input_as_import(
    cache: &mut CacheHub,
    input: BuilderInput,
) -> Result<NickelValue, BuilderError> {
    match input {
        BuilderInput::Path(path) => {
            let format = InputFormat::from_path(&path).unwrap_or_default();
            Ok(NickelValue::from(Term::Import(Import::Path {
                path,
                format,
            })))
        }
        BuilderInput::Source {
            source,
            name,
            format,
        } => {
            let mut import_path = OsString::new();
            // See https://github.com/tweag/nickel/issues/2362 and the documentation of
            // IN_MEMORY_SOURCE_PATH_PREFIX
            import_path.push(IN_MEMORY_SOURCE_PATH_PREFIX);
            import_path.push(&name);

            cache
                .sources
                .add_source(SourcePath::Path(name.clone().into(), format), source)
                .map_err(|err| BuilderError::Io {
                    path: Some(name.into()),
                    error: err,
                })?;
            Ok(NickelValue::from(Term::Import(Import::Path {
                path: import_path,
                format,
            })))
        }
    }
}

#[cfg(feature = "doc")]
mod doc {
    use crate::{
        error::{Error, ExportErrorKind, IOError},
        eval::value::{Container, NickelValue, ValueContentRef},
        position::PosTable,
        term::{Term, record::RecordData},
    };

    use comrak::{Arena, format_commonmark, parse_document};
    use comrak::{
        arena_tree::{Children, NodeEdge},
        nodes::{
            Ast, AstNode, ListDelimType, ListType, NodeCode, NodeHeading, NodeList, NodeValue,
        },
    };

    use serde::{Deserialize, Serialize};

    use std::{collections::HashMap, io::Write};

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
        pub fn extract_from_term(value: &NickelValue) -> Option<Self> {
            match value.content_ref() {
                ValueContentRef::Record(Container::Empty) => Some(Self {
                    fields: HashMap::new(),
                }),
                ValueContentRef::Record(Container::Alloc(record)) => {
                    Self::extract_from_record(record)
                }
                ValueContentRef::Term(Term::RecRecord(data)) => {
                    Self::extract_from_record(&data.record)
                }
                _ => None,
            }
        }

        fn extract_from_record(record: &RecordData) -> Option<Self> {
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
                        .0
                        .as_ref()
                        .and_then(|m| m.annotation.typ.as_ref())
                        .map(|lt| lt.label.typ.to_string());

                    let contracts = field
                        .metadata
                        .0
                        .iter()
                        .flat_map(|m| m.annotation.contracts.iter())
                        .map(|lt| lt.label.typ.to_string())
                        .collect();

                    (
                        ident.label().to_owned(),
                        DocumentationField {
                            fields,
                            typ,
                            contracts,
                            documentation: field.metadata.doc().map(ToOwned::to_owned),
                        },
                    )
                })
                .collect();

            Some(Self { fields })
        }

        pub fn write_json(&self, out: &mut dyn Write) -> Result<(), Error> {
            serde_json::to_writer(out, self).map_err(|e| {
                Error::export_error(PosTable::new(), ExportErrorKind::Other(e.to_string()))
            })
        }

        pub fn write_markdown(&self, out: &mut dyn Write) -> Result<(), Error> {
            // comrak expects a fmt::Write and we have an io::Write, so wrap it.
            // (There's also the fmt2io crate for this, but that's overkill)
            struct IoToFmt<'a>(&'a mut dyn Write);
            impl<'a> std::fmt::Write for IoToFmt<'a> {
                fn write_str(&mut self, s: &str) -> std::fmt::Result {
                    self.0.write_all(s.as_bytes()).map_err(|_| std::fmt::Error)
                }
            }

            let document = ast_node(NodeValue::Document);

            // Our nodes in the Markdown document are owned by this arena
            let arena = Arena::new();

            // The default ComrakOptions disables all extensions (essentially reducing to
            // CommonMark)
            let options = comrak::Options::default();

            self.markdown_append(0, &arena, &document, &options);
            format_commonmark(&document, &options, &mut IoToFmt(out))
                .map_err(|e| Error::IOError(IOError(e.to_string())))?;

            Ok(())
        }

        /// Recursively walk the given `DocOutput`, recursing into fields, looking for
        /// documentation. This documentation is then appended to the provided document.
        fn markdown_append<'a>(
            &'a self,
            header_level: u8,
            arena: &'a Arena<'a>,
            document: &'a AstNode<'a>,
            options: &comrak::Options,
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
        arena: &'a Arena<'a>,
        md: &str,
        options: &comrak::Options,
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
        if let NodeValue::Heading(NodeHeading {
            level,
            setext,
            closed,
        }) = ast.value
        {
            ast.value = NodeValue::Heading(NodeHeading {
                level: header_level + level,
                setext,
                closed,
            });
        }
        ast
    }

    /// Creates a codespan header of the provided string with the provided header level.
    fn mk_header<'a>(ident: &str, header_level: u8, arena: &'a Arena<'a>) -> &'a AstNode<'a> {
        let res = arena.alloc(ast_node(NodeValue::Heading(NodeHeading {
            level: header_level,
            setext: false,
            closed: false,
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
        arena: &'a Arena<'a>,
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
        arena: &'a Arena<'a>,
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
        // We probably shouldn't have to, but after diving into comrak's rendering engine, it seems
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
    use crate::eval::cache::CacheImpl;
    use assert_matches::assert_matches;

    fn build_test_program(s: &str) -> Result<Program<CacheImpl>, BuilderError> {
        ProgramBuilder::new().add_source_string(s, "<test>").build()
    }

    fn eval_full(s: &str) -> Result<NickelValue, Error> {
        let mut p = build_test_program(s)?;
        p.eval_full()
    }

    fn typecheck(s: &str) -> Result<(), Error> {
        let mut p = build_test_program(s)?;
        p.typecheck(TypecheckMode::Walk)
    }

    #[test]
    fn evaluation_full() {
        use crate::{mk_array, mk_record, term::make as mk_term};

        let t = eval_full("[(1 + 1), (\"a\" ++ \"b\"), ([ 1, [1 + 2] ])]").unwrap();

        // [2, "ab", [1, [3]]]
        let expd = mk_array!(
            mk_term::integer(2),
            NickelValue::string_posless("ab"),
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

    #[test]
    fn builder_evaluates_single_source() {
        let mut prog: Program<CacheImpl> = ProgramBuilder::new()
            .add_source_string("1 + 2", "<builder-test>")
            .build()
            .unwrap();
        let v = prog.eval_full().unwrap();
        assert_eq!(v.without_pos(), crate::term::make::integer(3).without_pos());
    }

    #[test]
    fn builder_merges_multiple_sources() {
        let mut prog: Program<CacheImpl> = ProgramBuilder::new()
            .add_source_string("{ a = 1 }", "<a.ncl>")
            .add_source_string("{ b = 2 }", "<b.ncl>")
            .build()
            .unwrap();
        let v = prog.eval_full().unwrap();
        let expected = crate::mk_record!(
            ("a", crate::term::make::integer(1)),
            ("b", crate::term::make::integer(2))
        );
        assert_eq!(v.without_pos(), expected);
    }

    #[test]
    fn builder_rejects_no_inputs() {
        let result: Result<Program<CacheImpl>, _> = ProgramBuilder::new().build();
        match result {
            Err(BuilderError::NoInputs) => {}
            Err(e) => panic!("expected IOError, got {e:?}"),
            Ok(_) => panic!("expected error for empty input set"),
        }
    }

    #[test]
    fn builder_extend_initial_env_basic() {
        let mut prog: Program<CacheImpl> = ProgramBuilder::new()
            .add_source_string("a + 1", "<extras-basic>")
            .extend_initial_env(vec![("a".into(), mk_term::integer(1))])
            .build()
            .unwrap();

        let v = prog.eval().unwrap();
        assert_eq!(v.without_pos(), mk_term::integer(2));
    }

    #[test]
    fn builder_no_extend_unchanged() {
        let mut prog: Program<CacheImpl> = ProgramBuilder::new()
            .add_source_string("a + 1", "<extras-absent>")
            .build()
            .unwrap();

        let err = prog.eval().expect_err("extras path should not leak");
        let msg = format!("{err:?}");
        assert!(
            msg.contains("UnboundIdentifier"),
            "expected an unbound-identifier error, got: {msg}",
        );
    }

    #[test]
    fn builder_extras_with_stdlib() {
        let mut prog: Program<CacheImpl> = ProgramBuilder::new()
            .add_source_string("std.array.length [a, a, a]", "<extras-stdlib>")
            .extend_initial_env(vec![("a".into(), mk_term::integer(7))])
            .build()
            .unwrap();

        let v = prog.eval().unwrap();
        assert_eq!(v.without_pos(), mk_term::integer(3));
    }

    #[test]
    fn builder_typecheck_accepts_extras() {
        // In walk mode, untyped code that references `a` typechecks as long as `a` is bound.
        // Registering `a` as `Dyn` via extras lets the typechecker resolve the identifier.
        let source = "a + 1";
        let mut prog: Program<CacheImpl> = ProgramBuilder::new()
            .add_source_string(source, "<extras-typecheck>")
            .extend_initial_env(vec![("a".into(), mk_term::integer(1))])
            .build()
            .unwrap();

        prog.typecheck(TypecheckMode::Walk)
            .expect("typecheck should resolve `a` from extras as Dyn");
    }

    #[test]
    fn typecheck_unbound_when_extras_absent() {
        let source = "a + 1";
        let mut prog: Program<CacheImpl> = ProgramBuilder::new()
            .add_source_string(source, "<extras-typecheck-absent>")
            .build()
            .unwrap();

        let err = prog
            .typecheck(TypecheckMode::Walk)
            .expect_err("typecheck must fail without extras");
        assert_matches!(err, Error::TypecheckError(_));
    }
}
