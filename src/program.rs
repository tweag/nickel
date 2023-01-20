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
use crate::error::{Error, ParseError, ToDiagnostic};
use crate::eval;
use crate::eval::cache::Cache as EvalCache;
use crate::eval::VirtualMachine;
use crate::identifier::Ident;
use crate::parser::{
    lexer::{Lexer, NormalToken, StringToken, Token},
    utils::mk_span,
};
use crate::term::{record::Field, RichTerm};
use codespan::FileId;
use codespan_reporting::term::termcolor::{Ansi, ColorChoice, StandardStream};
use std::ffi::OsString;
use std::io::{self, Cursor, Read};
use std::result::Result;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ColorOpt {
    Auto,
    Always,
    Never,
}

impl std::str::FromStr for ColorOpt {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "auto" => Ok(Self::Auto),
            "always" => Ok(Self::Always),
            "never" => Ok(Self::Never),
            _ => Err("possible values are 'auto', 'always' or 'never'."),
        }
    }
}

#[derive(Clone, Default)]
pub struct QueryPath(pub Vec<Ident>);

impl QueryPath {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn parse(cache: &mut Cache, input: String) -> Result<Self, ParseError> {
        // The current state of the path parser. We're either expecting an ident (or a quoted
        // ident: a string), a dot after an ident, or we are parsing a string.
        enum State {
            ExpectIdent,
            ExpectDot,
            ParsingStr { buffer: String },
        }

        let mut state = State::ExpectIdent;
        let format_name = "query-path";
        let input_id = cache.add_tmp(format_name, input);

        let s = cache.source(input_id);
        // We piggy back on the Nickel lexer. That way, we always stay in sync with the syntax, we
        // don't duplicate lexing logic, and spinning up a lexer should ideally be pretty fast.
        let lexer = Lexer::new(s);
        let mut idents = Vec::new();

        for token_res in lexer {
            let (left, token, right) = token_res.map_err(|err| {
                ParseError::from_lalrpop::<()>(lalrpop_util::ParseError::from(err), input_id)
            })?;

            match state {
                State::ExpectIdent => match token {
                    Token::Normal(NormalToken::Identifier(id_str)) => {
                        idents.push(Ident::new(id_str));
                        state = State::ExpectDot;
                    }
                    Token::Normal(NormalToken::DoubleQuote) => {
                        state = State::ParsingStr {
                            buffer: String::new(),
                        };
                    }
                    _ => {
                        return Err(ParseError::ExternalFormatError(
                            String::from(format_name),
                            String::from("unexpected token, expected an identifier"),
                            Some(mk_span(input_id, left, right)),
                        ))
                    }
                },
                State::ExpectDot => match token {
                    Token::Normal(NormalToken::Dot) => {
                        state = State::ExpectIdent;
                    }
                    _ => {
                        return Err(ParseError::ExternalFormatError(
                            String::from(format_name),
                            String::from("unexpected token, expected `.` or end of input"),
                            Some(mk_span(input_id, left, right)),
                        ))
                    }
                },
                State::ParsingStr { mut buffer } => match token {
                    Token::Str(StringToken::Literal(s))
                    | Token::Str(StringToken::EscapedAscii(s)) => {
                        buffer.push_str(s);
                        state = State::ParsingStr { buffer }
                    }
                    Token::Str(StringToken::EscapedChar(c)) => {
                        buffer.push(c);
                        state = State::ParsingStr { buffer }
                    }
                    Token::Str(StringToken::DoubleQuote) => {
                        idents.push(Ident::new(buffer));
                        state = State::ExpectDot;
                    }
                    _ => {
                        return Err(ParseError::ExternalFormatError(
                            String::from(format_name),
                            String::from("unexpected token while parsing a quoted identifier"),
                            Some(mk_span(input_id, left, right)),
                        ))
                    }
                },
            }
        }

        match state {
            State::ExpectDot => Ok(QueryPath(idents)),
            State::ExpectIdent => Err(ParseError::UnexpectedEOF(
                input_id,
                vec![String::from("<identifier>")],
            )),
            State::ParsingStr { .. } => Err(ParseError::UnexpectedEOF(
                input_id,
                vec![String::from("\"")],
            )),
        }
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
    pub fn new_from_stdin() -> std::io::Result<Self> {
        Program::new_from_source(io::stdin(), "<stdin>")
    }

    pub fn new_from_file(path: impl Into<OsString>) -> std::io::Result<Self> {
        let mut cache = Cache::new(ErrorTolerance::Strict);
        let main_id = cache.add_file(path)?;
        let vm = VirtualMachine::new(cache);

        Ok(Self {
            main_id,
            vm,
            color_opt: ColorOpt::Auto,
        })
    }

    /// Create a program by reading it from a generic source.
    pub fn new_from_source<T, S>(source: T, source_name: S) -> std::io::Result<Self>
    where
        T: Read,
        S: Into<OsString> + Clone,
    {
        let mut cache = Cache::new(ErrorTolerance::Strict);
        let main_id = cache.add_source(source_name, source)?;
        let vm = VirtualMachine::new(cache);

        Ok(Self {
            main_id,
            vm,
            color_opt: ColorOpt::Auto,
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

    /// Same as `eval_full`, but does not substitute all variables.
    pub fn eval_deep(&mut self) -> Result<RichTerm, Error> {
        let (t, initial_env) = self.prepare_eval()?;
        self.vm.reset();
        self.vm.eval_deep(t, &initial_env).map_err(|e| e.into())
    }

    /// Wrapper for [`query`].
    pub fn query(&mut self, path: Option<String>) -> Result<Field, Error> {
        let initial_env = self.vm.prepare_stdlib()?;
        let query_path = path
            .map(|p| QueryPath::parse(self.vm.import_resolver_mut(), p))
            .transpose()?
            .unwrap_or_default();
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
        E: ToDiagnostic<FileId>,
    {
        report(self.vm.import_resolver_mut(), error, self.color_opt)
    }

    /// Build an error report as a string and return it.
    pub fn report_as_str<E>(&mut self, error: E) -> String
    where
        E: ToDiagnostic<FileId>,
    {
        let cache = self.vm.import_resolver_mut();
        let contracts_id = cache.id_of("<stdlib/contract.ncl>");
        let diagnostics = error.to_diagnostic(cache.files_mut(), contracts_id);
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

    /// Create a markdown file with documentation for the specified program in `.nickel/doc/program_main_file_name.md`
    #[cfg(feature = "doc")]
    pub fn output_doc(&mut self, out: &mut dyn std::io::Write) -> Result<(), Error> {
        doc::output_doc(self.vm.import_resolver_mut(), self.main_id, out)
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
    E: ToDiagnostic<FileId>,
{
    let writer = StandardStream::stderr(color_opt.into());
    let config = codespan_reporting::term::Config::default();
    let contracts_id = cache.id_of("<stdlib/contract.ncl>");
    let diagnostics = error.to_diagnostic(cache.files_mut(), contracts_id);

    let result = diagnostics.iter().try_for_each(|d| {
        codespan_reporting::term::emit(&mut writer.lock(), &config, cache.files_mut(), d)
    });
    match result {
        Ok(()) => (),
        Err(err) => panic!(
            "Program::report: could not print an error on stderr: {}",
            err
        ),
    };
}

impl From<ColorOpt> for ColorChoice {
    fn from(c: ColorOpt) -> Self {
        match c {
            ColorOpt::Auto => ColorChoice::Auto,
            ColorOpt::Always => ColorChoice::Always,
            ColorOpt::Never => ColorChoice::Never,
        }
    }
}

#[cfg(feature = "doc")]
mod doc {
    use crate::cache::Cache;
    use crate::error::{Error, IOError};
    use crate::term::{RichTerm, Term};
    use codespan::FileId;
    use comrak::arena_tree::NodeEdge;
    use comrak::nodes::{Ast, AstNode, NodeCode, NodeHeading, NodeValue};
    use comrak::{format_commonmark, parse_document, Arena, ComrakOptions};
    use std::io::Write;

    /// Create a markdown file with documentation for the specified FileId.
    pub fn output_doc(
        cache: &mut Cache,
        file_id: FileId,
        out: &mut dyn Write,
    ) -> Result<(), Error> {
        cache.parse(file_id)?;
        // unwrap(): at this point the term was correctly parsed and should exist in cache
        let term = cache.get_ref(file_id).unwrap();
        let document = AstNode::from(NodeValue::Document);

        // Our nodes in the Markdown document are owned by this arena
        let arena = Arena::new();

        // The default ComrakOptions disables all extensions (essentially reducing to CommonMark)
        let options = ComrakOptions::default();

        to_markdown(term, 0, &arena, &document, &options)?;
        format_commonmark(&document, &options, out)
            .map_err(|e| Error::IOError(IOError(e.to_string())))?;

        Ok(())
    }

    /// Recursively walk the given richterm, recursing into fields of record, looking for documentation.
    /// This documentation is then added to the provided document.
    fn to_markdown<'a>(
        rt: &'a RichTerm,
        header_level: u32,
        arena: &'a Arena<AstNode<'a>>,
        document: &'a AstNode<'a>,
        options: &ComrakOptions,
    ) -> Result<(), Error> {
        match rt.term.as_ref() {
            Term::Record(record) | Term::RecRecord(record, _, _) => {
                // Sorting fields for a deterministic output
                let mut entries: Vec<(_, _)> = record.fields.iter().collect();
                entries.sort_by_key(|(k, _)| *k);

                for (ident, field) in entries {
                    let header = mk_header(ident.label(), header_level + 1, arena);
                    document.append(header);

                    if let Some(ref doc) = field.metadata.doc {
                        document.append(parse_documentation(header_level, arena, doc, options));
                    }

                    if let Some(ref value) = field.value {
                        to_markdown(value, header_level + 1, arena, document, options)?;
                    }
                }
            }
            _ => (),
        }
        Ok(())
    }

    /// Parses a string into markdown and increases any headers in the markdown by the specified level.
    /// This allows having headers in documentation without clashing with the structure of the document.
    fn parse_documentation<'a>(
        header_level: u32,
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

    fn increase_header_level(header_level: u32, ast: &mut Ast) -> &Ast {
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
        header_level: u32,
        arena: &'a Arena<AstNode<'a>>,
    ) -> &'a AstNode<'a> {
        let res = arena.alloc(AstNode::from(NodeValue::Heading(NodeHeading {
            level: header_level,
            setext: false,
        })));

        let code = arena.alloc(AstNode::from(NodeValue::Code(NodeCode {
            num_backticks: 1,
            literal: ident.bytes().collect(),
        })));

        res.append(code);

        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::EvalError;
    use crate::eval::cache::CBNCache;
    use crate::position::TermPos;
    use crate::term::array::ArrayAttrs;
    use assert_matches::assert_matches;
    use std::io::Cursor;

    type EC = CBNCache;

    fn eval_full(s: &str) -> Result<RichTerm, Error> {
        let src = Cursor::new(s);

        let mut p: Program<EC> = Program::new_from_source(src, "<test>").map_err(|io_err| {
            Error::EvalError(EvalError::Other(
                format!("IO error: {}", io_err),
                TermPos::None,
            ))
        })?;
        p.eval_full()
    }

    fn typecheck(s: &str) -> Result<(), Error> {
        let src = Cursor::new(s);

        let mut p: Program<EC> = Program::new_from_source(src, "<test>").map_err(|io_err| {
            Error::EvalError(EvalError::Other(
                format!("IO error: {}", io_err),
                TermPos::None,
            ))
        })?;
        p.typecheck()
    }

    #[test]
    fn evaluation_full() {
        use crate::{mk_array, mk_record, term::Term};

        let t = eval_full("[(1 + 1), (\"a\" ++ \"b\"), ([ 1, [1 + 2] ])]").unwrap();

        // [2, "ab", [1, [3]]]
        let expd = mk_array!(
            Term::Num(2_f64),
            Term::Str(String::from("ab")),
            mk_array!(
                Term::Num(1_f64),
                mk_array!(Term::Num(3_f64); ArrayAttrs::new().closurized());
                ArrayAttrs::new().closurized()
            );
            ArrayAttrs::new().closurized()
        );

        assert_eq!(t.without_pos(), expd.without_pos());

        let t = eval_full("let x = 1 in let y = 1 + x in let z = {foo.bar.baz = y} in z").unwrap();
        // Records are parsed as RecRecords, so we need to build one by hand
        let expd = mk_record!((
            "foo",
            mk_record!(("bar", mk_record!(("baz", Term::Num(2.0)))))
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
