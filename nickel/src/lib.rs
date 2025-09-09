//! A stable library interface to the Nickel language.
//!
//! Most of the Nickel language is implemented in `nickel-lang-core`, but
//! `nickel-lang-core` exposes implementation details that frequently change.
//! This library implements a more basic interface, with fewer features. Its
//! goals are:
//!
//! - Simplicity: it should be easy to do basic things, like evaluating
//!   some Nickel code into a structured representation.
//! - Stability: we aim not to break public APIs. This doesn't mean
//!   we'll *never* break them, just that stability is a priority.
//! - Embeddability: our interfaces are designed to be usable from
//!   other languages.

use std::{
    cell::RefCell,
    ffi::OsString,
    io::{Cursor, Write},
    rc::Rc,
};

use codespan_reporting::term::termcolor::{Ansi, NoColor, WriteColor};
use malachite::base::{num::conversion::traits::RoundingFrom, rounding_modes::RoundingMode};

use nickel_lang_core::{
    error::{report::DiagnosticsWrapper, IntoDiagnostics, NullReporter},
    eval::{cache::CacheImpl, Closure, Environment},
    files::Files,
    identifier::Ident,
    program::Program,
    serialize::{to_string, validate, ExportFormat},
    term::{self, record::RecordData, RichTerm, Term},
};

pub use nickel_lang_core::error::report::ErrorFormat;

/// Provides a destination for the output of `std.trace`.
#[derive(Clone)]
pub struct Trace {
    // This is a little annoying, in that VirtualMachine already takes a generic
    // trace and boxes it. Since we don't want a generic parameter on Context,
    // it means we need to double-box it.
    inner: Rc<RefCell<dyn Write>>,
}

impl Trace {
    pub fn new<W: Write + 'static>(write: W) -> Self {
        Trace {
            inner: Rc::new(RefCell::new(write)),
        }
    }
}

impl Write for Trace {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.inner.borrow_mut().write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.borrow_mut().flush()
    }
}

impl Default for Trace {
    fn default() -> Trace {
        Trace::new(std::io::sink())
    }
}

/// The main entry point.
#[derive(Clone, Default)]
pub struct Context {
    import_paths: Vec<OsString>,
    trace: Trace,
    name: Option<String>,
}

/// Controls the usage of ANSI escape codes in error messages.
pub enum Color {
    Ansi,
    Off,
}

/// A Nickel evaluation error.
pub struct Error {
    error: Box<nickel_lang_core::error::Error>,
    files: nickel_lang_core::files::Files,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl Error {
    pub fn format<W: Write>(
        mut self,
        write: &mut W,
        format: ErrorFormat,
        color: Color,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let diagnostics = DiagnosticsWrapper::from(self.error.into_diagnostics(&mut self.files));
        match format {
            ErrorFormat::Text => {
                let mut ansi;
                let mut no_color;
                let writer: &mut dyn WriteColor = match color {
                    Color::Ansi => {
                        ansi = Ansi::new(write);
                        &mut ansi
                    }
                    Color::Off => {
                        no_color = NoColor::new(write);
                        &mut no_color
                    }
                };
                diagnostics.diagnostics.iter().try_for_each(|d| {
                    Ok(codespan_reporting::term::emit(
                        writer,
                        &codespan_reporting::term::Config::default(),
                        &self.files,
                        d,
                    )?)
                })
            }
            ErrorFormat::Json => Ok(serde_json::to_writer(write, &diagnostics)?),
            ErrorFormat::Yaml => Ok(serde_yaml::to_writer(write, &diagnostics)?),
            ErrorFormat::Toml => Ok(write.write_all(toml::to_string(&diagnostics)?.as_bytes())?),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error {
            error: Box::new(nickel_lang_core::error::Error::IOError(e.into())),
            files: Files::new(),
        }
    }
}

impl From<nickel_lang_core::error::ExportError> for Error {
    fn from(e: nickel_lang_core::error::ExportError) -> Self {
        Error {
            error: Box::new(e.into()),
            files: Files::new(),
        }
    }
}

impl Context {
    /// Set the interpreter's search path for imports.
    ///
    /// When importing a file, Nickel searches for it relative to the file doing the
    /// import. If not found, it will search in the paths provided here.
    ///
    /// Note that unlike the Nickel command line program, the embedded Nickel
    /// interpreter does not automatically honor the `NICKEL_IMPORT_PATH`
    /// environment variable. If you want to support `NICKEL_IMPORT_PATH`, you
    /// should read the environment variable yourself and pass the resulting
    /// paths to this method.
    pub fn with_import_paths(self, import_paths: Vec<OsString>) -> Self {
        Context {
            import_paths,
            ..self
        }
    }

    /// Provide a destination for the output of `std.trace`.
    ///
    /// If you don't provide a destination, `std.trace` will have
    /// no effect.
    pub fn with_trace(self, trace: Trace) -> Self {
        Context { trace, ..self }
    }

    /// Provide a name for the main input program.
    ///
    /// This is used to format error messages. If you read the main input
    /// program from a file, its path is a good choice.
    pub fn with_source_name(self, name: String) -> Self {
        Context {
            name: Some(name),
            ..self
        }
    }

    /// Evaluate a Nickel program deeply, returning the resulting expression.
    pub fn eval_deep(&mut self, src: &str) -> Result<Expr, Error> {
        let mut program: Program<CacheImpl> = Program::new_from_source(
            Cursor::new(src),
            self.name.as_deref().unwrap_or("<source>"),
            self.trace.clone(),
            NullReporter {},
        )?;
        program.add_import_paths(self.import_paths.iter());
        let rt = program.eval_full().map_err(|error| Error {
            error: Box::new(error),
            files: program.files(),
        })?;

        Ok(Expr { rt })
    }

    /// Evaluate a Nickel program deeply, returning the resulting expression.
    ///
    /// This differs from [`eval`][Self::eval] in that it ignores fields marked
    /// as `not_exported`.
    pub fn eval_deep_for_export(&mut self, src: &str) -> Result<Expr, Error> {
        let mut program: Program<CacheImpl> = Program::new_from_source(
            Cursor::new(src),
            self.name.as_deref().unwrap_or("<source>"),
            self.trace.clone(),
            NullReporter {},
        )?;
        program.add_import_paths(self.import_paths.iter());
        let rt = program.eval_full_for_export().map_err(|error| Error {
            error: Box::new(error),
            files: program.files(),
        })?;

        Ok(Expr { rt })
    }

    /// Evaluate a Nickel program to weak head normal form (WHNF).
    ///
    /// The result of this evaluation is a null, bool, number, string,
    /// enum, record, or array. In case it's a record, array, or enum
    /// variant, the payload (record values, array elements, or enum
    /// payloads) will be left unevaluated.
    pub fn eval_shallow(&mut self, src: &str) -> Result<(VirtualMachine, Expr), Error> {
        let mut program: Program<CacheImpl> = Program::new_from_source(
            Cursor::new(src),
            self.name.as_deref().unwrap_or("<source>"),
            self.trace.clone(),
            NullReporter {},
        )?;
        program.add_import_paths(self.import_paths.iter());
        let rt = program.eval().map_err(|error| Error {
            error: Box::new(error),
            files: program.files(),
        })?;

        Ok((VirtualMachine { program }, Expr { rt }))
    }
}

pub struct VirtualMachine {
    program: Program<CacheImpl>,
}

impl VirtualMachine {
    /// Evaluate an expression to weak head normal form (WHNF).
    ///
    /// This has an effect only if the expression is a thunk (see
    /// [`Expr::is_thunk`]).
    ///
    /// The result of this evaluation is a null, bool, number, string,
    /// enum, record, or array. In case it's a record, array, or enum
    /// variant, the payload (record values, array elements, or enum
    /// payloads) will be left unevaluated.
    pub fn eval_shallow(&mut self, expr: Expr) -> Result<Expr, Error> {
        // The term should already be closurized because there's no
        // public API for making an `Expr` with an unclosurized term.
        // But Program::eval_closure wants an explicit Closure.
        let clos = Closure {
            body: expr.rt,
            env: Environment::new(),
        };

        let rt = self.program.eval_closure(clos).map_err(|error| Error {
            error: Box::new(error.into()),
            files: self.program.files(),
        })?;

        Ok(Expr { rt })
    }
}

/// A Nickel expression.
///
/// This might be fully evaluated (for example, if you got it from [`Context::eval_full`])
/// but might have unevaluated thunks (if you got it from [`Context::eval_shallow`]).
#[derive(Clone)]
pub struct Expr {
    rt: RichTerm,
}

/// A Nickel record.
///
/// Fields can be accessed either by name or by index.
pub struct Record<'a> {
    data: &'a RecordData,
}

/// A Nickel array.
pub struct Array<'a> {
    array: &'a term::array::Array,
}

impl Expr {
    fn export(&self, format: ExportFormat) -> Result<String, Error> {
        validate(format, &self.rt)?;
        Ok(to_string(format, &self.rt)?)
    }

    /// Convert this expression to JSON.
    ///
    /// This is fallible because enum variants have no canonical conversion to
    /// JSON: if the expression contains any enum variants, this will fail.
    pub fn to_json(&self) -> Result<String, Error> {
        self.export(ExportFormat::Json)
    }

    /// Convert this expression to YAML.
    ///
    /// This is fallible because enum variants have no canonical conversion to
    /// YAML: if the expression contains any enum variants, this will fail.
    pub fn to_yaml(&self) -> Result<String, Error> {
        self.export(ExportFormat::Yaml)
    }

    /// Convert this expression to TOML.
    ///
    /// This is fallible because enum variants have no canonical conversion to
    /// TOML: if the expression contains any enum variants, this will fail.
    pub fn to_toml(&self) -> Result<String, Error> {
        self.export(ExportFormat::Toml)
    }

    /// Is this expression the "null" value?
    pub fn is_null(&self) -> bool {
        matches!(self.rt.as_ref(), Term::Null)
    }

    /// Get the boolean value of this expression, if it is a boolean.
    pub fn as_bool(&self) -> Option<bool> {
        match self.rt.as_ref() {
            Term::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Is this expression a boolean?
    pub fn is_bool(&self) -> bool {
        self.as_bool().is_some()
    }

    /// Is this expression a number?
    pub fn is_num(&self) -> bool {
        matches!(self.rt.as_ref(), Term::Num(_))
    }

    /// If this expression is an integer within the range of an `i64`, return it.
    pub fn as_i64(&self) -> Option<i64> {
        match self.rt.as_ref() {
            Term::Num(n) => n.try_into().ok(),
            _ => None,
        }
    }

    /// If this expression is a number, round it to an `f64` and return it.
    pub fn as_f64(&self) -> Option<f64> {
        match self.rt.as_ref() {
            Term::Num(n) => Some(f64::rounding_from(n, RoundingMode::Nearest).0),
            _ => None,
        }
    }

    /// If this expression is a string, return it.
    pub fn as_str(&self) -> Option<&str> {
        match self.rt.as_ref() {
            Term::Str(s) => Some(s),
            _ => None,
        }
    }

    /// Is this expression a string?
    pub fn is_str(&self) -> bool {
        self.as_str().is_some()
    }

    /// If this expression is an enum tag, return it.
    pub fn as_enum_tag(&self) -> Option<&str> {
        match self.rt.as_ref() {
            Term::Enum(tag) => Some(tag.label()),
            _ => None,
        }
    }

    /// Is this expression an enum tag?
    pub fn is_enum_tag(&self) -> bool {
        self.as_enum_tag().is_some()
    }

    /// If this expression is an enum variant, return its tag and its payload.
    pub fn as_enum_variant(&self) -> Option<(&str, Expr)> {
        match self.rt.as_ref() {
            Term::EnumVariant { tag, arg, attrs: _ } => {
                Some((tag.label(), Expr { rt: arg.clone() }))
            }
            _ => None,
        }
    }

    /// Is this expression an enum variant?
    pub fn is_enum_variant(&self) -> bool {
        matches!(self.rt.as_ref(), Term::EnumVariant { .. })
    }

    /// Is this expression a record?
    pub fn is_record(&self) -> bool {
        self.as_record().is_some()
    }

    /// If this expression is a record, return a handle to it.
    ///
    /// The returned record handle can be used to access the
    /// record's elements.
    pub fn as_record(&self) -> Option<Record<'_>> {
        match self.rt.as_ref() {
            Term::Record(data) => Some(Record { data }),
            _ => None,
        }
    }

    /// If this expression is an array, return a handle to it.
    ///
    /// The returned array handle can be used to access the
    /// array's elements.
    pub fn as_array(&self) -> Option<Array<'_>> {
        match self.rt.as_ref() {
            Term::Array(array, _) => Some(Array { array }),
            _ => None,
        }
    }

    /// Is this expression an array?
    pub fn is_array(&self) -> bool {
        self.as_array().is_some()
    }

    pub fn is_thunk(&self) -> bool {
        !self.rt.as_ref().is_eff_whnf()
    }
}

impl Record<'_> {
    /// Returns the number of key/value pairs in this record.
    pub fn len(&self) -> usize {
        self.data.fields.len()
    }

    /// Is this record empty?
    pub fn is_empty(&self) -> bool {
        self.data.fields.is_empty()
    }

    /// If this field name is present in the record, return the field value.
    pub fn value_by_name(&self, key: &str) -> Option<Expr> {
        self.data
            .fields
            .get(&Ident::new(key))
            .and_then(|fld| fld.value.as_ref())
            .map(|rt| Expr { rt: rt.clone() })
    }

    /// Return the field name and value at the given index.
    ///
    /// If this record was fully evaluated, every defined field will have a value
    /// (i.e. the `Option<Expr>` returned here will never be `None`). However,
    /// partially evaluated records may have fields with no value.
    pub fn key_value_by_index(&self, idx: usize) -> Option<(&str, Option<Expr>)> {
        self.data.fields.get_index(idx).map(|(key, fld)| {
            (
                key.label(),
                fld.value.as_ref().map(|rt| Expr { rt: rt.clone() }),
            )
        })
    }
}

impl Array<'_> {
    /// Returns the number of elements in this array.
    pub fn len(&self) -> usize {
        self.array.len()
    }

    /// Is this array empty?
    pub fn is_empty(&self) -> bool {
        self.array.is_empty()
    }

    /// Returns the element at the requested index, if the index is in-bounds.
    pub fn get(&self, idx: usize) -> Option<Expr> {
        self.array.get(idx).map(|rt| Expr { rt: rt.clone() })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_inspection() {
        let expr = Context::default()
            .eval_deep("{ foo = [1, 2], bar = \"hi\" }")
            .unwrap();

        assert!(expr.is_record());
        let rec = expr.as_record().unwrap();
        assert_eq!(2, rec.len());
        assert_eq!(Some("hi"), rec.value_by_name("bar").unwrap().as_str());

        let foo = rec.value_by_name("foo").unwrap();
        assert!(foo.is_array());
        let arr = foo.as_array().unwrap();
        assert_eq!(2, arr.len());
        assert_eq!(Some(1), arr.get(0).unwrap().as_i64());
    }

    #[test]
    fn lazy_inspection() {
        let (mut vm, expr) = Context::default()
            .eval_shallow("{ foo = [1, 2 + 3], bar = \"hi\", baz = 'Tag (1 + 1) }")
            .unwrap();

        assert!(expr.is_record());
        let rec = expr.as_record().unwrap();
        assert_eq!(3, rec.len());

        let foo = rec.value_by_name("foo").unwrap();
        assert!(foo.is_thunk());
        let foo = vm.eval_shallow(foo).unwrap();
        assert!(foo.is_array());
        let arr = foo.as_array().unwrap();
        assert_eq!(2, arr.len());

        let elt = arr.get(0).unwrap();
        assert_eq!(Some(1), elt.as_i64());

        let elt = arr.get(1).unwrap();
        assert!(elt.is_thunk());
        assert_eq!(Some(5), vm.eval_shallow(elt).unwrap().as_i64());

        let baz = rec.value_by_name("baz").unwrap();
        assert!(baz.is_thunk());
        let baz = vm.eval_shallow(baz).unwrap();
        assert!(baz.is_enum_variant());
        let (tag, inner) = baz.as_enum_variant().unwrap();
        assert_eq!(tag, "Tag");
        assert!(inner.is_thunk());
        assert_eq!(Some(2), vm.eval_shallow(inner).unwrap().as_i64());
    }
}
