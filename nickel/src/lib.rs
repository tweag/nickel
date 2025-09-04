//! A stable library interface to the Nickel language.
//!
//! Most of the Nickel language is implemented in `nickel-lang-core`, but
//! `nickel-lang-core` exposes implementation details that frequently change.
//! This library implements a more basic interface, with fewer features. Its
//! goals are:
//!
//! - Simplicity: it should be easy to do basic things, like evaluating
//!   some Nickel code into a structured representation.
//! - Stability: we aim not to break public APIs.
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
    eval::cache::CacheImpl,
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
    pub fn with_import_paths(self, import_paths: Vec<OsString>) -> Self {
        Context {
            import_paths,
            ..self
        }
    }

    pub fn with_trace(self, trace: Trace) -> Self {
        Context { trace, ..self }
    }

    pub fn with_source_name(self, name: String) -> Self {
        Context {
            name: Some(name),
            ..self
        }
    }

    pub fn eval(&mut self, src: &str) -> Result<Expr, Error> {
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
}

/// An evaluated Nickel expression.
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

    pub fn to_json(&self) -> Result<String, Error> {
        self.export(ExportFormat::Json)
    }

    pub fn to_yaml(&self) -> Result<String, Error> {
        self.export(ExportFormat::Yaml)
    }

    pub fn to_toml(&self) -> Result<String, Error> {
        self.export(ExportFormat::Toml)
    }

    pub fn is_null(&self) -> bool {
        matches!(self.rt.as_ref(), Term::Null)
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self.rt.as_ref() {
            Term::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn is_bool(&self) -> bool {
        self.as_bool().is_some()
    }

    pub fn is_num(&self) -> bool {
        matches!(self.rt.as_ref(), Term::Num(_))
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self.rt.as_ref() {
            Term::Num(n) => n.try_into().ok(),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self.rt.as_ref() {
            Term::Num(n) => Some(f64::rounding_from(n, RoundingMode::Nearest).0),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self.rt.as_ref() {
            Term::Str(s) => Some(s),
            _ => None,
        }
    }

    pub fn is_str(&self) -> bool {
        self.as_str().is_some()
    }

    pub fn as_enum_tag(&self) -> Option<&str> {
        match self.rt.as_ref() {
            Term::Enum(tag) => Some(tag.label()),
            _ => None,
        }
    }

    pub fn is_enum_tag(&self) -> bool {
        self.as_enum_tag().is_some()
    }

    pub fn as_enum_variant(&self) -> Option<(&str, Expr)> {
        match self.rt.as_ref() {
            Term::EnumVariant { tag, arg, attrs: _ } => {
                Some((tag.label(), Expr { rt: arg.clone() }))
            }
            _ => None,
        }
    }

    pub fn is_enum_variant(&self) -> bool {
        matches!(self.rt.as_ref(), Term::EnumVariant { .. })
    }

    pub fn is_record(&self) -> bool {
        self.as_record().is_some()
    }

    pub fn as_record(&self) -> Option<Record<'_>> {
        match self.rt.as_ref() {
            Term::Record(data) => Some(Record { data }),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<Array<'_>> {
        match self.rt.as_ref() {
            Term::Array(array, _) => Some(Array { array }),
            _ => None,
        }
    }

    pub fn is_array(&self) -> bool {
        self.as_array().is_some()
    }
}

impl Record<'_> {
    pub fn len(&self) -> usize {
        self.data.fields.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.fields.is_empty()
    }

    pub fn value_by_name(&self, key: &str) -> Option<Expr> {
        self.data
            .fields
            .get(&Ident::new(key))
            .and_then(|fld| fld.value.as_ref())
            .map(|rt| Expr { rt: rt.clone() })
    }

    pub fn key_value_by_index(&self, idx: usize) -> Option<(&str, Expr)> {
        self.data.fields.get_index(idx).and_then(|(key, fld)| {
            fld.value
                .as_ref()
                .map(|rt| (key.label(), Expr { rt: rt.clone() }))
        })
    }
}

impl Array<'_> {
    pub fn len(&self) -> usize {
        self.array.len()
    }

    pub fn is_empty(&self) -> bool {
        self.array.is_empty()
    }

    pub fn get(&self, idx: usize) -> Option<Expr> {
        self.array.get(idx).map(|rt| Expr { rt: rt.clone() })
    }
}
