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
    bytecode::value::{self, NickelValue},
    deserialize::RustDeserializationError as DeserializationError,
    error::{IntoDiagnostics, NullReporter, report::DiagnosticsWrapper},
    eval::{Closure, Environment, cache::CacheImpl},
    files::Files,
    identifier::{Ident, LocIdent},
    program::Program,
    serialize::{ExportFormat, to_string, validate},
    term::{
        self,
        record::{Field, RecordData},
    },
};

/// Available export formats for error diagnostics.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub enum ErrorFormat {
    /// Text with ANSI color codes.
    #[default]
    AnsiText,
    /// Text without ANSI color codes.
    Text,
    Json,
    Yaml,
    Toml,
}

#[cfg(feature = "capi")]
pub mod capi;

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
#[derive(Clone)]
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
        &self,
        write: &mut W,
        format: ErrorFormat,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut err = self.clone();
        let diagnostics = DiagnosticsWrapper::from(err.error.into_diagnostics(&mut err.files));
        match format {
            ErrorFormat::Text | ErrorFormat::AnsiText => {
                let mut ansi;
                let mut no_color;
                let writer: &mut dyn WriteColor = if format == ErrorFormat::AnsiText {
                    ansi = Ansi::new(write);
                    &mut ansi
                } else {
                    no_color = NoColor::new(write);
                    &mut no_color
                };
                let config = codespan_reporting::term::Config::default();
                diagnostics.diagnostics.iter().try_for_each(|d| {
                    Ok(codespan_reporting::term::emit(
                        writer, &config, &err.files, d,
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
    ///
    /// "Deeply" means that we recursively evaluate records and arrays. For
    /// an alternative, see [`eval_shallow`][Self::eval_shallow].
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

        Ok(Expr { value: rt })
    }

    /// Evaluate a Nickel program deeply, returning the resulting expression.
    ///
    /// This differs from [`eval_deep`][Self::eval_deep] in that it ignores fields marked
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

        Ok(Expr { value: rt })
    }

    /// Evaluate a Nickel program to weak head normal form (WHNF).
    ///
    /// The result of this evaluation is a null, bool, number, string,
    /// enum, record, or array. In case it's a record, array, or enum
    /// variant, the payload (record values, array elements, or enum
    /// payloads) will be left unevaluated.
    ///
    /// Together with the expression, this returns a Nickel virtual machine that
    /// can be used to further evaluate unevaluated sub-expressions.
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

        Ok((VirtualMachine { program }, Expr { value: rt }))
    }
}

/// A Nickel virtual machine.
///
/// This can be used to further evaluate unevaluated subexpressions (thunks).
/// You can create one of these with [`Context::eval_shallow`].
pub struct VirtualMachine {
    program: Program<CacheImpl>,
}

impl VirtualMachine {
    /// Evaluate an expression to weak head normal form (WHNF).
    ///
    /// This has no effect if the expression is already evaluated (see
    /// [`Expr::is_value`]).
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
            value: expr.value,
            env: Environment::new(),
        };

        let value = self.program.eval_closure(clos).map_err(|error| Error {
            error: Box::new(error.into()),
            files: self.program.files(),
        })?;

        Ok(Expr { value })
    }
}

/// A Nickel expression.
///
/// This might be fully evaluated (for example, if you got it from [`Context::eval_deep`])
/// or might have unevaluated sub-expressions (if you got it from [`Context::eval_shallow`]).
#[derive(Clone)]
pub struct Expr {
    value: NickelValue,
}

/// A Nickel record.
///
/// Fields can be accessed either by name or by index.
///
/// This is a reference internally, and borrows from data owned by an [`Expr`].
#[derive(Clone)]
pub struct Record<'a> {
    data: &'a RecordData,
}

/// An iterator over names and values in a [`Record`].
pub struct RecordIter<'a> {
    inner: indexmap::map::Iter<'a, LocIdent, Field>,
}

/// A Nickel array.
///
/// This is a reference internally, and borrows from data owned by an [`Expr`].
#[derive(Clone)]
pub struct Array<'a> {
    array: &'a value::Array,
}

/// An iterator over elements in an [`Array`].
pub struct ArrayIter<'a> {
    inner: <&'a value::Array as IntoIterator>::IntoIter,
}

/// A Nickel number.
///
/// This is a reference internally, and borrows from data owned by an [`Expr`].
#[derive(Clone)]
pub struct Number<'a> {
    num: &'a term::Number,
}

/// Metadata attached to a record field.
///
/// This is a reference internally, and borrows from data owned by an [`Expr`].
#[derive(Clone)]
pub struct FieldMetadata<'a> {
    inner: &'a nickel_lang_core::term::record::FieldMetadata,
}

/// A merge priority for a Nickel field.
pub enum MergePriority<'a> {
    /// The lowest priority, corresponding to a `default` annotation in Nickel syntax.
    Default,
    /// A numeric priority value, corresponding to a `priority n` annotation in Nickel syntax.
    Priority(Number<'a>),
    /// The lowest priority, corresponding to a `force` annotation in Nickel syntax.
    Force,
}

impl Expr {
    fn export(&self, format: ExportFormat) -> Result<String, Error> {
        validate(format, &self.value)?;
        Ok(to_string(format, &self.value)?)
    }

    /// Convert this expression to JSON.
    ///
    /// This is fallible because enum variants have no canonical conversion to
    /// JSON: if the expression contains any enum variants, this will fail.
    /// This also fails if the expression contains any unevaluated sub-expressions.
    pub fn to_json(&self) -> Result<String, Error> {
        self.export(ExportFormat::Json)
    }

    /// Convert this expression to YAML.
    ///
    /// This is fallible because enum variants have no canonical conversion to
    /// YAML: if the expression contains any enum variants, this will fail.
    /// This also fails if the expression contains any unevaluated sub-expressions.
    pub fn to_yaml(&self) -> Result<String, Error> {
        self.export(ExportFormat::Yaml)
    }

    /// Convert this expression to TOML.
    ///
    /// This is fallible because enum variants have no canonical conversion to
    /// TOML: if the expression contains any enum variants, this will fail.
    /// This also fails if the expression contains any unevaluated sub-expressions.
    pub fn to_toml(&self) -> Result<String, Error> {
        self.export(ExportFormat::Toml)
    }

    /// Is this expression the "null" value?
    pub fn is_null(&self) -> bool {
        self.value.is_null()
    }

    /// Get the boolean value of this expression, if it is a boolean.
    pub fn as_bool(&self) -> Option<bool> {
        self.value.as_bool()
    }

    /// Is this expression a boolean?
    pub fn is_bool(&self) -> bool {
        self.as_bool().is_some()
    }

    /// Is this expression a number?
    pub fn is_number(&self) -> bool {
        self.as_number().is_some()
    }

    /// Get the number value of this expression, if it is a number.
    pub fn as_number(&self) -> Option<Number<'_>> {
        self.value.as_number().map(|num| Number { num: &num.0 })
    }

    /// If this expression is an integer within the range of an `i64`, return it.
    pub fn as_i64(&self) -> Option<i64> {
        self.as_number().and_then(|n| n.as_i64())
    }

    /// If this expression is a number, round it to an `f64` and return it.
    pub fn as_f64(&self) -> Option<f64> {
        self.as_number().map(|n| n.as_f64())
    }

    /// If this expression is a string, return it.
    pub fn as_str(&self) -> Option<&str> {
        self.value.as_string().map(|s| s.0.as_str())
    }

    /// Is this expression a string?
    pub fn is_str(&self) -> bool {
        self.as_str().is_some()
    }

    /// If this expression is an enum tag, return it.
    pub fn as_enum_tag(&self) -> Option<&str> {
        self.value.as_enum_tag().map(|tag| tag.label())
    }

    /// Is this expression an enum tag?
    pub fn is_enum_tag(&self) -> bool {
        self.as_enum_tag().is_some()
    }

    /// If this expression is an enum variant, return its tag and its payload.
    pub fn as_enum_variant(&self) -> Option<(&str, Expr)> {
        self.value.as_enum_variant().and_then(|enum_var| {
            Some((
                enum_var.tag.label(),
                Expr {
                    value: enum_var.arg.as_ref()?.clone(),
                },
            ))
        })
    }

    /// Is this expression an enum variant?
    pub fn is_enum_variant(&self) -> bool {
        self.as_enum_variant().is_some()
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
        self.value
            .as_record()
            .map(|record| Record { data: &record.0 })
    }

    /// If this expression is an array, return a handle to it.
    ///
    /// The returned array handle can be used to access the
    /// array's elements.
    pub fn as_array(&self) -> Option<Array<'_>> {
        self.value.as_array().map(|array| Array {
            array: &array.array,
        })
    }

    /// Is this expression an array?
    pub fn is_array(&self) -> bool {
        self.as_array().is_some()
    }

    /// Has this expression been evaluated?
    ///
    /// An evaluated expression is either null, or it's a number, bool, string,
    /// record, array, or enum. If this expression is not a value, you probably
    /// got it from looking inside the result of [`Context::eval_shallow`],
    /// and you can use the [`VirtualMachine`] you got from `eval_shallow` to
    /// evaluate this expression further.
    pub fn is_value(&self) -> bool {
        self.value.is_whnf()
    }

    /// Converts this expression into any type that implements `serde::Deserialize`.
    ///
    /// This expression should be fully evaluated, or the conversion will fail. The
    /// conversion might also fail if the expression doesn't match the expected shape
    /// of the deserializable type.
    ///
    /// Unlike Nickel's `std.serialize` function, this conversion *does* support
    /// Nickel enum variants.
    ///
    /// # Example
    ///
    /// ```rust
    /// #[derive(serde::Deserialize)]
    /// struct Point { x: f64, y: f64 }
    ///
    /// let mut context = nickel_lang::Context::default();
    /// let p: Point = context.eval_deep("{ x = 1, y = 2.3 }").unwrap().to_serde().unwrap();
    /// assert_eq!(p.x, 1.0);
    /// ```
    pub fn to_serde<'de, T: serde::Deserialize<'de>>(&self) -> Result<T, DeserializationError> {
        T::deserialize(self.value.clone())
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
            .map(|rt| Expr { value: rt.clone() })
    }

    /// Return the field name and value at the given index.
    ///
    /// If this record was deeply evaluated, every defined field will have a value
    /// (i.e. the `Option<Expr>` returned here will never be `None`). However,
    /// shallowly evaluated records may have fields with no value.
    pub fn key_value_by_index(&self, idx: usize) -> Option<(&str, Option<Expr>)> {
        self.data.fields.get_index(idx).map(|(key, fld)| {
            (
                key.label(),
                fld.value.as_ref().map(|rt| Expr { value: rt.clone() }),
            )
        })
    }

    /// Returns an iterator over the field names and values of this record.
    pub fn iter(&self) -> RecordIter<'_> {
        self.clone().into_iter()
    }
}

impl<'a> IntoIterator for Record<'a> {
    type Item = (&'a str, Option<Expr>);
    type IntoIter = RecordIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        RecordIter {
            inner: self.data.fields.iter(),
        }
    }
}

impl<'a> Iterator for RecordIter<'a> {
    type Item = (&'a str, Option<Expr>);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(k, v)| {
            (
                k.label(),
                v.value.as_ref().map(|rt| Expr { value: rt.clone() }),
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
        self.array.get(idx).map(|rt| Expr { value: rt.clone() })
    }

    /// Returns an iterator over the elements of this array.
    pub fn iter(&self) -> ArrayIter<'_> {
        self.clone().into_iter()
    }
}

impl<'a> IntoIterator for Array<'a> {
    type Item = Expr;
    type IntoIter = ArrayIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        ArrayIter {
            inner: self.array.into_iter(),
        }
    }
}

impl<'a> Iterator for ArrayIter<'a> {
    type Item = Expr;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|rt| Expr { value: rt.clone() })
    }
}

impl Number<'_> {
    /// If this number is an integer within the range of an `i64`, return it.
    pub fn as_i64(&self) -> Option<i64> {
        self.num.try_into().ok()
    }

    /// The value of this number, rounded to the nearest `f64`.
    pub fn as_f64(&self) -> f64 {
        f64::rounding_from(self.num, RoundingMode::Nearest).0
    }

    /// The exact value of this number, as a rational number.
    ///
    /// Both the numerator and denominator are provided as decimal strings.
    pub fn as_rational(&self) -> (String, String) {
        (
            self.num.numerator_ref().to_string(),
            self.num.denominator_ref().to_string(),
        )
    }
}

impl FieldMetadata<'_> {
    /// The documentation attached to the field (if any).
    pub fn doc(&self) -> Option<&str> {
        self.inner.doc.as_deref()
    }

    /// Is this field optional?
    pub fn optional(&self) -> bool {
        self.inner.opt
    }

    /// Is this field marked as `not_exported`?
    pub fn not_exported(&self) -> bool {
        self.inner.not_exported
    }

    /// Returns the merge priority of the field, if one was set.
    ///
    /// If no merge priority was set, returns `None`. (An unset
    /// priority has the same behavior as a numeric priority of zero.)
    pub fn merge_priority(&self) -> Option<MergePriority<'_>> {
        match &self.inner.priority {
            term::MergePriority::Bottom => Some(MergePriority::Default),
            term::MergePriority::Neutral => None,
            term::MergePriority::Numeral(num) => Some(MergePriority::Priority(Number { num })),
            term::MergePriority::Top => Some(MergePriority::Force),
        }
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
        assert!(expr.is_value());
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
        assert!(expr.is_value());
        let rec = expr.as_record().unwrap();
        assert_eq!(3, rec.len());

        let foo = rec.value_by_name("foo").unwrap();
        assert!(!foo.is_value());
        let foo = vm.eval_shallow(foo).unwrap();
        assert!(foo.is_array());
        let arr = foo.as_array().unwrap();
        assert_eq!(2, arr.len());

        let elt = arr.get(0).unwrap();
        assert_eq!(Some(1), elt.as_i64());

        let elt = arr.get(1).unwrap();
        assert!(!elt.is_value());
        assert_eq!(Some(5), vm.eval_shallow(elt).unwrap().as_i64());

        let baz = rec.value_by_name("baz").unwrap();
        assert!(!baz.is_value());
        let baz = vm.eval_shallow(baz).unwrap();
        assert!(baz.is_enum_variant());
        let (tag, inner) = baz.as_enum_variant().unwrap();
        assert_eq!(tag, "Tag");
        assert!(!inner.is_value());
        assert_eq!(Some(2), vm.eval_shallow(inner).unwrap().as_i64());
    }

    #[test]
    fn error_format() {
        let Err(err) =
            Context::default().eval_deep("{ port | Number = \"80\", name = \"myserver\" }")
        else {
            panic!("wanted an error");
        };
        let mut out = Vec::new();
        err.format(&mut out, ErrorFormat::Text).unwrap();
    }
}
