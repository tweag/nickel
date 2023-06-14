//! Web assembly interface to the REPL.
use super::simple_frontend::{input, serialize, InputError, InputResult};
use super::{Repl, ReplImpl};
use crate::cache::Cache;
use crate::error::IntoDiagnostics;
use crate::eval::cache::CacheImpl;
use crate::serialize::ExportFormat;
use codespan::{FileId, Files};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label, LabelStyle, Severity},
    term::termcolor::Ansi,
};
use serde::Serialize;
use serde_repr::Serialize_repr;
use std::convert::TryInto;
use std::io::Cursor;
use wasm_bindgen::prelude::*;

/// Return codes of the WASM REPL.
///
/// wasm-bindgen doesn't support exporting arbitrary enumeration. Thus we have to encode these
/// enums as structures with a tag and values. The values that are actually set depend on the
/// tag.
#[wasm_bindgen]
#[derive(Clone, Copy, Eq, PartialEq)]
pub enum WasmResultTag {
    Success = 0,
    Blank = 1,
    Partial = 2,
    Error = 3,
}

/// Severity of an error diagnostic. WASM wrapper for the corresponding codespan type.
#[derive(Serialize_repr, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
pub enum WasmErrorSeverity {
    Bug = 5,
    /// An error.
    Error = 4,
    /// A warning.
    Warning = 3,
    /// A note.
    Note = 2,
    /// A help message.
    Help = 1,
}

impl From<Severity> for WasmErrorSeverity {
    fn from(s: Severity) -> WasmErrorSeverity {
        match s {
            Severity::Bug => WasmErrorSeverity::Bug,
            Severity::Error => WasmErrorSeverity::Error,
            Severity::Warning => WasmErrorSeverity::Warning,
            Severity::Note => WasmErrorSeverity::Note,
            Severity::Help => WasmErrorSeverity::Help,
        }
    }
}

/// Style of an error label. WASM wrapper for the corresponding codespan type.
#[derive(Serialize_repr, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
pub enum WasmErrorLabelStyle {
    Primary = 0,
    Secondary = 1,
}

impl From<LabelStyle> for WasmErrorLabelStyle {
    fn from(label_style: LabelStyle) -> WasmErrorLabelStyle {
        match label_style {
            LabelStyle::Primary => WasmErrorLabelStyle::Primary,
            LabelStyle::Secondary => WasmErrorLabelStyle::Secondary,
        }
    }
}

/// A serializable error diagnostic. WASM wrapper for the corresponding codespan type.
#[derive(Serialize)]
pub struct WasmErrorDiagnostic {
    pub severity: WasmErrorSeverity,
    msg: String,
    notes: Vec<String>,
    labels: Vec<WasmErrorLabel>,
}

impl WasmErrorDiagnostic {
    fn from_codespan(files: &Files<String>, diag: Diagnostic<FileId>) -> Self {
        WasmErrorDiagnostic {
            severity: diag.severity.into(),
            msg: diag.message,
            notes: diag.notes,
            labels: diag
                .labels
                .into_iter()
                .map(|label| WasmErrorLabel::from_codespan(files, label))
                .collect(),
        }
    }
}

/// A serializable error label. WASM wrapper for the corresponding codespan type.
#[derive(Serialize)]
pub struct WasmErrorLabel {
    msg: String,
    pub style: WasmErrorLabelStyle,
    pub line_start: usize,
    pub col_start: usize,
    pub line_end: usize,
    pub col_end: usize,
}

impl WasmErrorLabel {
    fn from_codespan(files: &Files<String>, label: Label<FileId>) -> Self {
        let start_loc = files.location(label.file_id, label.range.start as u32);
        let end_loc = files.location(label.file_id, label.range.end as u32);

        let (line_start, col_start, line_end, col_end) = match (start_loc, end_loc) {
            (Ok(start_loc), Ok(end_loc)) => (
                start_loc.line.to_usize(),
                start_loc.column.to_usize(),
                end_loc.line.to_usize(),
                end_loc.column.to_usize(),
            ),
            (Ok(loc), _) | (_, Ok(loc)) => (
                loc.line.to_usize(),
                loc.column.to_usize(),
                loc.line.to_usize(),
                loc.column.to_usize(),
            ),
            _ => (0, 0, 0, 0),
        };

        WasmErrorLabel {
            msg: label.message,
            style: label.style.into(),
            line_start,
            col_start,
            line_end,
            col_end,
        }
    }
}

/// WASM wrapper for the result type of the initialization of the REPL.
#[wasm_bindgen]
pub struct WasmInitResult {
    msg: String,
    pub tag: WasmResultTag,
    state: ReplState,
}

#[wasm_bindgen]
impl WasmInitResult {
    #[wasm_bindgen(getter)]
    pub fn msg(&self) -> String {
        self.msg.clone()
    }

    pub fn repl(self) -> ReplState {
        self.state
    }

    /// Make a `WasmInitResult` result from an `InputError`.
    fn error(mut state: ReplState, error: InputError) -> Self {
        WasmInitResult {
            msg: err_to_string(state.0.cache_mut(), error),
            tag: WasmResultTag::Error,
            state,
        }
    }
}

/// WASM wrapper for the result type of an execution of the REPL.
#[wasm_bindgen]
pub struct WasmInputResult {
    msg: String,
    pub tag: WasmResultTag,
    errors: JsValue,
}

#[wasm_bindgen]
impl WasmInputResult {
    #[wasm_bindgen(getter)]
    pub fn msg(&self) -> String {
        self.msg.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn errors(&self) -> JsValue {
        self.errors.clone()
    }

    /// Make a `WasmInputResult` from an `InputError`.
    fn error(cache: &mut Cache, error: InputError) -> Self {
        let (msg, errors) = match error {
            InputError::NickelError(err) => {
                let stdlib_ids = cache.get_all_stdlib_modules_file_id();
                let diagnostics = err.into_diagnostics(cache.files_mut(), stdlib_ids.as_ref());

                let msg = diags_to_string(cache, &diagnostics);
                let errors: Vec<WasmErrorDiagnostic> = diagnostics
                    .into_iter()
                    .map(|diag| WasmErrorDiagnostic::from_codespan(cache.files(), diag))
                    .collect();
                (msg, errors)
            }
            InputError::Other(err) => (err, Vec::new()),
        };

        WasmInputResult {
            msg,
            tag: WasmResultTag::Error,
            errors: serde_wasm_bindgen::to_value(&errors).unwrap(),
        }
    }

    /// Generate a serializable empty list.
    fn empty_errors() -> JsValue {
        serde_wasm_bindgen::to_value(&Vec::<WasmErrorDiagnostic>::new()).unwrap()
    }
}

impl From<InputResult> for WasmInputResult {
    fn from(ir: InputResult) -> Self {
        match ir {
            InputResult::Success(msg) => WasmInputResult {
                msg,
                tag: WasmResultTag::Success,
                errors: WasmInputResult::empty_errors(),
            },
            InputResult::Blank => WasmInputResult {
                msg: String::new(),
                tag: WasmResultTag::Blank,
                errors: WasmInputResult::empty_errors(),
            },
            InputResult::Partial => WasmInputResult {
                msg: String::new(),
                tag: WasmResultTag::Partial,
                errors: WasmInputResult::empty_errors(),
            },
        }
    }
}

/// WASM-compatible wrapper around `ReplImpl`.
#[wasm_bindgen]
pub struct ReplState(ReplImpl<CacheImpl>);

/// WASM-compatible wrapper around `serialize::ExportFormat`.
#[wasm_bindgen]
pub enum WasmExportFormat {
    Raw = "raw",
    Json = "json",
    Yaml = "yaml",
    Toml = "toml",
}

pub type ExportFormaParseError = ();

impl TryInto<ExportFormat> for WasmExportFormat {
    type Error = ExportFormaParseError;

    fn try_into(self) -> Result<ExportFormat, ExportFormaParseError> {
        match self {
            WasmExportFormat::Raw => Ok(ExportFormat::Raw),
            WasmExportFormat::Json => Ok(ExportFormat::Json),
            WasmExportFormat::Yaml => Ok(ExportFormat::Yaml),
            WasmExportFormat::Toml => Ok(ExportFormat::Toml),
            _ => Err(()),
        }
    }
}

/// A `Write` implementor that calls a callback whenever it gets written to.
struct CallbackWriter {
    callback: Option<js_sys::Function>,
    buf: Vec<u8>,
}

impl CallbackWriter {
    fn new(callback: Option<js_sys::Function>) -> Self {
        Self {
            callback,
            buf: Vec::new(),
        }
    }
}

impl std::io::Write for CallbackWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.buf.extend_from_slice(buf);
        if buf.contains(&b'\n') {
            self.flush()?;
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        if let Some(callback) = &self.callback {
            let js_string = JsValue::from_str(String::from_utf8_lossy(&self.buf).trim());
            callback
                .call1(&JsValue::NULL, &js_string)
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("{e:?}")))?;
        }
        self.buf.clear();
        Ok(())
    }
}

/// Render error diagnostics as a string.
pub fn diags_to_string(cache: &mut Cache, diags: &[Diagnostic<FileId>]) -> String {
    let mut buffer = Ansi::new(Cursor::new(Vec::new()));
    let config = codespan_reporting::term::Config::default();

    diags
        .iter()
        .try_for_each(|d| {
            codespan_reporting::term::emit(&mut buffer, &config, cache.files_mut(), d)
        })
        .unwrap();

    String::from_utf8(buffer.into_inner().into_inner()).unwrap()
}

/// Render an error as a string (similar to [`diags_to_string`](./meth.diags_to_string.html)).
pub fn err_to_string(cache: &mut Cache, error: InputError) -> String {
    match error {
        InputError::NickelError(nickel_err) => {
            let stdlib_ids = cache.get_all_stdlib_modules_file_id();
            let diags = nickel_err.into_diagnostics(cache.files_mut(), stdlib_ids.as_ref());
            diags_to_string(cache, &diags)
        }
        InputError::Other(msg) => msg,
    }
}

/// Return a new instance of the WASM REPL, with the standard library loaded.
#[wasm_bindgen]
pub fn repl_init(trace_callback: Option<js_sys::Function>) -> WasmInitResult {
    let trace = CallbackWriter::new(trace_callback);
    let mut repl = ReplImpl::new(trace);
    match repl.load_stdlib() {
        Ok(()) => WasmInitResult {
            msg: String::new(),
            tag: WasmResultTag::Success,
            state: ReplState(repl),
        },
        Err(err) => WasmInitResult::error(ReplState(repl), err.into()),
    }
}

/// Evaluate an input in the WASM REPL.
#[wasm_bindgen]
pub fn repl_input(state: &mut ReplState, line: &str) -> WasmInputResult {
    input(&mut state.0, line)
        .map(WasmInputResult::from)
        .unwrap_or_else(|err| WasmInputResult::error(state.0.cache_mut(), err))
}

/// Evaluate an input in the WASM REPL and serialize it.
#[wasm_bindgen]
pub fn repl_serialize(
    state: &mut ReplState,
    format: WasmExportFormat,
    line: &str,
) -> WasmInputResult {
    serialize(&mut state.0, format.try_into().unwrap_or_default(), line)
        .map(WasmInputResult::from)
        .unwrap_or_else(|err| WasmInputResult::error(state.0.cache_mut(), err))
}
