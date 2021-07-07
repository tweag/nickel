//! Web assembly interface to the REPL.
use super::simple_frontend::{input, serialize, InputError, InputResult};
use super::{REPLImpl, REPL};
use crate::cache::Cache;
use crate::error::ToDiagnostic;
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
pub enum WASMResultTag {
    Success = 0,
    Blank = 1,
    Partial = 2,
    Error = 3,
}

/// Severity of an error diagnostic. WASM wrapper for the corresponding codespan type.
#[derive(Serialize_repr, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
pub enum WASMErrorSeverity {
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

impl From<Severity> for WASMErrorSeverity {
    fn from(s: Severity) -> WASMErrorSeverity {
        match s {
            Severity::Bug => WASMErrorSeverity::Bug,
            Severity::Error => WASMErrorSeverity::Error,
            Severity::Warning => WASMErrorSeverity::Warning,
            Severity::Note => WASMErrorSeverity::Note,
            Severity::Help => WASMErrorSeverity::Help,
        }
    }
}

/// Style of an error label. WASM wrapper for the corresponding codespan type.
#[derive(Serialize_repr, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
pub enum WASMErrorLabelStyle {
    Primary = 0,
    Secondary = 1,
}

impl From<LabelStyle> for WASMErrorLabelStyle {
    fn from(label_style: LabelStyle) -> WASMErrorLabelStyle {
        match label_style {
            LabelStyle::Primary => WASMErrorLabelStyle::Primary,
            LabelStyle::Secondary => WASMErrorLabelStyle::Secondary,
        }
    }
}

/// A serializable error diagnostic. WASM wrapper for the corresponding codespan type.
#[derive(Serialize)]
pub struct WASMErrorDiagnostic {
    pub severity: WASMErrorSeverity,
    msg: String,
    notes: Vec<String>,
    labels: Vec<WASMErrorLabel>,
}

impl WASMErrorDiagnostic {
    fn from_codespan(files: &Files<String>, diag: Diagnostic<FileId>) -> Self {
        WASMErrorDiagnostic {
            severity: diag.severity.into(),
            msg: diag.message,
            notes: diag.notes,
            labels: diag
                .labels
                .into_iter()
                .map(|label| WASMErrorLabel::from_codespan(files, label))
                .collect(),
        }
    }
}

/// A serializable error label. WASM wrapper for the corresponding codespan type.
#[derive(Serialize)]
pub struct WASMErrorLabel {
    msg: String,
    pub style: WASMErrorLabelStyle,
    pub line_start: usize,
    pub col_start: usize,
    pub line_end: usize,
    pub col_end: usize,
}

impl WASMErrorLabel {
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

        WASMErrorLabel {
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
pub struct WASMInitResult {
    msg: String,
    pub tag: WASMResultTag,
    state: REPLState,
}

#[wasm_bindgen]
impl WASMInitResult {
    #[wasm_bindgen(getter)]
    pub fn msg(&self) -> String {
        self.msg.clone()
    }

    pub fn repl(self) -> REPLState {
        self.state
    }

    /// Make an `WASMInitResult` result from an `InputError`.
    fn error(mut state: REPLState, error: InputError) -> Self {
        WASMInitResult {
            msg: err_to_string(&mut state.0.cache_mut(), &error),
            tag: WASMResultTag::Error,
            state,
        }
    }
}

/// WASM wrapper for the result type of an execution of the REPL.
#[wasm_bindgen]
pub struct WASMInputResult {
    msg: String,
    pub tag: WASMResultTag,
    errors: JsValue,
}

#[wasm_bindgen]
impl WASMInputResult {
    #[wasm_bindgen(getter)]
    pub fn msg(&self) -> String {
        self.msg.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn errors(&self) -> JsValue {
        self.errors.clone()
    }

    /// Make an `WASMInputResult` from an `InputError`.
    fn error(cache: &mut Cache, error: InputError) -> Self {
        let (msg, errors) = match error {
            InputError::NickelError(err) => {
                let contracts_id = cache.id_of("<stdlib/contracts.ncl>");
                let diagnostics = err.to_diagnostic(cache.files_mut(), contracts_id);

                let msg = diags_to_string(cache, &diagnostics);
                let errors: Vec<WASMErrorDiagnostic> = diagnostics
                    .into_iter()
                    .map(|diag| WASMErrorDiagnostic::from_codespan(cache.files(), diag))
                    .collect();
                (msg, errors)
            }
            InputError::Other(err) => (err, Vec::new()),
        };

        WASMInputResult {
            msg,
            tag: WASMResultTag::Error,
            errors: JsValue::from_serde(&errors).unwrap(),
        }
    }

    /// Generate a serializable empty list.
    fn empty_errors() -> JsValue {
        JsValue::from_serde(&Vec::<WASMErrorDiagnostic>::new()).unwrap()
    }
}

impl From<InputResult> for WASMInputResult {
    fn from(ir: InputResult) -> Self {
        match ir {
            InputResult::Success(msg) => WASMInputResult {
                msg,
                tag: WASMResultTag::Success,
                errors: WASMInputResult::empty_errors(),
            },
            InputResult::Blank => WASMInputResult {
                msg: String::new(),
                tag: WASMResultTag::Blank,
                errors: WASMInputResult::empty_errors(),
            },
            InputResult::Partial => WASMInputResult {
                msg: String::new(),
                tag: WASMResultTag::Partial,
                errors: WASMInputResult::empty_errors(),
            },
        }
    }
}

/// WASM-compatible wrapper around `REPLImpl`.
#[wasm_bindgen]
pub struct REPLState(REPLImpl);

/// WASM-compatible wrapper around `serialize::ExportFormat`.
#[wasm_bindgen]
pub enum WASMExportFormat {
    Raw = "raw",
    Json = "json",
    Yaml = "yaml",
    Toml = "toml",
}

pub type ExportFormaParseError = ();

impl TryInto<ExportFormat> for WASMExportFormat {
    type Error = ExportFormaParseError;

    fn try_into(self) -> Result<ExportFormat, ExportFormaParseError> {
        match self {
            WASMExportFormat::Raw => Ok(ExportFormat::Raw),
            WASMExportFormat::Json => Ok(ExportFormat::Json),
            WASMExportFormat::Yaml => Ok(ExportFormat::Yaml),
            WASMExportFormat::Toml => Ok(ExportFormat::Toml),
            _ => Err(()),
        }
    }
}

/// Render error diagnostics as a string.
pub fn diags_to_string(cache: &mut Cache, diags: &Vec<Diagnostic<FileId>>) -> String {
    let mut buffer = Ansi::new(Cursor::new(Vec::new()));
    let config = codespan_reporting::term::Config::default();

    diags
        .iter()
        .try_for_each(|d| {
            codespan_reporting::term::emit(&mut buffer, &config, cache.files_mut(), &d)
        })
        .unwrap();

    String::from_utf8(buffer.into_inner().into_inner()).unwrap()
}

/// Render an error as a string (similar to [`diags_to_string`](./meth.diags_to_string.html)).
pub fn err_to_string(cache: &mut Cache, error: &InputError) -> String {
    match error {
        InputError::NickelError(nickel_err) => {
            let contracts_id = cache.id_of("<stdlib/contracts.ncl>");
            let diags = nickel_err.to_diagnostic(cache.files_mut(), contracts_id);
            diags_to_string(cache, &diags)
        }
        InputError::Other(msg) => msg.clone(),
    }
}

/// Return a new instance of the WASM REPL, with the standard library loaded.
#[wasm_bindgen]
pub fn repl_init() -> WASMInitResult {
    let mut repl = REPLImpl::new();
    match repl.load_stdlib() {
        Ok(()) => WASMInitResult {
            msg: String::new(),
            tag: WASMResultTag::Success,
            state: REPLState(repl),
        },
        Err(err) => WASMInitResult::error(REPLState(repl), err.into()),
    }
}

/// Evaluate an input in the WASM REPL.
#[wasm_bindgen]
pub fn repl_input(state: &mut REPLState, line: &str) -> WASMInputResult {
    input(&mut state.0, line)
        .map(WASMInputResult::from)
        .unwrap_or_else(|err| WASMInputResult::error(state.0.cache_mut(), err))
}

/// Evaluate an input in the WASM REPL and serialize it.
pub fn repl_serialize(
    state: &mut REPLState,
    format: WASMExportFormat,
    line: &str,
) -> WASMInputResult {
    serialize(&mut state.0, format.try_into().unwrap_or_default(), line)
        .map(WASMInputResult::from)
        .unwrap_or_else(|err| WASMInputResult::error(state.0.cache_mut(), err))
}
