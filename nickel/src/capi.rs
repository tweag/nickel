//! C bindings for the Nickel language
//!
//! These bindings cover
//! - evaluation (including lazy/partial evaluation),
//! - inspection of the results as structured values,
//! - serialization to JSON, TOML, and YAML,
//! - error reporting.
//!
//! Here is a basic example that evaluates a Nickel expression
//! and inspects the result. For more examples, see the C api
//! tests in the Nickel repository.
//!
//! ```c
//! nickel_context *ctx = nickel_context_alloc();
//! nickel_expr *expr = nickel_expr_alloc();
//!
//! /* Passing a null pointer as the "error" param is the easy option, but we won't get
//!    nice diagnostics. */
//! nickel_result result = nickel_context_eval_deep(ctx, "{ foo | Number = 1 }", expr, NULL);
//! assert(result == NICKEL_RESULT_OK);
//!
//! /* Now the result of evaluation is in `expr`. It's a record with one element */
//! assert(nickel_expr_is_record(expr));
//! nickel_record const *rec = nickel_expr_as_record(expr);
//! assert(nickel_record_len(rec) == 1);
//!
//! /* The single element of the record has key "foo" and value `1` */
//! char const* key;
//! uintptr_t len;
//! nickel_expr *val = nickel_expr_alloc();
//! nickel_record_key_value_by_index(rec, 0, &key, &len, val);
//!
//! /* Careful: the strings coming from the nickel API are not null-terminated! */
//! assert(len == 3);
//! assert(!strncmp(key, "foo", 3));
//! assert(nickel_expr_is_number(val));
//! assert(nickel_expr_as_i64(val) == 1);
//! ```
//!
//! # Ownership and lifetimes
//!
//! We have a uniform interface for owned data: all of the owning types (`nickel_context`,
//! `nickel_expr`, `nickel_error`, `nickel_string`) are only made available to C via
//! opaque pointers. For example, a `nickel_context` can only be created by `nickel_context_alloc`,
//! which returns a `*mut nickel_context` pointing to some data on the Rust heap. That
//! data is now owned by the `*mut nickel_context` pointer -- there is no longer any
//! owner on the Rust side to move out the data and invalidate the pointer.
//!
//! The data behind a `*mut nickel_context` pointer can be modified by various API
//! functions (like `nickel_context_eval_deep`), but it will only be destroyed
//! through a call to `nickel_context_free`.
//!
//! The other owned data types are similar: you create a `*mut nickel_expr` with
//! `nickel_expr_alloc`, modify that allocated expression with various API functions,
//! and then ultimately free it with `nickel_expr_free`.

// Note to future maintainers: preserve the invariant describe above! Pointers to
// the owning types should only come from `nickel_something_alloc` functions, and
// in particular should never be temporary pointers to things owned on the Rust
// side.

#![allow(clippy::missing_safety_doc)]
#![allow(non_camel_case_types)]
#![deny(missing_docs)]

use std::{
    ffi::{CStr, c_char, c_int, c_void},
    io::Write,
    ptr,
};

use nickel_lang_core::bytecode::value::{self, Container, NickelValue};

use crate::{Array, Context, Error, ErrorFormat, Expr, Number, Record};

/// The main entry point.
pub struct nickel_context {
    inner: Context,
}

impl nickel_context {
    /// Convert a `*mut nickel_contest` to a `&mut Context`.
    ///
    /// # Safety
    ///
    /// Assumes that `this` was originally a valid pointer to `Context`.
    unsafe fn as_rust_mut(this: &mut *mut Self) -> &mut Context {
        // Safety: pre-condition of this unsafe function
        unsafe { &mut this.as_mut().unwrap().inner }
    }
}

/// A Nickel error.
///
/// If you want to collect an error message from a fallible function
/// (like `nickel_context_eval_deep`), first allocate an error using
/// `nickel_error_alloc`, and then pass the resulting pointer to your fallible
/// function. If that function fails, it will save the error data in your
/// `nickel_error`.
pub struct nickel_error {
    inner: Option<Error>,
}

/// A Nickel expression.
///
/// This might be fully evaluated (for example, if you got it from [`nickel_context_eval_deep`])
/// or might have unevaluated sub-expressions (if you got it from [`nickel_context_eval_shallow`]).
pub struct nickel_expr {
    inner: Expr,
}

impl nickel_expr {
    /// Convert a `*nickel_expr` to a `&Expr`.
    ///
    /// # Safety
    ///
    /// Assumes that `this` was originally a valid pointer to `Expr`.
    unsafe fn as_rust(this: &*const Self) -> &Expr {
        // Safety: pre-condition of this unsafe function
        unsafe { &(*this).as_ref().unwrap().inner }
    }

    /// Convert a `*mut nickel_expr` to a `&mut Expr`.
    ///
    /// # Safety
    ///
    /// Assumes that `this` was originally a valid pointer to `Expr`.
    unsafe fn as_rust_mut(this: &mut *mut Self) -> &mut Expr {
        // Safety: pre-condition of this unsafe function
        unsafe { &mut (*this).as_mut().unwrap().inner }
    }
}

/// A Nickel array.
///
/// See [`nickel_expr_is_array`] and [`nickel_expr_as_array`].
// This is not really an empty struct, it's just an opaque type to
// hide the implementation. The C API only ever uses `*const Array`,
// and they are secretly pointers to `nickel_lang_core::term::array::Array`.
//
// Note that we represent empty arrays (in the sense of inlined empty arrays: Container::Empty) as
// a null pointer.
pub struct nickel_array {}

impl nickel_array {
    /// Convert a `*nickel_array` to an `Array`.
    ///
    /// # Safety
    ///
    /// Assumes that `this` was originally a valid pointer to a `term::array::Array`, or is `null`
    /// (the array is then considered empty).
    unsafe fn as_rust(this: &*const Self) -> Array<'_> {
        if this.is_null() {
            Array {
                array: Container::Empty,
            }
        } else {
            Array {
                // Safety: pre-condition of this unsafe function
                array: Container::Alloc(unsafe {
                    (*this as *const value::ArrayData).as_ref().unwrap()
                }),
            }
        }
    }
}

impl<'a> From<Array<'a>> for *const nickel_array {
    fn from(arr: Array<'a>) -> Self {
        match arr.array {
            Container::Empty => ptr::null(),
            Container::Alloc(data) => data as *const _ as *const nickel_array,
        }
    }
}

/// A Nickel record.
///
/// See [`nickel_expr_is_record`] and [`nickel_expr_as_record`].
// This is not really an empty struct, it's just an opaque type to hide the implementation. The C
// API only ever uses `*const Record`, and they are secretly pointers to
// `nickel_lang_core::term::record::RecordData`.
//
// Note that we represent empty records (in the sense of inlined empty records: Container::Empty)
// as a null pointer.
pub struct nickel_record {}

impl nickel_record {
    /// Convert a `*nickel_record` to a `Record`.
    ///
    /// # Safety
    ///
    /// Assumes that `this` was originally a valid pointer to a `term::record::RecordData`, or is `null`
    /// (the record is then considered empty).
    unsafe fn as_rust(this: &*const Self) -> Record<'_> {
        if this.is_null() {
            Record {
                data: Container::Empty,
            }
        } else {
            Record {
                data: Container::Alloc(
                    // Safety: pre-condition of this unsafe function
                    unsafe { (*this as *const value::RecordData).as_ref().unwrap() },
                ),
            }
        }
    }
}

impl<'a> From<Record<'a>> for *const nickel_record {
    fn from(rec: Record<'a>) -> Self {
        match rec.data {
            Container::Empty => ptr::null(),
            Container::Alloc(data) => data as *const _ as *const nickel_record,
        }
    }
}

/// A Nickel string.
// It would be nice to put `repr(transparent)` here, but (1) we don't
// actually need to cast `String` pointers to `nickel_string` pointers,
// and (2) adding `repr(transparent)` makes cbindgen expose `String`
// even though it's private.
pub struct nickel_string {
    inner: String,
}

/// A Nickel number.
///
/// See [`nickel_expr_is_number`] and [`nickel_expr_as_number`].
// This is not really an empty struct, it's just an opaque type to
// hide the implementation. The C API only ever uses `*const Number`,
// and they are secretly pointers to `nickel_lang_core::term::Number`.
pub struct nickel_number {}

impl nickel_number {
    /// Convert a `*nickel_number` to a `Number`.
    ///
    /// # Safety
    ///
    /// Assumes that `this` was originally a valid pointer to a `term::Number`.
    unsafe fn as_rust(this: &*const Self) -> Number<'_> {
        Number {
            // Safety: pre-condition of this unsafe function
            num: unsafe {
                (*this as *const nickel_lang_core::term::Number)
                    .as_ref()
                    .unwrap()
            },
        }
    }
}

impl<'a> From<Number<'a>> for *const nickel_number {
    fn from(n: Number<'a>) -> Self {
        n.num as *const _ as *const nickel_number
    }
}

/// Allocate a new [`nickel_context`], which can be used to evaluate Nickel expressions.
///
/// Returns a newly-allocated [`nickel_context`] that can be freed with [`nickel_context_free`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_context_alloc() -> *mut nickel_context {
    Box::into_raw(Box::new(nickel_context {
        inner: Context::default(),
    }))
}

/// Free a [`nickel_context`] that was created with [`nickel_context_alloc`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_context_free(ctx: *mut nickel_context) {
    // Safety: `ctx` is required to be a valid pointer to a box-allocated context, as
    // returned by `nickel_context_alloc`.
    let _ = unsafe { Box::from_raw(ctx) };
}

/// A callback function for writing data.
///
/// This function will be called with a buffer (`buf`) of data, having length
/// `len`. It need not consume the entire buffer, and should return the number
/// of bytes consumed.
// This Option<fn> pattern seems to be cbindgen's preferred way of encoding
// a nullable function pointer (since rust fns are never null).
// https://github.com/mozilla/cbindgen/issues/326#issuecomment-584288686
pub type nickel_write_callback =
    Option<extern "C" fn(context: *mut c_void, buf: *const u8, len: usize) -> usize>;

/// A callback function for flushing data that was written by a write callback.
pub type nickel_flush_callback = Option<extern "C" fn(context: *const c_void)>;

/// For functions that can fail, these are the interpretations of the return value.
#[repr(C)]
pub enum nickel_result {
    /// A successful result.
    NICKEL_RESULT_OK = 0,
    /// A bad result.
    NICKEL_RESULT_ERR = 1,
}

/// For functions that can fail, these are the interpretations of the return value.
#[repr(C)]
pub enum nickel_error_format {
    /// Format an error as human-readable text.
    NICKEL_ERROR_FORMAT_TEXT = 0,
    /// Format an error as human-readable text, with ANSI color codes.
    NICKEL_ERROR_FORMAT_ANSI_TEXT = 1,
    /// Format an error as JSON.
    NICKEL_ERROR_FORMAT_JSON = 2,
    /// Format an error as YAML.
    NICKEL_ERROR_FORMAT_YAML = 3,
    /// Format an error as TOML.
    NICKEL_ERROR_FORMAT_TOML = 4,
}

impl From<nickel_error_format> for ErrorFormat {
    fn from(e: nickel_error_format) -> Self {
        match e {
            nickel_error_format::NICKEL_ERROR_FORMAT_TEXT => ErrorFormat::Text,
            nickel_error_format::NICKEL_ERROR_FORMAT_ANSI_TEXT => ErrorFormat::AnsiText,
            nickel_error_format::NICKEL_ERROR_FORMAT_JSON => ErrorFormat::Json,
            nickel_error_format::NICKEL_ERROR_FORMAT_YAML => ErrorFormat::Yaml,
            nickel_error_format::NICKEL_ERROR_FORMAT_TOML => ErrorFormat::Toml,
        }
    }
}

struct CTrace {
    write: nickel_write_callback,
    flush: nickel_flush_callback,
    context: *mut c_void,
}

impl Write for CTrace {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let count = if let Some(w) = self.write {
            w(self.context, buf.as_ptr(), buf.len())
        } else {
            buf.len()
        };
        if count == usize::MAX {
            Err(std::io::Error::other("trace failed to write"))
        } else {
            Ok(count)
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        if let Some(flush) = self.flush {
            flush(self.context);
        }
        Ok(())
    }
}

/// Provide a callback that will be called when evaluating Nickel
/// code that uses `std.trace`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_context_set_trace_callback(
    mut ctx: *mut nickel_context,
    write: nickel_write_callback,
    flush: nickel_flush_callback,
    user_data: *mut c_void,
) {
    let trace = Box::new(CTrace {
        write,
        flush,
        context: user_data,
    });

    // Safety: `ctx` is required to be a valid pointer to a box-allocated context, as
    // returned by `nickel_context_alloc`.
    unsafe {
        nickel_context::as_rust_mut(&mut ctx).vm_ctxt.trace = trace;
    }
}

/// Provide a name for the main input program.
///
/// This is used to format error messages. If you read the main input
/// program from a file, its path is a good choice.
///
/// `name` should be a UTF-8-encoded, null-terminated string. It is only
/// borrowed temporarily; the pointer need not remain valid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_context_set_source_name(
    mut ctx: *mut nickel_context,
    name: *const c_char,
) {
    // Safety: `name` is assumed to be a valid C string.
    let name = String::from_utf8_lossy(unsafe { CStr::from_ptr(name).to_bytes() }).into_owned();

    // Safety: `ctx` is required to be a valid pointer to a box-allocated context, as
    // returned by `nickel_context_alloc`.
    unsafe {
        nickel_context::as_rust_mut(&mut ctx).name = Some(name);
    }
}

/// Perform some sort of evaluation, and return the error appropriately.
///
/// # Safety
///
/// `src` must be a null-terminated string, and all of the nickel_xxx pointers
/// must be valid (i.e. allocated by the corresponding `nickel_xxx_alloc` function)
/// or null. We panic on a null `ctx`, but null `out_expr` and/or `out_error` are
/// allowed.
unsafe fn do_eval<F>(
    f: F,
    mut ctx: *mut nickel_context,
    src: *const c_char,
    mut out_expr: *mut nickel_expr,
    out_error: *mut nickel_error,
) -> nickel_result
where
    F: FnOnce(&mut crate::Context, &str) -> Result<Expr, Error>,
{
    // Safety: the various expectations are requirements from C caller (`src` is a valid UTF8 C
    // String, pointers are valid and allocated from the corresponding `xxx_alloc` functions, etc.
    unsafe {
        let src = CStr::from_ptr(src).to_str().unwrap();
        match f(nickel_context::as_rust_mut(&mut ctx), src) {
            Ok(expr) => {
                if !out_expr.is_null() {
                    // Safety: `out_expr` is required to be a valid pointer to an expression allocated
                    // via `nickel_expr_alloc`.
                    *nickel_expr::as_rust_mut(&mut out_expr) = expr;
                }
                nickel_result::NICKEL_RESULT_OK
            }
            Err(e) => {
                if !out_error.is_null() {
                    // Safety: `out_error` is required to be a valid pointer to an expression allocated
                    // via `nickel_error_alloc`.
                    (*out_error).inner = Some(e);
                }
                nickel_result::NICKEL_RESULT_ERR
            }
        }
    }
}

/// Evaluate a Nickel program deeply.
///
/// "Deeply" means that we recursively evaluate records and arrays. For
/// an alternative, see [`nickel_context_eval_shallow`].
///
/// - `src` is a null-terminated string containing UTF-8-encoded Nickel source.
/// - `out_expr` either NULL or something that was created with [`nickel_expr_alloc`]
/// - `out_error` can be NULL if you aren't interested in getting detailed
///   error messages
///
/// If evaluation is successful, returns `NICKEL_RESULT_OK` and replaces
/// the value at `out_expr` (if non-NULL) with the newly-evaluated Nickel expression.
///
/// If evaluation fails, returns `NICKEL_RESULT_ERR` and replaces the
/// value at `out_error` (if non-NULL) by a pointer to a newly-allocated Nickel error.
/// That error should be freed with `nickel_error_free` when you are
/// done with it.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_context_eval_deep(
    ctx: *mut nickel_context,
    src: *const c_char,
    out_expr: *mut nickel_expr,
    out_error: *mut nickel_error,
) -> nickel_result {
    // Safety: the safety requirements of `do_eval` are expected from a C call.
    unsafe { do_eval(|ctx, src| ctx.eval_deep(src), ctx, src, out_expr, out_error) }
}

/// Evaluate a Nickel program deeply.
///
/// This differs from [`nickel_context_eval_deep`] in that it ignores
/// fields marked as `not_exported`.
///
/// - `src` is a null-terminated string containing UTF-8-encoded Nickel source.
/// - `out_expr` either NULL or something that was created with [`nickel_expr_alloc`]
/// - `out_error` can be NULL if you aren't interested in getting detailed
///   error messages
///
/// If evaluation is successful, returns `NICKEL_RESULT_OK` and replaces
/// the value at `out_expr` (if non-NULL) with the newly-evaluated Nickel expression.
///
/// If evaluation fails, returns `NICKEL_RESULT_ERR` and replaces the
/// value at `out_error` (if non-NULL) by a pointer to a newly-allocated Nickel error.
/// That error should be freed with `nickel_error_free` when you are
/// done with it.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_context_eval_deep_for_export(
    ctx: *mut nickel_context,
    src: *const c_char,
    out_expr: *mut nickel_expr,
    out_error: *mut nickel_error,
) -> nickel_result {
    // Safety: the safety requirements of `do_eval` are expected from a C call.
    unsafe {
        do_eval(
            |ctx, src| ctx.eval_deep_for_export(src),
            ctx,
            src,
            out_expr,
            out_error,
        )
    }
}

/// Evaluate a Nickel program to weak head normal form (WHNF).
///
/// The result of this evaluation is a null, bool, number, string,
/// enum, record, or array. In case it's a record, array, or enum
/// variant, the payload (record values, array elements, or enum
/// payloads) will be left unevaluated.
///
/// Sub-expressions of the result can be evaluated further by [nickel_context_eval_expr_shallow].
///
/// - `src` is a null-terminated string containing UTF-8-encoded Nickel source.
/// - `out_expr` is either NULL or something that was created with [`nickel_expr_alloc`]
/// - `out_error` can be NULL if you aren't interested in getting detailed
///   error messages
///
/// If evaluation is successful, returns `NICKEL_RESULT_OK` and replaces the value at `out_expr`
/// (if non-NULL) with the newly-evaluated Nickel expression.
///
/// If evaluation fails, returns `NICKEL_RESULT_ERR` and replaces the value at `out_error` (if
/// non-NULL) by a pointer to a newly-allocated Nickel error. That error should be freed with
/// `nickel_error_free` when you are done with it.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_context_eval_shallow(
    mut ctx: *mut nickel_context,
    src: *const c_char,
    mut out_expr: *mut nickel_expr,
    out_error: *mut nickel_error,
) -> nickel_result {
    // Safety: the various expectations are requirements from C caller (`src` is a valid UTF8 C
    // String, pointers are valid and allocated from the corresponding `xxx_alloc` functions, etc.
    unsafe {
        let src = CStr::from_ptr(src).to_str().unwrap();
        match nickel_context::as_rust_mut(&mut ctx).eval_shallow(src) {
            Ok(expr) => {
                if !out_expr.is_null() {
                    *nickel_expr::as_rust_mut(&mut out_expr) = expr;
                }
                nickel_result::NICKEL_RESULT_OK
            }
            Err(e) => {
                if !out_error.is_null() {
                    (*out_error).inner = Some(e);
                }
                nickel_result::NICKEL_RESULT_ERR
            }
        }
    }
}

/// Allocate a new Nickel expression.
///
/// The returned expression pointer can be used to store the results of
/// evaluation, for example by passing it as the `out_expr` location of
/// `nickel_context_eval_deep`.
///
/// Each call to `nickel_expr_alloc` should be paired with a call to
/// `nickel_expr_free`. The various functions (like `nickel_context_eval_deep`)
/// that take an `out_expr` parameter overwrite the existing expression
/// contents, and do not affect the pairing of `nickel_expr_alloc` and
/// `nickel_expr_free`.
///
/// For example:
///
/// ```c
/// nickel_context *ctx = nickel_context_alloc();
/// nickel_context *expr = nickel_expr_alloc();
///
/// nickel_context_eval_deep(ctx, "{ foo = 1 }", expr, NULL);
///
/// // now expr is a record
/// printf("record: %d\n", nickel_expr_is_record(expr));
///
/// nickel_context_eval_deep(ctx, "[1, 2, 3]", expr, NULL);
///
/// // now expr is an array
/// printf("array: %d\n", nickel_expr_is_array(expr));
///
/// // the calls to nickel_context_eval_deep haven't created any new exprs:
/// // we only need to free it once
/// nickel_expr_free(expr);
/// nickel_context_free(ctx);
/// ```
///
/// An `Expr` owns its data. There are various ways to get a reference to
/// data owned by an expression, which are then invalidated when the expression
/// is freed (by `nickel_expr_free`) or overwritten (for example, by
/// `nickel_context_deep_eval`).
///
/// ```c
/// nickel_context *ctx = nickel_context_alloc();
/// nickel_expr *expr = nickel_expr_alloc();
///
/// nickel_context_eval_deep(ctx, "{ foo = 1 }", expr, NULL);
///
/// nickel_record *rec = nickel_expr_as_record(expr);
/// nickel_expr *field = nickel_expr_alloc();
/// nickel_record_value_by_name(rec, "foo", field);
///
/// // Now `rec` points to data owned by `expr`, but `field`
/// // owns its own data. The following deallocation invalidates
/// // `rec`, but not `field`.
/// nickel_expr_free(expr);
/// printf("number: %d\n", nickel_expr_is_number(field));
/// ```
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_alloc() -> *mut nickel_expr {
    Box::into_raw(Box::new(nickel_expr {
        inner: Expr {
            value: NickelValue::null(),
        },
    }))
}

/// Free a Nickel expression.
///
/// See [`nickel_expr_alloc`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_free(expr: *mut nickel_expr) {
    // Safety: `expr` is required to be a valid pointer to a box-allocated expression, as returned
    // by `nickel_expr_alloc`.
    let _ = unsafe { Box::from_raw(expr) };
}

/// Is this expression a boolean?
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_is_bool(expr: *const nickel_expr) -> c_int {
    // Safety: `expr` is expected to be a valid pointer to an expression
    unsafe { nickel_expr::as_rust(&expr).is_bool() as c_int }
}

/// Is this expression a number?
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_is_number(expr: *const nickel_expr) -> c_int {
    // Safety: `expr` is expected to be a valid pointer to an expression
    unsafe { nickel_expr::as_rust(&expr).is_number() as c_int }
}

/// Is this expression a string?
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_is_str(expr: *const nickel_expr) -> c_int {
    // Safety: `expr` is expected to be a valid pointer to an expression
    unsafe { nickel_expr::as_rust(&expr).is_string() as c_int }
}

/// Is this expression an enum tag?
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_is_enum_tag(expr: *const nickel_expr) -> c_int {
    // Safety: `expr` is expected to be a valid pointer to an expression
    unsafe { nickel_expr::as_rust(&expr).is_enum_tag() as c_int }
}

/// Is this expression an enum variant?
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_is_enum_variant(expr: *const nickel_expr) -> c_int {
    // Safety: `expr` is expected to be a valid pointer to an expression
    unsafe { nickel_expr::as_rust(&expr).is_enum_variant() as c_int }
}

/// Is this expression a record?
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_is_record(expr: *const nickel_expr) -> c_int {
    // Safety: `expr` is expected to be a valid pointer to an expression
    unsafe { nickel_expr::as_rust(&expr).is_record() as c_int }
}

/// Is this expression an array?
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_is_array(expr: *const nickel_expr) -> c_int {
    // Safety: `expr` is expected to be a valid pointer to an expression
    unsafe { nickel_expr::as_rust(&expr).is_array() as c_int }
}

/// Has this expression been evaluated?
///
/// An evaluated expression is either null, or it's a number, bool, string,
/// record, array, or enum. If this expression is not a value, you probably
/// got it from looking inside the result of [`nickel_context_eval_shallow`],
/// and you can use the [`VirtualMachine`] you got from
/// `nickel_context_eval_shallow` to evaluate this expression further.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_is_value(expr: *const nickel_expr) -> c_int {
    // Safety: `expr` is expected to be a valid pointer to an expression
    unsafe { nickel_expr::as_rust(&expr).is_value() as c_int }
}

/// Is this expression null?
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_is_null(expr: *const nickel_expr) -> c_int {
    // Safety: `expr` is expected to be a valid pointer to an expression
    unsafe { nickel_expr::as_rust(&expr).is_null() as c_int }
}

/// If this expression is a boolean, returns that boolean.
///
/// # Panics
///
/// Panics if `expr` is not a boolean.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_as_bool(expr: *const nickel_expr) -> c_int {
    // Safety: `expr` is expected to be a valid pointer to an expression
    unsafe { nickel_expr::as_rust(&expr).as_bool().unwrap() as c_int }
}

/// If this expression is a string, returns that string.
///
/// A pointer to the string contents, which are UTF-8 encoded, is returned in
/// `out_str`. These contents are *not* null-terminated. The return value of this
/// function is the length of these contents.
///
/// The returned string contents are owned by this `Expr`, and will be invalidated
/// when the `Expr` is freed with [`nickel_expr_free`].
///
/// # Panics
///
/// Panics if `expr` is not a string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_as_str(
    expr: *const nickel_expr,
    out_str: *mut *const c_char,
) -> usize {
    // Safety: `expr` is expected to be a valid pointer to an expression
    let s = unsafe { nickel_expr::as_rust(&expr).as_str().unwrap() };
    // Safety: `out_str` is a valid C string.
    unsafe {
        *out_str = s.as_ptr() as *const c_char;
    }
    s.len()
}

/// If this expression is a number, returns the number.
///
/// The returned number pointer borrows from `expr`, and will be invalidated
/// when `expr` is overwritten or freed.
///
/// # Panics
///
/// Panics if `expr` is not an number.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_as_number(expr: *const nickel_expr) -> *const nickel_number {
    // Safety: `expr` is expected to be a valid pointer to an expression
    unsafe { nickel_expr::as_rust(&expr).as_number().unwrap().into() }
}

/// If this expression is an enum tag, returns its string value.
///
/// A pointer to the string contents, which are UTF-8 encoded, is returned in
/// `out_str`. These contents are *not* null-terminated. The return value of this
/// function is the length of these contents.
///
/// The returned string contents point to an interned string and will never be
/// invalidated.
///
/// # Panics
///
/// Panics if `expr` is null or is not an enum tag.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_as_enum_tag(
    expr: *const nickel_expr,
    out_str: *mut *const c_char,
) -> usize {
    // Safety: `expr` is expected to be a valid pointer to an expression
    let s = unsafe { nickel_expr::as_rust(&expr).as_enum_tag().unwrap() };
    // Safety: `out_str` is a valid C string.
    unsafe { *out_str = s.as_ptr() as *const c_char };
    s.len()
}

/// If this expression is an enum variant, returns its string value and its payload.
///
/// A pointer to the string contents, which are UTF-8 encoded, is returned in
/// `out_str`. These contents are *not* null-terminated. The return value of this
/// function is the length of these contents.
///
/// The returned string contents point to an interned string and will never be
/// invalidated.
///
/// # Panics
///
/// Panics if `expr` is not an enum tag.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_as_enum_variant(
    expr: *const nickel_expr,
    out_str: *mut *const c_char,
    mut out_expr: *mut nickel_expr,
) -> usize {
    // Safety: pre-conditions of this function.
    unsafe {
        let (s, payload) = nickel_expr::as_rust(&expr).as_enum_variant().unwrap();
        *out_str = s.as_ptr() as *const c_char;
        *nickel_expr::as_rust_mut(&mut out_expr) = payload;
        s.len()
    }
}

/// If this expression is a record, returns the record.
///
/// The returned record pointer borrows from `expr`, and will be invalidated
/// when `expr` is overwritten or freed.
///
/// # Panics
///
/// Panics if `expr` is not an record.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_as_record(expr: *const nickel_expr) -> *const nickel_record {
    // Safety: `expr` is expected to be a valid pointer to an expression
    unsafe { nickel_expr::as_rust(&expr).as_record().unwrap().into() }
}

/// If this expression is an array, returns the array.
///
/// The returned array pointer borrows from `expr`, and will be invalidated
/// when `expr` is overwritten or freed.
///
/// # Panics
///
/// Panics if `expr` is not an array.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_expr_as_array(expr: *const nickel_expr) -> *const nickel_array {
    // Safety: `expr` is expected to be a valid pointer to an expression
    unsafe { nickel_expr::as_rust(&expr).as_array().unwrap().into() }
}

/// Converts a Rust result into a reasonable C format.
///
/// # Safety
///
/// `out_string` and `out_err` must be valid must be valid (i.e. allocated by
/// the corresponding `nickel_xxx_alloc` function) or null.
unsafe fn export_result(
    result: Result<String, Error>,
    out_string: *mut nickel_string,
    out_err: *mut nickel_error,
) -> nickel_result {
    match result {
        Ok(s) => {
            if !out_string.is_null() {
                // Safety: `out_string` is a valid pointer to `nickel_string`.
                unsafe { (*out_string).inner = s };
            }
            nickel_result::NICKEL_RESULT_OK
        }
        Err(e) => {
            if !out_err.is_null() {
                // Safety: `out_err` is a valid pointer to `nickel_error`.
                unsafe { (*out_err).inner = Some(e) };
            }
            nickel_result::NICKEL_RESULT_ERR
        }
    }
}

/// Converts an expression to JSON.
///
/// This is fallible because enum variants have no canonical conversion to
/// JSON: if the expression contains any enum variants, this will fail.
/// This also fails if the expression contains any unevaluated sub-expressions.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_context_to_json(
    mut ctx: *mut nickel_context,
    expr: *const nickel_expr,
    out_string: *mut nickel_string,
    out_err: *mut nickel_error,
) -> nickel_result {
    // Safety: function pre-conditions
    unsafe {
        export_result(
            nickel_context::as_rust_mut(&mut ctx).to_json(nickel_expr::as_rust(&expr)),
            out_string,
            out_err,
        )
    }
}

/// Converts an expression to YAML.
///
/// This is fallible because enum variants have no canonical conversion to
/// YAML: if the expression contains any enum variants, this will fail.
/// This also fails if the expression contains any unevaluated sub-expressions.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_context_to_yaml(
    mut ctx: *mut nickel_context,
    expr: *const nickel_expr,
    out_string: *mut nickel_string,
    out_err: *mut nickel_error,
) -> nickel_result {
    // Safety: function pre-conditions
    unsafe {
        export_result(
            nickel_context::as_rust_mut(&mut ctx).to_yaml(nickel_expr::as_rust(&expr)),
            out_string,
            out_err,
        )
    }
}

/// Converts an expression to TOML.
///
/// This is fallible because enum variants have no canonical conversion to
/// TOML: if the expression contains any enum variants, this will fail.
/// This also fails if the expression contains any unevaluated sub-expressions.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_context_to_toml(
    mut ctx: *mut nickel_context,
    expr: *const nickel_expr,
    out_string: *mut nickel_string,
    out_err: *mut nickel_error,
) -> nickel_result {
    // Safety: function pre-conditions
    unsafe {
        export_result(
            nickel_context::as_rust_mut(&mut ctx).to_toml(nickel_expr::as_rust(&expr)),
            out_string,
            out_err,
        )
    }
}

/// Is this number an integer within the range of an `int64_t`?
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_number_is_i64(num: *const nickel_number) -> c_int {
    // Safety: `num` is expected to be a valid pointer to `nickel_number`
    unsafe { nickel_number::as_rust(&num).as_i64().is_some() as c_int }
}

/// If this number is an integer within the range of an `int64_t`, returns it.
///
/// # Panics
///
/// Panics if this number is not an integer in the appropriate range (you should
/// check with [`nickel_number_is_i64`] first).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_number_as_i64(num: *const nickel_number) -> i64 {
    // Safety: `num` is expected to be a valid pointer to `nickel_number`
    unsafe { nickel_number::as_rust(&num).as_i64().unwrap() }
}

/// The value of this number, rounded to the nearest `double`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_number_as_f64(num: *const nickel_number) -> f64 {
    // Safety: `num` is expected to be a valid pointer to `nickel_number`
    unsafe { nickel_number::as_rust(&num).as_f64() }
}

/// The value of this number, as an exact rational number.
///
/// - `out_numerator` must have been allocated with [`nickel_string_alloc`]. It
///   will be overwritten with the numerator, as a decimal string.
/// - `out_denominator` must have been allocated with [`nickel_string_alloc`].
///   It will be overwritten with the denominator, as a decimal string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_number_as_rational(
    num: *const nickel_number,
    out_numerator: *mut nickel_string,
    out_denominator: *mut nickel_string,
) {
    // Safety: pre-conditions of this function
    unsafe {
        let (numerator, denominator) = nickel_number::as_rust(&num).as_rational();
        *out_numerator = nickel_string { inner: numerator };
        *out_denominator = nickel_string { inner: denominator };
    }
}

/// The number of elements of this Nickel array.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_array_len(arr: *const nickel_array) -> usize {
    // Safety: `arr` is expected to be a valid pointer to a nickel array, or `null`, which is
    // handled by `nickel_array::as_rust`.
    unsafe { nickel_array::as_rust(&arr).len() }
}

/// Retrieve the element at the given array index.
///
/// The retrieved element will be written to `out_expr`, which must have been allocated with
/// [`nickel_expr_alloc`].
///
/// # Panics
///
/// Panics if the given index is out of bounds.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_array_get(
    arr: *const nickel_array,
    idx: usize,
    mut out_expr: *mut nickel_expr,
) {
    // Safety: `arr` is expected to be a valid pointer to a nickel array, or `null`, which is
    // handled by `nickel_array::as_rust`.
    //
    // `out_expr` is required to be a valid pointer to an allocated `nickel_expr`.
    unsafe {
        *nickel_expr::as_rust_mut(&mut out_expr) = nickel_array::as_rust(&arr).get(idx).unwrap()
    }
}

/// The number of keys in this Nickel record.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_record_len(rec: *const nickel_record) -> usize {
    // Safety: `rec` is expected to be a valid pointer to a `nickel_record`, or `null`, which is
    // handled by `nickel_record::as_rust`.
    unsafe { nickel_record::as_rust(&rec).len() }
}

/// Retrieve the key and value at the given index.
///
/// If this record was deeply evaluated, every key will come with a value.
/// However, shallowly evaluated records may have fields with no value.
///
/// Returns 1 if the key came with a value, and 0 if it didn't. The value
/// will be written to `out_expr` if it is non-NULL.
///
/// # Panics
///
/// Panics if `idx` is out of range.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_record_key_value_by_index(
    rec: *const nickel_record,
    idx: usize,
    out_key: *mut *const c_char,
    out_key_len: *mut usize,
    mut out_expr: *mut nickel_expr,
) -> c_int {
    // Safety: pre-conditions of this function
    unsafe {
        let rec = nickel_record::as_rust(&rec);
        let (key, val) = rec.key_value_by_index(idx).unwrap();
        *out_key = key.as_ptr() as *const c_char;
        *out_key_len = key.len();
        if let Some(val) = val {
            if !out_expr.is_null() {
                *nickel_expr::as_rust_mut(&mut out_expr) = val;
            }
            1
        } else {
            0
        }
    }
}

/// Look up a key in this record and return its value, if there is one.
///
/// Returns 1 if the key has a value, and 0 if it didn't. The value is
/// written to `out_expr` if it is non-NULL.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_record_value_by_name(
    rec: *const nickel_record,
    key: *const c_char,
    mut out_expr: *mut nickel_expr,
) -> c_int {
    // Safety: pre-conditions of this function
    unsafe {
        let rec = nickel_record::as_rust(&rec);
        let key = CStr::from_ptr(key).to_str().unwrap();
        if let Some(expr) = rec.value_by_name(key) {
            if !out_expr.is_null() {
                *nickel_expr::as_rust_mut(&mut out_expr) = expr;
            }
            1
        } else {
            0
        }
    }
}

/// Allocates a new string.
///
/// The lifecycle management of a string is much like that of an expression
/// (see `nickel_expr_alloc`). It gets allocated here, modified by various other
/// functions, and finally is freed by a call to `nickel_string_free`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_string_alloc() -> *mut nickel_string {
    Box::into_raw(Box::new(nickel_string {
        inner: String::new(),
    }))
}

/// Frees a string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_string_free(s: *mut nickel_string) {
    // Safety: `s` is required to be a valid pointer to a box-allocated string, as returned by
    // `nickel_string_alloc`.
    let _ = unsafe { Box::from_raw(s) };
}

/// Retrieve the data inside a string.
///
/// A pointer to the string contents, which are UTF-8 encoded, is written to
/// `data`. These contents are *not* null-terminated, but their length (in bytes)
/// is written to `len`. The string contents will be invalidated when `s` is
/// freed or overwritten.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_string_data(
    s: *const nickel_string,
    data: *mut *const c_char,
    len: *mut usize,
) {
    // Safety: pre-conditions of this function
    unsafe {
        let s = s.as_ref().unwrap();
        *data = s.inner.as_ptr() as *const c_char;
        *len = s.inner.len();
    }
}

/// Evaluate an expression to weak head normal form (WHNF).
///
/// This has no effect if the expression is already evaluated (see
/// [`nickel_expr_is_value`]).
///
/// The result of this evaluation is a null, bool, number, string,
/// enum, record, or array. In case it's a record, array, or enum
/// variant, the payload (record values, array elements, or enum
/// payloads) will be left unevaluated.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_context_eval_expr_shallow(
    mut ctx: *mut nickel_context,
    expr: *const nickel_expr,
    mut out_expr: *mut nickel_expr,
    out_error: *mut nickel_error,
) -> nickel_result {
    // We clone `expr` instead of consuming it (as the rust API does). The clone is
    // cheap (it's only a refcount bump) and this makes the allocation/free pairing
    // easier to get right: only `nickel_expr_alloc` creates an expr and only `nickel_expr_free`
    // frees one.
    //
    // unwraps:
    //  - we assume vm is non-null
    //  - we assume vm was previously overwritten by nickel_context_eval_shallow, so its
    //    inner value is Some

    // Safety: pre-conditions of this function
    unsafe {
        match nickel_context::as_rust_mut(&mut ctx)
            .eval_expr_shallow(nickel_expr::as_rust(&expr).clone())
        {
            Ok(out) => {
                if !out_expr.is_null() {
                    *nickel_expr::as_rust_mut(&mut out_expr) = out;
                }
                nickel_result::NICKEL_RESULT_OK
            }
            Err(e) => {
                if !out_error.is_null() {
                    (*out_error).inner = Some(e);
                }
                nickel_result::NICKEL_RESULT_ERR
            }
        }
    }
}

/// Allocate a new `nickel_error`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_error_alloc() -> *mut nickel_error {
    Box::into_raw(Box::new(nickel_error { inner: None }))
}

/// Frees a `nickel_error`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_error_free(err: *mut nickel_error) {
    // Safety: `err` is required to be a valid pointer to a box-allocated error, as
    // returned by `nickel_error_alloc`.
    let _ = unsafe { Box::from_raw(err) };
}

/// Write out an error as a user- or machine-readable diagnostic.
///
/// - `err` must have been allocated by `nickel_error_alloc` and initialized by some failing
///   function (like `nickel_context_eval_deep`).
/// - `write` is a callback function that will be invoked with UTF-8 encoded data.
/// - `write_payload` is optional extra data to pass to `write`
/// - `format` selects the error-rendering format.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_error_display(
    err: *const nickel_error,
    write: nickel_write_callback,
    write_payload: *mut c_void,
    format: nickel_error_format,
) -> nickel_result {
    // Safety: `err` is required to be valid pointer to `nickel_error`.
    let err = unsafe { err.as_ref() }
        .unwrap()
        .inner
        .as_ref()
        .expect("uninitialized error");
    let mut write = CTrace {
        write,
        flush: None,
        context: write_payload,
    };
    if err.format(&mut write, format.into()).is_err() {
        nickel_result::NICKEL_RESULT_ERR
    } else {
        nickel_result::NICKEL_RESULT_OK
    }
}

/// Write out an error as a user- or machine-readable diagnostic.
///
/// This is like `nickel_error_format`, but writes the error to a string instead
/// of via a callback function.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn nickel_error_format_as_string(
    err: *const nickel_error,
    out_string: *mut nickel_string,
    format: nickel_error_format,
) -> nickel_result {
    // Safety: `err` is required to be valid pointer to `nickel_error`.
    let err = unsafe { err.as_ref() }
        .unwrap()
        .inner
        .as_ref()
        .expect("uninitialized error");
    let mut out = Vec::new();
    if err.format(&mut out, format.into()).is_err() {
        nickel_result::NICKEL_RESULT_ERR
    } else {
        if !out_string.is_null() {
            // unwrap: Nickel's formatting always produces UTF-8.
            // Safety: `out_string` is required to be valid pointer to `nickel_string`
            unsafe {
                (*out_string).inner = String::from_utf8(out).unwrap();
            }
        }
        nickel_result::NICKEL_RESULT_OK
    }
}
