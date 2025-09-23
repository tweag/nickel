//! C bindings for the Nickel language
//!
//! TODO: example

#![allow(clippy::missing_safety_doc)]
#![allow(non_camel_case_types)]
#![deny(missing_docs)]

use std::{
    ffi::{c_char, c_int, c_void, CStr},
    io::Write,
};

use nickel_lang_core::term::Term;

use crate::{Array, Context, Error, Expr, Number, Record, Trace, VirtualMachine};

// TODO: put wrapper structs around Error
// just so that we can have C-API-specific documentation for it
// TODO: serialization functions
// TODO: error reporting

/// The main entry point.
pub struct nickel_context {}

impl nickel_context {
    unsafe fn as_rust_mut(this: &mut *mut Self) -> &mut Context {
        (*this as *mut Context).as_mut().unwrap()
    }
}

/// A Nickel expression.
///
/// This might be fully evaluated (for example, if you got it from [`nickel_context_eval_deep`])
/// or might have unevaluated sub-expressions (if you got it from [`nickel_context_eval_shallow`]).
// This is only used to hide `Expr`, and to attach C-API-specific documentation to.
// We only ever use pointers to `nickel_expr`, which are secretly pointers to `Expr`.
pub struct nickel_expr {}

impl nickel_expr {
    /// Convert a *nickel_expr to a &Expr.
    ///
    /// # Safety
    ///
    /// Assumes that `this` was originally a valid pointer to Expr.
    unsafe fn as_rust(this: &*const Self) -> &Expr {
        (*this as *const Expr).as_ref().unwrap()
    }

    /// Convert a *mut nickel_expr to a &mut Expr.
    ///
    /// # Safety
    ///
    /// Assumes that `this` was originally a valid pointer to Expr.
    unsafe fn as_rust_mut(this: &mut *mut Self) -> &mut Expr {
        (*this as *mut Expr).as_mut().unwrap()
    }
}

/// A Nickel array.
///
/// See [`nickel_expr_is_array`] and [`nickel_expr_as_array`].
// This is not really an empty struct, it's just an opaque type to
// hide the implementation. The C API only ever uses `*const Array`,
// and they are secretly pointers to `nickel_lang_core::term::array::Array`.
pub struct nickel_array {}

impl nickel_array {
    unsafe fn as_rust(this: &*const Self) -> Array<'_> {
        Array {
            array: (*this as *const nickel_lang_core::term::array::Array)
                .as_ref()
                .unwrap(),
        }
    }
}

impl<'a> From<Array<'a>> for *const nickel_array {
    fn from(arr: Array<'a>) -> Self {
        arr.array as *const _ as *const nickel_array
    }
}

/// A Nickel record.
///
/// See [`nickel_expr_is_record`] and [`nickel_expr_as_record`].
// This is not really an empty struct, it's just an opaque type to
// hide the implementation. The C API only ever uses `*const Record`,
// and they are secretly pointers to `nickel_lang_core::term::record::RecordData`.
pub struct nickel_record {}

impl nickel_record {
    unsafe fn as_rust(this: &*const Self) -> Record<'_> {
        Record {
            data: (*this as *const nickel_lang_core::term::record::RecordData)
                .as_ref()
                .unwrap(),
        }
    }
}

impl<'a> From<Record<'a>> for *const nickel_record {
    fn from(rec: Record<'a>) -> Self {
        rec.data as *const _ as *const nickel_record
    }
}

/// A Nickel number.
///
/// See [`nickel_expr_is_number`] and [`nickel_expr_as_number`].
// This is not really an empty struct, it's just an opaque type to
// hide the implementation. The C API only ever uses `*const Number`,
// and they are secretly pointers to `nickel_lang_core::term::Number`.
pub struct nickel_number {}

impl nickel_number {
    unsafe fn as_rust(this: &*const Self) -> Number<'_> {
        Number {
            num: (*this as *const nickel_lang_core::term::Number)
                .as_ref()
                .unwrap(),
        }
    }
}

impl<'a> From<Number<'a>> for *const nickel_number {
    fn from(n: Number<'a>) -> Self {
        n.num as *const _ as *const nickel_number
    }
}

/// A Nickel virtual machine, which can be used for evaluating partially-evaluated expressions.
pub struct nickel_virtual_machine {
    inner: Option<VirtualMachine>,
}

/// Allocate a new [`nickel_context`], which can be used to evaluate Nickel expressions.
///
/// Returns a newly-allocated [`nickel_context`] that can be freed with [`nickel_context_free`].
#[no_mangle]
pub unsafe extern "C" fn nickel_context_alloc() -> *mut nickel_context {
    Box::into_raw(Box::new(Context::default())) as *mut nickel_context
}

/// Free a [`nickel_context`] that was created with [`nickel_context_alloc`].
#[no_mangle]
pub unsafe extern "C" fn nickel_context_free(ctx: *mut nickel_context) {
    let _ = Box::from_raw(ctx as *mut Context);
}

/// A callback function for writing data.
///
/// This function will be called with a buffer (`buf`) of data, having length
/// `len`. It need not consume the entire buffer, and should return the number
/// of bytes consumed.
pub type nickel_write_callback =
    extern "C" fn(context: *const c_void, buf: *const u8, len: usize) -> usize;

/// A callback function for flushing data that was written by a write callback.
pub type nickel_flush_callback = extern "C" fn(context: *const c_void);

/// For functions that can fail, these are the interpretations of the return value.
#[repr(C)]
pub enum nickel_result {
    /// A successful result.
    NICKEL_RESULT_OK = 0,
    /// A bad result.
    NICKEL_RESULT_ERR = 1,
}

struct CTrace {
    write: nickel_write_callback,
    flush: Option<nickel_flush_callback>,
    context: *const c_void,
}

impl Write for CTrace {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let count = (self.write)(self.context, buf.as_ptr(), buf.len());
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
#[no_mangle]
pub unsafe extern "C" fn nickel_context_set_trace_callback(
    mut ctx: *mut nickel_context,
    write: nickel_write_callback,
    // TODO: if this is non-optional, are they allowed to pass NULL?
    flush: Option<nickel_flush_callback>,
    user_data: *const c_void,
) {
    let trace = Trace::new(CTrace {
        write,
        flush,
        context: user_data,
    });
    nickel_context::as_rust_mut(&mut ctx).trace = trace;
}

/// Provide a name for the main input program.
///
/// This is used to format error messages. If you read the main input
/// program from a file, its path is a good choice.
///
/// `name` should be a UTF-8-encoded, nul-terminated string. It is only
/// borrowed temporarily; the pointer need not remain valid.
#[no_mangle]
pub unsafe extern "C" fn nickel_context_set_source_name(
    mut ctx: *mut nickel_context,
    name: *const c_char,
) {
    let name = String::from_utf8_lossy(CStr::from_ptr(name).to_bytes()).into_owned();
    nickel_context::as_rust_mut(&mut ctx).name = Some(name);
}

/// Perform some sort of evaluation, and return the error appropriately.
unsafe fn do_eval<F: FnOnce(&mut crate::Context, &str) -> Result<Expr, Error>>(
    f: F,
    mut ctx: *mut nickel_context,
    src: *const c_char,
    mut out_expr: *mut nickel_expr,
    out_error: *mut *mut Error,
) -> nickel_result {
    let src = CStr::from_ptr(src).to_str().unwrap();
    match f(nickel_context::as_rust_mut(&mut ctx), src) {
        Ok(expr) => {
            if !out_expr.is_null() {
                *nickel_expr::as_rust_mut(&mut out_expr) = expr;
            }
            nickel_result::NICKEL_RESULT_OK
        }
        Err(e) => {
            if !out_error.is_null() {
                *out_error = Box::into_raw(Box::new(e));
            }
            nickel_result::NICKEL_RESULT_ERR
        }
    }
}

/// Evaluate a Nickel program deeply.
///
/// "Deeply" means that we recursively evaluate records and arrays. For
/// an alternative, see [`nickel_context_eval_shallow`].
///
/// - `src` is a nul-terminated string containing UTF-8-encoded Nickel source.
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
#[no_mangle]
pub unsafe extern "C" fn nickel_context_eval_deep(
    ctx: *mut nickel_context,
    src: *const c_char,
    out_expr: *mut nickel_expr,
    out_error: *mut *mut Error,
) -> nickel_result {
    do_eval(|ctx, src| ctx.eval_deep(src), ctx, src, out_expr, out_error)
}

/// Evaluate a Nickel program deeply.
///
/// This differs from [`nickel_context_eval_deep`] in that it ignores
/// fields marked as `not_exported`.
///
/// - `src` is a nul-terminated string containing UTF-8-encoded Nickel source.
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
#[no_mangle]
pub unsafe extern "C" fn nickel_context_eval_deep_for_export(
    ctx: *mut nickel_context,
    src: *const c_char,
    out_expr: *mut nickel_expr,
    out_error: *mut *mut Error,
) -> nickel_result {
    do_eval(
        |ctx, src| ctx.eval_deep_for_export(src),
        ctx,
        src,
        out_expr,
        out_error,
    )
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
///
/// - `src` is a nul-terminated string containing UTF-8-encoded Nickel source.
/// - `out_expr` is either NULL or something that was created with [`nickel_expr_alloc`]
/// - `out_error` can be NULL if you aren't interested in getting detailed
///   error messages
/// - `out_virtual_machine` is either NULL or something that was created
///   with [`nickel_virtual_machine_alloc`]
///
/// If evaluation is successful, returns `NICKEL_RESULT_OK` and replaces
/// the value at `out_expr` (if non-NULL) with the newly-evaluated Nickel expression,
/// and the value at `out_virtual_machine` (if non-NULL) with a virtual machine
/// that can be used for further evaluation.
///
/// If evaluation fails, returns `NICKEL_RESULT_ERR` and replaces the
/// value at `out_error` (if non-NULL) by a pointer to a newly-allocated Nickel error.
/// That error should be freed with `nickel_error_free` when you are
/// done with it.
#[no_mangle]
pub unsafe extern "C" fn nickel_context_eval_shallow(
    mut ctx: *mut nickel_context,
    src: *const c_char,
    mut out_expr: *mut nickel_expr,
    out_virtual_machine: *mut nickel_virtual_machine,
    out_error: *mut *mut Error,
) -> nickel_result {
    let src = CStr::from_ptr(src).to_str().unwrap();
    match nickel_context::as_rust_mut(&mut ctx).eval_shallow(src) {
        Ok((vm, expr)) => {
            if !out_expr.is_null() {
                *nickel_expr::as_rust_mut(&mut out_expr) = expr;
            }
            if !out_virtual_machine.is_null() {
                *out_virtual_machine = nickel_virtual_machine { inner: Some(vm) };
            }
            nickel_result::NICKEL_RESULT_OK
        }
        Err(e) => {
            if !out_error.is_null() {
                *out_error = Box::into_raw(Box::new(e));
            }
            nickel_result::NICKEL_RESULT_ERR
        }
    }
}

/// Frees a Nickel error message.
#[no_mangle]
pub unsafe extern "C" fn nickel_error_free(err: *mut Error) {
    let _ = Box::from_raw(err);
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
/// /* now expr is a record */
/// printf("record: %d\n", nickel_expr_is_record(expr));
///
/// nickel_context_eval_deep(ctx, "[1, 2, 3]", expr, NULL);
///
/// /* now expr is an array */
/// printf("array: %d\n", nickel_expr_is_array(expr));
///
/// /* the calls to nickel_context_eval_deep haven't created any new exprs:
///    we only need to free it once */
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
/// /* Now `rec` points to data owned by `expr`, but `field`
///    owns its own data. The following deallocation invalidates
///    `rec`, but not `field`. */
/// nickel_expr_free(expr);
/// printf("number: %d\n", nickel_expr_is_number(field));
/// ```
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_alloc() -> *mut nickel_expr {
    Box::into_raw(Box::new(Expr {
        rt: Term::Null.into(),
    })) as *mut nickel_expr
}

/// Free a Nickel expression.
///
/// See [`nickel_expr_alloc`].
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_free(expr: *mut nickel_expr) {
    let _ = Box::from_raw(expr as *mut Expr);
}

/// Is this expression a boolean?
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_is_bool(expr: *const nickel_expr) -> c_int {
    nickel_expr::as_rust(&expr).is_bool() as c_int
}

/// Is this expression a number?
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_is_number(expr: *const nickel_expr) -> c_int {
    nickel_expr::as_rust(&expr).is_num() as c_int
}

/// Is this expression a string?
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_is_str(expr: *const nickel_expr) -> c_int {
    nickel_expr::as_rust(&expr).is_str() as c_int
}

/// Is this expression an enum tag?
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_is_enum_tag(expr: *const nickel_expr) -> c_int {
    nickel_expr::as_rust(&expr).is_enum_tag() as c_int
}

/// Is this expression an enum variant?
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_is_enum_variant(expr: *const nickel_expr) -> c_int {
    nickel_expr::as_rust(&expr).is_enum_variant() as c_int
}

/// Is this expression a record?
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_is_record(expr: *const nickel_expr) -> c_int {
    nickel_expr::as_rust(&expr).is_record() as c_int
}

/// Is this expression an array?
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_is_array(expr: *const nickel_expr) -> c_int {
    nickel_expr::as_rust(&expr).is_array() as c_int
}

/// Has this expression been evaluated?
///
/// An evaluated expression is either null, or it's a number, bool, string,
/// record, array, or enum. If this expression is not a value, you probably
/// got it from looking inside the result of [`nickel_context_eval_shallow`],
/// and you can use the [`VirtualMachine`] you got from
/// `nickel_context_eval_shallow` to evaluate this expression further.
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_is_value(expr: *const nickel_expr) -> c_int {
    nickel_expr::as_rust(&expr).is_value() as c_int
}

/// Is this expression null?
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_is_null(expr: *const nickel_expr) -> c_int {
    nickel_expr::as_rust(&expr).is_null() as c_int
}

/// If this expression is a boolean, returns that boolean.
///
/// # Panics
///
/// Panics if `expr` is not a boolean.
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_as_bool(expr: *const nickel_expr) -> c_int {
    nickel_expr::as_rust(&expr).as_bool().unwrap() as c_int
}

/// If this expression is a string, returns that string.
///
/// A pointer to the string contents, which are UTF-8 encoded, is returned in
/// `out_str`. These contents are *not* nul-terminated. The return value of this
/// function is the length of these contents.
///
/// The returned string contents are owned by this `Expr`, and will be invalidated
/// when the `Expr` is freed with [`nickel_expr_free`].
///
/// # Panics
///
/// Panics if `expr` is not a string.
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_as_str(
    expr: *const nickel_expr,
    out_str: *mut *const c_char,
) -> usize {
    let s = nickel_expr::as_rust(&expr).as_str().unwrap();
    *out_str = s.as_ptr() as *const c_char;
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
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_as_number(expr: *const nickel_expr) -> *const nickel_number {
    nickel_expr::as_rust(&expr).as_num().unwrap().into()
}

/// If this expression is an enum tag, returns its string value.
///
/// A pointer to the string contents, which are UTF-8 encoded, is returned in
/// `out_str`. These contents are *not* nul-terminated. The return value of this
/// function is the length of these contents.
///
/// The returned string contents point to an interned string and will never be
/// invalidated.
///
/// # Panics
///
/// Panics if `expr` is null or is not an enum tag.
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_as_enum_tag(
    expr: *const nickel_expr,
    out_str: *mut *const c_char,
) -> usize {
    let s = nickel_expr::as_rust(&expr).as_enum_tag().unwrap();
    *out_str = s.as_ptr() as *const c_char;
    s.len()
}

/// If this expression is an enum variant, returns its string value and its payload.
///
/// A pointer to the string contents, which are UTF-8 encoded, is returned in
/// `out_str`. These contents are *not* nul-terminated. The return value of this
/// function is the length of these contents.
///
/// The returned string contents point to an interned string and will never be
/// invalidated.
///
/// # Panics
///
/// Panics if `expr` is not an enum tag.
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_as_enum_variant(
    expr: *const nickel_expr,
    out_str: *mut *const c_char,
    mut out_expr: *mut nickel_expr,
) -> usize {
    let (s, payload) = nickel_expr::as_rust(&expr).as_enum_variant().unwrap();
    *out_str = s.as_ptr() as *const c_char;
    *nickel_expr::as_rust_mut(&mut out_expr) = payload;
    s.len()
}

/// If this expression is a record, returns the record.
///
/// The returned record pointer borrows from `expr`, and will be invalidated
/// when `expr` is overwritten or freed.
///
/// # Panics
///
/// Panics if `expr` is not an record.
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_as_record(expr: *const nickel_expr) -> *const nickel_record {
    nickel_expr::as_rust(&expr).as_record().unwrap().into()
}

/// If this expression is an array, returns the array.
///
/// The returned array pointer borrows from `expr`, and will be invalidated
/// when `expr` is overwritten or freed.
///
/// # Panics
///
/// Panics if `expr` is not an array.
#[no_mangle]
pub unsafe extern "C" fn nickel_expr_as_array(expr: *const nickel_expr) -> *const nickel_array {
    nickel_expr::as_rust(&expr).as_array().unwrap().into()
}

/// Is this number an integer within the range of an `int64_t`?
#[no_mangle]
pub unsafe extern "C" fn nickel_number_is_i64(num: *const nickel_number) -> c_int {
    nickel_number::as_rust(&num).as_i64().is_some() as c_int
}

/// If this number is an integer within the range of an `int64_t`, returns it.
///
/// # Panics
///
/// Panics if this number is not an integer in the appropriate range (you should
/// check with [`nickel_number_is_i64`] first).
#[no_mangle]
pub unsafe extern "C" fn nickel_number_as_i64(num: *const nickel_number) -> i64 {
    nickel_number::as_rust(&num).as_i64().unwrap()
}

/// The value of this number, rounded to the nearest `double`.
#[no_mangle]
pub unsafe extern "C" fn nickel_number_as_f64(num: *const nickel_number) -> f64 {
    nickel_number::as_rust(&num).as_f64()
}

/// The value of this number, as an exact rational number.
///
/// - `out_numerator` must have been allocated with [`nickel_string_alloc`]. It
///   will be overwritten with the numerator, as a decimal string.
/// - `out_denominator` must have been allocated with [`nickel_string_alloc`].
///   It will be overwritten with the denominator, as a decimal string.
#[no_mangle]
pub unsafe extern "C" fn nickel_number_as_rational(
    num: *const nickel_number,
    out_numerator: *mut String,
    out_denominator: *mut String,
) {
    let (numerator, denominator) = nickel_number::as_rust(&num).as_rational();
    *out_numerator = numerator;
    *out_denominator = denominator;
}

/// The number of elements of this Nickel array.
#[no_mangle]
pub unsafe extern "C" fn nickel_array_len(arr: *const nickel_array) -> usize {
    nickel_array::as_rust(&arr).len()
}

/// Retrieve the element at the given array index.
///
/// The retrieved element will be written to `out_expr`, which must have been allocated with
/// [`nickel_expr_alloc`].
///
/// # Panics
///
/// Panics if the given index is out of bounds.
#[no_mangle]
pub unsafe extern "C" fn nickel_array_get(
    arr: *const nickel_array,
    idx: usize,
    mut out_expr: *mut nickel_expr,
) {
    *nickel_expr::as_rust_mut(&mut out_expr) = nickel_array::as_rust(&arr).get(idx).unwrap()
}

/// The number of keys in this Nickel record.
#[no_mangle]
pub unsafe extern "C" fn nickel_record_len(rec: *const nickel_record) -> usize {
    nickel_record::as_rust(&rec).len()
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
#[no_mangle]
pub unsafe extern "C" fn nickel_record_key_value_by_index(
    rec: *const nickel_record,
    idx: usize,
    out_key: *mut *const c_char,
    out_key_len: *mut usize,
    mut out_expr: *mut nickel_expr,
) -> c_int {
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

/// Look up a key in this record and return its value, if there is one.
///
/// Returns 1 if the key has a value, and 0 if it didn't. The value is
/// written to `out_expr` if it is non-NULL.
#[no_mangle]
pub unsafe extern "C" fn nickel_record_value_by_name(
    rec: *const nickel_record,
    key: *const c_char,
    mut out_expr: *mut nickel_expr,
) -> c_int {
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

/// Allocates a new string.
///
/// The lifecycle management of a string is much like that of an expression
/// (see `nickel_expr_alloc`). It gets allocated here, modified by various other
/// functions, and finally is freed by a call to `nickel_string_free`.
#[no_mangle]
pub unsafe extern "C" fn nickel_string_alloc() -> *mut String {
    Box::into_raw(Box::new(String::new()))
}

/// Frees a string.
#[no_mangle]
pub unsafe extern "C" fn nickel_string_free(s: *mut String) {
    let _ = Box::from_raw(s);
}

/// Retrieve the data inside a string.
///
/// A pointer to the string contents, which are UTF-8 encoded, is written to
/// `data`. These contents are *not* nul-terminated, but their length (in bytes)
/// is written to `len`. The string contents will be invalidated when `s` is
/// freed or overwritten.
#[no_mangle]
pub unsafe extern "C" fn nickel_string_data(
    s: *const String,
    data: *mut *const c_char,
    len: *mut usize,
) {
    let s = s.as_ref().unwrap();
    *data = s.as_ptr() as *const c_char;
    *len = s.len();
}

/// Allocate space for a virtual machine.
///
/// The virtual machine can be initialized by `nickel_context_eval_shallow`.
#[no_mangle]
pub unsafe extern "C" fn nickel_virtual_machine_alloc() -> *mut nickel_virtual_machine {
    Box::into_raw(Box::new(nickel_virtual_machine { inner: None }))
}

/// Free a virtual machine.
#[no_mangle]
pub unsafe extern "C" fn nickel_virtual_machine_free(vm: *mut nickel_virtual_machine) {
    let _ = Box::from_raw(vm);
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
#[no_mangle]
pub unsafe extern "C" fn nickel_virtual_machine_eval_shallow(
    vm: *mut nickel_virtual_machine,
    expr: *const nickel_expr,
    mut out_expr: *mut nickel_expr,
    out_error: *mut *mut Error,
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
    match vm
        .as_mut()
        .unwrap()
        .inner
        .as_mut()
        .unwrap()
        .eval_shallow(nickel_expr::as_rust(&expr).clone())
    {
        Ok(out) => {
            if !out_expr.is_null() {
                *nickel_expr::as_rust_mut(&mut out_expr) = out;
            }
            nickel_result::NICKEL_RESULT_OK
        }
        Err(e) => {
            if !out_error.is_null() {
                *out_error = Box::into_raw(Box::new(e));
            }
            nickel_result::NICKEL_RESULT_ERR
        }
    }
}
