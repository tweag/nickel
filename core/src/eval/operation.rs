//! Implementation of primitive operations.
//!
//! Define functions which perform the evaluation of primitive operators. The machinery required
//! for the strict evaluation of the operands is mainly handled by [crate::eval], and marginally in
//! [`VirtualMachine::continuate_operation`].
//!
//! On the other hand, the functions `process_unary_operation` and `process_binary_operation`
//! receive evaluated operands and implement the actual semantics of operators.
use super::{
    Cache, Closure, Environment, ImportResolver, VirtualMachine,
    cache::lazy::Thunk,
    contract_eq::contract_eq,
    merge::{self, MergeMode, split},
    stack::StrAccData,
    subst,
};

#[cfg(feature = "nix-experimental")]
use crate::nix_ffi;

use crate::{
    cache::InputFormat,
    bytecode::value::{
        Array, ArrayBody, EnumVariantBody, InlineValue, NickelValue, NumberBody, RecordBody,
        ValueContent, ValueContentRef, ValueContentRefMut,
    },
    closurize::Closurize,
    error::{EvalCtxt, EvalError, EvalErrorData, IllegalPolymorphicTailAction, Warning},
    identifier::LocIdent,
    label::{Polarity, TypeVarData, ty_path},
    metrics::increment,
    mk_app, mk_fun, mk_record,
    parser::utils::parse_number_sci,
    position::{PosIdx, PosTable, TermPos},
    serialize::{self, ExportFormat},
    stdlib::internals,
    term::{make as mk_term, record::*, string::NickelString, *},
};

#[cfg(feature = "metrics")]
use crate::pretty::PrettyPrintCap;

use malachite::{
    Integer,
    base::{
        num::{arithmetic::traits::Pow, basic::traits::Zero, conversion::traits::RoundingFrom},
        rounding_modes::RoundingMode,
    },
};

use md5::digest::Digest;
use simple_counter::*;
use unicode_segmentation::UnicodeSegmentation;

use std::{convert::TryFrom, iter::Extend};

generate_counter!(FreshVariableCounter, usize);

/// Result of the equality of two terms.
///
/// The equality of two terms can either be computed directly for base types (`Number`, `String`,
/// etc.), in which case `Bool` is returned. Otherwise, composite values such as arrays or records
/// generate new subequalities, as represented by the last variant as a vector of pairs of terms.
/// This list should be non-empty (it if was empty, `eq` should have returned `Bool(true)`
/// directly).  The first element of this non-empty list is encoded as the two first parameters of
/// `Eqs`, while the last vector parameter is the (potentially empty) tail.
///
/// See [`eq`].
enum EqResult {
    Bool(bool),
    Eqs(NickelValue, NickelValue, Vec<(Closure, Closure)>),
}

/// An operation continuation as stored on the stack.
#[derive(PartialEq, Clone)]
pub enum OperationCont {
    Op1(
        /* unary operation */ UnaryOp,
        /* original position of the argument before evaluation */ PosIdx,
    ),
    // The last parameter saves the strictness mode before the evaluation of the operator
    Op2First(
        /* the binary operation */ BinaryOp,
        /* second argument, to evaluate next */ Closure,
        /* original position of the first argument */ PosIdx,
    ),
    Op2Second(
        /* binary operation */ BinaryOp,
        /* first argument, evaluated */ Closure,
        /* original position of the first argument before evaluation */ PosIdx,
        /* original position of the second argument before evaluation */ PosIdx,
    ),
    OpN {
        op: NAryOp,                        /* the n-ary operation */
        evaluated: Vec<(Closure, PosIdx)>, /* evaluated arguments and their original position */
        current_pos_idx: PosIdx, /* original position of the argument being currently evaluated */
        pending: Vec<Closure>,   /* a stack (meaning the order of arguments is to be reversed)
                                 of arguments yet to be evaluated */
    },
}

impl std::fmt::Debug for OperationCont {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OperationCont::Op1(op, _) => write!(f, "Op1 {op:?}"),
            OperationCont::Op2First(op, _, _) => write!(f, "Op2First {op:?}"),
            OperationCont::Op2Second(op, _, _, _) => write!(f, "Op2Second {op:?}"),
            OperationCont::OpN { op, .. } => write!(f, "OpN {op:?}"),
        }
    }
}

impl<'ctxt, R: ImportResolver, C: Cache> VirtualMachine<'ctxt, R, C> {
    /// Process to the next step of the evaluation of an operation.
    ///
    /// Depending on the content of the stack, it either starts the evaluation of the first
    /// argument, starts the evaluation of the second argument, or finally process with the
    /// operation if both arguments are evaluated (for binary operators).
    pub fn continuate_operation(&mut self, mut clos: Closure) -> Result<Closure, EvalError> {
        let (cont, cs_len, pos_idx) = self.stack.pop_op_cont().expect("Condition already checked");
        self.call_stack.truncate(cs_len);
        match cont {
            OperationCont::Op1(u_op, arg_pos_idx) => {
                self.process_unary_operation(u_op, clos, arg_pos_idx, pos_idx)
            }
            OperationCont::Op2First(b_op, mut snd_clos, fst_pos_idx) => {
                std::mem::swap(&mut clos, &mut snd_clos);
                self.stack.push_op_cont(
                    OperationCont::Op2Second(b_op, snd_clos, fst_pos_idx, clos.value.pos_idx()),
                    cs_len,
                    pos_idx,
                );
                Ok(clos)
            }
            OperationCont::Op2Second(b_op, fst_clos, fst_pos_idx, snd_pos_idx) => self
                .process_binary_operation(b_op, fst_clos, fst_pos_idx, clos, snd_pos_idx, pos_idx),
            OperationCont::OpN {
                op,
                mut evaluated,
                current_pos_idx,
                mut pending,
            } => {
                evaluated.push((clos, current_pos_idx));

                if let Some(next) = pending.pop() {
                    let current_pos = next.value.pos_idx();
                    self.stack.push_op_cont(
                        OperationCont::OpN {
                            op,
                            evaluated,
                            current_pos_idx: current_pos,
                            pending,
                        },
                        cs_len,
                        pos_idx,
                    );

                    Ok(next)
                } else {
                    self.process_nary_operation(op, evaluated, pos_idx)
                }
            }
        }
    }
    /// Evaluate a unary operation.
    ///
    /// The argument is expected to be evaluated (in WHNF). `pos_op` corresponds to the whole
    /// operation position, that may be needed for error reporting.
    fn process_unary_operation(
        &mut self,
        u_op: UnaryOp,
        clos: Closure,
        pos_arg: PosIdx,
        pos_op: PosIdx,
    ) -> Result<Closure, EvalError> {
        increment!(format!("primop:{u_op}"));

        let Closure { value, env } = clos;
        let pos = value.pos_idx();
        let pos_op_inh = pos_op;

        macro_rules! mk_type_error {
            (op_name=$op_name:expr, $expected:expr) => {
                Err(EvalError {
                    error: EvalErrorData::UnaryPrimopTypeError {
                        primop: String::from($op_name),
                        expected: String::from($expected),
                        pos_arg,
                        arg_evaluated: value,
                    },
                    ctxt: self.eval_ctxt(),
                })
            };
            ($expected:expr) => {
                mk_type_error!(op_name = u_op.to_string(), $expected)
            };
            ($expected:expr, $arg_number:expr) => {
                Err(EvalError {
                    error: EvalErrorData::NAryPrimopTypeError {
                        primop: u_op.to_string(),
                        expected: String::from($expected),
                        arg_number: $arg_number,
                        pos_arg,
                        arg_evaluated: value,
                        pos_op,
                    },
                    ctxt: self.eval_ctxt(),
                })
            };
        }

        match u_op {
            UnaryOp::IfThenElse => {
                if let Some(b) = value.as_bool() {
                    let (fst, ..) = self
                        .stack
                        .pop_arg(&self.contex.cache)
                        .expect("if-then-else primop isn't saturated");
                    let (snd, ..) = self
                        .stack
                        .pop_arg(&self.context.cache)
                        .expect("if-then-else primop isn't saturated");

                    Ok(if b { fst } else { snd })
                } else {
                    // Not using mk_type_error! because of a non-uniform message
                    self.throw_with_ctxt(EvalErrorData::TypeError {
                        expected: String::from("Bool"),
                        message: String::from(
                            "the condition in an if expression must have type Bool",
                        ),
                        orig_pos: pos_arg,
                        term: value,
                    })
                }
            }
            UnaryOp::Typeof => {
                let result = type_tag(&value);
                Ok(NickelValue::enum_variant(LocIdent::from(result), None, pos_op_inh).into())
            }
            UnaryOp::Cast => {
                let result = type_tag(&value);
                Ok(
                    NickelValue::enum_variant(LocIdent::from(result), Some(value), pos_op_inh)
                        .into(),
                )
            }
            UnaryOp::BoolAnd =>
            // The syntax should not allow partially applied boolean operators.
            {
                if let Some((next, ..)) = self.stack.pop_arg(&self.context.cache) {
                    match value.as_bool() {
                        Some(true) => Ok(next),
                        // FIXME: this does not check that the second argument is actually a
                        // boolean. This means `true && 2` silently evaluates to `2`. This is
                        // simpler and more efficient, but can make debugging harder. In any case,
                        // it should be solved only once primary operators have better support for
                        // laziness in some arguments.
                        Some(false) => {
                            Ok(value.with_pos_idx(&mut self.pos_table, pos_op_inh).into())
                        }
                        _ => mk_type_error!("Bool", 1),
                    }
                } else {
                    self.throw_with_ctxt(EvalErrorData::NotEnoughArgs(
                        2,
                        String::from("&&"),
                        pos_op,
                    ))
                }
            }
            UnaryOp::BoolOr => {
                if let Some((next, ..)) = self.stack.pop_arg(&self.context.cache) {
                    match value.as_bool() {
                        Some(true) => {
                            Ok(value.with_pos_idx(&mut self.pos_table, pos_op_inh).into())
                        }
                        // FIXME: this does not check that the second argument is actually a
                        // boolean. This means `false || 2` silently evaluates to `2`. This is
                        // simpler and more efficient, but can make debugging harder. In any case,
                        // it should be solved only once primary operators have better support for
                        // laziness in some arguments.
                        Some(false) => Ok(next),
                        _ => mk_type_error!("Bool", 1),
                    }
                } else {
                    self.throw_with_ctxt(EvalErrorData::NotEnoughArgs(
                        2,
                        String::from("||"),
                        pos_op,
                    ))
                }
            }
            UnaryOp::BoolNot => {
                if let Some(b) = value.as_bool() {
                    Ok(NickelValue::bool_value_posless(!b)
                        .with_pos_idx(&mut self.pos_table, pos_op_inh)
                        .into())
                } else {
                    mk_type_error!("Bool")
                }
            }
            UnaryOp::Blame => {
                if let ValueContent::Label(lens) = value.content() {
                    let label = lens.take().0;

                    self.throw_with_ctxt(EvalErrorData::BlameError {
                        evaluated_arg: label.get_evaluated_arg(&self.context.cache),
                        label,
                    })
                } else {
                    mk_type_error!("Label")
                }
            }
            // match_sharedterm!(match (t) {
            //     Term::Lbl(label) => Err(EvalErrorData::BlameError {
            //         evaluated_arg: label.get_evaluated_arg(&self.context.cache),
            //         label,
            //         call_stack: std::mem::take(&mut self.call_stack),
            //     }),
            //     _ => mk_type_error!("Label"),
            // }),
            UnaryOp::EnumEmbed(_id) => {
                if let Some(_) = value.as_enum_variant() {
                    Ok(value.with_pos_idx(&mut self.pos_table, pos_op_inh).into())
                } else {
                    mk_type_error!("Enum")
                }
            }
            UnaryOp::TagsOnlyMatch { has_default } => {
                let (cases_closure, ..) = self
                    .stack
                    .pop_arg(&self.context.cache)
                    .expect("missing arg for match");

                let default = if has_default {
                    Some(
                        self.stack
                            .pop_arg(&self.context.cache)
                            .map(|(clos, ..)| clos)
                            .expect("missing default case for match"),
                    )
                } else {
                    None
                };

                if let Some(enum_body) = value.as_enum_variant()
                    && enum_body.arg.is_none()
                {
                    let Closure {
                        value: cases_val,
                        env: cases_env,
                    } = cases_closure;

                    let mut cases = match cases_val.content() {
                        ValueContent::Record(lens) => lens.take().0.fields,
                        ValueContent::Inline(lens)
                            if matches!(lens.take(), InlineValue::EmptyRecord) =>
                        {
                            Default::default()
                        }
                        _ => panic!("invalid argument for %match%"),
                    };

                    cases
                        .swap_remove(&enum_body.tag)
                        .map(|field| Closure {
                            // The record containing the match cases, as well as the match primop
                            // itself, aren't accessible in the surface language. They are
                            // generated by the interpreter, and should never contain field without
                            // definition.
                            value: field.value.expect("%match% cases must have a definition"),
                            env: cases_env,
                        })
                        .or(default)
                        .ok_or_else(|| EvalError {
                            error: EvalErrorData::NonExhaustiveEnumMatch {
                                expected: cases.keys().copied().collect(),
                                found: NickelValue::enum_variant_posless(enum_body.tag, None)
                                    .with_pos_idx(&mut self.pos_table, pos),
                                pos: pos_op_inh,
                            },
                            ctxt: self.eval_ctxt(),
                        })
                } else if let Some(clos) = default {
                    Ok(clos)
                } else {
                    mk_type_error!("Enum", 2)
                }
            }
            UnaryOp::LabelFlipPol => {
                if let ValueContent::Label(lens) = value.content() {
                    let mut label = lens.take().0;
                    label.polarity = label.polarity.flip();
                    Ok(NickelValue::label(label, pos_op_inh).into())
                } else {
                    mk_type_error!("Label")
                }
            }
            UnaryOp::LabelPol => {
                if let Some(label) = value.as_label() {
                    Ok(label
                        .0
                        .polarity
                        .into()
                        .with_pos_idx(&mut self.pos_table, pos_op_inh)
                        .into())
                } else {
                    mk_type_error!("Label")
                }
            }
            UnaryOp::LabelGoDom => {
                if let ValueContent::Label(lens) = value.content() {
                    let mut label = lens.take().0;
                    label.path.push(ty_path::Elem::Domain);
                    Ok(NickelValue::label(label, pos_op_inh).into())
                } else {
                    mk_type_error!("Label")
                }
            }
            UnaryOp::LabelGoCodom => {
                if let ValueContent::Label(lens) = value.content() {
                    let mut label = lens.take().0;
                    label.path.push(ty_path::Elem::Codomain);
                    Ok(NickelValue::label(label, pos_op_inh).into())
                } else {
                    mk_type_error!("Label")
                }
            }
            UnaryOp::LabelGoArray => {
                if let ValueContent::Label(lens) = value.content() {
                    let mut label = lens.take().0;
                    label.path.push(ty_path::Elem::Array);
                    Ok(NickelValue::label(label, pos_op_inh).into())
                } else {
                    mk_type_error!("Label")
                }
            }
            UnaryOp::LabelGoDict => {
                if let ValueContent::Label(lens) = value.content() {
                    let mut label = lens.take().0;
                    label.path.push(ty_path::Elem::Dict);
                    Ok(NickelValue::label(label, pos_op_inh).into())
                } else {
                    mk_type_error!("Label")
                }
            }
            UnaryOp::RecordAccess(id) => {
                if let Some(RecordBody(record)) = value.as_record() {
                    // We have to apply potentially pending contracts. Right now, this
                    // means that repeated field access will re-apply the contract again
                    // and again, which is not optimal. The same thing happens with array
                    // contracts. There are several way to improve this, but this is left
                    // as future work.
                    match record
                        .get_value_with_ctrs(&id)
                        .map_err(|err| self.err_with_ctxt(err.into_eval_err(pos, pos_op)))?
                    {
                        Some(value) => {
                            self.call_stack
                                .enter_field(id, pos, value.pos_idx(), pos_op);
                            Ok(Closure { value, env })
                        }
                        None => match record.sealed_tail.as_ref() {
                            Some(t) if t.has_field(&id.ident()) => {
                                self.throw_with_ctxt(EvalErrorData::IllegalPolymorphicTailAccess {
                                    action: IllegalPolymorphicTailAction::FieldAccess {
                                        field: id.to_string(),
                                    },
                                    evaluated_arg: t.label.get_evaluated_arg(&self.context.cache),
                                    label: t.label.clone(),
                                    call_stack: std::mem::take(&mut self.call_stack),
                                })
                            }
                            _ => self.throw_with_ctxt(EvalErrorData::FieldMissing {
                                id,
                                field_names: record.field_names(RecordOpKind::IgnoreEmptyOpt),
                                operator: String::from("(.)"),
                                pos_record: pos,
                                pos_op,
                            }),
                        }, //TODO include the position of operators on the stack
                    }
                } else {
                    // Not using mk_type_error! because of a non-uniform message
                    self.throw_with_ctxt(EvalErrorData::TypeError {
                        expected: String::from("Record"),
                        message: String::from("field access only makes sense for records"),
                        orig_pos: pos_arg,
                        term: value,
                    })
                }
            }
            UnaryOp::RecordFields(op_kind) => {
                if let Some(RecordBody(record)) = value.as_record() {
                    let fields_as_terms: Array = record
                        .field_names(op_kind)
                        .into_iter()
                        .map(|id| {
                            NickelValue::string(id.label(), self.pos_table.push_block(id.pos))
                        })
                        .collect();

                    Ok(Closure {
                        value: NickelValue::array_force_pos(
                            &mut self.pos_table,
                            fields_as_terms,
                            Vec::new(),
                            pos_op_inh,
                        ),
                        env,
                    })
                } else {
                    mk_type_error!("Record")
                }
            }
            UnaryOp::RecordValues => {
                if let Some(RecordBody(record)) = value.as_record() {
                    let mut values = record
                        .iter_without_opts()
                        .collect::<Result<Vec<_>, _>>()
                        .map_err(|miss_def_err| {
                            self.err_with_ctxt(miss_def_err.into_eval_err(pos, pos_op))
                        })?;

                    values.sort_by_key(|(id, _)| *id);
                    let terms = values.into_iter().map(|(_, value)| value).collect();

                    Ok(Closure {
                        // TODO: once sure that the Record is properly closurized, we can safely
                        // assume that the extracted array here is, in turn, also closuried.
                        value: NickelValue::array_force_pos(
                            &mut self.pos_table,
                            terms,
                            Vec::new(),
                            pos_op_inh,
                        ),
                        env,
                    })
                } else {
                    mk_type_error!("Record")
                }
            }
            UnaryOp::ArrayMap => {
                let (f, _) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    self.err_with_ctxt(EvalErrorData::NotEnoughArgs(
                        2,
                        String::from("array/map"),
                        pos_op,
                    ))
                })?;

                if let ValueContent::Array(lens) = value.content() {
                    let array_body = lens.take();
                    let f_as_var = f.value.closurize(&mut self.context.cache, f.env);

                    // Array elements are closurized to preserve laziness of data
                    // structures. It maintains the invariant that any data structure only
                    // contain indices (that is, currently, variables).
                    let ts = array_body
                        .array
                        .into_iter()
                        .map(|t| {
                            let t_with_ctrs = RuntimeContract::apply_all(
                                t,
                                array_body.pending_contracts.iter().cloned(),
                                pos,
                            );

                            NickelValue::term(Term::App(f_as_var.clone(), t_with_ctrs), pos_op_inh)
                                .closurize(&mut self.context.cache, env.clone())
                        })
                        .collect();

                    Ok(NickelValue::array_force_pos(
                        &mut self.pos_table,
                        ts,
                        Vec::new(),
                        pos_op_inh,
                    )
                    .into())
                } else {
                    mk_type_error!("Array")
                }
            }
            UnaryOp::ArrayGen => {
                let (f, _) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    self.err_with_ctxt(EvalErrorData::NotEnoughArgs(
                        2,
                        String::from("array/generate"),
                        pos_op,
                    ))
                })?;

                let Some(NumberBody(n)) = value.as_number() else {
                    return mk_type_error!("Number");
                };

                if n < &Number::ZERO {
                    return self.throw_with_ctxt(EvalErrorData::Other(
                            format!(
                                "array/generate expects its first argument to be a positive number, got {n}"
                            ),
                            pos_op,
                    ));
                }

                let Ok(n_int) = u32::try_from(n) else {
                    return self.throw_with_ctxt(EvalErrorData::Other(
                        format!(
                            "array/generate expects its first argument to be an integer \
                            smaller than {}, got {n}",
                            u32::MAX,
                        ),
                        pos_op,
                    ));
                };

                let f_closure = f.value.closurize(&mut self.context.cache, f.env);

                // Array elements are closurized to preserve laziness of data structures. It
                // maintains the invariant that any data structure only contain indices (that is,
                // currently, variables).
                let ts = (0..n_int)
                    .map(|n| {
                        mk_app!(f_closure.clone(), NickelValue::number_posless(n.into()))
                            .closurize(&mut self.context.cache, env.clone())
                    })
                    .collect();

                Ok(Closure {
                    value: NickelValue::array_force_pos(
                        &mut self.pos_table,
                        ts,
                        Vec::new(),
                        pos_op_inh,
                    ),
                    env: Environment::new(),
                })
            }
            UnaryOp::RecordMap => {
                let (f, ..) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    self.err_with_ctxt(EvalErrorData::NotEnoughArgs(
                        2,
                        String::from("record/map"),
                        pos_op,
                    ))
                })?;

                match value.content() {
                    ValueContent::Record(lens) => {
                        let record = lens.take().0;
                        // While it's certainly possible to allow mapping over
                        // a record with a sealed tail, it's not entirely obvious
                        // how that should behave. It's also not clear that this
                        // is something users will actually need to do, so we've
                        // decided to prevent this until we have a clearer idea
                        // of potential use-cases.
                        if let Some(record::SealedTail { label, .. }) = record.sealed_tail {
                            return self.throw_with_ctxt(
                                EvalErrorData::IllegalPolymorphicTailAccess {
                                    action: IllegalPolymorphicTailAction::Map,
                                    evaluated_arg: label.get_evaluated_arg(&self.context.trace),
                                    label,
                                    call_stack: std::mem::take(&mut self.call_stack),
                                },
                            );
                        }

                        let f_closure = f.value.closurize(&mut self.context.trace, f.env);

                        // As for `ArrayMap` (see above), we closurize the content of fields

                        let fields = record
                            .fields
                            .into_iter()
                            .filter(|(_, field)| !field.is_empty_optional())
                            .map_values_closurize(&mut self.context.trace, &env, |id, t| {
                                let pos = self.pos_table.get(t.pos_idx()).into_inherited();

                                mk_app!(
                                    f_closure.clone(),
                                    NickelValue::string_posless(id.label()),
                                    t
                                )
                                .with_pos(&mut self.pos_table, pos)
                            })
                            .map_err(|miss_field_err| {
                                self.err_with_ctxt(miss_field_err.into_eval_err(pos, pos_op))
                            })?;

                        // By construction, mapping freezes the record. We set the frozen flag so
                        // that operations that require the record to be frozen don't have to
                        // perform the work again.
                        let attrs = record.attrs.frozen();

                        Ok(NickelValue::record_force_pos(
                            &mut self.pos_table,
                            RecordData {
                                fields,
                                attrs,
                                ..record
                            },
                            pos_op_inh,
                        )
                        .into())
                    }
                    _ => mk_type_error!("Record", 1),
                }
            }
            UnaryOp::Seq => self
                .stack
                .pop_arg(&self.context.cache)
                .map(|(next, ..)| next)
                .ok_or_else(|| {
                    self.err_with_ctxt(EvalErrorData::NotEnoughArgs(2, String::from("seq"), pos_op))
                }),
            UnaryOp::DeepSeq => {
                // Build a `NickelValue` that forces a given list of terms, and at the end resumes the
                // evaluation of the argument on the top of the stack.
                //
                // Requires its first argument to be non-empty.
                fn seq_terms<I>(mut it: I, pos_op_inh: PosIdx) -> NickelValue
                where
                    I: Iterator<Item = NickelValue>,
                {
                    let first = it
                        .next()
                        .expect("expected the argument to be a non-empty iterator");

                    it.fold(
                        // unwrap(): an unary operation is never inline, so `try_with_pos_idx`
                        // can't fail
                        mk_term::op1(UnaryOp::DeepSeq, first)
                            .try_with_pos_idx(pos_op_inh)
                            .unwrap(),
                        |acc, t| {
                            // unwrap(): an unary operation is never inline, so `try_with_pos_idx`
                            // can't fail
                            mk_app!(mk_term::op1(UnaryOp::DeepSeq, t), acc)
                                .try_with_pos_idx(pos_op_inh)
                                .unwrap()
                        },
                    )
                }

                match value.content_ref() {
                    ValueContentRef::Record(body) => {
                        let defined = body
                            .0
                            // `iter_without_opts` takes care of applying pending contracts
                            .iter_without_opts()
                            .collect::<Result<Vec<_>, _>>()
                            .map_err(|missing_def_err| {
                                self.err_with_ctxt(missing_def_err.into_eval_err(pos, pos_op))
                            })?;

                        let terms = defined.into_iter().map(|(_, field)| field);

                        Ok(Closure {
                            value: seq_terms(terms, pos_op),
                            env,
                        })
                    }
                    ValueContentRef::Array(array_data) => {
                        let terms = seq_terms(
                            array_data.array.iter().map(|t| {
                                let t_with_ctr = RuntimeContract::apply_all(
                                    t.clone(),
                                    array_data.pending_contracts.iter().cloned(),
                                    pos.to_inherited_block(&mut self.pos_table),
                                )
                                .closurize(&mut self.context.cache, env.clone());
                                t_with_ctr
                            }),
                            pos_op,
                        );

                        Ok(terms.into())
                    }
                    ValueContentRef::EnumVariant(EnumVariantBody {
                        tag,
                        arg: Some(arg),
                    }) => Ok(Closure {
                        value: seq_terms(std::iter::once(arg.clone()), pos_op),
                        env,
                    }),
                    _ => {
                        if let Some((next, ..)) = self.stack.pop_arg(&self.context.cache) {
                            Ok(next)
                        } else {
                            self.throw_with_ctxt(EvalErrorData::NotEnoughArgs(
                                2,
                                String::from("deep_seq"),
                                pos_op,
                            ))
                        }
                    }
                }
            }
            UnaryOp::ArrayLength => {
                //TODO[RFC007]: empty array
                if let Some(array_data) = value.as_array() {
                    // A num does not have any free variable so we can drop the environment
                    Ok(NickelValue::number(array_data.array.len(), pos_op_inh).into())
                } else {
                    mk_type_error!("Array")
                }
            }
            UnaryOp::ChunksConcat => {
                let StrAccData {
                    mut acc,
                    curr_indent: indent,
                    env: env_chunks,
                    curr_pos,
                } = self.stack.pop_str_acc().unwrap();

                if let Some(s) = value.to_nickel_string() {
                    let s = if indent != 0 {
                        let indent_str: String = std::iter::once('\n')
                            .chain((0..indent).map(|_| ' '))
                            .collect();
                        s.as_str().replace('\n', &indent_str).into()
                    } else {
                        s.clone()
                    };

                    acc.push_str(&s);

                    let mut next_opt = self.stack.pop_str_chunk();

                    // Pop consecutive string literals to find the next expression to evaluate
                    while let Some(StrChunk::Literal(s)) = next_opt {
                        acc.push_str(&s);
                        next_opt = self.stack.pop_str_chunk();
                    }

                    if let Some(StrChunk::Expr(e, indent)) = next_opt {
                        self.stack.push_str_acc(StrAccData {
                            acc,
                            curr_indent: indent,
                            env: env_chunks.clone(),
                            curr_pos: e.pos_idx(),
                        });

                        Ok(Closure {
                            value: NickelValue::term(
                                Term::Op1(UnaryOp::ChunksConcat, e),
                                pos_op_inh,
                            ),
                            env: env_chunks,
                        })
                    } else {
                        Ok(NickelValue::string(acc.into(), pos_op_inh).into())
                    }
                } else {
                    // Since the error halts the evaluation, we don't bother cleaning the stack of
                    // the remaining string chunks.
                    //
                    // Not using mk_type_error! because of a non-uniform message
                    self.throw_with_ctxt(EvalErrorData::TypeError {
                        expected: String::from("Stringable"),
                        message: String::from(
                            "interpolated values must be Stringable (string, number, boolean, enum tag or null)",
                        ),
                        orig_pos: curr_pos,
                        term: value,
                    })
                }
            }
            UnaryOp::StringTrim => {
                if let Some(s) = value.as_string() {
                    Ok(NickelValue::string(s.0.trim(), pos_op_inh).into())
                } else {
                    mk_type_error!("String")
                }
            }
            UnaryOp::StringChars => {
                if let Some(s) = value.as_string() {
                    let ts = s.0.characters();
                    Ok(NickelValue::array_force_pos(
                        &mut self.pos_table,
                        ts,
                        Vec::new(),
                        pos_op_inh,
                    )
                    .into())
                } else {
                    mk_type_error!("String")
                }
            }
            UnaryOp::StringUppercase => {
                if let Some(s) = value.as_string() {
                    Ok(NickelValue::string(s.0.to_uppercase(), pos_op_inh).into())
                } else {
                    mk_type_error!("String")
                }
            }
            UnaryOp::StringLowercase => {
                if let Some(s) = value.as_string() {
                    Ok(NickelValue::string(s.0.to_lowercase(), pos_op_inh).into())
                } else {
                    mk_type_error!("String")
                }
            }
            UnaryOp::StringLength => {
                if let Some(s) = value.as_string() {
                    let length = s.0.graphemes(true).count();
                    Ok(NickelValue::number(length, pos_op_inh).into())
                } else {
                    mk_type_error!("String")
                }
            }
            UnaryOp::ToString => value
                .to_nickel_string()
                .map(|s| NickelValue::string(s, pos_op_inh).into())
                .ok_or_else(|| {
                    self.err_with_ctxt(EvalErrorData::Other(
                        format!(
                            "to_string: can't convert an argument of type {} to string",
                            value.type_of().unwrap()
                        ),
                        pos,
                    ))
                }),
            UnaryOp::NumberFromString => {
                if let Some(s) = value.as_string() {
                    let n = parse_number_sci(&s.0).map_err(|_| {
                        self.err_with_ctxt(EvalErrorData::Other(
                            format!(
                                "number/from_string: invalid number literal `{}`",
                                s.0.as_str()
                            ),
                            pos,
                        ))
                    })?;

                    Ok(NickelValue::number(n, pos_op_inh).into())
                } else {
                    mk_type_error!("String")
                }
            }
            UnaryOp::EnumFromString => {
                if let Some(s) = value.as_string() {
                    Ok(NickelValue::enum_tag(LocIdent::from(&s.0), pos_op_inh).into())
                } else {
                    mk_type_error!("String")
                }
            }
            UnaryOp::StringIsMatch => {
                if let Some(s) = value.as_string() {
                    let re = regex::Regex::new(&s.0).map_err(|err| {
                        self.err_with_ctxt(EvalErrorData::Other(err.to_string(), pos_op))
                    })?;

                    let matcher = eta_expand(UnaryOp::StringIsMatchCompiled(re.into()), pos_op_inh);
                    Ok(NickelValue::term(matcher, pos_op_inh).into())
                } else {
                    mk_type_error!("String", 1)
                }
            }
            UnaryOp::StringFind => {
                if let Some(s) = value.as_string() {
                    let re = regex::Regex::new(&s.0).map_err(|err| {
                        self.err_with_ctxt(EvalErrorData::Other(err.to_string(), pos_op))
                    })?;

                    let matcher = eta_expand(UnaryOp::StringFindCompiled(re.into()), pos_op_inh);
                    Ok(NickelValue::term(matcher, pos_op_inh).into())
                } else {
                    mk_type_error!("String", 1)
                }
            }
            UnaryOp::StringFindAll => {
                if let Some(s) = value.as_string() {
                    let re = regex::Regex::new(&s.0).map_err(|err| {
                        self.err_with_ctxt(EvalErrorData::Other(err.to_string(), pos_op))
                    })?;

                    let matcher = eta_expand(UnaryOp::StringFindAllCompiled(re.into()), pos_op_inh);
                    Ok(NickelValue::term(matcher, pos_op_inh).into())
                } else {
                    mk_type_error!("String", 1)
                }
            }
            UnaryOp::StringIsMatchCompiled(regex) => {
                if let Some(s) = value.as_string() {
                    Ok(s.0
                        .matches_regex(&regex)
                        .with_pos_idx(&mut self.pos_table, pos_op_inh)
                        .into())
                } else {
                    mk_type_error!(op_name = "a compiled regular expression match", "String")
                }
            }
            UnaryOp::StringFindCompiled(regex) => {
                if let Some(s) = value.as_string() {
                    use crate::term::string::RegexFindResult;

                    let result = match s.0.find_regex(&regex) {
                        None => mk_record!(
                            ("matched", NickelValue::string_posless("")),
                            ("index", NickelValue::number_posless(-1)),
                            ("groups", NickelValue::empty_array())
                        ),
                        Some(RegexFindResult {
                            matched: mtch,
                            index,
                            groups,
                        }) => mk_record!(
                            ("matched", NickelValue::string_posless(mtch)),
                            ("index", NickelValue::number_posless(index)),
                            (
                                "groups",
                                NickelValue::array_posless(
                                    Array::from_iter(
                                        groups
                                            .into_iter()
                                            // Unmatched groups get turned into empty strings. It
                                            // might be nicer to have a 'Some s / 'None instead,
                                            // but that would be an API break.
                                            .map(|s| NickelValue::string_posess(
                                                s.unwrap_or_default()
                                            ))
                                    ),
                                    Vec::new()
                                )
                            )
                        ),
                    };

                    Ok(result.with_pos_idx(&mut self.pos_table, pos_op_inh).into())
                } else {
                    mk_type_error!(op_name = "a compiled regular expression match", "String")
                }
            }
            UnaryOp::StringFindAllCompiled(regex) => {
                if let Some(s) = value.as_string() {
                    let result = NickelValue::array_force_pos(
                        &mut self.pos_table,
                        Array::from_iter(s.0.find_all_regex(&regex).map(|found| {
                            mk_record!(
                                ("matched", NickelValue::string_posless(found.matched)),
                                ("index", NickelValue::number_posless(found.index)),
                                (
                                    "groups",
                                    NickelValue::array_posless(
                                        Array::from_iter(
                                            found
                                                .groups
                                                .into_iter()
                                                // Unmatched groups get turned into empty strings. It
                                                // might be nicer to have a 'Some s / 'None instead,
                                                // but that would be an API break.
                                                .map(|s| NickelValue::string_posless(
                                                    s.unwrap_or_default()
                                                ))
                                        ),
                                        Vec::new(),
                                    )
                                )
                            )
                        })),
                        Vec::new(),
                        pos_op_inh,
                    );

                    Ok(result.into())
                } else {
                    mk_type_error!(op_name = "a compiled regular expression match", "String")
                }
            }
            UnaryOp::Force {
                ignore_not_exported,
            } => {
                /// `Seq` the `terms` iterator and then resume evaluating the `cont` continuation.
                fn seq_terms<I>(terms: I, pos: PosIdx, cont: NickelValue) -> NickelValue
                where
                    I: Iterator<Item = NickelValue>,
                {
                    terms
                        .fold(cont, |acc, t| mk_app!(mk_term::op1(UnaryOp::Seq, t), acc))
                        .with_pos_idx(&mut self.pos_table, pos)
                }

                match value.content() {
                    // TODO[RFC007]: it's intentional that we don't want to handle empty arrays here
                    Term::Record(lens) => {
                        let fields = lens
                            .take()
                            .0
                            .fields
                            .into_iter()
                            .filter(|(_, field)| {
                                !(field.is_empty_optional()
                                    || (ignore_not_exported && field.metadata.not_exported))
                            })
                            .map_values_closurize(&mut self.context.cache, &env, |_, value| {
                                mk_term::op1(
                                    UnaryOp::Force {
                                        ignore_not_exported,
                                    },
                                    value,
                                )
                            })
                            .map_err(|e| self.err_with_ctxt(e.into_eval_err(pos, pos_op)))?;

                        let terms = fields.clone().into_values().map(|field| {
                            field.value.expect(
                                "map_values_closurize ensures that values without a \
                                            definition throw a MissingFieldDefError",
                            )
                        });

                        let cont = NickelValue::record_force_pos(
                            &mut self.pos_table,
                            RecordData { fields, ..record },
                            pos.to_inherited_block(&mut self.pos_table),
                        );

                        Ok(seq_terms(terms, pos_op, cont).into())
                    }
                    //TODO[RFC007] We intentionally do NOT want to handle empty arrays here
                    ValueContent::Array(lens) => {
                        let ArrayBody {
                            array: ts,
                            pending_contracts,
                        } = lens.take();
                        let pos_inh = pos.to_inherited_block(&mut self.pos_table);

                        let ts = ts
                            .into_iter()
                            .map(|t| {
                                mk_term::op1(
                                    UnaryOp::Force {
                                        ignore_not_exported,
                                    },
                                    RuntimeContract::apply_all(
                                        t,
                                        pending_contracts.iter().cloned(),
                                        pos_inh,
                                    ),
                                )
                                .closurize(&mut self.context.cache, env.clone())
                            })
                            // It's important to collect here, otherwise the two usages below
                            // will each do their own .closurize(...) calls and end up with
                            // different closures, which means that `cont` won't be properly
                            // updated.
                            .collect::<Array>();

                        let terms = ts.clone().into_iter();
                        let cont = NickelValue::array(ts, Vec::new(), pos_inh);

                        Ok(seq_terms(terms, pos_op, cont).into())
                    }
                    // For an enum variant, `force x` is simply equivalent to `deep_seq x x`, as
                    // there's no lazy pending contract to apply.
                    Term::EnumVariant { tag, arg, attrs } => {
                        let arg = mk_term::op1(
                            UnaryOp::Force {
                                ignore_not_exported,
                            },
                            arg,
                        )
                        .closurize(&mut self.context.cache, env.clone());
                        let cont = NickelValue::new(
                            Term::EnumVariant {
                                tag,
                                arg: arg.clone(),
                                attrs,
                            },
                            pos.into_inherited(),
                        );

                        Ok(Closure {
                            body: seq_terms(std::iter::once(arg), pos_op, cont),
                            env,
                        })
                    }
                    _ => Ok(Closure {
                        body: NickelValue { term: t, pos },
                        env,
                    }),
                }
            }
            UnaryOp::RecDefault => {
                Ok(RecPriority::Bottom.propagate_in_term(&mut self.context.cache, value, env, pos))
            }
            UnaryOp::RecForce => {
                Ok(RecPriority::Top.propagate_in_term(&mut self.context.cache, value, env, pos))
            }
            UnaryOp::RecordEmptyWithTail => match_sharedterm!(match (t) {
                Term::Record(r) => {
                    let mut empty = RecordData::empty();
                    empty.sealed_tail = r.sealed_tail;
                    Ok(Closure {
                        body: NickelValue::new(Term::Record(empty), pos_op.into_inherited()),
                        env,
                    })
                }
                _ => mk_type_error!("Record"),
            }),
            UnaryOp::RecordFreeze => match_sharedterm!(match (t) {
                Term::Record(record) => {
                    let mut record = record;

                    if record.attrs.frozen {
                        // A frozen record shouldn't have a polymorphic tail
                        debug_assert!(record.sealed_tail.is_none());

                        return Ok(Closure {
                            body: NickelValue::new(Term::Record(record), pos),
                            env,
                        });
                    }

                    // It's not clear what the semantics of freezing a record with a sealed tail
                    // would be, as their might be dependencies between the sealed part and the
                    // unsealed part. Merging is disallowed on records with tail, so we disallow
                    // freezing as well.
                    if let Some(record::SealedTail { label, .. }) = record.sealed_tail {
                        return Err(EvalErrorData::IllegalPolymorphicTailAccess {
                            action: IllegalPolymorphicTailAction::Freeze,
                            evaluated_arg: label.get_evaluated_arg(&self.context.cache),
                            label,
                            call_stack: std::mem::take(&mut self.call_stack),
                        });
                    }

                    let fields = record
                        .fields
                        .into_iter()
                        .map(|(id, field)| {
                            let value = field.value.map(|value| {
                                let pos = value.pos;
                                RuntimeContract::apply_all(value, field.pending_contracts, pos)
                            });

                            let field = Field {
                                value,
                                pending_contracts: Vec::new(),
                                ..field
                            }
                            .closurize(&mut self.context.cache, env.clone());

                            (id, field)
                        })
                        .collect();

                    let attrs = record.attrs.frozen();

                    Ok(Closure {
                        body: NickelValue::new(
                            Term::Record(RecordData {
                                fields,
                                attrs,
                                sealed_tail: None,
                            }),
                            pos_op.into_inherited(),
                        ),
                        env,
                    })
                }
                _ => mk_type_error!("Record"),
            }),
            UnaryOp::Trace => {
                if let Some(s) = value.as_string() {
                    let _ = writeln!(self.context.trace, "std.trace: {s}");
                    Ok(())
                } else {
                    mk_type_error!("String")
                }?;

                self.stack
                    .pop_arg(&self.context.cache)
                    .map(|(next, ..)| next)
                    .ok_or_else(|| EvalErrorData::NotEnoughArgs(2, String::from("trace"), pos_op))
            }
            UnaryOp::LabelPushDiag => {
                match_sharedterm!(match (t) {
                    Term::Lbl(label) => {
                        let mut label = label;
                        label.push_diagnostic();
                        Ok(Closure {
                            body: NickelValue::new(Term::Lbl(label), pos),
                            env,
                        })
                    }
                    _ => mk_type_error!("Label"),
                })
            }
            #[cfg(feature = "nix-experimental")]
            UnaryOp::EvalNix => {
                if let Some(s) = value.as_string() {
                    let base_dir = pos_op
                        .into_opt()
                        .map(|span| self.import_resolver().get_base_dir_for_nix(span.src_id))
                        .unwrap_or_default();

                    let json = nix_ffi::eval_to_json(&String::from(s), &base_dir).map_err(|e| {
                        EvalErrorData::Other(
                            format!("nix code failed to evaluate:\n {}", e.what()),
                            pos,
                        )
                    })?;
                    Ok(Closure::atomic_closure(
                        serde_json::from_str(&json).map_err(|e| {
                            EvalErrorData::Other(format!("nix produced invalid json: {e}"), pos)
                        })?,
                    ))
                } else {
                    // Not using mk_type_error! because of a non-uniform message
                    Err(EvalErrorData::TypeError {
                        expected: String::from("String"),
                        message: String::from("eval_nix takes a string of nix code as an argument"),
                        orig_pos: pos_arg,
                        term: value,
                    })
                }
            }
            UnaryOp::EnumGetArg => {
                if let Term::EnumVariant { arg, .. } = &*value {
                    Ok(Closure {
                        value: arg.clone(),
                        env,
                    })
                } else {
                    mk_type_error!("Enum variant")
                }
            }
            UnaryOp::EnumMakeVariant => {
                let Term::Str(tag) = &*value else {
                    return mk_type_error!("String");
                };

                let (arg_clos, _) = self.stack.pop_arg(&self.contex.cache).ok_or_else(|| {
                    EvalErrorData::NotEnoughArgs(2, String::from("enum/make_variant"), pos)
                })?;
                let arg_pos = arg_clos.value.pos;
                let arg = NickelValue::new(Term::Closure(Thunk::new(arg_clos)), arg_pos);

                Ok(Closure::atomic_closure(NickelValue::new(
                    Term::EnumVariant {
                        tag: LocIdent::new(tag).with_pos(pos),
                        arg,
                        attrs: EnumVariantAttrs { closurized: true },
                    },
                    pos_op_inh,
                )))
            }
            UnaryOp::EnumGetTag => match &*value {
                Term::EnumVariant { tag, .. } | Term::Enum(tag) => Ok(Closure::atomic_closure(
                    NickelValue::new(Term::Enum(*tag), pos_op_inh),
                )),
                _ => mk_type_error!("Enum"),
            },
            UnaryOp::EnumIsVariant => {
                let result = matches!(&*value, Term::EnumVariant { .. });
                Ok(Closure::atomic_closure(NickelValue::new(
                    Term::Bool(result),
                    pos_op_inh,
                )))
            }
            UnaryOp::PatternBranch => {
                // The continuation, that we must evaluate in the augmented environment.
                let (mut cont, _) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    EvalErrorData::NotEnoughArgs(2, String::from("with_env"), pos_op)
                })?;

                match_sharedterm!(match (t) {
                    Term::Record(data) => {
                        for (id, field) in data.fields {
                            debug_assert!(field.metadata.is_empty());

                            if let Some(value) = field.value {
                                match_sharedterm!(match (value.term) {
                                    Term::Closure(idx) => {
                                        cont.env.insert(id.ident(), idx);
                                    }
                                    _ => {
                                        cont.env.insert(
                                            id.ident(),
                                            self.context.cache.add(
                                                Closure {
                                                    body: value,
                                                    env: env.clone(),
                                                },
                                                BindingType::Normal,
                                            ),
                                        );
                                    }
                                });
                            } else {
                                // This should not really happen, as `with_env` is intended to be
                                // used with very simple records: no metadata, no recursive fields,
                                // no field without definition, etc.
                                debug_assert!(false);
                            }
                        }

                        Ok(cont)
                    }
                    _ => mk_type_error!("Record"),
                })
            }
            UnaryOp::ContractCustom => {
                let contract = if let Term::Fun(..) | Term::Match(_) = &*value {
                    value.closurize(&mut self.context.cache, env)
                } else {
                    return mk_type_error!("Function or MatchExpression");
                };

                Ok(Closure::atomic_closure(NickelValue::new(
                    Term::CustomContract(contract),
                    pos_op_inh,
                )))
            }
            UnaryOp::ContractPostprocessResult => {
                let (tag, arg) = match (*value).clone() {
                    Term::EnumVariant { tag, arg, .. } => (tag, arg),
                    _ => return mk_type_error!("[| 'Ok, 'Error _ |]"),
                };

                // We pop the second argument which isn't strict: we don't need to evaluate the
                // label if there's no error.
                let (label_closure, pos_label) = self.stack.pop_arg(&self.context.cache).unwrap();

                match (tag.label(), arg) {
                    ("Ok", value) => Ok(Closure { value, env }),
                    ("Error", err_data) => {
                        // In the error case, we first need to force the error data so that
                        // primitive values (strings) can be extracted from it, attach the
                        // corresponding data to the label, and then blame.
                        //
                        // To do so, we setup the stack to represent the evaluation context
                        // `%contract/blame% (%label/with_error_data% (%force% [.]) label)` and
                        // then continue with `err_data`.
                        self.stack.push_op_cont(
                            OperationCont::Op1(UnaryOp::Blame, pos_arg),
                            self.call_stack.len(),
                            pos_op_inh,
                        );
                        self.stack.push_op_cont(
                            OperationCont::Op2First(
                                BinaryOp::LabelWithErrorData,
                                label_closure,
                                pos_label,
                            ),
                            self.call_stack.len(),
                            pos_op_inh,
                        );
                        self.stack.push_op_cont(
                            OperationCont::Op1(
                                UnaryOp::Force {
                                    ignore_not_exported: false,
                                },
                                pos_arg,
                            ),
                            self.call_stack.len(),
                            pos_op_inh,
                        );

                        Ok(Closure {
                            value: err_data,
                            env,
                        })
                    }
                    _ => mk_type_error!("[| 'Ok, 'Error {..} |]'"),
                }
            }
            UnaryOp::ContractAttachDefaultLabel => {
                if !matches!(value.as_ref(), Term::EnumVariant { .. }) {
                    return mk_type_error!("[| 'Ok, 'Error _ |]");
                }
                // The stack should already contain the default label to attach, so push
                // the (potential) error data.
                self.stack.push_arg(Closure { value: value, env }, pos_arg);

                Ok(Closure {
                    value: internals::add_default_check_label(),
                    env: Environment::new(),
                })
            }
            UnaryOp::NumberArcCos => Self::process_unary_number_operation(
                value,
                pos_arg,
                pos_op,
                "number/arccos",
                f64::acos,
            ),
            UnaryOp::NumberArcSin => Self::process_unary_number_operation(
                value,
                pos_arg,
                pos_op,
                "number/arcsin",
                f64::asin,
            ),
            UnaryOp::NumberArcTan => Self::process_unary_number_operation(
                value,
                pos_arg,
                pos_op,
                "number/arctan",
                f64::atan,
            ),
            UnaryOp::NumberCos => {
                Self::process_unary_number_operation(value, pos_arg, pos_op, "number/cos", f64::cos)
            }
            UnaryOp::NumberSin => {
                Self::process_unary_number_operation(value, pos_arg, pos_op, "number/sin", f64::sin)
            }
            UnaryOp::NumberTan => {
                Self::process_unary_number_operation(value, pos_arg, pos_op, "number/tan", f64::tan)
            }
        }
    }

    fn process_unary_number_operation<Op>(
        body: NickelValue,
        arg_pos: TermPos,
        pos_op: TermPos,
        op_name: &str,
        op: Op,
    ) -> Result<Closure, EvalError>
    where
        Op: Fn(f64) -> f64,
    {
        if let Term::Num(ref n) = &*body.term {
            let result_as_f64 = op(f64::rounding_from(n, RoundingMode::Nearest).0);
            let result = Number::try_from_float_simplest(result_as_f64).map_err(|_| {
                EvalErrorData::Other(
                    format!(
                        "invalid arithmetic operation: \
                        {op_name}({n}) returned {result_as_f64}, \
                        but {result_as_f64} isn't representable in Nickel"
                    ),
                    pos_op,
                )
            })?;

            Ok(Closure::atomic_closure(NickelValue::new(
                Term::Num(result),
                pos_op.into_inherited(),
            )))
        } else {
            Err(EvalErrorData::UnaryPrimopTypeError {
                primop: String::from(op_name),
                expected: String::from("Number"),
                pos_arg: arg_pos,
                arg_evaluated: body,
            })
        }
    }

    /// Evaluate a binary operation.
    ///
    /// Both arguments are expected to be evaluated (in WHNF). `pos_op` corresponds to the whole
    /// operation position, that may be needed for error reporting.
    fn process_binary_operation(
        &mut self,
        b_op: BinaryOp,
        fst_clos: Closure,
        fst_pos: PosIdx,
        clos: Closure,
        snd_pos: PosIdx,
        pos_op: PosIdx,
    ) -> Result<Closure, EvalError> {
        increment!(format!("primop:{b_op}"));

        let Closure {
            value: NickelValue {
                term: t1,
                pos: pos1,
            },
            env: env1,
        } = fst_clos;
        let Closure {
            value: NickelValue {
                term: t2,
                pos: pos2,
            },
            env: env2,
        } = clos;
        let pos_op_inh = pos_op.into_inherited();

        macro_rules! mk_type_error {
            (op_name=$op_name:expr, $expected:expr, $arg_number:expr, $term:expr, $pos:expr) => {
                Err(EvalErrorData::NAryPrimopTypeError {
                    primop: String::from($op_name),
                    expected: String::from($expected),
                    arg_number: $arg_number,
                    arg_pos: {
                        match $arg_number {
                            1 => fst_pos,
                            2 => snd_pos,
                            _ => unimplemented!(),
                        }
                    },
                    arg_evaluated: NickelValue {
                        term: $term,
                        pos: $pos,
                    },
                    op_pos: pos_op,
                })
            };
            ($expected:expr, $arg_number:expr, $term:expr, $pos:expr) => {
                mk_type_error!(
                    op_name = b_op.to_string(),
                    $expected,
                    $arg_number,
                    $term,
                    $pos
                )
            };
        }

        match b_op {
            BinaryOp::Seal => {
                if let Term::SealingKey(s) = &*t1 {
                    if let Term::Lbl(lbl) = &*t2 {
                        Ok(Closure::atomic_closure(
                            mk_fun!("x", Term::Sealed(*s, mk_term::var("x"), lbl.clone()))
                                .with_pos(pos_op_inh),
                        ))
                    } else {
                        mk_type_error!("Label", 2, t2, pos2)
                    }
                } else {
                    mk_type_error!("SealingKey", 1, t1, pos1)
                }
            }
            BinaryOp::Plus => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Num(n1 + n2),
                            pos_op_inh,
                        )))
                    } else {
                        mk_type_error!("Number", 2, t2, pos2)
                    }
                } else {
                    mk_type_error!("Number", 1, t1, pos1)
                }
            }
            BinaryOp::Sub => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Num(n1 - n2),
                            pos_op_inh,
                        )))
                    } else {
                        mk_type_error!("Number", 2, t2, pos2)
                    }
                } else {
                    mk_type_error!("Number", 1, t1, pos1)
                }
            }
            BinaryOp::Mult => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Num(n1 * n2),
                            pos_op_inh,
                        )))
                    } else {
                        mk_type_error!("Number", 2, t2, pos2)
                    }
                } else {
                    mk_type_error!("Number", 1, t1, pos1)
                }
            }
            BinaryOp::Div => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        if n2 == &Number::ZERO {
                            Err(EvalErrorData::Other(
                                String::from("division by zero"),
                                pos_op,
                            ))
                        } else {
                            Ok(Closure::atomic_closure(NickelValue::new(
                                Term::Num(n1 / n2),
                                pos_op_inh,
                            )))
                        }
                    } else {
                        mk_type_error!("Number", 2, t2, pos2)
                    }
                } else {
                    mk_type_error!("Number", 1, t1, pos1)
                }
            }
            BinaryOp::Modulo => {
                let Term::Num(ref n1) = *t1 else {
                    return mk_type_error!("Number", 1, t1, pos1);
                };

                let Term::Num(ref n2) = *t2 else {
                    return mk_type_error!("Number", 2, t2, pos2);
                };

                if n2 == &Number::ZERO {
                    return Err(EvalErrorData::Other(
                        String::from("division by zero (%)"),
                        pos2,
                    ));
                }

                // This is the equivalent of `truncate()` for `Number`
                let quotient = Number::from(Integer::rounding_from(n1 / n2, RoundingMode::Down).0);

                Ok(Closure::atomic_closure(NickelValue::new(
                    Term::Num(n1 - quotient * n2),
                    pos_op_inh,
                )))
            }
            BinaryOp::NumberArcTan2 => {
                let Term::Num(ref n1) = *t1 else {
                    return mk_type_error!("Number", 1, t1, pos1);
                };

                let Term::Num(ref n2) = *t2 else {
                    return mk_type_error!("Number", 2, t2, pos2);
                };

                let y = f64::rounding_from(n1, RoundingMode::Nearest).0;
                let x = f64::rounding_from(n2, RoundingMode::Nearest).0;

                let result_as_f64 = y.atan2(x);

                let result = Number::try_from_float_simplest(result_as_f64).map_err(|_| {
                    EvalErrorData::Other(
                        format!(
                            "invalid arithmetic operation: \
                            number/arctan2({n1}, {n2}) returned {result_as_f64}, \
                            but {result_as_f64} isn't representable in Nickel"
                        ),
                        pos_op,
                    )
                })?;

                Ok(Closure::atomic_closure(NickelValue::new(
                    Term::Num(result),
                    pos_op_inh,
                )))
            }
            BinaryOp::NumberLog => {
                let Term::Num(ref n1) = *t1 else {
                    return mk_type_error!("Number", 1, t1, pos1);
                };

                let Term::Num(ref n2) = *t2 else {
                    return mk_type_error!("Number", 2, t2, pos2);
                };

                let n = f64::rounding_from(n1, RoundingMode::Nearest).0;

                let result_as_f64 = if n2 == &Number::from(2) {
                    n.log2()
                } else if n2 == &Number::from(10) {
                    n.log10()
                } else {
                    let base = f64::rounding_from(n2, RoundingMode::Nearest).0;
                    n.log(base)
                };

                let result = Number::try_from_float_simplest(result_as_f64).map_err(|_| {
                    EvalErrorData::Other(
                        format!(
                            "invalid arithmetic operation: \
                            number/log({n1}, {n2}) returned {result_as_f64}, \
                            but {result_as_f64} isn't representable in Nickel"
                        ),
                        pos_op,
                    )
                })?;

                Ok(Closure::atomic_closure(NickelValue::new(
                    Term::Num(result),
                    pos_op_inh,
                )))
            }
            BinaryOp::Pow => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        // Malachite's Rationals don't support exponents larger than `u64`. Anyway,
                        // the result of such an operation would be huge and impractical to
                        // store.
                        //
                        // We first try to convert the rational to an `i64`, in which case the
                        // power is computed in an exact way.
                        //
                        // If the conversion fails, we fallback to converting both the exponent and
                        // the value to the nearest `f64`, perform the exponentiation, and convert
                        // the result back to rationals, with a possible loss of precision.
                        let result = if let Ok(n2_as_i64) = i64::try_from(n2) {
                            n1.pow(n2_as_i64)
                        } else {
                            let result_as_f64 = f64::rounding_from(n1, RoundingMode::Nearest)
                                .0
                                .powf(f64::rounding_from(n2, RoundingMode::Nearest).0);
                            // The following conversion fails if the result is NaN or +/-infinity
                            Number::try_from_float_simplest(result_as_f64).map_err(|_| {
                                EvalErrorData::Other(
                                    format!(
                                        "invalid arithmetic operation: \
                                        {n1}^{n2} returned {result_as_f64}, \
                                        but {result_as_f64} isn't representable in Nickel"
                                    ),
                                    pos_op,
                                )
                            })?
                        };

                        Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Num(result),
                            pos_op_inh,
                        )))
                    } else {
                        mk_type_error!("Number", 2, t2, pos2)
                    }
                } else {
                    mk_type_error!("Number", 1, t1, pos1)
                }
            }
            BinaryOp::StringConcat => {
                if let Term::Str(s1) = &*t1 {
                    if let Term::Str(s2) = &*t2 {
                        let ss: [&str; 2] = [s1, s2];
                        Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Str(ss.concat().into()),
                            pos_op_inh,
                        )))
                    } else {
                        mk_type_error!("String", 2, t2, pos2)
                    }
                } else {
                    mk_type_error!("String", 1, t1, pos1)
                }
            }
            BinaryOp::ContractApply | BinaryOp::ContractCheck => {
                // Doing just one `if let Term::Type` and putting the call to `increment!` there
                // looks sensible at first, but it's annoying to explain to rustc and clippy that
                // we match on `typ` but use it only if the `metrics` feature is enabled (we get
                // unused variable warning otherwise). It's simpler to just make a separate `if`
                // conditionally included.
                #[cfg(feature = "metrics")]
                if let Term::Type { typ, .. } = &*t1 {
                    increment!(format!(
                        "primop:contract/apply:{}",
                        typ.pretty_print_cap(40)
                    ));
                }

                let t1 = if let Term::Type { typ: _, contract } = &*t1 {
                    // The contract generation from a static type might return any kind of
                    // contract, including e.g. a record or a custom contract. The result needs to
                    // be evaluated first, and then passed to `b_op` again. In that case, we don't
                    // bother tracking the argument and updating the label: this will be done by
                    // the next call to `b_op`.

                    // We set the stack to represent the evaluation context `<b_op> [.] label` and
                    // proceed to evaluate `<typ.contract()>`
                    self.stack.push_op_cont(
                        OperationCont::Op2First(
                            b_op,
                            Closure {
                                value: NickelValue {
                                    term: t2,
                                    pos: pos2,
                                },
                                env: env2,
                            },
                            fst_pos,
                        ),
                        self.call_stack.len(),
                        pos_op_inh,
                    );

                    return Ok(Closure {
                        value: contract.clone(),
                        env: env1,
                    });
                } else {
                    t1
                };

                let t2 = t2.into_owned();

                if let Term::Lbl(mut label) = t2 {
                    increment!(format!(
                        "contract:originates_from_type {}",
                        label.typ.pretty_print_cap(40)
                    ));

                    #[cfg(feature = "metrics")]
                    if let Some(field) = label.field_name {
                        increment!(format!("contract:originates_from_field {field}"));
                    }

                    // Pop the contract argument to track its cache index in the label for better
                    // error reporting, and because we might add post-processing steps on the stack
                    // which need to sit underneath the value and the label (they will be run after
                    // the contract application is evaluated). We'll just push the value and the
                    // label back on the stack at the end.
                    let (idx, stack_value_pos) =
                        self.stack.pop_arg_as_idx(&mut self.context.cache).ok_or_else(|| {
                            EvalErrorData::NotEnoughArgs(3, String::from("contract/apply"), pos_op)
                        })?;

                    // We update the label and convert it back to a term form that can be cheaply cloned
                    label.arg_pos = self.context.cache.get_then(idx.clone(), |c| c.value.pos);
                    label.arg_idx = Some(idx.clone());
                    let new_label = NickelValue::new(Term::Lbl(label), pos2);

                    // If we're evaluating a plain contract application but we are applying
                    // something with the signature of a custom contract, we need to setup some
                    // post-processing.
                    //
                    // We prepare the stack so that `contract/postprocess_result` will be applied
                    // afterwards. This primop converts the result of a custom contract `'Ok value`
                    // or `'Error err_data` to either `value` or a proper contract error with
                    // `err_data` included in the label.
                    //
                    // That is, prepare the stack to represent the evaluation context
                    // `%contract/postprocess_result% [.] label`
                    if matches!(
                        (&*t1, &b_op),
                        (
                            Term::CustomContract(_) | Term::Record(_),
                            BinaryOp::ContractApply
                        )
                    ) {
                        self.stack
                            .push_arg(Closure::atomic_closure(new_label.clone()), pos_op_inh);

                        self.stack.push_op_cont(
                            OperationCont::Op1(
                                UnaryOp::ContractPostprocessResult,
                                pos1.into_inherited(),
                            ),
                            self.call_stack.len(),
                            pos_op_inh,
                        );
                    }

                    // Contract checks are allowed to specify a blame location, but they don't
                    // have to. We insert an op to check if they omitted the blame location and
                    // put in a default one if not.
                    //
                    // Prepare the stack to represent the evaluation context
                    // `%contract/attach_default_label% [.] label`
                    if matches!(&b_op, BinaryOp::ContractCheck) {
                        self.stack
                            .push_arg(Closure::atomic_closure(new_label.clone()), pos_op_inh);

                        self.stack.push_op_cont(
                            OperationCont::Op1(
                                UnaryOp::ContractAttachDefaultLabel,
                                pos1.into_inherited(),
                            ),
                            self.call_stack.len(),
                            pos_op_inh,
                        );
                    }

                    // Now that we've updated the label, we push the checked value and the new
                    // label back on the stack, so that they become the two arguments of the
                    // contract (transformed to something that can be applied directly). That is,
                    // we prepare the stack to represent the evaluation context `[.] label value`
                    // and proceed with the evaluation of `functoid`.
                    self.stack.push_tracked_arg(idx, stack_value_pos);
                    self.stack
                        .push_arg(Closure::atomic_closure(new_label), pos2.into_inherited());

                    // We convert the contract (which can be a custom contract, a record, a naked
                    // function, etc.) to a form that can be applied to a label and a value.
                    let functoid = match &*t1 {
                        Term::Fun(..) | Term::Match { .. } => {
                            let as_naked = NickelValue {
                                term: t1,
                                pos: pos1,
                            };

                            // Warn on naked function contracts, but not if they came from the
                            // stdlib. Some stdlib functions return naked function contracts.
                            if let Some(pos) = pos1.as_opt_ref() {
                                if !self.context.import_resolver.files().is_stdlib(pos.src_id) {
                                    self.warn(Warning::NakedFunctionContract {
                                        func_pos: pos1,
                                        app_pos: pos_op,
                                    });
                                }
                            }

                            if let BinaryOp::ContractApply = b_op {
                                Closure {
                                    value: as_naked,
                                    env: env1,
                                }
                            } else {
                                // Prepare the stack to represent the evaluation context `[.]
                                // as_naked` and proceed with `$naked_to_custom`
                                self.stack.push_arg(
                                    Closure {
                                        value: as_naked,
                                        env: env1,
                                    },
                                    fst_pos,
                                );

                                Closure {
                                    value: internals::naked_to_custom(),
                                    env: Environment::new(),
                                }
                            }
                        }
                        Term::CustomContract(ctr) => Closure {
                            value: ctr.clone(),
                            env: env1,
                        },
                        Term::Record(..) => {
                            // Prepare the stack to represent the evaluation context `[.] t1` and
                            // proceed with `$record_contract`
                            self.stack.push_arg(
                                Closure {
                                    value: NickelValue {
                                        term: t1,
                                        pos: pos1,
                                    },
                                    env: env1,
                                },
                                fst_pos,
                            );

                            Closure {
                                value: internals::record_contract(),
                                env: Environment::new(),
                            }
                        }
                        _ => return mk_type_error!("Contract", 1, t1, pos1),
                    };

                    Ok(functoid)
                } else {
                    mk_type_error!("Label", 2, t2.into(), pos2)
                }
            }
            BinaryOp::LabelWithErrorData => {
                // We need to extract plain values from a Nickel data structure, which most likely
                // contains closures at least, even if it's fully evaluated. As for serialization,
                // we thus need to fully substitute all variables first.
                let t1 = subst(
                    &self.context.cache,
                    NickelValue {
                        term: t1,
                        pos: pos1,
                    },
                    &self.initial_env,
                    &env1,
                )
                .term
                .into_owned();

                let t2 = t2.into_owned();

                let Term::Lbl(mut label) = t2 else {
                    return mk_type_error!("Label", 2, t2.into(), pos2);
                };

                if let Term::Record(mut record_data) = t1 {
                    // If the contract returned a label as part of its error
                    // data, blame that one instead.
                    if let Some(Term::Lbl(user_label)) = record_data
                        .fields
                        .swap_remove(&LocIdent::from("blame_location"))
                        .and_then(|field| field.value)
                        .map(|v| v.term.into_owned())
                    {
                        label = user_label;
                    }

                    if let Some(Term::Str(msg)) = record_data
                        .fields
                        .swap_remove(&LocIdent::from("message"))
                        .and_then(|field| field.value)
                        .map(|v| v.term.into_owned())
                    {
                        label = label.with_diagnostic_message(msg.into_inner());
                    }

                    if let Some(notes_term) = record_data
                        .fields
                        .swap_remove(&LocIdent::from("notes"))
                        .and_then(|field| field.value)
                    {
                        if let Term::Array(array, _) = notes_term.into() {
                            let notes = array
                                .into_iter()
                                .map(|element| {
                                    let term = element.term.into_owned();

                                    if let Term::Str(s) = term {
                                        Ok(s.into_inner())
                                    } else {
                                        mk_type_error!(
                                            "String (notes)",
                                            1,
                                            term.into(),
                                            element.pos
                                        )
                                    }
                                })
                                .collect::<Result<Vec<_>, _>>()?;

                            label = label.with_diagnostic_notes(notes);
                        }
                    }

                    Ok(Closure::atomic_closure(NickelValue::new(
                        Term::Lbl(label),
                        pos2,
                    )))
                } else {
                    mk_type_error!("Record", 1, t1.into(), pos1)
                }
            }
            BinaryOp::Unseal => {
                if let Term::SealingKey(s1) = &*t1 {
                    // Return a function that either behaves like the identity or
                    // const unwrapped_term

                    Ok(if let Term::Sealed(s2, t, _) = t2.into_owned() {
                        if *s1 == s2 {
                            Closure {
                                value: mk_fun!("-invld", t),
                                env: env2,
                            }
                        } else {
                            Closure::atomic_closure(mk_term::id())
                        }
                    } else {
                        Closure::atomic_closure(mk_term::id())
                    })
                } else {
                    mk_type_error!("SealingKey", 1, t1, pos1)
                }
            }
            BinaryOp::Eq => {
                let c1 = Closure {
                    value: NickelValue {
                        term: t1,
                        pos: pos1,
                    },
                    env: env1,
                };
                let c2 = Closure {
                    value: NickelValue {
                        term: t2,
                        pos: pos2,
                    },
                    env: env2,
                };

                match eq(&mut self.context.cache, c1, c2, pos_op_inh)? {
                    EqResult::Bool(b) => match (b, self.stack.pop_eq()) {
                        (false, _) => {
                            self.stack.clear_eqs();
                            Ok(Closure::atomic_closure(NickelValue::new(
                                Term::Bool(false),
                                pos_op_inh,
                            )))
                        }
                        (true, None) => Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Bool(true),
                            pos_op_inh,
                        ))),
                        (true, Some((c1, c2))) => {
                            let t1 = c1.value.closurize(&mut self.context.cache, c1.env);
                            let t2 = c2.value.closurize(&mut self.context.cache, c2.env);

                            Ok(Closure {
                                value: NickelValue::new(Term::Op2(BinaryOp::Eq, t1, t2), pos_op),
                                env: Environment::new(),
                            })
                        }
                    },
                    EqResult::Eqs(t1, t2, subeqs) => {
                        self.stack.push_eqs(subeqs.into_iter());

                        Ok(Closure {
                            value: NickelValue::new(Term::Op2(BinaryOp::Eq, t1, t2), pos_op),
                            env: Environment::new(),
                        })
                    }
                }
            }
            BinaryOp::LessThan => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Bool(n1 < n2),
                            pos_op_inh,
                        )))
                    } else {
                        mk_type_error!("Number", 2, t2, pos2)
                    }
                } else {
                    mk_type_error!("Number", 1, t1, pos1)
                }
            }
            BinaryOp::LessOrEq => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Bool(n1 <= n2),
                            pos_op_inh,
                        )))
                    } else {
                        mk_type_error!("Number", 2, t2, pos2)
                    }
                } else {
                    mk_type_error!("Number", 1, t1, pos1)
                }
            }
            BinaryOp::GreaterThan => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Bool(n1 > n2),
                            pos_op_inh,
                        )))
                    } else {
                        mk_type_error!("Number", 2, t2, pos2)
                    }
                } else {
                    mk_type_error!("Number", 1, t1, pos1)
                }
            }
            BinaryOp::GreaterOrEq => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Bool(n1 >= n2),
                            pos_op_inh,
                        )))
                    } else {
                        mk_type_error!("Number", 2, t2, pos2)
                    }
                } else {
                    mk_type_error!("Number", 1, t1, pos1)
                }
            }
            BinaryOp::LabelGoField => match_sharedterm!(match (t1) {
                Term::Str(field) => match_sharedterm!(match (t2) {
                    Term::Lbl(l) => {
                        let mut l = l;
                        l.path.push(ty_path::Elem::Field(field.into_inner().into()));
                        Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Lbl(l),
                            pos_op_inh,
                        )))
                    }
                    _ => mk_type_error!("Label", 2, t2, pos2),
                }),
                _ => mk_type_error!("String", 1, t1, pos1),
            }),
            BinaryOp::RecordGet => {
                match_sharedterm!(match (t1) {
                    Term::Str(id) => {
                        if let Term::Record(record) = &*t2 {
                            // We have to apply potential pending contracts. Right now, this
                            // means that repeated field access will re-apply the contract again
                            // and again, which is not optimal. The same thing happens with array
                            // contracts. There are several way to improve this, but this is left
                            // as future work.
                            let ident = LocIdent::from(&id);
                            match record.get_value_with_ctrs(&ident).map_err(
                                |missing_field_err| missing_field_err.into_eval_err(pos2, pos_op),
                            )? {
                                Some(value) => {
                                    self.call_stack.enter_field(ident, pos2, value.pos, pos_op);
                                    Ok(Closure {
                                        body: value,
                                        env: env2,
                                    })
                                }
                                None => match record.sealed_tail.as_ref() {
                                    Some(t) if t.has_dyn_field(&id) => {
                                        Err(EvalErrorData::IllegalPolymorphicTailAccess {
                                            action: IllegalPolymorphicTailAction::FieldAccess {
                                                field: id.to_string(),
                                            },
                                            evaluated_arg: t
                                                .label
                                                .get_evaluated_arg(&self.context.cache),
                                            label: t.label.clone(),
                                            call_stack: std::mem::take(&mut self.call_stack),
                                        })
                                    }
                                    _ => Err(EvalErrorData::FieldMissing {
                                        id: ident,
                                        field_names: record
                                            .field_names(RecordOpKind::IgnoreEmptyOpt),
                                        operator: String::from("(.$)"),
                                        pos_record: pos2,
                                        pos_op,
                                    }),
                                },
                            }
                        } else {
                            // Not using mk_type_error! because of a non-uniform message
                            Err(EvalErrorData::TypeError {
                                expected: String::from("Record"),
                                message: String::from("field access only makes sense for records"),
                                orig_pos: snd_pos,
                                term: NickelValue {
                                    term: t2,
                                    pos: pos2,
                                },
                            })
                        }
                    }
                    // This error should be impossible to trigger. The parser
                    // prevents a dynamic field access where the field name is not syntactically
                    // a string.
                    _ => mk_type_error!("String", 1, t1, pos1),
                })
            }
            BinaryOp::RecordInsert {
                metadata,
                pending_contracts,
                ext_kind,
                op_kind,
            } => {
                if let Term::Str(id) = &*t1 {
                    match_sharedterm!(match (t2) {
                        Term::Record(record) => {
                            let mut fields = record.fields;

                            // If a defined value is expected for this field, it must be
                            // provided as an additional argument, so we pop it from the stack
                            let value = if let RecordExtKind::WithValue = ext_kind {
                                let (value_closure, _) =
                                    self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                                        EvalErrorData::NotEnoughArgs(
                                            3,
                                            String::from("insert"),
                                            pos_op,
                                        )
                                    })?;

                                let closurized = value_closure
                                    .body
                                    .closurize(&mut self.context.cache, value_closure.env);
                                Some(closurized)
                            } else {
                                None
                            };

                            match fields.insert(
                                LocIdent::from(id),
                                Field {
                                    value,
                                    metadata: *metadata,
                                    pending_contracts,
                                },
                            ) {
                                Some(t)
                                    if matches!(op_kind, RecordOpKind::ConsiderAllFields)
                                        || !t.is_empty_optional() =>
                                {
                                    Err(EvalErrorData::Other(
                                        format!(
                                            "record/insert: \
                                            tried to extend a record with the field {id}, \
                                            but it already exists"
                                        ),
                                        pos_op,
                                    ))
                                }
                                _ => Ok(Closure {
                                    // Insertion preserves the frozenness
                                    body: Term::Record(RecordData { fields, ..record }).into(),
                                    env: env2,
                                }),
                            }
                        }
                        _ => mk_type_error!(op_name = "record/insert", "Record", 2, t2, pos2),
                    })
                } else {
                    mk_type_error!(op_name = "record/insert", "String", 1, t1, pos1)
                }
            }
            BinaryOp::RecordRemove(op_kind) => match_sharedterm!(match (t1) {
                Term::Str(id) => match_sharedterm!(match (t2) {
                    Term::Record(record) => {
                        let mut fields = record.fields;
                        let fetched = fields.swap_remove(&LocIdent::from(&id));
                        if fetched.is_none()
                            || matches!(
                                (op_kind, fetched),
                                (
                                    RecordOpKind::IgnoreEmptyOpt,
                                    Some(Field {
                                        value: None,
                                        metadata: FieldMetadata { opt: true, .. },
                                        ..
                                    })
                                )
                            )
                        {
                            match record.sealed_tail.as_ref() {
                                Some(t) if t.has_dyn_field(&id) => {
                                    Err(EvalErrorData::IllegalPolymorphicTailAccess {
                                        action: IllegalPolymorphicTailAction::FieldRemove {
                                            field: id.to_string(),
                                        },
                                        evaluated_arg: t
                                            .label
                                            .get_evaluated_arg(&self.context.cache),
                                        label: t.label.clone(),
                                        call_stack: std::mem::take(&mut self.call_stack),
                                    })
                                }
                                _ => {
                                    // We reconstruct the record's data to have access to
                                    // `data.field_names()`
                                    let record = RecordData { fields, ..record };

                                    Err(EvalErrorData::FieldMissing {
                                        id: id.into(),
                                        field_names: record.field_names(op_kind),
                                        operator: String::from("record/remove"),
                                        pos_record: pos2,
                                        pos_op,
                                    })
                                }
                            }
                        } else {
                            Ok(Closure {
                                body: NickelValue::new(
                                    // Removal preserves the frozenness
                                    Term::Record(RecordData { fields, ..record }),
                                    pos_op_inh,
                                ),
                                env: env2,
                            })
                        }
                    }
                    _ => mk_type_error!("Record", 2, t2, pos2),
                }),
                _ => mk_type_error!("String", 1, t1, pos1),
            }),
            BinaryOp::RecordHasField(op_kind) => match_sharedterm!(match (t1) {
                Term::Str(id) => {
                    if let Term::Record(record) = &*t2 {
                        Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Bool(matches!(
                                record.fields.get(&LocIdent::from(id.into_inner())),
                                Some(field) if matches!(op_kind, RecordOpKind::ConsiderAllFields) || !field.is_empty_optional()
                            )),
                            pos_op_inh,
                        )))
                    } else {
                        mk_type_error!("Record", 2, t2, pos2)
                    }
                }
                _ => mk_type_error!("String", 1, t1, pos1),
            }),
            BinaryOp::RecordFieldIsDefined(op_kind) => match_sharedterm!(match (t1) {
                Term::Str(id) => {
                    if let Term::Record(record) = &*t2 {
                        Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Bool(matches!(
                                record.fields.get(&LocIdent::from(id.into_inner())),
                                Some(field @ Field { value: Some(_), ..}) if matches!(op_kind, RecordOpKind::ConsiderAllFields) || !field.is_empty_optional()
                            )),
                            pos_op_inh,
                        )))
                    } else {
                        mk_type_error!("Record", 2, t2, pos2)
                    }
                }
                _ => mk_type_error!("String", 1, t1, pos1),
            }),
            BinaryOp::ArrayConcat => match_sharedterm!(match (t1) {
                Term::Array(ts1, attrs1) => match_sharedterm!(match (t2) {
                    Term::Array(ts2, attrs2) => {
                        let mut ts1 = ts1;
                        // NOTE: the [eval_closure] function in [eval] should've made sure
                        // that the array is closurized. We leave a debug_assert! here just
                        // in case something goes wrong in the future. If the assert failed,
                        // you may need to map closurize over `ts1` and `ts2`.
                        debug_assert!(
                            attrs1.closurized,
                            "the left-hand side of ArrayConcat (@) is not closurized."
                        );
                        debug_assert!(
                            attrs2.closurized,
                            "the right-hand side of ArrayConcat (@) is not closurized."
                        );

                        // We have two sets of contracts from the LHS and RHS arrays.
                        // - Common contracts between the two sides can be put into
                        // `pending_contracts` of the resulting concatenation as they're
                        // shared by all elements: we don't have to apply them just yet.
                        // - Contracts thats are specific to the LHS or the RHS have to
                        // applied because we don't have a way of tracking which elements
                        // should take which contracts.

                        // Separate contracts between the parts that aren't common, and
                        // must be applied right away, and the common part, which can be
                        // kept lazy.
                        let mut ctrs_left = attrs1.pending_contracts;
                        // We use a vector of `Option` so that we can set the elements to
                        // remove to `None` and make a single pass at the end
                        // to retain the remaining ones.
                        let mut ctrs_right_sieve: Vec<_> =
                            attrs2.pending_contracts.into_iter().map(Some).collect();
                        let mut ctrs_common = Vec::new();

                        // We basically compute the intersection (`ctr_common`),
                        // `ctrs_left - ctr_common`, and `ctrs_right - ctr_common`.
                        let ctrs_left_dedup: Vec<_> = ctrs_left
                            .into_iter()
                            .filter(|ctr| {
                                // We don't deduplicate polymorphic contracts, because
                                // they're not idempotent.
                                if ctr.can_have_poly_ctrs() {
                                    return true;
                                }

                                // We check if there is a remaining contract in
                                // `ctrs_right_sieve` which matches `ctr`: in this case,
                                // `twin_index` will hold its index.
                                let twin_index = ctrs_right_sieve.iter().position(|other_ctr| {
                                    other_ctr.as_ref().is_some_and(|other_ctr| {
                                        contract_eq(
                                            &ctr.contract,
                                            &env1,
                                            &other_ctr.contract,
                                            &env2,
                                        )
                                    })
                                });

                                if let Some(index) = twin_index {
                                    // unwrap(): we know that the contract at this index is
                                    // `Some`, because all elements are initially some when
                                    // creating `ctrs_right_sieve` and then we don't
                                    // consider `None` values when computing a new `index`
                                    // in the `position` above.
                                    let common = ctrs_right_sieve[index].take().unwrap();
                                    ctrs_common.push(common);
                                    false
                                } else {
                                    true
                                }
                            })
                            .collect();

                        let ctrs_right_empty = ctrs_right_sieve.iter().all(Option::is_none);
                        let ctrs_right_dedup = ctrs_right_sieve.into_iter().flatten();

                        let ctrs_left_empty = ctrs_left_dedup.is_empty();

                        let arr = if ctrs_right_empty && ctrs_left_empty {
                            ts1.extend(ts2);
                            ts1
                        } else if ctrs_left_empty {
                            ts1.extend(ts2.into_iter().map(|t| {
                                RuntimeContract::apply_all(t, ctrs_right_dedup.clone(), pos1)
                                    .closurize(&mut self.context.cache, env1.clone())
                            }));
                            ts1
                        } else {
                            let mut ts = Array::default();

                            ts.extend(ts1.into_iter().map(|t| {
                                RuntimeContract::apply_all(t, ctrs_left_dedup.iter().cloned(), pos1)
                                    .closurize(&mut self.context.cache, env1.clone())
                            }));

                            ts.extend(ts2.into_iter().map(|t| {
                                RuntimeContract::apply_all(t, ctrs_right_dedup.clone(), pos2)
                                    .closurize(&mut self.context.cache, env2.clone())
                            }));

                            ts
                        };

                        let attrs = ArrayAttrs {
                            closurized: true,
                            pending_contracts: ctrs_common,
                        };

                        Ok(Closure {
                            body: NickelValue::new(Term::Array(arr, attrs), pos_op_inh),
                            env: Environment::new(),
                        })
                    }
                    _ => {
                        mk_type_error!("Array", 2, t2, pos2)
                    }
                }),
                _ => {
                    mk_type_error!("Array", 1, t1, pos1)
                }
            }),
            BinaryOp::ArrayAt => match (&*t1, &*t2) {
                (Term::Array(ts, attrs), Term::Num(n)) => {
                    let Ok(n_as_usize) = usize::try_from(n) else {
                        return Err(EvalErrorData::Other(
                            format!(
                                "array/at expects its second argument to be a \
                                positive integer smaller than {}, got {n}",
                                usize::MAX
                            ),
                            pos_op,
                        ));
                    };

                    if n_as_usize >= ts.len() {
                        return Err(EvalErrorData::Other(
                            format!(
                                "array/at: index out of bounds. \
                                Expected an index between 0 and {}, got {}",
                                ts.len(),
                                n
                            ),
                            pos_op,
                        ));
                    }

                    let elem_with_ctr = RuntimeContract::apply_all(
                        ts.get(n_as_usize).unwrap().clone(),
                        attrs.pending_contracts.iter().cloned(),
                        pos1.into_inherited(),
                    );

                    Ok(Closure {
                        value: elem_with_ctr,
                        env: env1,
                    })
                }
                (Term::Array(..), _) => mk_type_error!("Number", 2, t2, pos2),
                (_, _) => mk_type_error!("Array", 1, t1, pos1),
            },
            BinaryOp::Merge(merge_label) => merge::merge(
                &mut self.context.cache,
                NickelValue {
                    term: t1,
                    pos: pos1,
                },
                env1,
                NickelValue {
                    term: t2,
                    pos: pos2,
                },
                env2,
                pos_op,
                MergeMode::Standard(merge_label),
                &mut self.call_stack,
            ),
            BinaryOp::Hash => {
                let mk_err_fst =
                    |t1| mk_type_error!("[| 'Md5, 'Sha1, 'Sha256, 'Sha512 |]", 1, t1, pos1);

                if let Term::Enum(id) = &*t1 {
                    if let Term::Str(s) = &*t2 {
                        let result = match id.as_ref() {
                            "Md5" => {
                                let mut hasher = md5::Md5::new();
                                hasher.update(s.as_ref());
                                format!("{:x}", hasher.finalize())
                            }
                            "Sha1" => {
                                let mut hasher = sha1::Sha1::new();
                                hasher.update(s.as_ref());
                                format!("{:x}", hasher.finalize())
                            }
                            "Sha256" => {
                                let mut hasher = sha2::Sha256::new();
                                hasher.update(s.as_ref());
                                format!("{:x}", hasher.finalize())
                            }
                            "Sha512" => {
                                let mut hasher = sha2::Sha512::new();
                                hasher.update(s.as_ref());
                                format!("{:x}", hasher.finalize())
                            }
                            _ => return mk_err_fst(t1),
                        };

                        Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Str(result.into()),
                            pos_op_inh,
                        )))
                    } else {
                        mk_type_error!("String", 2, t2, pos2)
                    }
                } else {
                    mk_err_fst(t1)
                }
            }
            BinaryOp::Serialize => {
                let mk_err_fst = |t1| mk_type_error!("[| 'Json, 'Yaml, 'Toml |]", 1, t1, pos1);

                if let Term::Enum(ref id) = t1.as_ref() {
                    // Serialization needs all variables term to be fully substituted
                    let initial_env = Environment::new();
                    let rt2 = subst(
                        &self.context.cache,
                        NickelValue {
                            term: t2,
                            pos: pos2,
                        },
                        &initial_env,
                        &env2,
                    );

                    let format = match id.to_string().as_str() {
                        "Json" => ExportFormat::Json,
                        "Yaml" => ExportFormat::Yaml,
                        "Toml" => ExportFormat::Toml,
                        _ => return mk_err_fst(t1),
                    };

                    serialize::validate(format, &rt2)?;
                    Ok(Closure::atomic_closure(NickelValue::new(
                        Term::Str(serialize::to_string(format, &rt2)?.into()),
                        pos_op_inh,
                    )))
                } else {
                    mk_err_fst(t1)
                }
            }
            BinaryOp::Deserialize => {
                let mk_err_fst = |t1| mk_type_error!("[| 'Json, 'Yaml, 'Toml |]", 1, t1, pos1);

                if let Term::Enum(id) = &*t1 {
                    if let Term::Str(s) = &*t2 {
                        let rt: NickelValue = match id.as_ref() {
                            "Json" => serde_json::from_str(s).map_err(|err| {
                                EvalErrorData::DeserializationError(
                                    String::from("json"),
                                    format!("{err}"),
                                    pos_op,
                                )
                            })?,
                            // TODO: we could try to generate better error positions here,
                            // but it will be some work.
                            //
                            // We pass `None` to `load_yaml` (so it produces a position-less
                            // `NickelValue`) even if we have a position for `s`, because `s` is
                            // likely not at offset zero in its file and so `load_yaml` will give
                            // the wrong error locations. Were it just a matter of offsetting the
                            // error location, this would be easy to fix. Unfortunately getting the
                            // locations right would involve handling location shifts caused by
                            // escape sequences and interpolation.
                            "Yaml" => {
                                crate::serialize::yaml::load_yaml_term(s, None).map_err(|err| {
                                    EvalError::DeserializationErrorWithInner {
                                        format: InputFormat::Yaml,
                                        inner: err,
                                        pos: pos_op,
                                    }
                                })?
                            }
                            "Toml" => toml::from_str(s).map_err(|err| {
                                EvalErrorData::DeserializationError(
                                    String::from("toml"),
                                    format!("{err}"),
                                    pos_op,
                                )
                            })?,
                            _ => return mk_err_fst(t1),
                        };

                        Ok(Closure::atomic_closure(rt.with_pos(pos_op_inh)))
                    } else {
                        mk_type_error!("String", 2, t2, pos2)
                    }
                } else {
                    mk_err_fst(t1)
                }
            }
            BinaryOp::StringSplit => match (&*t1, &*t2) {
                (Term::Str(input), Term::Str(separator)) => {
                    let result = input.split(separator);
                    Ok(Closure::atomic_closure(NickelValue::new(
                        Term::Array(result, ArrayAttrs::new().closurized()),
                        pos_op_inh,
                    )))
                }
                (Term::Str(_), _) => mk_type_error!("String", 2, t2, pos2),
                (_, _) => mk_type_error!("String", 1, t1, pos1),
            },
            BinaryOp::StringContains => match (&*t1, &*t2) {
                (Term::Str(s1), Term::Str(s2)) => {
                    let result = s1.contains(s2.as_str());
                    Ok(Closure::atomic_closure(NickelValue::new(
                        Term::Bool(result),
                        pos_op_inh,
                    )))
                }
                (Term::Str(_), _) => mk_type_error!("String", 2, t2, pos2),
                (_, _) => mk_type_error!("String", 1, t1, pos1),
            },
            BinaryOp::StringCompare => match (&*t1, &*t2) {
                (Term::Str(s1), Term::Str(s2)) => {
                    use std::cmp::Ordering;
                    Ok(Closure::atomic_closure(NickelValue::new(
                        Term::Enum(LocIdent::new_with_pos(
                            match s1.cmp(s2) {
                                Ordering::Less => "Lesser",
                                Ordering::Equal => "Equal",
                                Ordering::Greater => "Greater",
                            },
                            pos_op_inh,
                        )),
                        pos_op_inh,
                    )))
                }
                (Term::Str(_), _) => mk_type_error!("String", 2, t2, pos2),
                (_, _) => mk_type_error!("String", 1, t1, pos1),
            },
            BinaryOp::ContractArrayLazyApp => {
                let (ctr, _) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    EvalErrorData::NotEnoughArgs(3, String::from("contract/array_lazy_app"), pos_op)
                })?;

                let Closure {
                    value: rt3,
                    env: env3,
                } = ctr;

                // FIXME: use match?
                let lbl = match_sharedterm!(match (t1) {
                    Term::Lbl(lbl) => lbl,
                    _ => return mk_type_error!("Label", 1, t1, pos1),
                });

                match_sharedterm!(match (t2) {
                    Term::Array(ts, attrs) => {
                        let mut attrs = attrs;
                        let mut final_env = env2;

                        // Preserve the environment of the contract in the resulting array.
                        let contract = rt3.closurize(&mut self.context.cache, env3);
                        RuntimeContract::push_dedup(
                            &mut attrs.pending_contracts,
                            &final_env,
                            RuntimeContract::new(contract, lbl),
                            &final_env,
                        );

                        let array_with_ctr = Closure {
                            body: NickelValue::new(Term::Array(ts, attrs), pos2),
                            env: final_env,
                        };

                        Ok(array_with_ctr)
                    }
                    _ => mk_type_error!("Array", 2, t2, pos2),
                })
            }
            BinaryOp::ContractRecordLazyApp => {
                // The contract is expected to be of type `String -> Contract`: it takes the name
                // of the field as a parameter, and returns a contract.
                let (
                    Closure {
                        value: contract_term,
                        env: contract_env,
                    },
                    _,
                ) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    EvalErrorData::NotEnoughArgs(
                        3,
                        String::from("contract/record_lazy_app"),
                        pos_op,
                    )
                })?;

                let label = match_sharedterm!(match (t1) {
                    Term::Lbl(label) => label,
                    _ => return mk_type_error!("Label", 1, t1, pos1),
                });

                match_sharedterm!(match (t2) {
                    Term::Record(record_data) => {
                        // due to a limitation of `match_sharedterm`: see the macro's
                        // documentation
                        let mut record_data = record_data;

                        // Applying a lazy contract unfreezes a record, as frozen record are
                        // expected to have all their contracts applied and thus an empty list of
                        // pending contracts.
                        record_data.attrs.frozen = false;

                        let mut contract_at_field = |id: LocIdent| {
                            let pos = contract_term.pos;
                            mk_app!(
                                contract_term.clone(),
                                NickelValue::new(Term::Str(id.into()), id.pos)
                            )
                            .with_pos(pos)
                            .closurize(&mut self.context.cache, contract_env.clone())
                        };

                        for (id, field) in record_data.fields.iter_mut() {
                            let runtime_ctr = RuntimeContract {
                                contract: contract_at_field(*id),
                                label: label.clone(),
                            };

                            RuntimeContract::push_dedup(
                                &mut field.pending_contracts,
                                &env2,
                                runtime_ctr,
                                &contract_env,
                            );
                        }

                        // IMPORTANT: here, we revert the record back to a `RecRecord`. The
                        // reason is that applying a contract over fields might change the
                        // value of said fields (the typical example is adding a value to a
                        // subrecord via the default value of a contract).
                        //
                        // We want recursive occurrences of fields to pick this new value as
                        // well: hence, we need to recompute the fixpoint, which is done by
                        // `fixpoint::revert`.
                        let reverted =
                            super::fixpoint::revert(&mut self.context.cache, record_data);

                        Ok(Closure {
                            body: NickelValue::new(reverted, pos2),
                            env: Environment::new(),
                        })
                    }
                    _ => mk_type_error!("Record", 2, t2, pos2),
                })
            }
            BinaryOp::LabelWithMessage => {
                let t1 = t1.into_owned();
                let t2 = t2.into_owned();

                let Term::Str(message) = t1 else {
                    return mk_type_error!("String", 1, t1.into(), pos1);
                };

                let Term::Lbl(label) = t2 else {
                    return mk_type_error!("String", 2, t2.into(), pos2);
                };

                Ok(Closure::atomic_closure(NickelValue::new(
                    Term::Lbl(label.with_diagnostic_message(message.into_inner())),
                    pos_op_inh,
                )))
            }
            BinaryOp::LabelWithNotes => {
                let t2 = t2.into_owned();

                // We need to extract plain strings from a Nickel array, which most likely
                // contains at least generated variables.
                // As for serialization, we thus fully substitute all variables first.
                let t1_subst = subst(
                    &self.context.cache,
                    NickelValue {
                        term: t1,
                        pos: pos1,
                    },
                    &Environment::new(),
                    &env1,
                );
                let t1 = t1_subst.term.into_owned();

                let Term::Array(array, _) = t1 else {
                    return mk_type_error!("Array", 1, t1.into(), pos1);
                };

                let notes = array
                    .into_iter()
                    .map(|element| {
                        let term = element.term.into_owned();

                        if let Term::Str(s) = term {
                            Ok(s.into_inner())
                        } else {
                            mk_type_error!("String", 1, term.into(), element.pos)
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let Term::Lbl(label) = t2 else {
                    return mk_type_error!("Label", 2, t2.into(), pos2);
                };

                Ok(Closure::atomic_closure(NickelValue::new(
                    Term::Lbl(label.with_diagnostic_notes(notes)),
                    pos_op_inh,
                )))
            }
            BinaryOp::LabelAppendNote => {
                let t1 = t1.into_owned();
                let t2 = t2.into_owned();

                let Term::Str(note) = t1 else {
                    return mk_type_error!("String", 1, t1.into(), pos1);
                };

                let Term::Lbl(label) = t2 else {
                    return mk_type_error!("Label", 2, t2.into(), pos2);
                };

                Ok(Closure::atomic_closure(NickelValue::new(
                    Term::Lbl(label.append_diagnostic_note(note.into_inner())),
                    pos2.into_inherited(),
                )))
            }
            BinaryOp::LabelLookupTypeVar => {
                let t1 = t1.into_owned();
                let t2 = t2.into_owned();

                let Term::SealingKey(key) = t1 else {
                    return mk_type_error!("SealingKey", 1, t1.into(), pos1);
                };

                let Term::Lbl(label) = t2 else {
                    return mk_type_error!("Label", 2, t2.into(), pos2);
                };

                Ok(Closure::atomic_closure(NickelValue::new(
                    label.type_environment.get(&key).unwrap().into(),
                    pos_op_inh,
                )))
            }
            BinaryOp::RecordSplitPair => {
                let t1 = t1.into_owned();
                let t2 = t2.into_owned();

                let Term::Record(record1) = t1 else {
                    return mk_type_error!("Record", 1, t1.into(), pos1);
                };

                let Term::Record(record2) = t2 else {
                    return mk_type_error!("Record", 2, t2.into(), pos2);
                };

                let split::SplitResult {
                    left,
                    center,
                    right,
                } = split::split(record1.fields, record2.fields);

                let left_only = Term::Record(RecordData {
                    fields: left,
                    sealed_tail: record1.sealed_tail,
                    attrs: record1.attrs,
                });

                let right_only = Term::Record(RecordData {
                    fields: right,
                    sealed_tail: record2.sealed_tail,
                    attrs: record2.attrs,
                });

                let (center1, center2): (IndexMap<LocIdent, Field>, IndexMap<LocIdent, Field>) =
                    center
                        .into_iter()
                        .map(|(id, (left, right))| ((id, left), (id, right)))
                        .unzip();

                let left_center = Term::Record(RecordData {
                    fields: center1,
                    sealed_tail: None,
                    attrs: RecordAttrs::default().closurized(),
                });

                let right_center = Term::Record(RecordData {
                    fields: center2,
                    sealed_tail: None,
                    attrs: RecordAttrs::default().closurized(),
                });

                Ok(Closure::atomic_closure(NickelValue::new(
                    Term::Record(RecordData {
                        fields: IndexMap::from([
                            (
                                LocIdent::from("left_only"),
                                Field::from(NickelValue::from(left_only)),
                            ),
                            (
                                LocIdent::from("left_center"),
                                Field::from(NickelValue::from(left_center)),
                            ),
                            (
                                LocIdent::from("right_center"),
                                Field::from(NickelValue::from(right_center)),
                            ),
                            (
                                LocIdent::from("right_only"),
                                Field::from(NickelValue::from(right_only)),
                            ),
                        ]),
                        attrs: RecordAttrs::default().closurized(),
                        sealed_tail: None,
                    }),
                    pos_op_inh,
                )))
            }
            BinaryOp::RecordDisjointMerge => {
                let t1 = t1.into_owned();
                let t2 = t2.into_owned();

                let Term::Record(mut record1) = t1 else {
                    return mk_type_error!("Record", 1, t1.into(), pos1);
                };

                let Term::Record(record2) = t2 else {
                    return mk_type_error!("Record", 2, t2.into(), pos2);
                };

                // As for merge, we refuse to combine two records if one of them has a sealed tail.
                // However, if only one of them does, because we don't do any recursive
                // re-evaluation here, it's fine to just pick this tail as the tail of the result.
                //
                // This behavior is actually useful, because disjoint_merge is used in the
                // implementation of builtin contracts to combine an unsealed tail with the
                // original body of the record. In that case, the unsealed tail might have an
                // additional sealed tail itself (tail can be sealed multiple times in a nested
                // way), and the right behavior is to just keep it.
                let sealed_tail = match (record1.sealed_tail, record2.sealed_tail) {
                    (Some(record::SealedTail { label, .. }), Some(_)) => {
                        return Err(EvalErrorData::IllegalPolymorphicTailAccess {
                            action: IllegalPolymorphicTailAction::Merge,
                            evaluated_arg: label.get_evaluated_arg(&self.context.cache),
                            label,
                            call_stack: std::mem::take(&mut self.call_stack),
                        });
                    }
                    (tail1, tail2) => tail1.or(tail2),
                };

                // Note that because of record closurization, we assume here that the record data
                // of each record are already closurized, so we don't really care about
                // environments. Should that invariant change, we might get into trouble (trouble
                // meaning undue `UnboundIdentifier` errors).
                debug_assert!(record1.attrs.closurized && record2.attrs.closurized);
                record1.fields.extend(record2.fields);
                record1.attrs.open = record1.attrs.open || record2.attrs.open;

                Ok(Closure::atomic_closure(NickelValue::new(
                    Term::Record(RecordData {
                        fields: record1.fields,
                        attrs: record1.attrs,
                        sealed_tail,
                    }),
                    pos_op_inh,
                )))
            }
        }
    }

    /// Evaluate a n-ary operation.
    ///
    /// Arguments are expected to be evaluated (in WHNF). `pos_op` corresponds to the whole
    /// operation position, that may be needed for error reporting.
    fn process_nary_operation(
        &mut self,
        n_op: NAryOp,
        args: Vec<(Closure, PosIdx)>,
        pos_op: PosIdx,
    ) -> Result<Closure, EvalError> {
        increment!(format!("primop:{n_op}"));

        let pos_op_inh = pos_op.into_inherited();

        let mk_type_error = |expected: &str,
                             arg_number: usize,
                             arg_pos: TermPos,
                             term: SharedTerm,
                             pos: TermPos| {
            Err(EvalErrorData::NAryPrimopTypeError {
                primop: n_op.to_string(),
                expected: expected.to_owned(),
                arg_number,
                pos_arg: arg_pos,
                arg_evaluated: NickelValue { term, pos },
                pos_op,
            })
        };

        // Currently, for fixed arity primitive operators, the parser must ensure that they get
        // exactly the right number of argument: if it is not the case, this is a bug, and we panic.
        match n_op {
            NAryOp::StringReplace | NAryOp::StringReplaceRegex => {
                let mut args_wo_env = args
                    .into_iter()
                    .map(|(clos, pos)| (clos.value.term, clos.value.pos, pos));
                let (fst, pos1, fst_pos) = args_wo_env.next().unwrap();
                let (snd, pos2, snd_pos) = args_wo_env.next().unwrap();
                let (thd, pos3, thd_pos) = args_wo_env.next().unwrap();
                debug_assert!(args_wo_env.next().is_none());

                match (&*fst, &*snd, &*thd) {
                    (Term::Str(s), Term::Str(from), Term::Str(to)) => {
                        let result = if let NAryOp::StringReplace = n_op {
                            s.replace(from.as_str(), to.as_str())
                        } else {
                            let re = regex::Regex::new(from)
                                .map_err(|err| EvalErrorData::Other(err.to_string(), pos_op))?;

                            s.replace_regex(&CompiledRegex(re), to)
                        };

                        Ok(Closure::atomic_closure(NickelValue::new(
                            Term::Str(result),
                            pos_op_inh,
                        )))
                    }
                    (Term::Str(_), Term::Str(_), _) => {
                        mk_type_error("String", 3, thd_pos, thd, pos3)
                    }
                    (Term::Str(_), _, _) => mk_type_error("String", 2, snd_pos, snd, pos2),
                    (_, _, _) => mk_type_error("String", 1, fst_pos, fst, pos1),
                }
            }
            NAryOp::StringSubstr => {
                let mut args_wo_env = args
                    .into_iter()
                    .map(|(clos, pos)| (clos.value.term, clos.value.pos, pos));
                let (fst, pos1, fst_pos) = args_wo_env.next().unwrap();
                let (snd, pos2, snd_pos) = args_wo_env.next().unwrap();
                let (thd, pos3, thd_pos) = args_wo_env.next().unwrap();
                debug_assert!(args_wo_env.next().is_none());

                match (&*fst, &*snd, &*thd) {
                    (Term::Str(s), Term::Num(start), Term::Num(end)) => s
                        .substring(start, end)
                        .map(|substr| {
                            Closure::atomic_closure(NickelValue::new(Term::Str(substr), pos_op_inh))
                        })
                        .map_err(|e| EvalErrorData::Other(format!("{e}"), pos_op)),
                    (Term::Str(_), Term::Num(_), _) => {
                        mk_type_error("Number", 3, thd_pos, thd, pos3)
                    }
                    (Term::Str(_), _, _) => mk_type_error("Number", 2, snd_pos, snd, pos2),
                    (_, _, _) => mk_type_error("String", 1, fst_pos, fst, pos1),
                }
            }
            NAryOp::MergeContract => {
                let mut args_iter = args.into_iter();

                let (
                    Closure {
                        value: NickelValue { term: t1, pos: _ },
                        env: _,
                    },
                    _,
                ) = args_iter.next().unwrap();

                let (
                    Closure {
                        value:
                            NickelValue {
                                term: t2,
                                pos: pos2,
                            },
                        env: env2,
                    },
                    _,
                ) = args_iter.next().unwrap();

                let (
                    Closure {
                        value:
                            NickelValue {
                                term: t3,
                                pos: pos3,
                            },
                        env: env3,
                    },
                    _,
                ) = args_iter.next().unwrap();

                debug_assert!(args_iter.next().is_none());

                match_sharedterm!(match (t1) {
                    Term::Lbl(lbl) => {
                        merge::merge(
                            &mut self.context.cache,
                            NickelValue {
                                term: t2,
                                pos: pos2,
                            },
                            env2,
                            NickelValue {
                                term: t3,
                                pos: pos3,
                            },
                            env3,
                            pos_op,
                            MergeMode::Contract(lbl),
                            &mut self.call_stack,
                        )
                    }
                    _ => Err(EvalErrorData::InternalError(
                        format!(
                            "The {n_op} operator was expecting \
                                a first argument of type Label, got {}",
                            t1.type_of()
                                .unwrap_or_else(|| String::from("<unevaluated>"))
                        ),
                        pos_op
                    )),
                })
            }
            NAryOp::RecordSealTail => {
                let mut args = args.into_iter();
                let (
                    Closure {
                        value:
                            NickelValue {
                                term: a1,
                                pos: pos1,
                            },
                        ..
                    },
                    fst_pos,
                ) = args.next().unwrap();
                let (
                    Closure {
                        value:
                            NickelValue {
                                term: a2,
                                pos: pos2,
                            },
                        ..
                    },
                    snd_pos,
                ) = args.next().unwrap();
                let (
                    Closure {
                        value:
                            NickelValue {
                                term: a3,
                                pos: pos3,
                            },
                        env: env3,
                    },
                    thd_pos,
                ) = args.next().unwrap();
                let (
                    Closure {
                        value:
                            NickelValue {
                                term: a4,
                                pos: pos4,
                            },
                        env: env4,
                    },
                    frth_pos,
                ) = args.next().unwrap();
                debug_assert!(args.next().is_none());

                match (&*a1, &*a2, &*a3, &*a4) {
                    (
                        Term::SealingKey(s),
                        Term::Lbl(label),
                        Term::Record(r),
                        Term::Record(tail),
                    ) => {
                        let mut r = r.clone();

                        let tail_closurized = NickelValue::from(Term::Record(tail.clone()))
                            .closurize(&mut self.context.cache, env4);
                        let fields = tail.fields.keys().map(|s| s.ident()).collect();
                        r.sealed_tail = Some(record::SealedTail::new(
                            *s,
                            label.clone(),
                            tail_closurized,
                            fields,
                        ));

                        let body = NickelValue::from(Term::Record(r));
                        Ok(Closure {
                            value: body,
                            env: env3,
                        })
                    }
                    (Term::SealingKey(_), Term::Lbl(_), Term::Record(_), _) => {
                        mk_type_error("Record", 4, frth_pos, a4, pos4)
                    }
                    (Term::SealingKey(_), Term::Lbl(_), _, _) => {
                        mk_type_error("Record", 3, thd_pos, a3, pos3)
                    }
                    (Term::SealingKey(_), _, _, _) => mk_type_error("Label", 2, snd_pos, a2, pos2),
                    (_, _, _, _) => mk_type_error("SealingKey", 1, fst_pos, a1, pos1),
                }
            }
            NAryOp::RecordUnsealTail => {
                let mut args = args.into_iter();
                let (
                    Closure {
                        value:
                            NickelValue {
                                term: a1,
                                pos: pos1,
                            },
                        ..
                    },
                    fst_pos,
                ) = args.next().unwrap();
                let (
                    Closure {
                        value:
                            NickelValue {
                                term: a2,
                                pos: pos2,
                            },
                        ..
                    },
                    snd_pos,
                ) = args.next().unwrap();
                let (
                    Closure {
                        value:
                            NickelValue {
                                term: a3,
                                pos: pos3,
                            },
                        env: env3,
                    },
                    thd_pos,
                ) = args.next().unwrap();
                debug_assert!(args.next().is_none());

                match (&*a1, &*a2, &*a3) {
                    (Term::SealingKey(s), Term::Lbl(l), Term::Record(r)) => r
                        .clone()
                        .sealed_tail
                        .and_then(|t| t.unseal(s).cloned())
                        .ok_or_else(|| EvalErrorData::BlameError {
                            evaluated_arg: l.get_evaluated_arg(&self.context.cache),
                            label: l.clone(),
                            call_stack: std::mem::take(&mut self.call_stack),
                        })
                        .map(|t| Closure {
                            value: t,
                            env: env3,
                        }),
                    (Term::SealingKey(_), Term::Lbl(_), _) => {
                        mk_type_error("Record", 3, thd_pos, a3, pos3)
                    }
                    (Term::SealingKey(_), _, _) => mk_type_error("Label", 2, snd_pos, a2, pos2),
                    (_, _, _) => mk_type_error("SealingKey", 1, fst_pos, a1, pos1),
                }
            }
            NAryOp::LabelInsertTypeVar => {
                let mut args = args.into_iter();

                let (
                    Closure {
                        value:
                            NickelValue {
                                term: key,
                                pos: key_pos,
                            },
                        ..
                    },
                    pos1,
                ) = args.next().unwrap();

                let (
                    Closure {
                        value:
                            NickelValue {
                                term: polarity,
                                pos: polarity_pos,
                            },
                        ..
                    },
                    pos2,
                ) = args.next().unwrap();

                let (
                    Closure {
                        value:
                            NickelValue {
                                term: label,
                                pos: label_pos,
                            },
                        ..
                    },
                    pos3,
                ) = args.next().unwrap();
                debug_assert!(args.next().is_none());

                let Term::SealingKey(key) = *key else {
                    return mk_type_error("SealingKey", 1, key_pos, key, pos1);
                };

                let Ok(polarity) = Polarity::try_from(polarity.as_ref()) else {
                    return mk_type_error("Polarity", 2, polarity_pos, polarity, pos2);
                };

                let Term::Lbl(label) = &*label else {
                    return mk_type_error("Label", 3, label_pos, label, pos3);
                };

                let mut new_label = label.clone();
                new_label
                    .type_environment
                    .insert(key, TypeVarData { polarity });

                Ok(Closure::atomic_closure(NickelValue::new(
                    Term::Lbl(new_label),
                    pos2.into_inherited(),
                )))
            }
            NAryOp::ArraySlice => {
                let mut args = args.into_iter();

                let (
                    Closure {
                        value:
                            NickelValue {
                                term: t1,
                                pos: pos1,
                            },
                        ..
                    },
                    fst_pos,
                ) = args.next().unwrap();

                let (
                    Closure {
                        value:
                            NickelValue {
                                term: t2,
                                pos: pos2,
                            },
                        ..
                    },
                    snd_pos,
                ) = args.next().unwrap();

                let (
                    Closure {
                        value:
                            NickelValue {
                                term: t3,
                                pos: pos3,
                            },
                        env: env3,
                    },
                    third_pos,
                ) = args.next().unwrap();
                debug_assert!(args.next().is_none());

                let Term::Num(ref start) = &*t1 else {
                    return mk_type_error("Number", 1, fst_pos, t1, pos1);
                };

                let Term::Num(ref end) = &*t2 else {
                    return mk_type_error("Number", 2, snd_pos, t2, pos2);
                };

                let t3_owned = t3.into_owned();

                let Term::Array(mut array, attrs) = t3_owned else {
                    return mk_type_error("Array", 3, third_pos, t3_owned.into(), pos3);
                };

                let Ok(start_as_usize) = usize::try_from(start) else {
                    return Err(EvalErrorData::Other(
                        format!(
                            "array/slice expects its first argument (start) to be a \
                            positive integer smaller than {}, got {start}",
                            usize::MAX
                        ),
                        pos_op,
                    ));
                };

                let Ok(end_as_usize) = usize::try_from(end) else {
                    return Err(EvalErrorData::Other(
                        format!(
                            "array/slice expects its second argument (end) to be a \
                            positive integer smaller than {}, got {end}",
                            usize::MAX
                        ),
                        pos_op,
                    ));
                };

                if end_as_usize < start_as_usize || end_as_usize > array.len() {
                    return Err(EvalErrorData::Other(
                        format!(
                            "array/slice: index out of bounds. Expected `start <= end <= {}`, but \
                            got `start={start}` and `end={end}`.",
                            array.len()
                        ),
                        pos_op,
                    ));
                }

                array.slice(start_as_usize, end_as_usize);
                Ok(Closure {
                    value: NickelValue::new(Term::Array(array, attrs), pos_op_inh),
                    env: env3,
                })
            }
        }
    }
}

// The enum tag returned by Typeof and Cast.
fn type_tag(v: &NickelValue) -> &'static str {
    use crate::bytecode::value::{BodyTag, InlineValue, ValueTag};
    // This is almost like `v.type_of()`, but there are a few subtle differences, and `type_tag`
    // has backward compatibility guarantees to uphold. Instead of relying on `type_of`, it's safer
    // to duplicate the logic here.
    match v.tag() {
        ValueTag::Pointer => match v.body_tag().unwrap() {
            BodyTag::Number => "Number",
            BodyTag::Array => "Array",
            BodyTag::Record => "Record",
            BodyTag::String => "String",
            BodyTag::Label => "Label",
            BodyTag::EnumVariant => "Enum",
            BodyTag::ForeignId => "ForeignId",
            BodyTag::CustomContract => "CustomContract",
            BodyTag::Type => "Type",
            _ => "Other",
        },
        // unwrap(): if the body tag is inline, `v` must be an inline value
        ValueTag::Inline => match v.as_inline().unwrap() {
            InlineValue::True | InlineValue::False => "Bool",
            InlineValue::Null => "Other",
            InlineValue::EmptyArray => "Array",
            InlineValue::EmptyRecord => "Record",
        },
    }
}

/// Compute the equality of two terms, represented as closures.
///
/// # Parameters
///
/// - `c1`: the closure of the first operand.
/// - `c2`: the closure of the second operand.
/// - `pos_op`: the position of the equality operation, used for error diagnostics.
///
/// # Return
///
/// If the comparison is successful, returns a bool indicating whether the values were equal,
/// otherwise returns an [`EvalError`] indicating that the values cannot be compared (typically two
/// functions).
///
/// # Uncomparable values
///
/// Comparing two functions is undecidable. Even in simple cases, it's not trivial to handle an
/// approximation (functions might capture free variables, you'd need to take eta-conversion into
/// account to equate e.g. `fun x => x` and `fun y => y`, etc.).
///
/// Thus, by default, comparing a function to something else always returns `false`. However, this
/// breaks the reflexivity property of equality, which users might rightfully rely on, because `fun
/// x => x` isn't equal to itself. Also, comparing two functions is probably never intentional nor
/// meaningful: thus we error out when trying to compare two functions. We still allow comparing
/// functions to something else, because it's useful to have tests like `if value == 1` or `if
/// value == null` typically in contracts without having to defensively check that `value` is a
/// function.
///
/// The same reasoning applies to foreign values (which we don't want to compare for security
/// reasons, at least right now, not because we can't).
fn eq<C: Cache>(
    cache: &mut C,
    c1: Closure,
    c2: Closure,
    pos_op: TermPos,
) -> Result<EqResult, EvalError> {
    let Closure {
        value: NickelValue {
            term: t1,
            pos: pos1,
        },
        env: env1,
    } = c1;
    let Closure {
        value: NickelValue {
            term: t2,
            pos: pos2,
        },
        env: env2,
    } = c2;

    // Take a list of subequalities, and either return `EqResult::Bool(true)` if it is empty, or
    // generate an appropriate `EqResult::Eqs` variant with closurized terms in it.
    fn gen_eqs<I, C: Cache>(
        cache: &mut C,
        mut it: I,
        env1: Environment,
        env2: Environment,
    ) -> EqResult
    where
        I: Iterator<Item = (NickelValue, NickelValue)>,
    {
        if let Some((t1, t2)) = it.next() {
            let eqs = it
                .map(|(t1, t2)| {
                    (
                        Closure {
                            value: t1,
                            env: env1.clone(),
                        },
                        Closure {
                            value: t2,
                            env: env2.clone(),
                        },
                    )
                })
                .collect();

            EqResult::Eqs(t1.closurize(cache, env1), t2.closurize(cache, env2), eqs)
        } else {
            EqResult::Bool(true)
        }
    }

    match (t1.into_owned(), t2.into_owned()) {
        (Term::Null, Term::Null) => Ok(EqResult::Bool(true)),
        (Term::Bool(b1), Term::Bool(b2)) => Ok(EqResult::Bool(b1 == b2)),
        (Term::Num(n1), Term::Num(n2)) => Ok(EqResult::Bool(n1 == n2)),
        (Term::Str(s1), Term::Str(s2)) => Ok(EqResult::Bool(s1 == s2)),
        (Term::Lbl(l1), Term::Lbl(l2)) => Ok(EqResult::Bool(l1 == l2)),
        (Term::SealingKey(s1), Term::SealingKey(s2)) => Ok(EqResult::Bool(s1 == s2)),
        (Term::Enum(id1), Term::Enum(id2)) => Ok(EqResult::Bool(id1.ident() == id2.ident())),
        (
            Term::EnumVariant {
                tag: tag1,
                arg: arg1,
                ..
            },
            Term::EnumVariant {
                tag: tag2,
                arg: arg2,
                ..
            },
        ) if tag1.ident() == tag2.ident() => {
            Ok(gen_eqs(cache, std::iter::once((arg1, arg2)), env1, env2))
        }
        (Term::Record(r1), Term::Record(r2)) => {
            let merge::split::SplitResult {
                left,
                center,
                right,
            } = merge::split::split(r1.fields, r2.fields);

            // As for other record operations, we ignore optional fields without a definition.
            if !left.values().all(Field::is_empty_optional)
                || !right.values().all(Field::is_empty_optional)
            {
                Ok(EqResult::Bool(false))
            } else if center.is_empty() {
                Ok(EqResult::Bool(true))
            } else {
                // We consider undefined values to be equal. We filter out pairs of undefined
                // values, but we reject pairs where one of the value is undefined and not the
                // other.
                let eqs: Result<Vec<_>, _> = center
                    .into_iter()
                    .filter_map(|(id, (field1, field2))| match (field1, field2) {
                        (
                            Field {
                                value: Some(value1),
                                pending_contracts: pending_contracts1,
                                ..
                            },
                            Field {
                                value: Some(value2),
                                pending_contracts: pending_contracts2,
                                ..
                            },
                        ) => {
                            let pos1 = value1.pos;
                            let pos2 = value2.pos;

                            let value1_with_ctr =
                                RuntimeContract::apply_all(value1, pending_contracts1, pos1);
                            let value2_with_ctr =
                                RuntimeContract::apply_all(value2, pending_contracts2, pos2);
                            Some(Ok((value1_with_ctr, value2_with_ctr)))
                        }
                        (Field { value: None, .. }, Field { value: None, .. }) => None,
                        (
                            Field {
                                value: value1 @ None,
                                metadata,
                                ..
                            },
                            Field { value: Some(_), .. },
                        )
                        | (
                            Field {
                                value: value1 @ Some(_),
                                ..
                            },
                            Field {
                                value: None,
                                metadata,
                                ..
                            },
                        ) => {
                            let pos_record = if value1.is_none() { pos1 } else { pos2 };

                            Some(Err(EvalErrorData::MissingFieldDef {
                                id,
                                metadata,
                                pos_record,
                                pos_access: pos_op,
                            }))
                        }
                    })
                    .collect();

                Ok(gen_eqs(cache, eqs?.into_iter(), env1, env2))
            }
        }
        (Term::Array(l1, a1), Term::Array(l2, a2)) if l1.len() == l2.len() => {
            // Equalities are tested in reverse order, but that shouldn't matter. If it
            // does, just do `eqs.rev()`

            // We should apply all contracts here, otherwise we risk having wrong values, think
            // record contracts with default values, wrapped terms, etc.

            let mut eqs = l1
                .into_iter()
                .map(|t| {
                    let pos = t.pos.into_inherited();
                    RuntimeContract::apply_all(t, a1.pending_contracts.iter().cloned(), pos)
                        .closurize(cache, env1.clone())
                })
                .collect::<Vec<_>>()
                .into_iter()
                .zip(l2.into_iter().map(|t| {
                    let pos = t.pos.into_inherited();
                    RuntimeContract::apply_all(t, a2.pending_contracts.iter().cloned(), pos)
                        .closurize(cache, env2.clone())
                }))
                .collect::<Vec<_>>();

            match eqs.pop() {
                None => Ok(EqResult::Bool(true)),
                Some((t1, t2)) => {
                    let eqs = eqs
                        .into_iter()
                        .map(|(t1, t2)| {
                            (
                                Closure {
                                    value: t1,
                                    env: Environment::new(),
                                },
                                Closure {
                                    value: t2,
                                    env: Environment::new(),
                                },
                            )
                        })
                        .collect::<Vec<_>>();

                    Ok(EqResult::Eqs(t1, t2, eqs))
                }
            }
        }
        // Function-like terms and foreign id can't be compared together.
        (
            t1 @ (Term::Fun(..) | Term::Match(_) | Term::CustomContract(_)),
            t2 @ (Term::Fun(..) | Term::Match(_) | Term::CustomContract(_)),
        )
        | (t1 @ Term::ForeignId(_), t2 @ Term::ForeignId(_)) => {
            Err(EvalErrorData::IncomparableValues {
                eq_pos: pos_op,
                left: NickelValue::new(t1, pos1),
                right: NickelValue::new(t2, pos2),
            })
        }
        (_, _) => Ok(EqResult::Bool(false)),
    }
}

/// Eta-expands a unary operator into a (lazy) function.
///
/// Regex-based primitive operations are evaluatedt to a function that captures the compiled
/// regexp, to avoid recompiling it at each call. [eta_expand] builds such a closure: given a
/// primary (in practice, regex) operator `%op1%`, [eta_expand] will return the expression `fun x
/// => %op1% x`. Each intermediate term is given the position index `pos_op`.
fn eta_expand(op: UnaryOp, pos_op: PosIdx) -> Term {
    let param = LocIdent::fresh();
    Term::Fun(
        param,
        NickelValue::term(
            Term::Op1(op, NickelValue::new(Term::Var(param), pos_op)),
            pos_op_inh,
        ),
    )
}

trait MapValuesClosurize: Sized {
    /// Returns a HashMap from `Ident` to `Field` by:
    ///
    /// 1. Appplying the pending contracts to each fields
    /// 2. Applying the provided function
    /// 3. Closurizing each result into the shared environment.
    ///
    /// Because we applied the pending contracts in 1., they are dropped in the result: all fields
    /// have an empty set of pending contracts.
    fn map_values_closurize<F, C: Cache>(
        self,
        cache: &mut C,
        env: &Environment,
        f: F,
    ) -> Result<IndexMap<LocIdent, Field>, record::MissingFieldDefError>
    where
        F: FnMut(LocIdent, NickelValue) -> NickelValue;
}

impl<Iter> MapValuesClosurize for Iter
where
    Iter: IntoIterator<Item = (LocIdent, Field)>,
{
    fn map_values_closurize<F, C: Cache>(
        self,
        cache: &mut C,
        env: &Environment,
        mut f: F,
    ) -> Result<IndexMap<LocIdent, Field>, record::MissingFieldDefError>
    where
        F: FnMut(LocIdent, NickelValue) -> NickelValue,
    {
        self.into_iter()
            .map(|(id, field)| {
                let value = field
                    .value
                    .map(|value| {
                        let pos = value.pos;
                        let value_with_ctrs = RuntimeContract::apply_all(
                            value,
                            field.pending_contracts.iter().cloned(),
                            pos,
                        );
                        f(id, value_with_ctrs)
                    })
                    .ok_or(record::MissingFieldDefError {
                        id,
                        metadata: field.metadata.clone(),
                    })?;

                let field = Field {
                    value: Some(value),
                    pending_contracts: Vec::new(),
                    ..field
                }
                .closurize(cache, env.clone());

                Ok((id, field))
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cache::resolvers::DummyResolver,
        error::NullReporter,
        eval::{cache::CacheImpl, Environment, VmContext},
    };

    // Initialize a VM with a default context
    fn with_vm(test: impl FnOnce(VirtualMachine<'_, DummyResolver, CacheImpl>)) {
        let mut vm_ctxt = VmContext::new(DummyResolver {}, std::io::sink(), NullReporter {});
        let vm = VirtualMachine::new_empty_env(&mut vm_ctxt);
        test(vm);
    }

    #[test]
    fn ite_operation() {
        with_vm(|mut vm| {
            let cont: OperationCont = OperationCont::Op1(UnaryOp::IfThenElse, TermPos::None);

            vm.stack
                .push_arg(Closure::atomic_closure(mk_term::integer(5)), TermPos::None);
            vm.stack
                .push_arg(Closure::atomic_closure(mk_term::integer(46)), TermPos::None);

            let mut clos = Closure {
                value: Term::Bool(true).into(),
                env: Environment::new(),
            };

            vm.stack.push_op_cont(cont, 0, TermPos::None);

            clos = vm.continuate_operation(clos).unwrap();

            assert_eq!(
                clos,
                Closure {
                    value: mk_term::integer(46),
                    env: Environment::new()
                }
            );
            assert_eq!(0, vm.stack.count_args());
        });
    }

    #[test]
    fn plus_first_term_operation() {
        with_vm(|mut vm| {
            let cont = OperationCont::Op2First(
                BinaryOp::Plus,
                Closure {
                    value: mk_term::integer(6),
                    env: Environment::new(),
                },
                TermPos::None,
            );

            let mut clos = Closure {
                body: mk_term::integer(7),
                env: Environment::new(),
            };

            vm.stack.push_op_cont(cont, 0, TermPos::None);

            clos = vm.continuate_operation(clos).unwrap();

            assert_eq!(
                clos,
                Closure {
                    value: mk_term::integer(6),
                    env: Environment::new()
                }
            );

            assert_eq!(1, vm.stack.count_conts());
            assert_eq!(
                (
                    OperationCont::Op2Second(
                        BinaryOp::Plus,
                        Closure {
                            value: mk_term::integer(7),
                            env: Environment::new(),
                        },
                        TermPos::None,
                        TermPos::None,
                    ),
                    0,
                    TermPos::None
                ),
                vm.stack.pop_op_cont().expect("Condition already checked.")
            );
        });
    }

    #[test]
    fn plus_second_term_operation() {
        with_vm(|mut vm| {
            let cont: OperationCont = OperationCont::Op2Second(
                BinaryOp::Plus,
                Closure {
                    value: mk_term::integer(7),
                    env: Environment::new(),
                },
                TermPos::None,
                TermPos::None,
            );

            let mut clos = Closure {
                body: mk_term::integer(6),
                env: Environment::new(),
            };

            vm.stack.push_op_cont(cont, 0, TermPos::None);
            clos = vm.continuate_operation(clos).unwrap();

            assert_eq!(
                clos,
                Closure {
                    value: mk_term::integer(13),
                    env: Environment::new()
                }
            );
        });
    }
}
