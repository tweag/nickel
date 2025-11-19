//! Implementation of primitive operations.
//!
//! Define functions which perform the evaluation of primitive operators. The machinery required
//! for the strict evaluation of the operands is mainly handled by [crate::eval], and marginally in
//! [`VirtualMachine::continuate_operation`].
//!
//! On the other hand, the functions `process_unary_operation` and `process_binary_operation`
//! receive evaluated operands and implement the actual semantics of operators.
use super::{
    Cache, Closure, Environment, ErrorKind, ImportResolver, VirtualMachine,
    cache::lazy::Thunk,
    contract_eq::contract_eq,
    merge::{self, MergeMode, split},
    stack::StrAccData,
    subst,
    value::{
        Array, ArrayData, Container, EnumVariantData, NickelValue, TypeData, ValueContentRef,
        ValueContentRefMut,
    },
};

#[cfg(feature = "nix-experimental")]
use crate::nix_ffi;

use crate::{
    cache::InputFormat,
    closurize::Closurize,
    combine::Combine,
    error::{EvalErrorKind, IllegalPolymorphicTailAction, Warning},
    identifier::LocIdent,
    label::{Polarity, TypeVarData, ty_path},
    metrics::increment,
    mk_app, mk_fun, mk_record,
    position::PosIdx,
    serialize::{self, ExportFormat},
    stdlib::internals,
    term::{make as mk_term, record::*, string::NickelString, *},
};

use nickel_lang_parser::utils::parse_number_sci;

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

/// All the data required to evaluate a unary operation.
struct Op1EvalData {
    /// The unary operation to apply.
    op: UnaryOp,
    /// The evaluated argument.
    arg: Closure,
    /// The position of the original expression of the argument, when it was put on the stack for
    /// evaluation.
    orig_pos_arg: PosIdx,
    /// The position of the primop application.
    pos_op: PosIdx,
}

/// All the data required to evaluate a binary operation.
struct Op2EvalData {
    /// The binary operation to apply.
    op: BinaryOp,
    /// The first argument evaluated.
    arg1: Closure,
    /// The second argument evaluated.
    arg2: Closure,
    /// The position of the original expression of the first argument, when it was put on the stack
    /// for evaluation.
    orig_pos_arg1: PosIdx,
    /// The position of the original expression of the second argument, when it was put on the
    /// stack for evaluation.
    orig_pos_arg2: PosIdx,
    /// The position of the primop application.
    pos_op: PosIdx,
}

struct OpNEvalData {
    /// The n-ary operation to apply.
    op: NAryOp,
    /// The arguments evaluated, together with the position of their original expression, when it
    /// was put on the stack for evaluation.
    args: Vec<(Closure, PosIdx)>,
    /// The position of the primop application.
    pos_op: PosIdx,
}

/// A string represention of the type of the first argument of serialization-related primitive
/// operations. This is a Nickel enum of the supported serialization formats.
static ENUM_FORMAT: &str = "[| 'Json, 'Yaml, 'Toml |]";

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
    /// argument, starts the evaluation of the second argument, or finally proceed with the
    /// operation if both arguments are evaluated (for binary operators).
    pub fn continuate_operation(&mut self, mut clos: Closure) -> Result<Closure, ErrorKind> {
        let (cont, cs_len, pos_idx) = self.stack.pop_op_cont().expect("Condition already checked");
        self.call_stack.truncate(cs_len);
        match cont {
            OperationCont::Op1(op, orig_pos_arg) => self.process_unary_operation(Op1EvalData {
                op,
                arg: clos,
                orig_pos_arg,
                pos_op: pos_idx,
            }),
            OperationCont::Op2First(b_op, mut snd_clos, fst_pos_idx) => {
                std::mem::swap(&mut clos, &mut snd_clos);
                self.stack.push_op_cont(
                    OperationCont::Op2Second(b_op, snd_clos, fst_pos_idx, clos.value.pos_idx()),
                    cs_len,
                    pos_idx,
                );
                Ok(clos)
            }
            OperationCont::Op2Second(op, arg1, orig_pos_arg1, orig_pos_arg2) => self
                .process_binary_operation(Op2EvalData {
                    op,
                    arg1,
                    orig_pos_arg1,
                    arg2: clos,
                    orig_pos_arg2,
                    pos_op: pos_idx,
                }),
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
                    self.process_nary_operation(OpNEvalData {
                        op,
                        args: evaluated,
                        pos_op: pos_idx,
                    })
                }
            }
        }
    }

    /// Evaluate a unary operation.
    ///
    /// The argument is expected to be evaluated (in WHNF). `pos_op` corresponds to the whole
    /// operation position, that may be needed for error reporting.
    fn process_unary_operation(&mut self, eval_data: Op1EvalData) -> Result<Closure, ErrorKind> {
        let Op1EvalData {
            orig_pos_arg,
            arg: Closure { value, env },
            pos_op,
            op,
        } = eval_data;

        increment!(format!("primop:{op}"));

        let pos = value.pos_idx();
        let pos_op_inh = eval_data.pos_op.to_inherited();

        macro_rules! mk_type_error {
            (op_name=$op_name:expr, $expected:expr) => {
                mk_type_error!(op_name = $op_name, $expected, value = value)
            };
            (op_name=$op_name:expr, $expected:expr, value=$value:expr) => {
                Err(Box::new(EvalErrorKind::UnaryPrimopTypeError {
                    primop: String::from($op_name),
                    expected: String::from($expected),
                    pos_arg: orig_pos_arg,
                    arg_evaluated: $value,
                }))
            };
            ($expected:expr) => {
                mk_type_error!(op_name = op.to_string(), $expected)
            };
            ($expected:expr, value=$value:expr) => {
                mk_type_error!(op_name = op.to_string(), $expected, value = $value)
            };
            ($expected:expr, value=$value:expr) => {};
            ($expected:expr, $arg_number:expr) => {
                mk_type_error!($expected, $arg_number, value = value)
            };
            ($expected:expr, $arg_number:expr, value=$value:expr) => {
                Err(Box::new(EvalErrorKind::NAryPrimopTypeError {
                    primop: op.to_string(),
                    expected: String::from($expected),
                    arg_number: $arg_number,
                    pos_arg: orig_pos_arg,
                    arg_evaluated: $value,
                    pos_op,
                }))
            };
        }

        match op {
            UnaryOp::IfThenElse => {
                if let Some(b) = value.as_bool() {
                    let (fst, ..) = self
                        .stack
                        .pop_arg(&self.context.cache)
                        .expect("if-then-else primop isn't saturated");
                    let (snd, ..) = self
                        .stack
                        .pop_arg(&self.context.cache)
                        .expect("if-then-else primop isn't saturated");

                    Ok(if b { fst } else { snd })
                } else {
                    // Not using mk_type_error! because of a non-uniform message
                    Err(Box::new(EvalErrorKind::TypeError {
                        expected: String::from("Bool"),
                        message: String::from(
                            "the condition in an if expression must have type Bool",
                        ),
                        orig_pos: orig_pos_arg,
                        term: value,
                    }))
                }
            }
            UnaryOp::Typeof => {
                Ok(NickelValue::enum_variant(type_tag(&value), None, pos_op_inh).into())
            }
            UnaryOp::Cast => {
                Ok(NickelValue::enum_variant(type_tag(&value), Some(value), pos_op_inh).into())
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
                        Some(false) => Ok(value.with_pos_idx(pos_op_inh).into()),
                        _ => mk_type_error!("Bool", 1),
                    }
                } else {
                    Err(Box::new(EvalErrorKind::NotEnoughArgs(
                        2,
                        String::from("&&"),
                        pos_op,
                    )))
                }
            }
            UnaryOp::BoolOr => {
                if let Some((next, ..)) = self.stack.pop_arg(&self.context.cache) {
                    match value.as_bool() {
                        Some(true) => Ok(value.with_pos_idx(pos_op_inh).into()),
                        // FIXME: this does not check that the second argument is actually a
                        // boolean. This means `false || 2` silently evaluates to `2`. This is
                        // simpler and more efficient, but can make debugging harder. In any case,
                        // it should be solved only once primary operators have better support for
                        // laziness in some arguments.
                        Some(false) => Ok(next),
                        _ => mk_type_error!("Bool", 1),
                    }
                } else {
                    Err(Box::new(EvalErrorKind::NotEnoughArgs(
                        2,
                        String::from("||"),
                        pos_op,
                    )))
                }
            }
            UnaryOp::BoolNot => {
                if let Some(b) = value.as_bool() {
                    Ok(NickelValue::bool_value_posless(!b)
                        .with_pos_idx(pos_op_inh)
                        .into())
                } else {
                    mk_type_error!("Bool")
                }
            }
            UnaryOp::Blame => {
                let Some(label) = value.as_label() else {
                    return mk_type_error!("Label");
                };

                Err(Box::new(EvalErrorKind::BlameError {
                    evaluated_arg: label.get_evaluated_arg(&self.context.cache),
                    label: label.clone(),
                }))
            }
            UnaryOp::EnumEmbed(_id) => {
                if value.as_enum_variant().is_some() {
                    Ok(value.with_pos_idx(pos_op_inh).into())
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

                if let Some(enum_variant) = value.as_enum_variant()
                    && enum_variant.arg.is_none()
                {
                    let Closure {
                        value: cases_val,
                        env: cases_env,
                    } = cases_closure;

                    let ValueContentRef::Record(container) = cases_val.content_ref() else {
                        panic!("invalid argument for %match%")
                    };

                    container
                        .get(enum_variant.tag)
                        .map(|field| Closure {
                            // The record containing the match cases, as well as the match primop
                            // itself, aren't accessible in the surface language. They are
                            // generated by the interpreter, and should never contain field without
                            // definition.
                            value: field
                                .value
                                .as_ref()
                                .cloned()
                                .expect("%match% cases must have a definition"),
                            env: cases_env,
                        })
                        .or(default)
                        .ok_or_else(|| {
                            Box::new(EvalErrorKind::NonExhaustiveEnumMatch {
                                expected: container.field_names(RecordOpKind::IgnoreEmptyOpt),
                                found: NickelValue::enum_variant_posless(enum_variant.tag, None)
                                    .with_pos_idx(pos),
                                pos: pos_op_inh,
                            })
                        })
                } else if let Some(clos) = default {
                    Ok(clos)
                } else {
                    mk_type_error!("Enum", 2)
                }
            }
            UnaryOp::LabelFlipPol => {
                let Some(label) = value.as_label() else {
                    return mk_type_error!("Label");
                };

                let mut label = label.clone();
                label.polarity = label.polarity.flip();
                Ok(NickelValue::label(label, pos_op_inh).into())
            }
            UnaryOp::LabelPol => {
                if let Some(label) = value.as_label() {
                    Ok(NickelValue::from(label.polarity)
                        .with_pos_idx(pos_op_inh)
                        .into())
                } else {
                    mk_type_error!("Label")
                }
            }
            UnaryOp::LabelGoDom => {
                let Some(label) = value.as_label() else {
                    return mk_type_error!("Label");
                };

                let mut label = label.clone();
                label.path.push(ty_path::Elem::Domain);
                Ok(NickelValue::label(label, pos_op_inh).into())
            }
            UnaryOp::LabelGoCodom => {
                let Some(label) = value.as_label() else {
                    return mk_type_error!("Label");
                };

                let mut label = label.clone();
                label.path.push(ty_path::Elem::Codomain);
                Ok(NickelValue::label(label, pos_op_inh).into())
            }
            UnaryOp::LabelGoArray => {
                let Some(label) = value.as_label() else {
                    return mk_type_error!("Label");
                };

                let mut label = label.clone();
                label.path.push(ty_path::Elem::Array);
                Ok(NickelValue::label(label, pos_op_inh).into())
            }
            UnaryOp::LabelGoDict => {
                let Some(label) = value.as_label() else {
                    return mk_type_error!("Label");
                };

                let mut label = label.clone();
                label.path.push(ty_path::Elem::Dict);
                Ok(NickelValue::label(label, pos_op_inh).into())
            }
            UnaryOp::RecordAccess(id) => {
                match value.as_record() {
                    Some(Container::Alloc(record)) => {
                        // We have to apply potentially pending contracts. Right now, this
                        // means that repeated field access will re-apply the contract again
                        // and again, which is not optimal. The same thing happens with array
                        // contracts. There are several way to improve this, but this is left
                        // as future work.
                        match record
                            .get_value_with_ctrs(&id)
                            .map_err(|err| err.into_eval_err(pos, pos_op))?
                        {
                            Some(value) => {
                                self.call_stack
                                    .enter_field(id, pos, value.pos_idx(), pos_op);
                                Ok(Closure { value, env })
                            }
                            None => match record.sealed_tail.as_ref() {
                                Some(t) if t.has_field(&id.ident()) => {
                                    Err(Box::new(EvalErrorKind::IllegalPolymorphicTailAccess {
                                        action: IllegalPolymorphicTailAction::FieldAccess {
                                            field: id.to_string(),
                                        },
                                        evaluated_arg: t
                                            .label
                                            .get_evaluated_arg(&self.context.cache),
                                        label: t.label.clone(),
                                    }))
                                }
                                _ => Err(Box::new(EvalErrorKind::FieldMissing {
                                    id,
                                    field_names: record.field_names(RecordOpKind::IgnoreEmptyOpt),
                                    operator: String::from("(.)"),
                                    pos_record: pos,
                                    pos_op,
                                })),
                            }, //TODO include the position of operators on the stack
                        }
                    }
                    Some(Container::Empty) => Err(Box::new(EvalErrorKind::FieldMissing {
                        id,
                        field_names: Vec::new(),
                        operator: String::from("(.)"),
                        pos_record: pos,
                        pos_op,
                    })),
                    None =>
                    // Not using mk_type_error! because of a non-uniform message
                    {
                        Err(Box::new(EvalErrorKind::TypeError {
                            expected: String::from("Record"),
                            message: String::from("field access only makes sense for records"),
                            orig_pos: orig_pos_arg,
                            term: value,
                        }))
                    }
                }
            }
            UnaryOp::RecordFields(op_kind) => {
                if let Some(container) = value.as_record() {
                    let fields_as_terms: Array = container
                        .field_names(op_kind)
                        .into_iter()
                        .map(|id| {
                            NickelValue::string(id.label(), self.context.pos_table.push(id.pos))
                        })
                        .collect();

                    Ok(Closure {
                        value: NickelValue::array(fields_as_terms, Vec::new(), pos_op_inh),
                        env,
                    })
                } else {
                    mk_type_error!("Record")
                }
            }
            UnaryOp::RecordValues => {
                if let Some(container) = value.as_record() {
                    let mut values = container
                        .into_opt()
                        .map(|r| r.iter_without_opts())
                        .into_iter()
                        .flatten()
                        .collect::<Result<Vec<_>, _>>()
                        .map_err(|miss_def_err| miss_def_err.into_eval_err(pos, pos_op))?;

                    values.sort_by_key(|(id, _)| *id);
                    let terms = values.into_iter().map(|(_, value)| value).collect();

                    Ok(Closure {
                        // as evaluated records are assumed to be closurized, we can assume that
                        // the extracted array here is, in turn, also closuried.
                        value: NickelValue::array(terms, Vec::new(), pos_op_inh),
                        env,
                    })
                } else {
                    mk_type_error!("Record")
                }
            }
            UnaryOp::ArrayMap => {
                let (f, _) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    Box::new(EvalErrorKind::NotEnoughArgs(
                        2,
                        String::from("array/map"),
                        pos_op,
                    ))
                })?;

                let Some(cont) = value.as_array() else {
                    return mk_type_error!("Array");
                };

                let Container::Alloc(array_data) = cont else {
                    return Ok(value.into());
                };

                let f_as_var = f.value.closurize(&mut self.context.cache, f.env);

                // Array elements are closurized to preserve laziness of data
                // structures. It maintains the invariant that any data structure only
                // contain indices (that is, currently, variables).
                let ts = array_data
                    .array
                    .iter()
                    .cloned()
                    .map(|t| {
                        let t_with_ctrs = RuntimeContract::apply_all(
                            t,
                            array_data.pending_contracts.iter().cloned(),
                            pos,
                        );

                        NickelValue::term(Term::app(f_as_var.clone(), t_with_ctrs), pos_op_inh)
                            .closurize(&mut self.context.cache, env.clone())
                    })
                    .collect();

                Ok(NickelValue::array(ts, Vec::new(), pos_op_inh).into())
            }
            UnaryOp::ArrayGen => {
                let (f, _) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    Box::new(EvalErrorKind::NotEnoughArgs(
                        2,
                        String::from("array/generate"),
                        pos_op,
                    ))
                })?;

                let Some(n) = value.as_number() else {
                    return mk_type_error!("Number");
                };

                if n < &Number::ZERO {
                    return Err(Box::new(EvalErrorKind::Other(
                        format!(
                            "array/generate expects its first argument to be a positive number, got {n}"
                        ),
                        pos_op,
                    )));
                }

                let Ok(n_int) = u32::try_from(n) else {
                    return Err(Box::new(EvalErrorKind::Other(
                        format!(
                            "array/generate expects its first argument to be an integer \
                            smaller than {}, got {n}",
                            u32::MAX,
                        ),
                        pos_op,
                    )));
                };

                let f_closure = f.value.closurize(&mut self.context.cache, f.env);

                // Array elements are closurized to preserve laziness of data structures. It
                // maintains the invariant that any data structure only contain indices (that is,
                // currently, variables).
                let ts = (0..n_int)
                    .map(|n| {
                        mk_app!(f_closure.clone(), NickelValue::number_posless(n))
                            .closurize(&mut self.context.cache, env.clone())
                    })
                    .collect();

                Ok(NickelValue::array(ts, Vec::new(), pos_op_inh).into())
            }
            UnaryOp::RecordMap => {
                let (f, ..) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    EvalErrorKind::NotEnoughArgs(2, String::from("record/map"), pos_op)
                })?;

                let Some(container) = value.as_record() else {
                    return mk_type_error!("Record");
                };

                if let Container::Alloc(record) = container {
                    let record = record.clone();
                    // While it's certainly possible to allow mapping over
                    // a record with a sealed tail, it's not entirely obvious
                    // how that should behave. It's also not clear that this
                    // is something users will actually need to do, so we've
                    // decided to prevent this until we have a clearer idea
                    // of potential use-cases.
                    if let Some(record::SealedTail { label, .. }) = record.sealed_tail {
                        return Err(Box::new(EvalErrorKind::IllegalPolymorphicTailAccess {
                            action: IllegalPolymorphicTailAction::Map,
                            evaluated_arg: label.get_evaluated_arg(&self.context.cache),
                            label,
                        }));
                    }

                    let f_closure = f.value.closurize(&mut self.context.cache, f.env);

                    // As for `ArrayMap` (see above), we closurize the content of fields

                    let fields = record
                        .fields
                        .into_iter()
                        .filter(|(_, field)| !field.is_empty_optional())
                        .map_values_closurize(&mut self.context.cache, &env, |id, t| {
                            let pos_idx = t.pos_idx().to_inherited();

                            mk_app!(
                                f_closure.clone(),
                                NickelValue::string_posless(id.label()),
                                t
                            )
                            .with_pos_idx(pos_idx)
                        })
                        .map_err(|miss_field_err| miss_field_err.into_eval_err(pos, pos_op))?;

                    // By construction, mapping freezes the record. We set the frozen flag so
                    // that operations that require the record to be frozen don't have to
                    // perform the work again.
                    let attrs = record.attrs.frozen();

                    Ok(NickelValue::record(
                        RecordData {
                            fields,
                            attrs,
                            ..record
                        },
                        pos_op_inh,
                    )
                    .into())
                } else {
                    Ok(value.with_pos_idx(pos_op).into())
                }
            }
            UnaryOp::Seq => self
                .stack
                .pop_arg(&self.context.cache)
                .map(|(next, ..)| next)
                .ok_or_else(|| {
                    Box::new(EvalErrorKind::NotEnoughArgs(2, String::from("seq"), pos_op))
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
                        mk_term::op1(UnaryOp::DeepSeq, first).with_pos_idx(pos_op_inh),
                        |acc, t| {
                            mk_app!(mk_term::op1(UnaryOp::DeepSeq, t), acc).with_pos_idx(pos_op_inh)
                        },
                    )
                }

                match value.content_ref() {
                    ValueContentRef::Record(Container::Alloc(record))
                        if !record.fields.is_empty() =>
                    {
                        let defined = record
                            // `iter_without_opts` takes care of applying pending contracts
                            .iter_without_opts()
                            .collect::<Result<Vec<_>, _>>()
                            .map_err(|missing_def_err| {
                                missing_def_err.into_eval_err(pos, pos_op)
                            })?;

                        let terms = defined.into_iter().map(|(_, field)| field);

                        Ok(Closure {
                            value: seq_terms(terms, pos_op),
                            env,
                        })
                    }
                    ValueContentRef::Array(Container::Alloc(array_data))
                        if !array_data.array.is_empty() =>
                    {
                        let terms = seq_terms(
                            array_data.array.iter().map(|t| {
                                RuntimeContract::apply_all(
                                    t.clone(),
                                    array_data.pending_contracts.iter().cloned(),
                                    pos.to_inherited(),
                                )
                                .closurize(&mut self.context.cache, env.clone())
                            }),
                            pos_op,
                        );

                        Ok(terms.into())
                    }
                    ValueContentRef::EnumVariant(EnumVariantData {
                        tag: _,
                        arg: Some(arg),
                    }) => Ok(Closure {
                        value: seq_terms(std::iter::once(arg.clone()), pos_op),
                        env,
                    }),
                    _ => {
                        if let Some((next, ..)) = self.stack.pop_arg(&self.context.cache) {
                            Ok(next)
                        } else {
                            Err(Box::new(EvalErrorKind::NotEnoughArgs(
                                2,
                                String::from("deep_seq"),
                                pos_op,
                            )))
                        }
                    }
                }
            }
            UnaryOp::ArrayLength => {
                //TODO[RFC007]: empty array
                if let Some(container) = value.as_array() {
                    // A num does not have any free variable so we can drop the environment
                    Ok(NickelValue::number(container.len(), pos_op_inh).into())
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

                        // TODO: we should set up the stack properly, and directly, for the
                        // continuation instead of allocating a new term here and returning it.
                        Ok(Closure {
                            value: NickelValue::term(
                                Term::op1(UnaryOp::ChunksConcat, e),
                                pos_op_inh,
                            ),
                            env: env_chunks,
                        })
                    } else {
                        Ok(NickelValue::string(acc, pos_op_inh).into())
                    }
                } else {
                    // Since the error halts the evaluation, we don't bother cleaning the stack of
                    // the remaining string chunks.
                    //
                    // Not using mk_type_error! because of a non-uniform message
                    Err(Box::new(EvalErrorKind::TypeError {
                        expected: String::from("Stringable"),
                        message: String::from(
                            "interpolated values must be Stringable (string, number, boolean, enum tag or null)",
                        ),
                        orig_pos: curr_pos,
                        term: value,
                    }))
                }
            }
            UnaryOp::StringTrim => {
                if let Some(s) = value.as_string() {
                    Ok(NickelValue::string(s.trim(), pos_op_inh).into())
                } else {
                    mk_type_error!("String")
                }
            }
            UnaryOp::StringChars => {
                if let Some(s) = value.as_string() {
                    let ts = s.characters();
                    Ok(NickelValue::array(ts, Vec::new(), pos_op_inh).into())
                } else {
                    mk_type_error!("String")
                }
            }
            UnaryOp::StringUppercase => {
                if let Some(s) = value.as_string() {
                    Ok(NickelValue::string(s.to_uppercase(), pos_op_inh).into())
                } else {
                    mk_type_error!("String")
                }
            }
            UnaryOp::StringLowercase => {
                if let Some(s) = value.as_string() {
                    Ok(NickelValue::string(s.to_lowercase(), pos_op_inh).into())
                } else {
                    mk_type_error!("String")
                }
            }
            UnaryOp::StringLength => {
                if let Some(s) = value.as_string() {
                    let length = s.graphemes(true).count();
                    Ok(NickelValue::number(length, pos_op_inh).into())
                } else {
                    mk_type_error!("String")
                }
            }
            UnaryOp::ToString => value
                .to_nickel_string()
                .map(|s| NickelValue::string(s, pos_op_inh).into())
                .ok_or_else(|| {
                    Box::new(EvalErrorKind::Other(
                        format!(
                            "to_string: can't convert an argument of type {} to string",
                            value.type_of().unwrap()
                        ),
                        pos,
                    ))
                }),
            UnaryOp::NumberFromString => {
                if let Some(s) = value.as_string() {
                    let n = parse_number_sci(s).map_err(|_| {
                        Box::new(EvalErrorKind::Other(
                            format!(
                                "number/from_string: invalid number literal `{}`",
                                s.as_str()
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
                    Ok(NickelValue::enum_tag(LocIdent::from(s), pos_op_inh).into())
                } else {
                    mk_type_error!("String")
                }
            }
            UnaryOp::StringIsMatch => {
                if let Some(s) = value.as_string() {
                    let re = regex::Regex::new(s)
                        .map_err(|err| Box::new(EvalErrorKind::Other(err.to_string(), pos_op)))?;

                    let matcher = eta_expand(UnaryOp::StringIsMatchCompiled(re.into()), pos_op_inh);
                    Ok(NickelValue::term(matcher, pos_op_inh).into())
                } else {
                    mk_type_error!("String", 1)
                }
            }
            UnaryOp::StringFind => {
                if let Some(s) = value.as_string() {
                    let re = regex::Regex::new(s)
                        .map_err(|err| Box::new(EvalErrorKind::Other(err.to_string(), pos_op)))?;

                    let matcher = eta_expand(UnaryOp::StringFindCompiled(re.into()), pos_op_inh);
                    Ok(NickelValue::term(matcher, pos_op_inh).into())
                } else {
                    mk_type_error!("String", 1)
                }
            }
            UnaryOp::StringFindAll => {
                if let Some(s) = value.as_string() {
                    let re = regex::Regex::new(s)
                        .map_err(|err| Box::new(EvalErrorKind::Other(err.to_string(), pos_op)))?;

                    let matcher = eta_expand(UnaryOp::StringFindAllCompiled(re.into()), pos_op_inh);
                    Ok(NickelValue::term(matcher, pos_op_inh).into())
                } else {
                    mk_type_error!("String", 1)
                }
            }
            UnaryOp::StringIsMatchCompiled(regex) => {
                if let Some(s) = value.as_string() {
                    Ok(s.matches_regex(&regex).with_pos_idx(pos_op_inh).into())
                } else {
                    mk_type_error!(op_name = "a compiled regular expression match", "String")
                }
            }
            UnaryOp::StringFindCompiled(regex) => {
                if let Some(s) = value.as_string() {
                    use crate::term::string::RegexFindResult;

                    let result = match s.find_regex(&regex) {
                        // This record doesn't need to be closurized, since all values are
                        // constant.
                        None => mk_record!(
                            ("matched", NickelValue::string_posless("")),
                            ("index", NickelValue::number_posless(-1)),
                            ("groups", NickelValue::empty_array())
                        ),
                        Some(RegexFindResult {
                            matched: mtch,
                            index,
                            groups,
                        }) => closurize_container(mk_record!(
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
                                            .map(|s| NickelValue::string_posless(
                                                s.unwrap_or_default()
                                            ))
                                    ),
                                    Vec::new()
                                )
                            )
                        )),
                    };

                    Ok(result.with_pos_idx(pos_op_inh).into())
                } else {
                    mk_type_error!(op_name = "a compiled regular expression match", "String")
                }
            }
            UnaryOp::StringFindAllCompiled(regex) => {
                if let Some(s) = value.as_string() {
                    let result = NickelValue::array(
                        Array::from_iter(s.find_all_regex(&regex).map(|found| {
                            closurize_container(mk_record!(
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
                            ))
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
                        .with_pos_idx(pos)
                }

                match value.content_ref() {
                    ValueContentRef::Record(Container::Alloc(record)) => {
                        let fields = record
                            .fields
                            .iter()
                            .filter(|(_, field)| {
                                !(field.is_empty_optional()
                                    || (ignore_not_exported && field.metadata.not_exported))
                            })
                            .map(|(id, field)| (*id, field.clone()))
                            .map_values_closurize(&mut self.context.cache, &env, |_, value| {
                                mk_term::op1(
                                    UnaryOp::Force {
                                        ignore_not_exported,
                                    },
                                    value,
                                )
                            })
                            .map_err(|e| e.into_eval_err(pos, pos_op))?;

                        let terms: Vec<NickelValue> = fields
                            .values()
                            .map(|field| {
                                field.value.as_ref().cloned().expect(
                                    "map_values_closurize ensures that values without a \
                                    definition throw a MissingFieldDefError",
                                )
                            })
                            .collect();

                        let pos_inh = pos.to_inherited();
                        let cont = NickelValue::record(
                            RecordData {
                                fields,
                                attrs: record.attrs,
                                sealed_tail: record.sealed_tail.clone(),
                            },
                            pos_inh,
                        );

                        Ok(seq_terms(terms.into_iter(), pos_op, cont).into())
                    }
                    ValueContentRef::Array(Container::Alloc(array_data)) => {
                        //unwrap(): the guard of the pattern exclude empty arrays
                        let ArrayData {
                            array,
                            pending_contracts,
                        } = array_data.clone();
                        let pos_inh = pos.to_inherited();

                        let ts = array
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
                    ValueContentRef::EnumVariant(data) => {
                        let EnumVariantData { tag, arg } = data.clone();

                        if let Some(arg) = arg {
                            let arg = mk_term::op1(
                                UnaryOp::Force {
                                    ignore_not_exported,
                                },
                                arg,
                            )
                            .closurize(&mut self.context.cache, env.clone());

                            let cont = NickelValue::enum_variant(
                                tag,
                                Some(arg.clone()),
                                pos.to_inherited(),
                            );

                            Ok(Closure {
                                value: seq_terms(std::iter::once(arg), pos_op, cont),
                                env,
                            })
                        } else {
                            Ok(Closure {
                                value: NickelValue::enum_tag(tag, pos_op_inh),
                                env,
                            })
                        }
                    }
                    _ => Ok(Closure { value, env }),
                }
            }
            UnaryOp::RecordEmptyWithTail => {
                let Some(container) = value.as_record() else {
                    return mk_type_error!("Record");
                };

                let mut result = RecordData::empty();
                result.sealed_tail = container
                    .into_opt()
                    .and_then(|record| record.sealed_tail.clone());

                Ok(Closure {
                    value: NickelValue::record(result, pos_op_inh),
                    env,
                })
            }
            UnaryOp::RecordFreeze => {
                // If the record is already frozen, there's nothing to do.
                if matches!(value.as_record(), Some(Container::Alloc(record)) if record.attrs.frozen)
                {
                    // A frozen record shouldn't have a polymorphic tail
                    debug_assert!(
                        // unwrap()s: the pattern in `matches` ensures that both unwrap will
                        // succeed.
                        value
                            .as_record()
                            .unwrap()
                            .unwrap_alloc()
                            .sealed_tail
                            .is_none()
                    );

                    return Ok(Closure { value, env });
                }

                let Some(container) = value.as_record() else {
                    return mk_type_error!("Record");
                };

                if let Container::Alloc(record) = container {
                    // It's not clear what the semantics of freezing a record with a sealed tail
                    // would be, as there might be dependencies between the sealed part and the
                    // unsealed part. Merging is disallowed on records with tail, so we disallow
                    // freezing as well.
                    if let Some(record::SealedTail { label, .. }) = &record.sealed_tail {
                        return Err(Box::new(EvalErrorKind::IllegalPolymorphicTailAccess {
                            action: IllegalPolymorphicTailAction::Freeze,
                            evaluated_arg: label.get_evaluated_arg(&self.context.cache),
                            label: label.clone(),
                        }));
                    }

                    let fields = record
                        .fields
                        .iter()
                        .map(|(id, field)| {
                            let field = field.clone();

                            let value = field.value.map(|value| {
                                let pos = value.pos_idx();
                                RuntimeContract::apply_all(value, field.pending_contracts, pos)
                            });

                            let field = Field {
                                value,
                                pending_contracts: Vec::new(),
                                ..field
                            }
                            .closurize(&mut self.context.cache, env.clone());

                            (*id, field)
                        })
                        .collect();

                    let attrs = record.attrs.frozen();

                    Ok(Closure {
                        value: NickelValue::record(
                            RecordData {
                                fields,
                                attrs,
                                sealed_tail: None,
                            },
                            pos_op_inh,
                        ),
                        env,
                    })
                } else {
                    // Ditto if the record is empty. We can also drop the environment.
                    Ok(value.into())
                }
            }
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
                    .ok_or_else(|| {
                        Box::new(EvalErrorKind::NotEnoughArgs(
                            2,
                            String::from("trace"),
                            pos_op,
                        ))
                    })
            }
            UnaryOp::LabelPushDiag => {
                let Some(label) = value.as_label() else {
                    return mk_type_error!("Label");
                };

                let mut label = label.clone();
                label.push_diagnostic();
                Ok(Closure {
                    value: NickelValue::label(label, pos),
                    env,
                })
            }
            #[cfg(feature = "nix-experimental")]
            UnaryOp::EvalNix => {
                if let Some(s) = value.as_string() {
                    let base_dir = self
                        .context
                        .pos_table
                        .get(pos_op)
                        .into_opt()
                        .map(|span| self.import_resolver().get_base_dir_for_nix(span.src_id))
                        .unwrap_or_default();

                    let json = nix_ffi::eval_to_json(&String::from(s), &base_dir).map_err(|e| {
                        Box::new(EvalErrorKind::Other(
                            format!("nix code failed to evaluate:\n {}", e.what()),
                            pos,
                        ))
                    })?;

                    let result: NickelValue = serde_json::from_str(&json).map_err(|e| {
                        Box::new(EvalErrorKind::Other(
                            format!("nix produced invalid json: {e}"),
                            pos,
                        ))
                    })?;

                    Ok(result.into())
                } else {
                    // Not using mk_type_error! because of a non-uniform message
                    Err(Box::new(EvalErrorKind::TypeError {
                        expected: String::from("String"),
                        message: String::from("eval_nix takes a string of nix code as an argument"),
                        orig_pos: orig_pos_arg,
                        term: value,
                    }))
                }
            }
            UnaryOp::EnumGetArg => {
                if let Some(EnumVariantData { arg: Some(arg), .. }) = value.as_enum_variant() {
                    Ok(Closure {
                        value: arg.clone(),
                        env,
                    })
                } else {
                    mk_type_error!("Enum variant")
                }
            }
            UnaryOp::EnumMakeVariant => {
                let Some(tag) = value.as_string() else {
                    return mk_type_error!("String");
                };

                let (arg_clos, _) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    Box::new(EvalErrorKind::NotEnoughArgs(
                        2,
                        String::from("enum/make_variant"),
                        pos,
                    ))
                })?;
                let arg_pos = arg_clos.value.pos_idx();
                let arg = NickelValue::thunk(Thunk::new(arg_clos), arg_pos);

                Ok(NickelValue::enum_variant(
                    LocIdent::new(tag).with_pos(self.context.pos_table.get(pos)),
                    Some(arg),
                    pos_op_inh,
                )
                .into())
            }
            UnaryOp::EnumGetTag => match value.as_enum_variant() {
                Some(EnumVariantData { tag, .. }) => {
                    Ok(NickelValue::enum_tag(*tag, pos_op_inh).into())
                }
                _ => mk_type_error!("Enum"),
            },
            UnaryOp::EnumIsVariant => Ok(NickelValue::bool_value(
                value
                    .as_enum_variant()
                    .is_some_and(|enum_variant| enum_variant.arg.is_some()),
                pos_op_inh,
            )
            .into()),
            UnaryOp::PatternBranch => {
                // The continuation, that we must evaluate in the augmented environment.
                let (mut cont, _) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    Box::new(EvalErrorKind::NotEnoughArgs(
                        2,
                        String::from("with_env"),
                        pos_op,
                    ))
                })?;

                let Some(container) = value.as_record() else {
                    return mk_type_error!("Record");
                };

                for (id, field) in container
                    .into_opt()
                    .into_iter()
                    .flat_map(|record| record.fields.iter())
                {
                    debug_assert!(field.metadata.is_empty());

                    if let Some(value) = &field.value {
                        if let Some(idx) = value.as_thunk() {
                            cont.env.insert(id.ident(), idx.clone());
                        } else {
                            cont.env.insert(
                                id.ident(),
                                self.context.cache.add(
                                    Closure {
                                        value: value.clone(),
                                        env: env.clone(),
                                    },
                                    BindingType::Normal,
                                ),
                            );
                        }
                    } else {
                        // This should not really happen, as `with_env` is intended to be
                        // used with very simple records: no metadata, no recursive fields,
                        // no field without definition, etc.
                        debug_assert!(false);
                    }
                }

                Ok(cont)
            }
            UnaryOp::ContractCustom => {
                let contract = if let Some(Term::Fun(..) | Term::Match(_)) = value.as_term() {
                    value.closurize(&mut self.context.cache, env)
                } else {
                    return mk_type_error!("Function or MatchExpression");
                };

                Ok(NickelValue::custom_contract(contract, pos_op_inh).into())
            }
            UnaryOp::ContractPostprocessResult => {
                let Some(EnumVariantData {
                    tag,
                    arg: Some(arg),
                }) = value.as_enum_variant()
                else {
                    return mk_type_error!("[| 'Ok, 'Error _ |]");
                };

                // We pop the second argument which isn't strict: we don't need to evaluate the
                // label if there's no error.
                let (label_closure, pos_label) = self.stack.pop_arg(&self.context.cache).unwrap();

                match (tag.label(), arg) {
                    ("Ok", value) => Ok(Closure {
                        value: value.clone(),
                        env,
                    }),
                    ("Error", err_data) => {
                        // In the error case, we first need to force the error data so that
                        // primitive values (strings) can be extracted from it, attach the
                        // corresponding data to the label, and then blame.
                        //
                        // To do so, we setup the stack to represent the evaluation context
                        // `%contract/blame% (%label/with_error_data% (%force% [.]) label)` and
                        // then continue with `err_data`.
                        self.stack.push_op_cont(
                            OperationCont::Op1(UnaryOp::Blame, orig_pos_arg),
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
                                orig_pos_arg,
                            ),
                            self.call_stack.len(),
                            pos_op_inh,
                        );

                        Ok(Closure {
                            value: err_data.clone(),
                            env,
                        })
                    }
                    _ => mk_type_error!("[| 'Ok, 'Error {..} |]'"),
                }
            }
            UnaryOp::ContractAttachDefaultLabel => {
                if !matches!(
                    value.as_enum_variant(),
                    Some(EnumVariantData { arg: Some(_), .. })
                ) {
                    return mk_type_error!("[| 'Ok, 'Error _ |]");
                }
                // The stack should already contain the default label to attach, so push
                // the (potential) error data.
                self.stack.push_arg(Closure { value, env }, orig_pos_arg);

                Ok(Closure {
                    value: internals::add_default_check_label(),
                    env: Environment::new(),
                })
            }
            UnaryOp::NumberArcCos => self.unary_number_op(
                f64::acos,
                Op1EvalData {
                    op,
                    arg: Closure { value, env },
                    orig_pos_arg,
                    pos_op,
                },
            ),
            UnaryOp::NumberArcSin => self.unary_number_op(
                f64::asin,
                Op1EvalData {
                    op,
                    arg: Closure { value, env },
                    orig_pos_arg,
                    pos_op,
                },
            ),
            UnaryOp::NumberArcTan => self.unary_number_op(
                f64::atan,
                Op1EvalData {
                    op,
                    arg: Closure { value, env },
                    orig_pos_arg,
                    pos_op,
                },
            ),
            UnaryOp::NumberCos => self.unary_number_op(
                f64::cos,
                Op1EvalData {
                    op,
                    arg: Closure { value, env },
                    orig_pos_arg,
                    pos_op,
                },
            ),
            UnaryOp::NumberSin => self.unary_number_op(
                f64::sin,
                Op1EvalData {
                    op,
                    arg: Closure { value, env },
                    orig_pos_arg,
                    pos_op,
                },
            ),
            UnaryOp::NumberTan => self.unary_number_op(
                f64::tan,
                Op1EvalData {
                    op,
                    arg: Closure { value, env },
                    orig_pos_arg,
                    pos_op,
                },
            ),
            UnaryOp::RecDefault => unimplemented!(),
            UnaryOp::RecForce => unimplemented!(),
        }
    }

    fn unary_number_op<F>(&mut self, f: F, eval_data: Op1EvalData) -> Result<Closure, ErrorKind>
    where
        F: Fn(f64) -> f64,
    {
        let Op1EvalData {
            op,
            arg: Closure { value, env: _ },
            orig_pos_arg,
            pos_op,
        } = eval_data;

        if let Some(n) = value.as_number() {
            let result_as_f64 = f(f64::rounding_from(n, RoundingMode::Nearest).0);
            let result = Number::try_from_float_simplest(result_as_f64).map_err(|_| {
                Box::new(EvalErrorKind::Other(
                    format!(
                        "invalid arithmetic operation: \
                        {op}({n}) returned {result_as_f64}, \
                        but {result_as_f64} isn't representable in Nickel",
                    ),
                    pos_op,
                ))
            })?;

            Ok(NickelValue::number(result, pos_op.to_inherited()).into())
        } else {
            Err(Box::new(EvalErrorKind::UnaryPrimopTypeError {
                primop: op.to_string(),
                expected: String::from("Number"),
                pos_arg: orig_pos_arg,
                arg_evaluated: value,
            }))
        }
    }

    /// Evaluate a binary operation.
    ///
    /// Both arguments are expected to be evaluated (in WHNF).
    fn process_binary_operation(&mut self, eval_data: Op2EvalData) -> Result<Closure, ErrorKind> {
        let Op2EvalData {
            op,
            arg1: Closure {
                value: value1,
                env: env1,
            },
            arg2:
                Closure {
                    value: mut value2,
                    env: env2,
                },
            orig_pos_arg1,
            orig_pos_arg2,
            pos_op,
        } = eval_data;

        increment!(format!("primop:{op}"));

        let pos1 = value1.pos_idx();
        let pos2 = value2.pos_idx();
        let pos_op_inh = pos_op.to_inherited();

        macro_rules! mk_type_error {
            (op_name=$op_name:expr, $expected:expr, $arg_number:expr, $arg_evaled:expr) => {
                Err(Box::new(EvalErrorKind::NAryPrimopTypeError {
                    primop: String::from($op_name),
                    expected: String::from($expected),
                    arg_number: $arg_number,
                    pos_arg: {
                        match $arg_number {
                            1 => orig_pos_arg1,
                            2 => orig_pos_arg2,
                            _ => unimplemented!(),
                        }
                    },
                    arg_evaluated: $arg_evaled,
                    pos_op,
                }))
            };
            ($expected:expr, $arg_number:expr, $arg_evaled:expr) => {
                mk_type_error!(
                    op_name = op.to_string(),
                    $expected,
                    $arg_number,
                    $arg_evaled
                )
            };
        }

        match op {
            BinaryOp::Seal => {
                let Some(key) = value1.as_sealing_key() else {
                    return mk_type_error!("SealingKey", 1, value1);
                };

                let Some(label) = value2.as_label() else {
                    return mk_type_error!("Label", 2, value2);
                };

                Ok(mk_fun!(
                    "x",
                    NickelValue::term(
                        Term::sealed(*key, mk_term::var("x"), label.clone()),
                        pos_op_inh
                    )
                )
                .into())
            }
            BinaryOp::Plus => self.binary_number_op(
                |n1, n2| n1 + n2,
                Op2EvalData {
                    op,
                    arg1: Closure {
                        value: value1,
                        env: env1,
                    },
                    arg2: Closure {
                        value: value2,
                        env: env2,
                    },
                    orig_pos_arg1,
                    orig_pos_arg2,
                    pos_op,
                },
            ),
            BinaryOp::Sub => self.binary_number_op(
                |n1, n2| n1 - n2,
                Op2EvalData {
                    op,
                    arg1: Closure {
                        value: value1,
                        env: env1,
                    },
                    arg2: Closure {
                        value: value2,
                        env: env2,
                    },
                    orig_pos_arg1,
                    orig_pos_arg2,
                    pos_op,
                },
            ),
            BinaryOp::Mult => self.binary_number_op(
                |n1, n2| n1 * n2,
                Op2EvalData {
                    op,
                    arg1: Closure {
                        value: value1,
                        env: env1,
                    },
                    arg2: Closure {
                        value: value2,
                        env: env2,
                    },
                    orig_pos_arg1,
                    orig_pos_arg2,
                    pos_op,
                },
            ),
            BinaryOp::Div => {
                let Some(n1) = value1.as_number() else {
                    return mk_type_error!("Number", 1, value1);
                };

                let Some(n2) = value2.as_number() else {
                    return mk_type_error!("Number", 2, value2);
                };

                if n2 == &Number::ZERO {
                    Err(Box::new(EvalErrorKind::Other(
                        String::from("division by zero"),
                        pos_op,
                    )))
                } else {
                    Ok(NickelValue::number(n1 / n2, pos_op_inh).into())
                }
            }
            BinaryOp::Modulo => {
                let Some(n1) = value1.as_number() else {
                    return mk_type_error!("Number", 1, value1);
                };

                let Some(n2) = value2.as_number() else {
                    return mk_type_error!("Number", 2, value2);
                };

                if n2 == &Number::ZERO {
                    return Err(Box::new(EvalErrorKind::Other(
                        String::from("division by zero (%)"),
                        pos2,
                    )));
                }

                // This is the equivalent of `truncate()` for `Number`
                let quotient = Number::from(Integer::rounding_from(n1 / n2, RoundingMode::Down).0);

                Ok(NickelValue::number(n1 - quotient * n2, pos_op_inh).into())
            }
            BinaryOp::NumberArcTan2 => {
                let Some(n1) = value1.as_number() else {
                    return mk_type_error!("Number", 1, value1);
                };

                let Some(n2) = value2.as_number() else {
                    return mk_type_error!("Number", 2, value2);
                };

                let y = f64::rounding_from(n1, RoundingMode::Nearest).0;
                let x = f64::rounding_from(n2, RoundingMode::Nearest).0;

                let result_as_f64 = y.atan2(x);

                let result = Number::try_from_float_simplest(result_as_f64).map_err(|_| {
                    Box::new(EvalErrorKind::Other(
                        format!(
                            "invalid arithmetic operation: \
                            number/arctan2({n1}, {n2}) returned {result_as_f64}, \
                            but {result_as_f64} isn't representable in Nickel"
                        ),
                        pos_op,
                    ))
                })?;

                Ok(NickelValue::number(result, pos_op_inh).into())
            }
            BinaryOp::NumberLog => {
                let Some(n1) = value1.as_number() else {
                    return mk_type_error!("Number", 1, value1);
                };

                let Some(n2) = value2.as_number() else {
                    return mk_type_error!("Number", 2, value2);
                };

                let n = f64::rounding_from(n1, RoundingMode::Nearest).0;

                let result_as_f64 = if n2 == &2 {
                    n.log2()
                } else if n2 == &Number::from(10) {
                    n.log10()
                } else {
                    let base = f64::rounding_from(n2, RoundingMode::Nearest).0;
                    n.log(base)
                };

                let result = Number::try_from_float_simplest(result_as_f64).map_err(|_| {
                    Box::new(EvalErrorKind::Other(
                        format!(
                            "invalid arithmetic operation: \
                            number/log({n1}, {n2}) returned {result_as_f64}, \
                            but {result_as_f64} isn't representable in Nickel"
                        ),
                        pos_op,
                    ))
                })?;

                Ok(NickelValue::number(result, pos_op_inh).into())
            }
            BinaryOp::Pow => {
                let Some(n1) = value1.as_number() else {
                    return mk_type_error!("Number", 1, value1);
                };

                let Some(n2) = value2.as_number() else {
                    return mk_type_error!("Number", 2, value2);
                };

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
                        Box::new(EvalErrorKind::Other(
                            format!(
                                "invalid arithmetic operation: \
                                        {n1}^{n2} returned {result_as_f64}, \
                                        but {result_as_f64} isn't representable in Nickel"
                            ),
                            pos_op,
                        ))
                    })?
                };

                Ok(NickelValue::number(result, pos_op_inh).into())
            }
            BinaryOp::StringConcat => {
                let Some(s1) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let Some(s2) = value2.as_string() else {
                    return mk_type_error!("String", 2, value2);
                };

                Ok(NickelValue::string(format!("{s1}{s2}"), pos_op_inh).into())
            }
            BinaryOp::ContractApply | BinaryOp::ContractCheck => {
                // Performing only one match `if let Term::Type` and putting the call to
                // `increment!` there looks sensible at first, but it's annoying to explain to
                // rustc and clippy that we match on `typ` but use it only if the `metrics` feature
                // is enabled (we get unused variable warning otherwise). It's simpler to just make
                // a separate `if` conditionally included.
                #[cfg(feature = "metrics")]
                if let Some(TypeData { typ, .. }) = value1.as_type() {
                    increment!(format!(
                        "primop:contract/apply:{}",
                        typ.pretty_print_cap(40)
                    ));
                }

                if let Some(TypeData { typ: _, contract }) = value1.as_type() {
                    // The contract generation from a static type might return any kind of
                    // contract, including e.g. a record or a custom contract. The result needs to
                    // be evaluated first, and then passed to `b_op` again. In that case, we don't
                    // bother tracking the argument and updating the label: this will be done by
                    // the next call to `b_op`.

                    // We set the stack to represent the evaluation context `<b_op> [.] label` and
                    // proceed to evaluate `<typ.contract()>`
                    self.stack.push_op_cont(
                        OperationCont::Op2First(
                            op,
                            Closure {
                                value: value2,
                                env: env2,
                            },
                            orig_pos_arg1,
                        ),
                        self.call_stack.len(),
                        pos_op_inh,
                    );

                    return Ok(Closure {
                        value: contract.clone(),
                        env: env1,
                    });
                }

                let Some(label) = value2.as_label() else {
                    return mk_type_error!("Label", 2, value2);
                };

                let mut label = label.clone();

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
                let (idx, stack_value_pos) = self
                    .stack
                    .pop_arg_as_idx(&mut self.context.cache)
                    .ok_or_else(|| {
                        Box::new(EvalErrorKind::NotEnoughArgs(
                            3,
                            String::from("contract/apply"),
                            pos_op,
                        ))
                    })?;

                // We update the label and convert it back to a term form that can be cheaply cloned
                label.arg_pos = self.context.pos_table.get(
                    self.context
                        .cache
                        .get_then(idx.clone(), |c| c.value.pos_idx()),
                );
                label.arg_idx = Some(idx.clone());
                let new_label = NickelValue::label(label, pos2);

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
                if let (
                    ValueContentRef::CustomContract(_) | ValueContentRef::Record(_),
                    BinaryOp::ContractApply,
                ) = (value1.content_ref(), &op)
                {
                    self.stack.push_arg(new_label.clone().into(), pos_op_inh);

                    self.stack.push_op_cont(
                        OperationCont::Op1(UnaryOp::ContractPostprocessResult, pos1.to_inherited()),
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
                if let BinaryOp::ContractCheck = &op {
                    self.stack.push_arg(new_label.clone().into(), pos_op_inh);

                    self.stack.push_op_cont(
                        OperationCont::Op1(
                            UnaryOp::ContractAttachDefaultLabel,
                            pos1.to_inherited(),
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
                self.stack.push_arg(new_label.into(), pos2.to_inherited());

                // We convert the contract (which can be a custom contract, a record, a naked
                // function, etc.) to a form that can be applied to a label and a value.
                let functoid = match value1.content_ref() {
                    ValueContentRef::Term(Term::Fun(..) | Term::Match { .. }) => {
                        // Warn on naked function contracts, but not if they came from the
                        // stdlib. Some stdlib functions return naked function contracts.
                        if let Some(pos) = self.context.pos_table.get(pos1).as_opt_ref() {
                            if !self.context.import_resolver.files().is_stdlib(pos.src_id) {
                                self.warn(Warning::NakedFunctionContract {
                                    func_pos: self.context.pos_table.get(pos1),
                                    app_pos: self.context.pos_table.get(pos_op),
                                });
                            }
                        }

                        if let BinaryOp::ContractApply = op {
                            Closure {
                                value: value1,
                                env: env1,
                            }
                        } else {
                            // Prepare the stack to represent the evaluation context `[.]
                            // as_naked` and proceed with `$naked_to_custom`
                            self.stack.push_arg(
                                Closure {
                                    value: value1,
                                    env: env1,
                                },
                                orig_pos_arg1,
                            );

                            internals::naked_to_custom().into()
                        }
                    }
                    ValueContentRef::CustomContract(ctr) => Closure {
                        value: ctr.clone(),
                        env: env1,
                    },
                    ValueContentRef::Record(..) => {
                        // Prepare the stack to represent the evaluation context `[.] t1` and
                        // proceed with `$record_contract`
                        self.stack.push_arg(
                            Closure {
                                value: value1,
                                env: env1,
                            },
                            orig_pos_arg1,
                        );

                        internals::record_contract().into()
                    }
                    _ => return mk_type_error!("Contract", 1, value1),
                };

                Ok(functoid)
            }
            BinaryOp::LabelWithErrorData => {
                // We need to extract plain values from a Nickel data structure, which most likely
                // contains closures at least, even if it's fully evaluated. As for serialization,
                // we thus need to fully substitute all variables first.
                let value1 = subst(
                    &self.context.pos_table,
                    &self.context.cache,
                    value1,
                    &self.initial_env,
                    &env1,
                );

                let Some(label) = value2.as_label() else {
                    return mk_type_error!("Label", 2, value2);
                };

                let mut label = label.clone();

                match value1.as_record() {
                    Some(Container::Empty) => Ok(NickelValue::label(label, pos2).into()),
                    Some(Container::Alloc(record_data)) => {
                        // If the contract returned a label as part of its error
                        // data, blame that one instead.
                        if let Some(user_label) = record_data
                            .fields
                            .get(&LocIdent::from("blame_location"))
                            .and_then(|field| field.value.as_ref())
                            .and_then(NickelValue::as_label)
                        {
                            label = user_label.clone();
                        }

                        if let Some(msg) = record_data
                            .fields
                            .get(&LocIdent::from("message"))
                            .and_then(|field| field.value.as_ref())
                            .and_then(NickelValue::as_string)
                        {
                            label = label.with_diagnostic_message(msg.clone().into_inner());
                        }

                        if let Some(notes) = record_data
                            .fields
                            .get(&LocIdent::from("notes"))
                            .and_then(|field| field.value.as_ref())
                            .and_then(NickelValue::as_array)
                            .and_then(Container::into_opt)
                        {
                            let notes = notes
                                .array
                                .iter()
                                .map(|element| {
                                    if let Some(s) = element.as_string() {
                                        Ok(s.clone().into_inner())
                                    } else {
                                        mk_type_error!("String (notes)", 1, element.clone())
                                    }
                                })
                                .collect::<Result<Vec<_>, _>>()?;

                            label = label.with_diagnostic_notes(notes);
                        }

                        Ok(NickelValue::label(label, pos2).into())
                    }
                    _ => {
                        mk_type_error!("Record", 1, value1)
                    }
                }
            }
            BinaryOp::Unseal => {
                if let Some(key) = value1.as_sealing_key() {
                    // The last argument (lazy, on the stack) of unseal is an expression raising
                    // blame. If the keys match, we ignore the blame and thus return a function
                    // `const unsealed_term_content`. Otherwise, we return `id`.
                    //
                    // Since the stack is set up as `[.] blame_expr`, this does ignore the error
                    // and proceed with the unsealed term in the happy path, or on the opposite
                    // drop the sealed term and proceed with the error on the stack otherwise.
                    Ok(if let Some(Term::Sealed(data)) = value2.as_term() {
                        if key == &data.key {
                            Closure {
                                value: mk_fun!(LocIdent::fresh(), data.inner.clone()),
                                env: env2,
                            }
                        } else {
                            mk_term::id().into()
                        }
                    } else {
                        mk_term::id().into()
                    })
                } else {
                    mk_type_error!("SealingKey", 1, value1)
                }
            }
            BinaryOp::Eq => {
                let c1 = Closure {
                    value: value1,
                    env: env1,
                };
                let c2 = Closure {
                    value: value2,
                    env: env2,
                };

                match eq(&mut self.context.cache, c1, c2, pos_op_inh)? {
                    EqResult::Bool(b) => match (b, self.stack.pop_eq()) {
                        (false, _) => {
                            self.stack.clear_eqs();
                            Ok(NickelValue::bool_value(false, pos_op_inh).into())
                        }
                        (true, None) => Ok(NickelValue::bool_value(true, pos_op_inh).into()),
                        (true, Some((c1, c2))) => {
                            let v1 = c1.value.closurize(&mut self.context.cache, c1.env);
                            let v2 = c2.value.closurize(&mut self.context.cache, c2.env);

                            // TODO: avoid term allocation in evaluation
                            Ok(NickelValue::term(Term::op2(BinaryOp::Eq, v1, v2), pos_op).into())
                        }
                    },
                    EqResult::Eqs(v1, v2, subeqs) => {
                        self.stack.push_eqs(subeqs.into_iter());

                        // TODO: avoid term allocation in evaluation
                        Ok(NickelValue::term(Term::op2(BinaryOp::Eq, v1, v2), pos_op).into())
                    }
                }
            }
            BinaryOp::LessThan => self.binary_number_cmp(
                |n1, n2| n1 < n2,
                Op2EvalData {
                    op,
                    arg1: Closure {
                        value: value1,
                        env: env1,
                    },
                    arg2: Closure {
                        value: value2,
                        env: env2,
                    },
                    orig_pos_arg1,
                    orig_pos_arg2,
                    pos_op,
                },
            ),
            BinaryOp::LessOrEq => self.binary_number_cmp(
                |n1, n2| n1 <= n2,
                Op2EvalData {
                    op,
                    arg1: Closure {
                        value: value1,
                        env: env1,
                    },
                    arg2: Closure {
                        value: value2,
                        env: env2,
                    },
                    orig_pos_arg1,
                    orig_pos_arg2,
                    pos_op,
                },
            ),
            BinaryOp::GreaterThan => self.binary_number_cmp(
                |n1, n2| n1 > n2,
                Op2EvalData {
                    op,
                    arg1: Closure {
                        value: value1,
                        env: env1,
                    },
                    arg2: Closure {
                        value: value2,
                        env: env2,
                    },
                    orig_pos_arg1,
                    orig_pos_arg2,
                    pos_op,
                },
            ),
            BinaryOp::GreaterOrEq => self.binary_number_cmp(
                |n1, n2| n1 >= n2,
                Op2EvalData {
                    op,
                    arg1: Closure {
                        value: value1,
                        env: env1,
                    },
                    arg2: Closure {
                        value: value2,
                        env: env2,
                    },
                    orig_pos_arg1,
                    orig_pos_arg2,
                    pos_op,
                },
            ),
            BinaryOp::LabelGoField => {
                let Some(field) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let Some(label) = value2.as_label() else {
                    return mk_type_error!("Label", 2, value2);
                };

                let mut label = label.clone();
                label
                    .path
                    .push(ty_path::Elem::Field(field.clone().into_inner().into()));
                Ok(NickelValue::label(label, pos_op_inh).into())
            }
            BinaryOp::RecordGet => {
                // This error should be impossible to trigger. The parser
                // prevents a dynamic field access where the field name is not syntactically
                // a string.
                let Some(id) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let Some(container) = value2.as_record() else {
                    // Not using mk_type_error! because of a non-uniform message
                    return Err(Box::new(EvalErrorKind::TypeError {
                        expected: String::from("Record"),
                        message: String::from("field access only makes sense for records"),
                        orig_pos: orig_pos_arg2,
                        term: value2,
                    }));
                };

                let ident = LocIdent::from(id);

                let Container::Alloc(record) = container else {
                    return Err(Box::new(EvalErrorKind::FieldMissing {
                        id: ident,
                        field_names: Vec::new(),
                        operator: BinaryOp::RecordGet.to_string(),
                        pos_record: pos2,
                        pos_op,
                    }));
                };

                // We have to apply potential pending contracts. Right now, this
                // means that repeated field access will re-apply the contract again
                // and again, which is not optimal. The same thing happens with array
                // contracts. There are several way to improve this, but this is left
                // as future work.
                match record
                    .get_value_with_ctrs(&ident)
                    .map_err(|missing_field_err| missing_field_err.into_eval_err(pos2, pos_op))?
                {
                    Some(value) => {
                        self.call_stack
                            .enter_field(ident, pos2, value.pos_idx(), pos_op);
                        Ok(Closure { value, env: env2 })
                    }
                    None => match record.sealed_tail.as_ref() {
                        Some(t) if t.has_dyn_field(id) => {
                            Err(Box::new(EvalErrorKind::IllegalPolymorphicTailAccess {
                                action: IllegalPolymorphicTailAction::FieldAccess {
                                    field: id.to_string(),
                                },
                                evaluated_arg: t.label.get_evaluated_arg(&self.context.cache),
                                label: t.label.clone(),
                            }))
                        }
                        _ => Err(Box::new(EvalErrorKind::FieldMissing {
                            id: ident,
                            field_names: record.field_names(RecordOpKind::IgnoreEmptyOpt),
                            operator: BinaryOp::RecordGet.to_string(),
                            pos_record: pos2,
                            pos_op,
                        })),
                    },
                }
            }
            BinaryOp::RecordInsert {
                metadata,
                pending_contracts,
                ext_kind,
                op_kind,
            } => {
                // Since we take ownership of the data hidden in `RecordInsert`, we can't pretty
                // print it anymore. However we do need a string representation for the error case,
                // which we reconstruct here.
                let op_name = || {
                    BinaryOp::RecordInsert {
                        ext_kind,
                        op_kind,
                        metadata: Box::new(FieldMetadata::default()),
                        pending_contracts: Vec::new(),
                    }
                    .to_string()
                };

                let Some(id) = value1.as_string() else {
                    return mk_type_error!(op_name = op_name(), "String", 1, value1);
                };

                let mut value2 = value2;

                if value2.is_inline_empty_record() {
                    // We are going to insert in the record, so we make sure that it's an allocated
                    // block and not an inline empty record.
                    value2 = NickelValue::empty_record_block(pos2);
                }

                let ValueContentRefMut::Record(Container::Alloc(record)) =
                    value2.content_make_mut()
                else {
                    // Theoretically, we could be in the case `Record(Container::Empty)` here, but
                    // we made sure to allocate a record block to specifically exclude this case.
                    return mk_type_error!(op_name = op_name(), "Record", 2, value2);
                };

                // If a defined value is expected for this field, it must be
                // provided as an additional argument, so we pop it from the stack
                let value = if let RecordExtKind::WithValue = ext_kind {
                    let (value_closure, _) =
                        self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                            Box::new(EvalErrorKind::NotEnoughArgs(
                                3,
                                String::from("insert"),
                                pos_op,
                            ))
                        })?;

                    let closurized = value_closure
                        .value
                        .closurize(&mut self.context.cache, value_closure.env);
                    Some(closurized)
                } else {
                    None
                };

                match record.fields.insert(
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
                        Err(Box::new(EvalErrorKind::Other(
                            format!(
                                "{}: \
                                tried to extend a record with the field {id}, \
                                but it already exists",
                                op_name(),
                            ),
                            pos_op,
                        )))
                    }
                    _ => Ok(Closure {
                        // Insertion preserves the frozenness
                        value: value2,
                        env: env2,
                    }),
                }
            }
            BinaryOp::RecordRemove(op_kind) => {
                let Some(id) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let mut value2 = value2;

                if value2.is_inline_empty_record() {
                    // We are going to insert in the record, so we make sure that it's an allocated
                    // block and not an inline empty record.
                    value2 = NickelValue::empty_record_block(pos2);
                }

                let ValueContentRefMut::Record(Container::Alloc(record)) =
                    value2.content_make_mut()
                else {
                    // Theoretically, we could be in the case `Record(Container::Empty)` here, but
                    // we made sure to allocate a record block to specifically exclude this case.
                    return mk_type_error!("Record", 2, value2);
                };

                let fetched = record.fields.swap_remove(&LocIdent::from(id));

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
                        Some(t) if t.has_dyn_field(id) => {
                            Err(Box::new(EvalErrorKind::IllegalPolymorphicTailAccess {
                                action: IllegalPolymorphicTailAction::FieldRemove {
                                    field: id.to_string(),
                                },
                                evaluated_arg: t.label.get_evaluated_arg(&self.context.cache),
                                label: t.label.clone(),
                            }))
                        }
                        _ => Err(Box::new(EvalErrorKind::FieldMissing {
                            id: id.into(),
                            field_names: record.field_names(op_kind),
                            operator: String::from("record/remove"),
                            pos_record: pos2,
                            pos_op,
                        })),
                    }
                } else {
                    Ok(Closure {
                        value: value2,
                        env: env2,
                    })
                }
            }
            BinaryOp::RecordHasField(op_kind) => {
                let Some(id) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let Some(container) = value2.as_record() else {
                    return mk_type_error!("Record", 2, value2);
                };

                Ok(NickelValue::bool_value(matches!(
                            container.get(id.clone().into()),
                            Some(field) if matches!(op_kind, RecordOpKind::ConsiderAllFields) || !field.is_empty_optional()
                        ),
                    pos_op_inh
                ).into())
            }
            BinaryOp::RecordFieldIsDefined(op_kind) => {
                let Some(id) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let Some(container) = value2.as_record() else {
                    return mk_type_error!("Record", 2, value2);
                };

                Ok(NickelValue::bool_value(
                        matches!(
                            container.get(id.clone().into()),
                            Some(field @ Field { value: Some(_), ..}) if matches!(op_kind, RecordOpKind::ConsiderAllFields) || !field.is_empty_optional()
                        ),
                        pos_op_inh,
                ).into())
            }
            BinaryOp::ArrayConcat => {
                let Some(container1) = value1.as_array() else {
                    return mk_type_error!("Array", 1, value1);
                };

                let Some(container2) = value2.as_array() else {
                    return mk_type_error!("Array", 2, value2);
                };

                // If one of the array is empty, we directly return the other.
                let Container::Alloc(array_data1) = container1 else {
                    return Ok(value2.into());
                };

                let Container::Alloc(array_data2) = container2 else {
                    return Ok(value1.into());
                };

                // In all generality, we need to apply the pending contracts on both sides, as they
                // can differ. Even if some are common, the order of contracts is meaningful, so
                // deduplicating the common part is not trivial.
                //
                // Still, we can handle the following case: if the pending contracts are the same
                // size and are equal pairwise, we can keep them lazy. The typical case is when
                // there's only one contract, but it's doesn't cost much to try them all.

                if array_data1.pending_contracts.len() == array_data2.pending_contracts.len()
                    && array_data1
                        .pending_contracts
                        .iter()
                        .zip(array_data2.pending_contracts.iter())
                        .all(|(ctr1, ctr2)| {
                            !ctr1.can_have_poly_ctrs()
                                && contract_eq(&ctr1.contract, &env1, &ctr2.contract, &env2)
                        })
                {
                    // Cloning a slice is cheap: it's better to clone the left hand side and extend
                    // it rather that building a chained iterator.
                    let mut result = array_data1.array.clone();
                    result.extend(array_data2.array.iter().cloned());

                    Ok(NickelValue::array(
                        result,
                        array_data1.pending_contracts.clone(),
                        pos_op_inh,
                    )
                    .into())
                } else {
                    // We need to collect in two phases, since the mapped closures capture
                    // `&mut self.cache`, so chaining the iterators first wouldn't work.

                    // We need to collect in two phases, since the mapped closures capture
                    // `&mut self.cache`, so chaining the iterators first wouldn't work.
                    //
                    // Technically, we could clone `array_data1` and ierate mutably over its
                    // elements in hope of sharing some of the structure. However, since it's a
                    // clone, the leaves will be shared, and mutable iteration over a shared
                    // persistent vector won't have much advantage over collecting a new array from
                    // an iterator.
                    let mut result: Array = container1
                        .iter()
                        .cloned()
                        .map(|elt| {
                            RuntimeContract::apply_all(
                                elt,
                                container1.iter_pending_contracts().cloned(),
                                pos1,
                            )
                            .closurize(&mut self.context.cache, env1.clone())
                        })
                        .collect();

                    result.extend(container2.iter().cloned().map(|elt| {
                        RuntimeContract::apply_all(
                            elt,
                            container2.iter_pending_contracts().cloned(),
                            pos2,
                        )
                        .closurize(&mut self.context.cache, env2.clone())
                    }));

                    Ok(NickelValue::array(result, Vec::new(), pos_op_inh).into())
                }
            }
            BinaryOp::ArrayAt => {
                let Some(container) = value1.as_array() else {
                    return mk_type_error!("Array", 1, value1);
                };

                let Some(n) = value2.as_number() else {
                    return mk_type_error!("Number", 2, value2);
                };

                let Ok(n_as_usize) = usize::try_from(n) else {
                    return Err(Box::new(EvalErrorKind::Other(
                        format!(
                            "array/at expects its second argument to be a \
                                positive integer smaller than {}, got {n}",
                            usize::MAX
                        ),
                        pos_op,
                    )));
                };

                let Container::Alloc(array_data) = container else {
                    return Err(Box::new(EvalErrorKind::Other(
                        "array/at: index out of bounds. \
                        Can't index into an empty array."
                            .to_owned(),
                        pos_op,
                    )));
                };

                if n_as_usize >= array_data.array.len() {
                    return Err(Box::new(EvalErrorKind::Other(
                        format!(
                            "array/at: index out of bounds. \
                                Expected an index between 0 and {}, got {}",
                            array_data.array.len(),
                            n
                        ),
                        pos_op,
                    )));
                }

                let elem_with_ctr = RuntimeContract::apply_all(
                    array_data.array.get(n_as_usize).unwrap().clone(),
                    array_data.pending_contracts.iter().cloned(),
                    pos1.to_inherited(),
                );

                Ok(Closure {
                    value: elem_with_ctr,
                    env: env1,
                })
            }
            BinaryOp::Merge(merge_label) => self.merge(
                value1,
                env1,
                value2,
                env2,
                pos_op,
                MergeMode::Standard(merge_label),
            ),
            BinaryOp::Hash => {
                let mk_err_fst =
                    || mk_type_error!("[| 'Md5, 'Sha1, 'Sha256, 'Sha512 |]", 1, value1.clone());

                let Some(enum_data) = value1.as_enum_variant() else {
                    return mk_err_fst();
                };

                if enum_data.arg.is_some() {
                    return mk_err_fst();
                }

                let Some(s) = value2.as_string() else {
                    return mk_type_error!("String", 2, value2);
                };

                let result = match enum_data.tag.as_ref() {
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
                    _ => return mk_err_fst(),
                };

                Ok(NickelValue::string(result, pos_op_inh).into())
            }
            BinaryOp::Serialize => {
                let mk_err_fst = || mk_type_error!(ENUM_FORMAT, 1, value1.clone());

                let Some(enum_data) = value1.as_enum_variant() else {
                    return mk_err_fst();
                };

                if enum_data.arg.is_some() {
                    return mk_err_fst();
                }

                // Serialization needs all variables term to be fully substituted
                let initial_env = Environment::new();
                let v2_subst = subst(
                    &self.context.pos_table,
                    &self.context.cache,
                    value2,
                    &initial_env,
                    &env2,
                );

                let format = match enum_data.tag.to_string().as_str() {
                    "Json" => ExportFormat::Json,
                    "Yaml" => ExportFormat::Yaml,
                    "Toml" => ExportFormat::Toml,
                    _ => return mk_err_fst(),
                };

                serialize::validate(format, &v2_subst)?;

                Ok(
                    NickelValue::string(serialize::to_string(format, &v2_subst)?, pos_op_inh)
                        .into(),
                )
            }
            BinaryOp::Deserialize => {
                let mk_err_fst = || mk_type_error!(ENUM_FORMAT, 1, value1.clone());

                let Some(enum_data) = value1.as_enum_variant() else {
                    return mk_err_fst();
                };

                if enum_data.arg.is_some() {
                    return mk_err_fst();
                }

                let Some(s) = value2.as_string() else {
                    return mk_type_error!("String", 2, value2);
                };

                let deser: NickelValue = match enum_data.tag.label() {
                    "Json" => serde_json::from_str(s).map_err(|err| {
                        Box::new(EvalErrorKind::DeserializationError(
                            String::from("json"),
                            format!("{err}"),
                            pos_op,
                        ))
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
                    "Yaml" => crate::serialize::yaml::load_yaml_value(
                        &mut self.context.pos_table,
                        s,
                        None,
                    )
                    .map_err(|err| {
                        Box::new(EvalErrorKind::DeserializationErrorWithInner {
                            format: InputFormat::Yaml,
                            inner: err,
                            pos: pos_op,
                        })
                    })?,
                    "Toml" => toml::from_str(s).map_err(|err| {
                        Box::new(EvalErrorKind::DeserializationError(
                            String::from("toml"),
                            format!("{err}"),
                            pos_op,
                        ))
                    })?,
                    _ => return mk_err_fst(),
                };

                Ok(deser.with_pos_idx(pos_op_inh).into())
            }
            BinaryOp::StringSplit => self.binary_string_fn(
                |input, sep| NickelValue::array(input.split(sep), Vec::new(), pos_op_inh),
                Op2EvalData {
                    op,
                    arg1: Closure {
                        value: value1,
                        env: env1,
                    },
                    arg2: Closure {
                        value: value2,
                        env: env2,
                    },
                    orig_pos_arg1,
                    orig_pos_arg2,
                    pos_op,
                },
            ),
            BinaryOp::StringContains => self.binary_string_fn(
                |s1, s2| NickelValue::bool_value(s1.contains(s2.as_str()), pos_op_inh),
                Op2EvalData {
                    op,
                    arg1: Closure {
                        value: value1,
                        env: env1,
                    },
                    arg2: Closure {
                        value: value2,
                        env: env2,
                    },
                    orig_pos_arg1,
                    orig_pos_arg2,
                    pos_op,
                },
            ),
            BinaryOp::StringCompare => {
                let as_term_pos = self.context.pos_table.get(pos_op_inh);

                self.binary_string_fn(
                    |s1, s2| {
                        use std::cmp::Ordering;

                        NickelValue::enum_tag(
                            LocIdent::new_with_pos(
                                match s1.cmp(s2) {
                                    Ordering::Less => "Lesser",
                                    Ordering::Equal => "Equal",
                                    Ordering::Greater => "Greater",
                                },
                                as_term_pos,
                            ),
                            pos_op_inh,
                        )
                    },
                    Op2EvalData {
                        op,
                        arg1: Closure {
                            value: value1,
                            env: env1,
                        },
                        arg2: Closure {
                            value: value2,
                            env: env2,
                        },
                        orig_pos_arg1,
                        orig_pos_arg2,
                        pos_op,
                    },
                )
            }
            BinaryOp::ContractArrayLazyApp => {
                let (ctr, _) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    Box::new(EvalErrorKind::NotEnoughArgs(
                        3,
                        String::from("contract/array_lazy_app"),
                        pos_op,
                    ))
                })?;

                let Closure {
                    value: ctr_val,
                    env: env_ctr,
                } = ctr;

                let Some(label) = value1.as_label() else {
                    return mk_type_error!("Label", 1, value1);
                };

                if value2.is_inline_empty_array() {
                    // We are going to insert in the array, so we make sure that it's an allocated
                    // block and not an inline empty array.
                    value2 = NickelValue::empty_array_block(pos2);
                }

                let ValueContentRefMut::Array(Container::Alloc(array_data)) =
                    value2.content_make_mut()
                else {
                    // Theoretically, we could be in the case `Array(Container::Empty)` here, but
                    // we made sure to allocate an array block to specifically exclude this case.
                    return mk_type_error!("Array", 2, value2);
                };

                // Preserve the environment of the contract in the resulting array.
                let contract = ctr_val.closurize(&mut self.context.cache, env_ctr);
                RuntimeContract::push_dedup(
                    &mut array_data.pending_contracts,
                    &env2,
                    RuntimeContract::new(contract, label.clone()),
                    &Environment::new(),
                );

                let array_with_ctr = Closure {
                    value: value2,
                    env: env2,
                };

                Ok(array_with_ctr)
            }
            BinaryOp::ContractRecordLazyApp => {
                // The contract is expected to be of type `String -> Contract`: it takes the name
                // of the field as a parameter, and returns a contract.
                let (
                    Closure {
                        value: ctr_val,
                        env: ctr_env,
                    },
                    _,
                ) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    Box::new(EvalErrorKind::NotEnoughArgs(
                        3,
                        String::from("contract/record_lazy_app"),
                        pos_op,
                    ))
                })?;

                let Some(label) = value1.as_label() else {
                    return mk_type_error!("Label", 1, value1);
                };

                let Some(container) = value2.as_record() else {
                    return mk_type_error!("Record", 2, value2);
                };

                let Container::Alloc(record_data) = container else {
                    return Ok(NickelValue::empty_record().with_pos_idx(pos2).into());
                };

                let mut record_data = record_data.clone();

                // Applying a lazy contract unfreezes a record, as frozen record are
                // expected to have all their contracts applied and thus an empty list of
                // pending contracts.
                record_data.attrs.frozen = false;

                let mut contract_at_field = |id: LocIdent| {
                    let pos = ctr_val.pos_idx();
                    mk_app!(
                        ctr_val.clone(),
                        NickelValue::string(id, self.context.pos_table.push(id.pos))
                    )
                    .with_pos_idx(pos)
                    .closurize(&mut self.context.cache, ctr_env.clone())
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
                        &ctr_env,
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
                let reverted = super::fixpoint::revert(&mut self.context.cache, record_data);

                Ok(NickelValue::term(reverted, pos2).into())
            }
            BinaryOp::LabelWithMessage => {
                let Some(message) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let ValueContentRefMut::Label(label) = value2.content_make_mut() else {
                    return mk_type_error!("Label", 2, value2);
                };

                label.set_diagnostic_message(message.clone().into_inner());
                Ok(value2.into())
            }
            BinaryOp::LabelWithNotes => {
                // We need to extract plain strings from a Nickel array, which most likely
                // contains at least generated variables.
                // As for serialization, we thus fully substitute all variables first.
                let val1_subst = subst(
                    &self.context.pos_table,
                    &self.context.cache,
                    value1.clone(),
                    &Environment::new(),
                    &env1,
                );

                let Some(array_data) = val1_subst.as_array() else {
                    return mk_type_error!("Array", 1, value1);
                };

                let ValueContentRefMut::Label(label) = value2.content_make_mut() else {
                    return mk_type_error!("Label", 2, value2);
                };

                let notes = array_data
                    .iter()
                    .map(|element| {
                        if let Some(s) = element.as_string() {
                            Ok(s.clone().into_inner())
                        } else {
                            mk_type_error!("String", 1, element.clone())
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                label.set_diagnostic_notes(notes);

                Ok(value2.into())
            }
            BinaryOp::LabelAppendNote => {
                let Some(note) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let ValueContentRefMut::Label(label) = value2.content_make_mut() else {
                    return mk_type_error!("Label", 2, value2);
                };

                label.append_diagnostic_note(note);
                Ok(value2.into())
            }
            BinaryOp::LabelLookupTypeVar => {
                let Some(key) = value1.as_sealing_key() else {
                    return mk_type_error!("SealingKey", 1, value1);
                };

                let Some(label) = value2.as_label() else {
                    return mk_type_error!("Label", 2, value2);
                };

                Ok(NickelValue::from(label.type_environment.get(key).unwrap())
                    .with_pos_idx(pos_op_inh)
                    .into())
            }
            BinaryOp::RecordSplitPair => {
                let Some(cont1) = value1.as_record() else {
                    return mk_type_error!("Record", 1, value1);
                };

                let Some(cont2) = value2.as_record() else {
                    return mk_type_error!("Record", 2, value2);
                };

                let record1 = cont1.into_opt();
                let record2 = cont2.into_opt();

                let split::SplitResult {
                    left,
                    center,
                    right,
                } = split::split_ref(
                    record1
                        .as_ref()
                        .map(|r| &r.fields)
                        .unwrap_or(&Default::default()),
                    record2
                        .as_ref()
                        .map(|r| &r.fields)
                        .unwrap_or(&Default::default()),
                );

                let left_only = NickelValue::record_posless(RecordData {
                    fields: left,
                    sealed_tail: record1
                        .as_ref()
                        .map(|r| r.sealed_tail.clone())
                        .unwrap_or_default(),
                    attrs: record1.as_ref().map(|r| r.attrs).unwrap_or_default(),
                });

                let right_only = NickelValue::record_posless(RecordData {
                    fields: right,
                    sealed_tail: record2
                        .as_ref()
                        .map(|r| r.sealed_tail.clone())
                        .unwrap_or_default(),
                    attrs: record2.as_ref().map(|r| r.attrs).unwrap_or_default(),
                });

                let (center1, center2): (IndexMap<LocIdent, Field>, IndexMap<LocIdent, Field>) =
                    center
                        .into_iter()
                        .map(|(id, (left, right))| ((id, left), (id, right)))
                        .unzip();

                let left_center = NickelValue::record_posless(RecordData {
                    fields: center1,
                    sealed_tail: None,
                    attrs: RecordAttrs::default(),
                });

                let right_center = NickelValue::record_posless(RecordData {
                    fields: center2,
                    sealed_tail: None,
                    attrs: RecordAttrs::default(),
                });

                Ok(closurize_container(NickelValue::record(
                    RecordData {
                        fields: IndexMap::from([
                            (LocIdent::from("left_only"), Field::from(left_only)),
                            (LocIdent::from("left_center"), Field::from(left_center)),
                            (LocIdent::from("right_center"), Field::from(right_center)),
                            (LocIdent::from("right_only"), Field::from(right_only)),
                        ]),
                        attrs: RecordAttrs::default(),
                        sealed_tail: None,
                    },
                    pos_op_inh,
                ))
                .into())
            }
            BinaryOp::RecordDisjointMerge => {
                let Some(container1) = value1.as_record() else {
                    return mk_type_error!("Record", 1, value1);
                };

                let Some(container2) = value2.as_record() else {
                    return mk_type_error!("Record", 2, value2);
                };

                let (record1, record2) = match (container1, container2) {
                    (Container::Alloc(record1), Container::Alloc(record2)) => (record1, record2),
                    (Container::Empty, _) => {
                        return Ok(Closure {
                            value: value2.with_pos_idx(pos_op_inh),
                            env: env2,
                        });
                    }
                    (_, Container::Empty) => {
                        return Ok(Closure {
                            value: value1.with_pos_idx(pos_op_inh),
                            env: env1,
                        });
                    }
                };

                // As for merge, we refuse to combine two records if one of them has a sealed tail.
                // However, if only one of them does, because we don't do any recursive
                // re-evaluation here, it's fine to just pick this tail as the tail of the result.
                //
                // This behavior is actually useful, because disjoint_merge is used in the
                // implementation of builtin contracts to combine an unsealed tail with the
                // original body of the record. In that case, the unsealed tail might have an
                // additional sealed tail itself (tail can be sealed multiple times in a nested
                // way), and the right behavior (tm) is to just keep it.
                let sealed_tail = match (&record1.sealed_tail, &record2.sealed_tail) {
                    (Some(record::SealedTail { label, .. }), Some(_)) => {
                        return Err(Box::new(EvalErrorKind::IllegalPolymorphicTailAccess {
                            action: IllegalPolymorphicTailAction::Merge,
                            evaluated_arg: label.get_evaluated_arg(&self.context.cache),
                            label: label.clone(),
                        }));
                    }
                    (tail1, tail2) => tail1.as_ref().or(tail2.as_ref()).cloned(),
                };

                // Note that because of record closurization, we assume here that the record data
                // of each record are already closurized, so we don't really care about
                // environments. Should that invariant change, we might get into trouble (trouble
                // meaning undue `UnboundIdentifier` errors).
                let mut fields =
                    IndexMap::with_capacity(record1.fields.len() + record2.fields.len());

                fields.extend(record1.fields.iter().map(|(k, v)| (*k, v.clone())));
                fields.extend(record2.fields.iter().map(|(k, v)| (*k, v.clone())));

                let attrs = Combine::combine(record1.attrs, record2.attrs);

                Ok(NickelValue::record(
                    RecordData {
                        fields,
                        attrs,
                        sealed_tail,
                    },
                    pos_op_inh,
                )
                .into())
            }
        }
    }

    fn binary_number_cmp<F>(&mut self, f: F, eval_data: Op2EvalData) -> Result<Closure, ErrorKind>
    where
        F: Fn(&Number, &Number) -> bool,
    {
        let pos_op_inh = eval_data.pos_op.to_inherited();

        self.binary_number_fn(
            |n1, n2| NickelValue::bool_value(f(n1, n2), pos_op_inh),
            eval_data,
        )
    }

    fn binary_number_op<F>(&mut self, f: F, eval_data: Op2EvalData) -> Result<Closure, ErrorKind>
    where
        F: Fn(&Number, &Number) -> Number,
    {
        let pos_op_inh = eval_data.pos_op.to_inherited();

        self.binary_number_fn(
            |n1, n2| NickelValue::number(f(n1, n2), pos_op_inh),
            eval_data,
        )
    }

    fn binary_number_fn<F>(&mut self, f: F, eval_data: Op2EvalData) -> Result<Closure, ErrorKind>
    where
        F: Fn(&Number, &Number) -> NickelValue,
    {
        let Op2EvalData {
            op,
            arg1: Closure {
                value: value1,
                env: _,
            },
            arg2: Closure {
                value: value2,
                env: _,
            },
            orig_pos_arg1,
            orig_pos_arg2,
            pos_op,
        } = eval_data;

        let Some(n1) = value1.as_number() else {
            return Err(Box::new(EvalErrorKind::NAryPrimopTypeError {
                primop: op.to_string(),
                expected: "Number".to_owned(),
                arg_number: 1,
                pos_arg: orig_pos_arg1,
                arg_evaluated: value1,
                pos_op,
            }));
        };

        let Some(n2) = value2.as_number() else {
            return Err(Box::new(EvalErrorKind::NAryPrimopTypeError {
                primop: op.to_string(),
                expected: "Number".to_owned(),
                arg_number: 2,
                pos_arg: orig_pos_arg2,
                arg_evaluated: value2,
                pos_op,
            }));
        };

        Ok(f(n1, n2).into())
    }

    fn binary_string_fn<F>(&mut self, f: F, eval_data: Op2EvalData) -> Result<Closure, ErrorKind>
    where
        F: Fn(&NickelString, &NickelString) -> NickelValue,
    {
        let Op2EvalData {
            op,
            arg1: Closure {
                value: value1,
                env: _,
            },
            arg2: Closure {
                value: value2,
                env: _,
            },
            orig_pos_arg1,
            orig_pos_arg2,
            pos_op,
        } = eval_data;

        let Some(s1) = value1.as_string() else {
            return Err(Box::new(EvalErrorKind::NAryPrimopTypeError {
                primop: op.to_string(),
                expected: "String".to_owned(),
                arg_number: 1,
                pos_arg: orig_pos_arg1,
                arg_evaluated: value1,
                pos_op,
            }));
        };

        let Some(s2) = value2.as_string() else {
            return Err(Box::new(EvalErrorKind::NAryPrimopTypeError {
                primop: op.to_string(),
                expected: "String".to_owned(),
                arg_number: 2,
                pos_arg: orig_pos_arg2,
                arg_evaluated: value2,
                pos_op,
            }));
        };

        Ok(f(s1, s2).into())
    }

    /// Evaluate a n-ary operation.
    ///
    /// Arguments are expected to be evaluated (in WHNF).
    fn process_nary_operation(&mut self, eval_data: OpNEvalData) -> Result<Closure, ErrorKind> {
        let OpNEvalData { op, args, pos_op } = eval_data;
        increment!(format!("primop:{op}"));
        let pos_op_inh = pos_op.to_inherited();

        let mk_type_error =
            |expected: &str, arg_number: usize, pos_arg: PosIdx, arg_evaluated: NickelValue| {
                Err(Box::new(EvalErrorKind::NAryPrimopTypeError {
                    primop: op.to_string(),
                    expected: expected.to_owned(),
                    arg_number,
                    pos_arg,
                    arg_evaluated,
                    pos_op,
                }))
            };

        // Currently, for fixed arity primitive operators, the parser must ensure that they get
        // exactly the right number of argument: if it is not the case, this is a bug, and we panic.
        match op {
            NAryOp::StringReplace | NAryOp::StringReplaceRegex => {
                let mut args_wo_env = args.into_iter().map(|(arg, pos)| (arg.value, pos));
                let (arg1, arg_pos1) = args_wo_env.next().unwrap();
                let (arg2, arg_pos2) = args_wo_env.next().unwrap();
                let (arg3, arg_pos3) = args_wo_env.next().unwrap();
                debug_assert!(args_wo_env.next().is_none());

                let Some(s) = arg1.as_string() else {
                    return mk_type_error("String", 1, arg_pos1, arg1);
                };

                let Some(from) = arg2.as_string() else {
                    return mk_type_error("String", 2, arg_pos2, arg2);
                };

                let Some(to) = arg3.as_string() else {
                    return mk_type_error("String", 3, arg_pos3, arg3);
                };

                let result = if let NAryOp::StringReplace = op {
                    s.replace(from.as_str(), to.as_str())
                } else {
                    let re = regex::Regex::new(from)
                        .map_err(|err| Box::new(EvalErrorKind::Other(err.to_string(), pos_op)))?;

                    s.replace_regex(&CompiledRegex(re), to)
                };

                Ok(NickelValue::string(result, pos_op_inh).into())
            }
            NAryOp::StringSubstr => {
                let mut args_wo_env = args.into_iter().map(|(arg, pos)| (arg.value, pos));
                let (arg1, arg_pos1) = args_wo_env.next().unwrap();
                let (arg2, arg_pos2) = args_wo_env.next().unwrap();
                let (arg3, arg_pos3) = args_wo_env.next().unwrap();
                debug_assert!(args_wo_env.next().is_none());

                let Some(s) = arg1.as_string() else {
                    return mk_type_error("String", 1, arg_pos1, arg1);
                };

                let Some(start) = arg2.as_number() else {
                    return mk_type_error("Number", 2, arg_pos2, arg2);
                };

                let Some(end) = arg3.as_number() else {
                    return mk_type_error("Number", 3, arg_pos3, arg3);
                };

                s.substring(start, end)
                    .map(|substr| NickelValue::string(substr, pos_op_inh).into())
                    .map_err(|e| Box::new(EvalErrorKind::Other(format!("{e}"), pos_op)))
            }
            NAryOp::MergeContract => {
                let mut args_iter = args.into_iter();

                let (
                    Closure {
                        value: arg1,
                        env: _,
                    },
                    _,
                ) = args_iter.next().unwrap();

                let (
                    Closure {
                        value: arg2,
                        env: env2,
                    },
                    _,
                ) = args_iter.next().unwrap();

                let (
                    Closure {
                        value: arg3,
                        env: env3,
                    },
                    _,
                ) = args_iter.next().unwrap();

                debug_assert!(args_iter.next().is_none());

                let Some(label) = arg1.as_label() else {
                    return Err(Box::new(EvalErrorKind::InternalError(
                        format!(
                            "The {op} operator was expecting \
                                a first argument of type Label, got {}",
                            arg1.type_of().unwrap_or("<unevaluated>")
                        ),
                        pos_op,
                    )));
                };

                self.merge(
                    arg2,
                    env2,
                    arg3,
                    env3,
                    pos_op,
                    MergeMode::Contract(label.clone()),
                )
            }
            NAryOp::RecordSealTail => {
                let mut args = args.into_iter();
                let (
                    Closure {
                        value: arg1,
                        env: _,
                    },
                    arg_pos1,
                ) = args.next().unwrap();

                let (
                    Closure {
                        value: arg2,
                        env: _,
                    },
                    arg_pos2,
                ) = args.next().unwrap();

                let (
                    Closure {
                        value: mut arg3,
                        env: env3,
                    },
                    arg_pos3,
                ) = args.next().unwrap();

                let (
                    Closure {
                        value: arg4,
                        env: env4,
                    },
                    arg_pos4,
                ) = args.next().unwrap();

                debug_assert!(args.next().is_none());

                let Some(s) = arg1.as_sealing_key() else {
                    return mk_type_error("SealingKey", 1, arg_pos1, arg1);
                };

                let Some(label) = arg2.as_label() else {
                    return mk_type_error("Label", 2, arg_pos2, arg2);
                };

                if arg3.is_inline_empty_record() {
                    // We are going to insert in the record, so we make sure that it's an allocated
                    // block and not an inline empty record.
                    arg3 = NickelValue::empty_record_block(arg3.pos_idx());
                }

                let ValueContentRefMut::Record(Container::Alloc(r)) = arg3.content_make_mut()
                else {
                    return mk_type_error("Record", 3, arg_pos3, arg3);
                };

                // Even if the record to seal is empty, the correctness of polymorphic contracts
                // relies on the symmetry of sealing/unsealing operations. It's wiser to always
                // seal a tail, even when empty.
                let Some(tail) = arg4.as_record() else {
                    return mk_type_error("Record", 4, arg_pos4, arg4);
                };

                let tail_closurized = arg4.clone().closurize(&mut self.context.cache, env4);
                let fields = tail
                    .into_opt()
                    .map(|r| r.fields.keys().map(|s| s.ident()).collect())
                    .unwrap_or_default();
                r.sealed_tail = Some(record::SealedTail::new(
                    *s,
                    label.clone(),
                    tail_closurized,
                    fields,
                ));

                Ok(Closure {
                    value: arg3,
                    env: env3,
                })
            }
            NAryOp::RecordUnsealTail => {
                let mut args = args.into_iter();
                let (
                    Closure {
                        value: arg1,
                        env: _,
                    },
                    arg_pos1,
                ) = args.next().unwrap();
                let (
                    Closure {
                        value: arg2,
                        env: _,
                    },
                    arg_pos2,
                ) = args.next().unwrap();
                let (
                    Closure {
                        value: arg3,
                        env: env3,
                    },
                    arg_pos3,
                ) = args.next().unwrap();

                debug_assert!(args.next().is_none());

                let Some(s) = arg1.as_sealing_key() else {
                    return mk_type_error("SealingKey", 1, arg_pos1, arg1);
                };

                let Some(label) = arg2.as_label() else {
                    return mk_type_error("Label", 2, arg_pos2, arg2);
                };

                let Some(container) = arg3.as_record() else {
                    return mk_type_error("Record", 3, arg_pos3, arg3);
                };

                container
                    .into_opt()
                    .and_then(|record| record.sealed_tail.as_ref())
                    .and_then(|tail| tail.unseal(s).cloned())
                    .ok_or_else(|| {
                        Box::new(EvalErrorKind::BlameError {
                            evaluated_arg: label.get_evaluated_arg(&self.context.cache),
                            label: label.clone(),
                        })
                    })
                    .map(|tail_unsealed| Closure {
                        value: tail_unsealed,
                        env: env3,
                    })
            }
            NAryOp::LabelInsertTypeVar => {
                let mut args = args.into_iter();

                let (
                    Closure {
                        value: arg1,
                        env: _,
                    },
                    arg_pos1,
                ) = args.next().unwrap();

                let (
                    Closure {
                        value: arg2,
                        env: _,
                    },
                    arg_pos2,
                ) = args.next().unwrap();

                let (
                    Closure {
                        value: mut arg3,
                        env: _,
                    },
                    arg_pos3,
                ) = args.next().unwrap();

                debug_assert!(args.next().is_none());

                let Some(key) = arg1.as_sealing_key() else {
                    return mk_type_error("SealingKey", 1, arg_pos1, arg1);
                };

                let Ok(polarity) = Polarity::try_from(&arg2) else {
                    return mk_type_error("Polarity", 2, arg_pos2, arg2);
                };

                let ValueContentRefMut::Label(label) = arg3.content_make_mut() else {
                    return mk_type_error("Label", 3, arg_pos3, arg3);
                };

                label
                    .type_environment
                    .insert(*key, TypeVarData { polarity });

                Ok(arg3.with_pos_idx(arg_pos3.to_inherited()).into())
            }
            NAryOp::ArraySlice => {
                let mut args = args.into_iter();

                let (
                    Closure {
                        value: arg1,
                        env: _,
                    },
                    arg_pos1,
                ) = args.next().unwrap();

                let (
                    Closure {
                        value: arg2,
                        env: _,
                    },
                    arg_pos2,
                ) = args.next().unwrap();

                let (
                    Closure {
                        value: mut arg3,
                        env: env3,
                    },
                    arg_pos3,
                ) = args.next().unwrap();

                debug_assert!(args.next().is_none());

                let Some(start) = arg1.as_number() else {
                    return mk_type_error("Number", 1, arg_pos1, arg1);
                };

                let Some(end) = arg2.as_number() else {
                    return mk_type_error("Number", 2, arg_pos2, arg2);
                };

                if arg3.is_inline_empty_array() {
                    // We are going to insert in the array, so we make sure that it's an allocated
                    // block and not an inline empty array.
                    arg3 = NickelValue::empty_array_block(arg3.pos_idx());
                }

                let ValueContentRefMut::Array(Container::Alloc(ArrayData { array, .. })) =
                    arg3.content_make_mut()
                else {
                    return mk_type_error("Array", 3, arg_pos3, arg3);
                };

                let Ok(start_as_usize) = usize::try_from(start) else {
                    return Err(Box::new(EvalErrorKind::Other(
                        format!(
                            "{op} expects its first argument (start) to be a \
                            positive integer smaller than {}, got {start}",
                            usize::MAX
                        ),
                        pos_op,
                    )));
                };

                let Ok(end_as_usize) = usize::try_from(end) else {
                    return Err(Box::new(EvalErrorKind::Other(
                        format!(
                            "{op} expects its second argument (end) to be a \
                            positive integer smaller than {}, got {end}",
                            usize::MAX
                        ),
                        pos_op,
                    )));
                };

                if end_as_usize < start_as_usize || end_as_usize > array.len() {
                    return Err(Box::new(EvalErrorKind::Other(
                        format!(
                            "{op}: index out of bounds. Expected `start <= end <= {}`, but \
                            got `start={start}` and `end={end}`.",
                            array.len()
                        ),
                        pos_op,
                    )));
                }

                array.slice(start_as_usize, end_as_usize);

                Ok(Closure {
                    value: arg3.with_pos_idx(pos_op_inh),
                    env: env3,
                })
            }
        }
    }
}

/// The enum tag returned by Typeof and Cast.
///
/// This function is a less precise version of `v.type_of()`, because `type_tag` has backward
/// compatibility guarantees to uphold. Instead of relying on
/// [crate::eval::value::NickelValue::type_of], it's safer to duplicate the logic here.
fn type_tag(v: &NickelValue) -> &'static str {
    match v.content_ref() {
        ValueContentRef::Null => "Other",
        ValueContentRef::Bool(_) => "Bool",
        ValueContentRef::Number(_) => "Number",
        ValueContentRef::Array(_) => "Array",
        ValueContentRef::Record(_) => "Record",
        ValueContentRef::String(_) => "String",
        ValueContentRef::Term(term) => match term {
            Term::RecRecord(..) => "Record",
            Term::Fun(..) | Term::Match { .. } => "Function",
            _ => "Other",
        },
        ValueContentRef::Label(_) => "Label",
        ValueContentRef::EnumVariant(_) => "Enum",
        ValueContentRef::ForeignId(_) => "ForeignId",
        ValueContentRef::CustomContract(_) => "CustomContract",
        ValueContentRef::Type(_) => "Type",
        _ => "Other",
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
    pos_op: PosIdx,
) -> Result<EqResult, ErrorKind> {
    let Closure {
        value: value1,
        env: env1,
    } = c1;

    let Closure {
        value: value2,
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
        if let Some((v1, v2)) = it.next() {
            let eqs = it
                .map(|(v1, v2)| {
                    (
                        Closure {
                            value: v1,
                            env: env1.clone(),
                        },
                        Closure {
                            value: v2,
                            env: env2.clone(),
                        },
                    )
                })
                .collect();

            EqResult::Eqs(v1.closurize(cache, env1), v2.closurize(cache, env2), eqs)
        } else {
            EqResult::Bool(true)
        }
    }

    match (value1.content_ref(), value2.content_ref()) {
        (ValueContentRef::Null, ValueContentRef::Null) => Ok(EqResult::Bool(true)),
        (ValueContentRef::Bool(b1), ValueContentRef::Bool(b2)) => Ok(EqResult::Bool(b1 == b2)),
        (ValueContentRef::Number(n1), ValueContentRef::Number(n2)) => Ok(EqResult::Bool(n1 == n2)),
        (ValueContentRef::String(s1), ValueContentRef::String(s2)) => Ok(EqResult::Bool(s1 == s2)),
        (ValueContentRef::Label(l1), ValueContentRef::Label(l2)) => Ok(EqResult::Bool(l1 == l2)),
        (ValueContentRef::SealingKey(k1), ValueContentRef::SealingKey(k2)) => {
            Ok(EqResult::Bool(k1 == k2))
        }
        (
            ValueContentRef::EnumVariant(EnumVariantData {
                tag: tag1,
                arg: None,
            }),
            ValueContentRef::EnumVariant(EnumVariantData {
                tag: tag2,
                arg: None,
            }),
        ) => Ok(EqResult::Bool(tag1.ident() == tag2.ident())),
        (
            ValueContentRef::EnumVariant(EnumVariantData {
                tag: tag1,
                arg: Some(arg1),
            }),
            ValueContentRef::EnumVariant(EnumVariantData {
                tag: tag2,
                arg: Some(arg2),
            }),
        ) if tag1.ident() == tag2.ident() => Ok(gen_eqs(
            cache,
            std::iter::once((arg1.clone(), arg2.clone())),
            env1,
            env2,
        )),
        // [^eq-empty-containers] We can't just handle the pattern `(Container::Empty,
        // Container::Empty)`, because the first could be the inlined empty record while the second
        // is allocated but empty as well (typically the case for the empty open record `{..}`), or
        // have only empty optional fields (which equality ignores). Hence we rely on
        // `has_only_empty_opts`, which handles both cases.
        (ValueContentRef::Record(container1), ValueContentRef::Record(container2))
            if container1.has_only_empty_opts() && container2.has_only_empty_opts() =>
        {
            Ok(EqResult::Bool(true))
        }
        (
            ValueContentRef::Record(Container::Alloc(r1)),
            ValueContentRef::Record(Container::Alloc(r2)),
        ) => {
            let merge::split::SplitResult {
                left,
                center,
                right,
            } = merge::split::split_ref(&r1.fields, &r2.fields);

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
                            let pos1 = value1.pos_idx();
                            let pos2 = value2.pos_idx();

                            let value1_with_ctr =
                                RuntimeContract::apply_all(value1, pending_contracts1, pos1);
                            let value2_with_ctr =
                                RuntimeContract::apply_all(value2, pending_contracts2, pos2);
                            Some(Ok((value1_with_ctr, value2_with_ctr)))
                        }
                        (Field { value: None, .. }, Field { value: None, .. }) => None,
                        (
                            Field {
                                value: v1 @ None,
                                metadata,
                                ..
                            },
                            Field { value: Some(_), .. },
                        )
                        | (
                            Field {
                                value: v1 @ Some(_),
                                ..
                            },
                            Field {
                                value: None,
                                metadata,
                                ..
                            },
                        ) => {
                            let pos_record = if v1.is_none() {
                                value1.pos_idx()
                            } else {
                                value2.pos_idx()
                            };

                            Some(Err(Box::new(EvalErrorKind::MissingFieldDef {
                                id,
                                metadata,
                                pos_record,
                                pos_access: pos_op,
                            })))
                        }
                    })
                    .collect();

                Ok(gen_eqs(cache, eqs?.into_iter(), env1, env2))
            }
        }
        // See [^eq-empty-containers]
        (ValueContentRef::Array(container1), ValueContentRef::Array(container2))
            if container1.is_empty() && container2.is_empty() =>
        {
            Ok(EqResult::Bool(true))
        }
        (
            ValueContentRef::Array(Container::Alloc(array_data1)),
            ValueContentRef::Array(Container::Alloc(array_data2)),
        ) if array_data1.array.len() == array_data2.array.len() => {
            // Equalities are tested in reverse order, but that shouldn't matter. If it
            // does, just do `eqs.rev()`

            // We should apply all contracts here, otherwise we risk having wrong values, think
            // record contracts with default values, wrapped terms, etc.

            let mut eqs = array_data1
                .array
                .iter()
                .cloned()
                .map(|elt| {
                    let pos = elt.pos_idx().to_inherited();
                    RuntimeContract::apply_all(
                        elt,
                        array_data1.pending_contracts.iter().cloned(),
                        pos,
                    )
                    .closurize(cache, env1.clone())
                })
                .collect::<Vec<_>>()
                .into_iter()
                .zip(array_data2.array.iter().cloned().map(|elt| {
                    let pos = elt.pos_idx().to_inherited();
                    RuntimeContract::apply_all(
                        elt,
                        array_data2.pending_contracts.iter().cloned(),
                        pos,
                    )
                    .closurize(cache, env2.clone())
                }))
                .collect::<Vec<_>>();

            match eqs.pop() {
                None => Ok(EqResult::Bool(true)),
                Some((v1, v2)) => {
                    let eqs = eqs
                        .into_iter()
                        .map(|(v1, v2)| (v1.into(), v2.into()))
                        .collect::<Vec<(Closure, Closure)>>();

                    Ok(EqResult::Eqs(v1, v2, eqs))
                }
            }
        }
        // Function-like terms and foreign ids can't be compared together.
        (ValueContentRef::ForeignId(_), ValueContentRef::ForeignId(_))
        | (ValueContentRef::CustomContract(_), ValueContentRef::CustomContract(_))
        | (
            ValueContentRef::Term(Term::Fun(..) | Term::Match(_) | Term::FunPattern(..)),
            ValueContentRef::Term(Term::Fun(..) | Term::Match(_) | Term::FunPattern(..)),
        ) => Err(Box::new(EvalErrorKind::IncomparableValues {
            eq_pos: pos_op,
            left: value1,
            right: value2,
        })),
        (_, _) => Ok(EqResult::Bool(false)),
    }
}

/// Eta-expands a unary operator into a (lazy) function.
///
/// Regex-based primitive operations are evaluated to a function that captures the compiled regexp,
/// to avoid recompiling it at each call. [eta_expand] builds such a closure: given a primary (in
/// practice, regex) operator `%op1%`, [eta_expand] will return the expression `fun x => %op1% x`.
/// Each intermediate term is given the position index `pos_op`.
fn eta_expand(op: UnaryOp, pos_op: PosIdx) -> Term {
    let param = LocIdent::fresh();

    Term::fun(
        param,
        NickelValue::term(
            Term::op1(op, NickelValue::term(Term::Var(param), pos_op)),
            pos_op,
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
                        let pos = value.pos_idx();
                        let value_with_ctrs = RuntimeContract::apply_all(
                            value,
                            field.pending_contracts.iter().cloned(),
                            pos,
                        );
                        f(id, value_with_ctrs)
                    })
                    .ok_or(record::MissingFieldDefErrorData {
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

/// Wrap a value in a [crate::term::Term::Closurize] operator with the same position index.
fn closurize_container(value: NickelValue) -> NickelValue {
    let pos_idx = value.pos_idx();
    NickelValue::term(Term::Closurize(value), pos_idx)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cache::resolvers::DummyResolver,
        error::NullReporter,
        eval::{Environment, VmContext, cache::CacheImpl},
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
            let cont: OperationCont = OperationCont::Op1(UnaryOp::IfThenElse, PosIdx::NONE);

            vm.stack.push_arg(mk_term::integer(5).into(), PosIdx::NONE);
            vm.stack.push_arg(mk_term::integer(46).into(), PosIdx::NONE);

            vm.stack.push_op_cont(cont, 0, PosIdx::NONE);

            assert_eq!(
                vm.continuate_operation(NickelValue::bool_true().into()),
                Ok(mk_term::integer(46).into())
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
                PosIdx::NONE,
            );

            vm.stack.push_op_cont(cont, 0, PosIdx::NONE);

            assert_eq!(
                vm.continuate_operation(mk_term::integer(7).into()),
                Ok(mk_term::integer(6).into())
            );
            assert_eq!(1, vm.stack.count_conts());
            assert_eq!(
                (
                    OperationCont::Op2Second(
                        BinaryOp::Plus,
                        mk_term::integer(7).into(),
                        PosIdx::NONE,
                        PosIdx::NONE,
                    ),
                    0,
                    PosIdx::NONE
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
                PosIdx::NONE,
                PosIdx::NONE,
            );

            vm.stack.push_op_cont(cont, 0, PosIdx::NONE);

            assert_eq!(
                vm.continuate_operation(mk_term::integer(6).into()),
                Ok(Closure {
                    value: mk_term::integer(13),
                    env: Environment::new()
                })
            );
        });
    }
}
