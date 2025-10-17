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
    bytecode::value::{
        Array, ArrayBody, EnumVariantBody, InlineValue, LabelBody, NickelValue, NumberBody,
        RecordBody, SealingKeyBody, StringBody, TermBody, TypeBody, ValueContent, ValueContentRef,
        ValueContentRefMut,
    },
    cache::InputFormat,
    closurize::Closurize,
    combine::Combine,
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

/// A string represention of the type of the first argument of serialization-related primitive
/// operations. This is a Nickel enum of the supported serialization formats.
static ENUM_FORMAT: &'static str = "[| 'Json, 'Yaml, 'Toml |]";

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
    pub fn continuate_operation(&mut self, mut clos: Closure) -> Result<Closure, EvalErrorData> {
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
    ) -> Result<Closure, EvalErrorData> {
        increment!(format!("primop:{u_op}"));

        let Closure { value, env } = clos;
        let pos = value.pos_idx();
        let pos_op_inh = pos_op.to_inherited_block(&mut self.context.pos_table);

        macro_rules! mk_type_error {
            (op_name=$op_name:expr, $expected:expr) => {
                mk_type_error!(op_name = $op_name, $expected, value = value)
            };
            (op_name=$op_name:expr, $expected:expr, value=$value:expr) => {
                Err(EvalErrorData::UnaryPrimopTypeError {
                    primop: String::from($op_name),
                    expected: String::from($expected),
                    pos_arg,
                    arg_evaluated: $value,
                })
            };
            ($expected:expr) => {
                mk_type_error!(op_name = u_op.to_string(), $expected)
            };
            ($expected:expr, value=$value:expr) => {
                mk_type_error!(op_name = u_op.to_string(), $expected, value = $value)
            };
            ($expected:expr, value=$value:expr) => {};
            ($expected:expr, $arg_number:expr) => {
                mk_type_error!($expected, $arg_number, value = value)
            };
            ($expected:expr, $arg_number:expr, value=$value:expr) => {
                Err(EvalErrorData::NAryPrimopTypeError {
                    primop: u_op.to_string(),
                    expected: String::from($expected),
                    arg_number: $arg_number,
                    pos_arg,
                    arg_evaluated: $value,
                    pos_op,
                })
            };
        }

        match u_op {
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
                    Err(EvalErrorData::TypeError {
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
                        Some(false) => Ok(value
                            .with_pos_idx(&mut self.context.pos_table, pos_op_inh)
                            .into()),
                        _ => mk_type_error!("Bool", 1),
                    }
                } else {
                    Err(EvalErrorData::NotEnoughArgs(2, String::from("&&"), pos_op))
                }
            }
            UnaryOp::BoolOr => {
                if let Some((next, ..)) = self.stack.pop_arg(&self.context.cache) {
                    match value.as_bool() {
                        Some(true) => Ok(value
                            .with_pos_idx(&mut self.context.pos_table, pos_op_inh)
                            .into()),
                        // FIXME: this does not check that the second argument is actually a
                        // boolean. This means `false || 2` silently evaluates to `2`. This is
                        // simpler and more efficient, but can make debugging harder. In any case,
                        // it should be solved only once primary operators have better support for
                        // laziness in some arguments.
                        Some(false) => Ok(next),
                        _ => mk_type_error!("Bool", 1),
                    }
                } else {
                    Err(EvalErrorData::NotEnoughArgs(2, String::from("||"), pos_op))
                }
            }
            UnaryOp::BoolNot => {
                if let Some(b) = value.as_bool() {
                    Ok(NickelValue::bool_value_posless(!b)
                        .with_pos_idx(&mut self.context.pos_table, pos_op_inh)
                        .into())
                } else {
                    mk_type_error!("Bool")
                }
            }
            UnaryOp::Blame => match value.content() {
                ValueContent::Label(lens) => {
                    let label = lens.take().0;

                    Err(EvalErrorData::BlameError {
                        evaluated_arg: label.get_evaluated_arg(&self.context.cache),
                        label,
                    })
                }
                lens => {
                    mk_type_error!("Label", value = lens.restore())
                }
            },
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
                    Ok(value
                        .with_pos_idx(&mut self.context.pos_table, pos_op_inh)
                        .into())
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
                        ValueContent::Inline(lens) if lens.peek().is_empty_record() => {
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
                        .ok_or_else(|| EvalErrorData::NonExhaustiveEnumMatch {
                            expected: cases.keys().copied().collect(),
                            found: NickelValue::enum_variant_posless(enum_body.tag, None)
                                .with_pos_idx(&mut self.context.pos_table, pos),
                            pos: pos_op_inh,
                        })
                } else if let Some(clos) = default {
                    Ok(clos)
                } else {
                    mk_type_error!("Enum", 2)
                }
            }
            UnaryOp::LabelFlipPol => match value.content() {
                ValueContent::Label(lens) => {
                    let mut label = lens.take().0;
                    label.polarity = label.polarity.flip();
                    Ok(NickelValue::label(label, pos_op_inh).into())
                }
                lens => {
                    mk_type_error!("Label", value = lens.restore())
                }
            },
            UnaryOp::LabelPol => {
                if let Some(label) = value.as_label() {
                    Ok(NickelValue::from(label.0.polarity)
                        .with_pos_idx(&mut self.context.pos_table, pos_op_inh)
                        .into())
                } else {
                    mk_type_error!("Label")
                }
            }
            UnaryOp::LabelGoDom => match value.content() {
                ValueContent::Label(lens) => {
                    let mut label = lens.take().0;
                    label.path.push(ty_path::Elem::Domain);
                    Ok(NickelValue::label(label, pos_op_inh).into())
                }
                lens => {
                    mk_type_error!("Label", value = lens.restore())
                }
            },
            UnaryOp::LabelGoCodom => match value.content() {
                ValueContent::Label(lens) => {
                    let mut label = lens.take().0;
                    label.path.push(ty_path::Elem::Codomain);
                    Ok(NickelValue::label(label, pos_op_inh).into())
                }
                lens => {
                    mk_type_error!("Label", value = lens.restore())
                }
            },
            UnaryOp::LabelGoArray => match value.content() {
                ValueContent::Label(lens) => {
                    let mut label = lens.take().0;
                    label.path.push(ty_path::Elem::Array);
                    Ok(NickelValue::label(label, pos_op_inh).into())
                }
                lens => {
                    mk_type_error!("Label", value = lens.restore())
                }
            },
            UnaryOp::LabelGoDict => match value.content() {
                ValueContent::Label(lens) => {
                    let mut label = lens.take().0;
                    label.path.push(ty_path::Elem::Dict);
                    Ok(NickelValue::label(label, pos_op_inh).into())
                }
                lens => {
                    mk_type_error!("Label", value = lens.restore())
                }
            },
            UnaryOp::RecordAccess(id) => {
                if let Some(RecordBody(record)) = value.as_record() {
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
                                Err(EvalErrorData::IllegalPolymorphicTailAccess {
                                    action: IllegalPolymorphicTailAction::FieldAccess {
                                        field: id.to_string(),
                                    },
                                    evaluated_arg: t.label.get_evaluated_arg(&self.context.cache),
                                    label: t.label.clone(),
                                })
                            }
                            _ => Err(EvalErrorData::FieldMissing {
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
                    Err(EvalErrorData::TypeError {
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
                            NickelValue::string(
                                id.label(),
                                self.context.pos_table.push_block(id.pos),
                            )
                        })
                        .collect();

                    Ok(Closure {
                        value: NickelValue::array_force_pos(
                            &mut self.context.pos_table,
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
                        .map_err(|miss_def_err| miss_def_err.into_eval_err(pos, pos_op))?;

                    values.sort_by_key(|(id, _)| *id);
                    let terms = values.into_iter().map(|(_, value)| value).collect();

                    Ok(Closure {
                        // TODO: once sure that the Record is properly closurized, we can safely
                        // assume that the extracted array here is, in turn, also closuried.
                        value: NickelValue::array_force_pos(
                            &mut self.context.pos_table,
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
                    EvalErrorData::NotEnoughArgs(2, String::from("array/map"), pos_op)
                })?;

                match value.content() {
                    ValueContent::Array(lens) => {
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

                                NickelValue::term(
                                    Term::App(f_as_var.clone(), t_with_ctrs),
                                    pos_op_inh,
                                )
                                .closurize(&mut self.context.cache, env.clone())
                            })
                            .collect();

                        Ok(NickelValue::array_force_pos(
                            &mut self.context.pos_table,
                            ts,
                            Vec::new(),
                            pos_op_inh,
                        )
                        .into())
                    }
                    lens => {
                        mk_type_error!("Array", value = lens.restore())
                    }
                }
            }
            UnaryOp::ArrayGen => {
                let (f, _) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    EvalErrorData::NotEnoughArgs(2, String::from("array/generate"), pos_op)
                })?;

                let Some(NumberBody(n)) = value.as_number() else {
                    return mk_type_error!("Number");
                };

                if n < &Number::ZERO {
                    return Err(EvalErrorData::Other(
                        format!(
                            "array/generate expects its first argument to be a positive number, got {n}"
                        ),
                        pos_op,
                    ));
                }

                let Ok(n_int) = u32::try_from(n) else {
                    return Err(EvalErrorData::Other(
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
                        mk_app!(f_closure.clone(), NickelValue::number_posless(n))
                            .closurize(&mut self.context.cache, env.clone())
                    })
                    .collect();

                Ok(Closure {
                    value: NickelValue::array_force_pos(
                        &mut self.context.pos_table,
                        ts,
                        Vec::new(),
                        pos_op_inh,
                    ),
                    env: Environment::new(),
                })
            }
            UnaryOp::RecordMap => {
                let (f, ..) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    EvalErrorData::NotEnoughArgs(2, String::from("record/map"), pos_op)
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
                            return Err(EvalErrorData::IllegalPolymorphicTailAccess {
                                action: IllegalPolymorphicTailAction::Map,
                                evaluated_arg: label.get_evaluated_arg(&self.context.cache),
                                label,
                            });
                        }

                        let f_closure = f.value.closurize(&mut self.context.cache, f.env);

                        // As for `ArrayMap` (see above), we closurize the content of fields

                        let fields = record
                            .fields
                            .into_iter()
                            .filter(|(_, field)| !field.is_empty_optional())
                            .map_values_closurize(&mut self.context.cache, &env, |id, t| {
                                let pos = self.context.pos_table.get(t.pos_idx()).into_inherited();

                                mk_app!(
                                    f_closure.clone(),
                                    NickelValue::string_posless(id.label()),
                                    t
                                )
                                .with_pos(&mut self.context.pos_table, pos)
                            })
                            .map_err(|miss_field_err| miss_field_err.into_eval_err(pos, pos_op))?;

                        // By construction, mapping freezes the record. We set the frozen flag so
                        // that operations that require the record to be frozen don't have to
                        // perform the work again.
                        let attrs = record.attrs.frozen();

                        Ok(NickelValue::record_force_pos(
                            &mut self.context.pos_table,
                            RecordData {
                                fields,
                                attrs,
                                ..record
                            },
                            pos_op_inh,
                        )
                        .into())
                    }
                    lens => mk_type_error!("Record", 1, value = lens.restore()),
                }
            }
            UnaryOp::Seq => self
                .stack
                .pop_arg(&self.context.cache)
                .map(|(next, ..)| next)
                .ok_or_else(|| EvalErrorData::NotEnoughArgs(2, String::from("seq"), pos_op)),
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
                                missing_def_err.into_eval_err(pos, pos_op)
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
                                    pos.to_inherited_block(&mut self.context.pos_table),
                                )
                                .closurize(&mut self.context.cache, env.clone());
                                t_with_ctr
                            }),
                            pos_op,
                        );

                        Ok(terms.into())
                    }
                    ValueContentRef::EnumVariant(EnumVariantBody {
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
                            Err(EvalErrorData::NotEnoughArgs(
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
                        Ok(NickelValue::string(acc, pos_op_inh).into())
                    }
                } else {
                    // Since the error halts the evaluation, we don't bother cleaning the stack of
                    // the remaining string chunks.
                    //
                    // Not using mk_type_error! because of a non-uniform message
                    Err(EvalErrorData::TypeError {
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
                        &mut self.context.pos_table,
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
                    EvalErrorData::Other(
                        format!(
                            "to_string: can't convert an argument of type {} to string",
                            value.type_of().unwrap()
                        ),
                        pos,
                    )
                }),
            UnaryOp::NumberFromString => {
                if let Some(s) = value.as_string() {
                    let n = parse_number_sci(&s.0).map_err(|_| {
                        EvalErrorData::Other(
                            format!(
                                "number/from_string: invalid number literal `{}`",
                                s.0.as_str()
                            ),
                            pos,
                        )
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
                    let re = regex::Regex::new(&s.0)
                        .map_err(|err| EvalErrorData::Other(err.to_string(), pos_op))?;

                    let matcher = eta_expand(UnaryOp::StringIsMatchCompiled(re.into()), pos_op_inh);
                    Ok(NickelValue::term(matcher, pos_op_inh).into())
                } else {
                    mk_type_error!("String", 1)
                }
            }
            UnaryOp::StringFind => {
                if let Some(s) = value.as_string() {
                    let re = regex::Regex::new(&s.0)
                        .map_err(|err| EvalErrorData::Other(err.to_string(), pos_op))?;

                    let matcher = eta_expand(UnaryOp::StringFindCompiled(re.into()), pos_op_inh);
                    Ok(NickelValue::term(matcher, pos_op_inh).into())
                } else {
                    mk_type_error!("String", 1)
                }
            }
            UnaryOp::StringFindAll => {
                if let Some(s) = value.as_string() {
                    let re = regex::Regex::new(&s.0)
                        .map_err(|err| EvalErrorData::Other(err.to_string(), pos_op))?;

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
                        .with_pos_idx(&mut self.context.pos_table, pos_op_inh)
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
                                            .map(|s| NickelValue::string_posless(
                                                s.unwrap_or_default()
                                            ))
                                    ),
                                    Vec::new()
                                )
                            )
                        ),
                    };

                    Ok(result
                        .with_pos_idx(&mut self.context.pos_table, pos_op_inh)
                        .into())
                } else {
                    mk_type_error!(op_name = "a compiled regular expression match", "String")
                }
            }
            UnaryOp::StringFindAllCompiled(regex) => {
                if let Some(s) = value.as_string() {
                    let result = NickelValue::array_force_pos(
                        &mut self.context.pos_table,
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
                fn seq_terms<I>(
                    pos_table: &mut PosTable,
                    terms: I,
                    pos: PosIdx,
                    cont: NickelValue,
                ) -> NickelValue
                where
                    I: Iterator<Item = NickelValue>,
                {
                    terms
                        .fold(cont, |acc, t| mk_app!(mk_term::op1(UnaryOp::Seq, t), acc))
                        .with_pos_idx(pos_table, pos)
                }

                match value.content() {
                    // TODO[RFC007]: it's intentional that we don't want to handle empty arrays here
                    ValueContent::Record(lens) => {
                        let record = lens.take().0;
                        let fields = record
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
                            .map_err(|e| e.into_eval_err(pos, pos_op))?;

                        let terms = fields.clone().into_values().map(|field| {
                            field.value.expect(
                                "map_values_closurize ensures that values without a \
                                            definition throw a MissingFieldDefError",
                            )
                        });

                        let pos_inh = pos.to_inherited_block(&mut self.context.pos_table);
                        // unwrap(): will go away soon
                        let cont =
                            NickelValue::record(RecordData { fields, ..record }, pos_inh).unwrap();

                        Ok(seq_terms(&mut self.context.pos_table, terms, pos_op, cont).into())
                    }
                    //TODO[RFC007] We intentionally do NOT want to handle empty arrays here
                    ValueContent::Array(lens) => {
                        let ArrayBody {
                            array: ts,
                            pending_contracts,
                        } = lens.take();
                        let pos_inh = pos.to_inherited_block(&mut self.context.pos_table);

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
                        let cont = NickelValue::array_force_pos(
                            &mut self.context.pos_table,
                            ts,
                            Vec::new(),
                            pos_inh,
                        );

                        Ok(seq_terms(&mut self.context.pos_table, terms, pos_op, cont).into())
                    }
                    // For an enum variant, `force x` is simply equivalent to `deep_seq x x`, as
                    // there's no lazy pending contract to apply.
                    ValueContent::EnumVariant(lens) => {
                        let EnumVariantBody { tag, arg } = lens.take();

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
                                pos.to_inherited_block(&mut self.context.pos_table),
                            );

                            Ok(Closure {
                                value: seq_terms(
                                    &mut self.context.pos_table,
                                    std::iter::once(arg),
                                    pos_op,
                                    cont,
                                ),
                                env,
                            })
                        } else {
                            Ok(Closure {
                                value: NickelValue::enum_tag(tag, pos_op_inh).into(),
                                env,
                            })
                        }
                    }
                    lens => Ok(Closure {
                        value: lens.restore(),
                        env,
                    }),
                }
            }
            UnaryOp::RecordEmptyWithTail => {
                let lens = value.content();

                let ValueContent::Record(lens) = lens else {
                    return mk_type_error!("Record", value = lens.restore());
                };

                let record = lens.take().0;
                let mut result = RecordData::empty();
                result.sealed_tail = record.sealed_tail;

                Ok(Closure {
                    // unwrap(): will go away soon
                    value: NickelValue::record(result, pos_op_inh).unwrap(),
                    env,
                })
            }
            UnaryOp::RecordFreeze => {
                // If the record is already frozen, there's nothing to do.
                if matches!(value.as_record(), Some(RecordBody(record)) if record.attrs.frozen) {
                    // A frozen record shouldn't have a polymorphic tail
                    debug_assert!(value.as_record().unwrap().0.sealed_tail.is_none());

                    return Ok(Closure { value, env });
                }

                // Ditto if the record is empty. We can also drop the environment.
                if value.is_empty_record() {
                    return Ok(value.into());
                }

                match value.content() {
                    ValueContent::Record(lens) => {
                        let record = lens.take().0;

                        // It's not clear what the semantics of freezing a record with a sealed tail
                        // would be, as their might be dependencies between the sealed part and the
                        // unsealed part. Merging is disallowed on records with tail, so we disallow
                        // freezing as well.
                        if let Some(record::SealedTail { label, .. }) = record.sealed_tail {
                            return Err(EvalErrorData::IllegalPolymorphicTailAccess {
                                action: IllegalPolymorphicTailAction::Freeze,
                                evaluated_arg: label.get_evaluated_arg(&self.context.cache),
                                label,
                            });
                        }

                        let fields = record
                            .fields
                            .into_iter()
                            .map(|(id, field)| {
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

                                (id, field)
                            })
                            .collect();

                        let attrs = record.attrs.frozen();

                        Ok(Closure {
                            value: NickelValue::record_force_pos(
                                &mut self.context.pos_table,
                                RecordData {
                                    fields,
                                    attrs,
                                    sealed_tail: None,
                                },
                                pos_op_inh,
                            ),
                            env,
                        })
                    }
                    lens => mk_type_error!("Record", value = lens.restore()),
                }
            }
            UnaryOp::Trace => {
                if let Some(s) = value.as_string() {
                    let _ = writeln!(self.context.trace, "std.trace: {}", s.0);
                    Ok(())
                } else {
                    mk_type_error!("String")
                }?;

                self.stack
                    .pop_arg(&self.context.cache)
                    .map(|(next, ..)| next)
                    .ok_or_else(|| EvalErrorData::NotEnoughArgs(2, String::from("trace"), pos_op))
            }
            UnaryOp::LabelPushDiag => match value.content() {
                ValueContent::Label(lens) => {
                    let mut label = lens.take().0;
                    label.push_diagnostic();
                    Ok(Closure {
                        value: NickelValue::label(label, pos),
                        env,
                    })
                }
                lens => {
                    mk_type_error!("Label", value = lens.restore())
                }
            },
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
                if let Some(EnumVariantBody { arg: Some(arg), .. }) = value.as_enum_variant() {
                    Ok(Closure {
                        value: arg.clone(),
                        env,
                    })
                } else {
                    mk_type_error!("Enum variant")
                }
            }
            UnaryOp::EnumMakeVariant => {
                let Some(StringBody(tag)) = value.as_string() else {
                    return mk_type_error!("String");
                };

                let (arg_clos, _) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    EvalErrorData::NotEnoughArgs(2, String::from("enum/make_variant"), pos)
                })?;
                let arg_pos = arg_clos.value.pos_idx();
                let arg = NickelValue::thunk(Thunk::new(arg_clos), arg_pos);

                Ok(NickelValue::enum_variant(
                    LocIdent::new(&tag).with_pos(self.context.pos_table.get(pos)),
                    Some(arg),
                    pos_op_inh,
                )
                .into())
            }
            UnaryOp::EnumGetTag => match value.as_enum_variant() {
                Some(EnumVariantBody { tag, .. }) => {
                    Ok(NickelValue::enum_tag(*tag, pos_op_inh).into())
                }
                _ => mk_type_error!("Enum"),
            },
            UnaryOp::EnumIsVariant => Ok(NickelValue::bool_value(
                value.as_enum_variant().is_some(),
                self.context.pos_table.make_inline(pos_op_inh),
            )
            .into()),
            UnaryOp::PatternBranch => {
                // The continuation, that we must evaluate in the augmented environment.
                let (mut cont, _) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    EvalErrorData::NotEnoughArgs(2, String::from("with_env"), pos_op)
                })?;

                match value.content() {
                    ValueContent::Record(lens) => {
                        let data = lens.take().0;

                        for (id, field) in data.fields {
                            debug_assert!(field.metadata.is_empty());

                            if let Some(value) = field.value {
                                if let Some(idx) = value.as_thunk() {
                                    cont.env.insert(id.ident(), idx.0.clone());
                                } else {
                                    cont.env.insert(
                                        id.ident(),
                                        self.context.cache.add(
                                            Closure {
                                                value,
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
                    lens => mk_type_error!("Record", value = lens.restore()),
                }
            }
            UnaryOp::ContractCustom => {
                let contract =
                    if let Some(TermBody(Term::Fun(..) | Term::Match(_))) = value.as_term() {
                        value.closurize(&mut self.context.cache, env)
                    } else {
                        return mk_type_error!("Function or MatchExpression");
                    };

                Ok(NickelValue::custom_contract(contract, pos_op_inh).into())
            }
            UnaryOp::ContractPostprocessResult => {
                let Some(EnumVariantBody {
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
                    Some(EnumVariantBody { arg: Some(_), .. })
                ) {
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
            UnaryOp::NumberArcCos => self.process_unary_number_operation(
                value,
                pos_arg,
                pos_op,
                "number/arccos",
                f64::acos,
            ),
            UnaryOp::NumberArcSin => self.process_unary_number_operation(
                value,
                pos_arg,
                pos_op,
                "number/arcsin",
                f64::asin,
            ),
            UnaryOp::NumberArcTan => self.process_unary_number_operation(
                value,
                pos_arg,
                pos_op,
                "number/arctan",
                f64::atan,
            ),
            UnaryOp::NumberCos => {
                self.process_unary_number_operation(value, pos_arg, pos_op, "number/cos", f64::cos)
            }
            UnaryOp::NumberSin => {
                self.process_unary_number_operation(value, pos_arg, pos_op, "number/sin", f64::sin)
            }
            UnaryOp::NumberTan => {
                self.process_unary_number_operation(value, pos_arg, pos_op, "number/tan", f64::tan)
            }
            UnaryOp::RecDefault => unimplemented!(),
            UnaryOp::RecForce => unimplemented!(),
        }
    }

    fn process_unary_number_operation<Op>(
        &mut self,
        body: NickelValue,
        arg_pos: PosIdx,
        pos_op: PosIdx,
        op_name: &str,
        op: Op,
    ) -> Result<Closure, EvalErrorData>
    where
        Op: Fn(f64) -> f64,
    {
        if let Some(n) = body.as_number() {
            let n = &n.0;
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

            Ok(NickelValue::number(
                result,
                pos_op.to_inherited_block(&mut self.context.pos_table),
            )
            .into())
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
    ) -> Result<Closure, EvalErrorData> {
        increment!(format!("primop:{b_op}"));

        let Closure {
            value: mut value1,
            env: env1,
        } = fst_clos;

        let Closure {
            value: mut value2,
            env: env2,
        } = clos;

        let pos1 = value1.pos_idx();
        let pos2 = value2.pos_idx();
        let pos_op_inh = pos_op.to_inherited_block(&mut self.context.pos_table);

        macro_rules! mk_type_error {
            (op_name=$op_name:expr, $expected:expr, $arg_number:expr, $arg_evaled:expr) => {
                Err(EvalErrorData::NAryPrimopTypeError {
                    primop: String::from($op_name),
                    expected: String::from($expected),
                    arg_number: $arg_number,
                    pos_arg: {
                        match $arg_number {
                            1 => fst_pos,
                            2 => snd_pos,
                            _ => unimplemented!(),
                        }
                    },
                    arg_evaluated: $arg_evaled,
                    pos_op,
                })
            };
            ($expected:expr, $arg_number:expr, $arg_evaled:expr) => {
                mk_type_error!(
                    op_name = b_op.to_string(),
                    $expected,
                    $arg_number,
                    $arg_evaled
                )
            };
        }

        match b_op {
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
                        Term::Sealed(key.0, mk_term::var("x"), label.0.clone()),
                        pos_op_inh
                    )
                )
                .into())
            }
            BinaryOp::Plus => self.binary_number_op(
                |n1, n2| n1 + n2,
                value1,
                pos1,
                value2,
                pos2,
                pos_op_inh,
                b_op.to_string(),
            ),
            BinaryOp::Sub => self.binary_number_op(
                |n1, n2| n1 - n2,
                value1,
                pos1,
                value2,
                pos2,
                pos_op_inh,
                b_op.to_string(),
            ),
            BinaryOp::Mult => self.binary_number_op(
                |n1, n2| n1 * n2,
                value1,
                pos1,
                value2,
                pos2,
                pos_op_inh,
                b_op.to_string(),
            ),
            BinaryOp::Div => {
                let Some(n1) = value1.as_number() else {
                    return mk_type_error!("Number", 1, value1);
                };

                let Some(n2) = value2.as_number() else {
                    return mk_type_error!("Number", 2, value2);
                };

                let n1 = &n1.0;
                let n2 = &n2.0;

                if n2 == &Number::ZERO {
                    Err(EvalErrorData::Other(
                        String::from("division by zero"),
                        pos_op,
                    ))
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

                let n1 = &n1.0;
                let n2 = &n2.0;

                if n2 == &Number::ZERO {
                    return Err(EvalErrorData::Other(
                        String::from("division by zero (%)"),
                        pos2,
                    ));
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

                let n1 = &n1.0;
                let n2 = &n2.0;
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

                Ok(NickelValue::number(result, pos_op_inh).into())
            }
            BinaryOp::NumberLog => {
                let Some(n1) = value1.as_number() else {
                    return mk_type_error!("Number", 1, value1);
                };

                let Some(n2) = value2.as_number() else {
                    return mk_type_error!("Number", 2, value2);
                };

                let n1 = &n1.0;
                let n2 = &n2.0;
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
                    EvalErrorData::Other(
                        format!(
                            "invalid arithmetic operation: \
                            number/log({n1}, {n2}) returned {result_as_f64}, \
                            but {result_as_f64} isn't representable in Nickel"
                        ),
                        pos_op,
                    )
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

                let n1 = &n1.0;
                let n2 = &n2.0;

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

                Ok(NickelValue::number(result, pos_op_inh).into())
            }
            BinaryOp::StringConcat => {
                let Some(s1) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let Some(s2) = value2.as_string() else {
                    return mk_type_error!("String", 2, value2);
                };

                Ok(NickelValue::string(format!("{}{}", &s1.0, &s2.0), pos_op_inh).into())
            }
            BinaryOp::ContractApply | BinaryOp::ContractCheck => {
                // Performing only one match `if let Term::Type` and putting the call to
                // `increment!` there looks sensible at first, but it's annoying to explain to
                // rustc and clippy that we match on `typ` but use it only if the `metrics` feature
                // is enabled (we get unused variable warning otherwise). It's simpler to just make
                // a separate `if` conditionally included.
                #[cfg(feature = "metrics")]
                if Some(TypeBody { typ, .. }) = value1.as_type() {
                    increment!(format!(
                        "primop:contract/apply:{}",
                        typ.pretty_print_cap(40)
                    ));
                }

                if let Some(TypeBody { typ: _, contract }) = value1.as_type() {
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
                                value: value2,
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
                }

                let mut label;

                match value2.content() {
                    ValueContent::Label(lens) => {
                        label = lens.take().0;
                    }
                    lens => {
                        return mk_type_error!("Label", 2, lens.restore());
                    }
                };

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
                        EvalErrorData::NotEnoughArgs(3, String::from("contract/apply"), pos_op)
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
                    ValueContentRef::CustomContract(_)
                    | ValueContentRef::Record(_)
                    | ValueContentRef::Inline(InlineValue::EmptyRecord),
                    BinaryOp::ContractApply,
                ) = (value1.content_ref(), &b_op)
                {
                    self.stack.push_arg(new_label.clone().into(), pos_op_inh);

                    self.stack.push_op_cont(
                        OperationCont::Op1(
                            UnaryOp::ContractPostprocessResult,
                            pos1.to_inherited_block(&mut self.context.pos_table),
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
                if let BinaryOp::ContractCheck = &b_op {
                    self.stack.push_arg(new_label.clone().into(), pos_op_inh);

                    self.stack.push_op_cont(
                        OperationCont::Op1(
                            UnaryOp::ContractAttachDefaultLabel,
                            pos1.to_inherited_block(&mut self.context.pos_table),
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
                self.stack.push_arg(
                    new_label.into(),
                    pos2.to_inherited_block(&mut self.context.pos_table),
                );

                // We convert the contract (which can be a custom contract, a record, a naked
                // function, etc.) to a form that can be applied to a label and a value.
                let functoid = match value1.content_ref() {
                    ValueContentRef::Term(TermBody(Term::Fun(..) | Term::Match { .. })) => {
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

                        if let BinaryOp::ContractApply = b_op {
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
                                fst_pos,
                            );

                            todo!("internals::naked_to_custom().into()")
                        }
                    }
                    ValueContentRef::CustomContract(ctr) => Closure {
                        value: ctr.0.clone(),
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
                            fst_pos,
                        );

                        todo!("internals::record_contract().into()")
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

                let Some(LabelBody(label)) = value2.as_label() else {
                    return mk_type_error!("Label", 2, value2);
                };

                let mut label = label.clone();

                if let Some(RecordBody(record_data)) = value1.as_record() {
                    // If the contract returned a label as part of its error
                    // data, blame that one instead.
                    if let Some(user_label) = record_data
                        .fields
                        .get(&LocIdent::from("blame_location"))
                        .and_then(|field| field.value.as_ref())
                        .and_then(NickelValue::as_label)
                    {
                        label = user_label.0.clone();
                    }

                    if let Some(msg) = record_data
                        .fields
                        .get(&LocIdent::from("message"))
                        .and_then(|field| field.value.as_ref())
                        .and_then(NickelValue::as_string)
                    {
                        label = label.with_diagnostic_message(msg.0.clone().into_inner());
                    }

                    if let Some(notes) = record_data
                        .fields
                        .get(&LocIdent::from("notes"))
                        .and_then(|field| field.value.as_ref())
                        .and_then(NickelValue::as_array)
                    {
                        let notes = notes
                            .array
                            .iter()
                            .map(|element| {
                                if let Some(s) = element.as_string() {
                                    Ok(s.0.clone().into_inner())
                                } else {
                                    mk_type_error!("String (notes)", 1, element.clone())
                                }
                            })
                            .collect::<Result<Vec<_>, _>>()?;

                        label = label.with_diagnostic_notes(notes);
                    }

                    Ok(NickelValue::label(label, pos2).into())
                } else {
                    mk_type_error!("Record", 1, value1)
                }
            }
            BinaryOp::Unseal => {
                if let Some(s1) = value1.as_sealing_key() {
                    // The last argument (lazy, on the stack) of unseal is an expression raising
                    // blame. If the keys match, we ignore the blame and thus return a function
                    // `const unsealed_term_content`. Otherwise, we return `id`.
                    //
                    // Since the stack is set up as `[.] blame_expr`, this does ignore the error
                    // and proceed with the unsealed term in the happy path, or on the opposite
                    // drop the sealed term and proceed with the error on the stack otherwise.
                    Ok(
                        if let Some(TermBody(Term::Sealed(s2, inner, _))) = value2.as_term() {
                            if s1.0 == *s2 {
                                Closure {
                                    value: mk_fun!(LocIdent::fresh(), inner.clone()),
                                    env: env2,
                                }
                            } else {
                                mk_term::id().into()
                            }
                        } else {
                            mk_term::id().into()
                        },
                    )
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

                match eq(
                    &mut self.context.cache,
                    &mut self.context.pos_table,
                    c1,
                    c2,
                    pos_op_inh,
                )? {
                    EqResult::Bool(b) => match (b, self.stack.pop_eq()) {
                        (false, _) => {
                            self.stack.clear_eqs();
                            Ok(NickelValue::bool_value(
                                false,
                                self.context.pos_table.make_inline(pos_op_inh),
                            )
                            .into())
                        }
                        (true, None) => Ok(NickelValue::bool_value(
                            true,
                            self.context.pos_table.make_inline(pos_op_inh),
                        )
                        .into()),
                        (true, Some((c1, c2))) => {
                            let v1 = c1.value.closurize(&mut self.context.cache, c1.env);
                            let v2 = c2.value.closurize(&mut self.context.cache, c2.env);

                            Ok(NickelValue::term(Term::Op2(BinaryOp::Eq, v1, v2), pos_op).into())
                        }
                    },
                    EqResult::Eqs(v1, v2, subeqs) => {
                        self.stack.push_eqs(subeqs.into_iter());

                        Ok(NickelValue::term(Term::Op2(BinaryOp::Eq, v1, v2), pos_op).into())
                    }
                }
            }
            BinaryOp::LessThan => self.binary_number_cmp(
                |n1, n2| n1 < n2,
                value1,
                pos1,
                value2,
                pos2,
                pos_op_inh,
                b_op.to_string(),
            ),
            BinaryOp::LessOrEq => self.binary_number_cmp(
                |n1, n2| n1 <= n2,
                value1,
                pos1,
                value2,
                pos2,
                pos_op_inh,
                b_op.to_string(),
            ),
            BinaryOp::GreaterThan => self.binary_number_cmp(
                |n1, n2| n1 > n2,
                value1,
                pos1,
                value2,
                pos2,
                pos_op_inh,
                b_op.to_string(),
            ),
            BinaryOp::GreaterOrEq => self.binary_number_cmp(
                |n1, n2| n1 >= n2,
                value1,
                pos1,
                value2,
                pos2,
                pos_op_inh,
                b_op.to_string(),
            ),
            BinaryOp::LabelGoField => {
                let Some(field) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let Some(label) = value2.as_label() else {
                    return mk_type_error!("Label", 2, value2);
                };

                let mut label = label.0.clone();
                label
                    .path
                    .push(ty_path::Elem::Field(field.0.clone().into_inner().into()));
                Ok(NickelValue::label(label, pos_op_inh).into())
            }
            BinaryOp::RecordGet => {
                // This error should be impossible to trigger. The parser
                // prevents a dynamic field access where the field name is not syntactically
                // a string.
                let Some(StringBody(id)) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let Some(RecordBody(record)) = value2.as_record() else {
                    // Not using mk_type_error! because of a non-uniform message
                    return Err(EvalErrorData::TypeError {
                        expected: String::from("Record"),
                        message: String::from("field access only makes sense for records"),
                        orig_pos: snd_pos,
                        term: value2,
                    });
                };

                // We have to apply potential pending contracts. Right now, this
                // means that repeated field access will re-apply the contract again
                // and again, which is not optimal. The same thing happens with array
                // contracts. There are several way to improve this, but this is left
                // as future work.
                let ident = LocIdent::from(id);
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
                            Err(EvalErrorData::IllegalPolymorphicTailAccess {
                                action: IllegalPolymorphicTailAction::FieldAccess {
                                    field: id.to_string(),
                                },
                                evaluated_arg: t.label.get_evaluated_arg(&self.context.cache),
                                label: t.label.clone(),
                            })
                        }
                        _ => Err(EvalErrorData::FieldMissing {
                            id: ident,
                            field_names: record.field_names(RecordOpKind::IgnoreEmptyOpt),
                            operator: String::from("(.$)"),
                            pos_record: pos2,
                            pos_op,
                        }),
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

                let Some(StringBody(id)) = value1.as_string() else {
                    return mk_type_error!(op_name = op_name(), "String", 1, value1);
                };

                let mut value2 = value2;

                if value2.is_empty_record() {
                    // We are going to insert in the record, so we make sure that it's an allocated
                    // block and not an inline empty record.
                    value2 = NickelValue::empty_record_block(pos2);
                }

                let ValueContentRefMut::Record(RecordBody(record)) = value2.content_make_mut()
                else {
                    return mk_type_error!(op_name = op_name(), "Record", 2, value2);
                };

                // If a defined value is expected for this field, it must be
                // provided as an additional argument, so we pop it from the stack
                let value = if let RecordExtKind::WithValue = ext_kind {
                    let (value_closure, _) =
                        self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                            EvalErrorData::NotEnoughArgs(3, String::from("insert"), pos_op)
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
                        Err(EvalErrorData::Other(
                            format!(
                                "{}: \
                                tried to extend a record with the field {id}, \
                                but it already exists",
                                op_name(),
                            ),
                            pos_op,
                        ))
                    }
                    _ => Ok(Closure {
                        // Insertion preserves the frozenness
                        value: value2,
                        env: env2,
                    }),
                }
            }
            BinaryOp::RecordRemove(op_kind) => {
                let Some(StringBody(id)) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let mut value2 = value2;

                if value2.is_empty_record() {
                    // We are going to insert in the record, so we make sure that it's an allocated
                    // block and not an inline empty record.
                    value2 = NickelValue::empty_record_block(pos2);
                }

                let ValueContentRefMut::Record(RecordBody(record)) = value2.content_make_mut()
                else {
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
                        Some(t) if t.has_dyn_field(&id) => {
                            Err(EvalErrorData::IllegalPolymorphicTailAccess {
                                action: IllegalPolymorphicTailAction::FieldRemove {
                                    field: id.to_string(),
                                },
                                evaluated_arg: t.label.get_evaluated_arg(&self.context.cache),
                                label: t.label.clone(),
                            })
                        }
                        _ => Err(EvalErrorData::FieldMissing {
                            id: id.into(),
                            field_names: record.field_names(op_kind),
                            operator: String::from("record/remove"),
                            pos_record: pos2,
                            pos_op,
                        }),
                    }
                } else {
                    Ok(Closure {
                        value: value2,
                        env: env2,
                    })
                }
            }
            BinaryOp::RecordHasField(op_kind) => {
                let Some(StringBody(id)) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let Some(RecordBody(record)) = value2.as_record() else {
                    return mk_type_error!("Record", 2, value2);
                };

                Ok(NickelValue::bool_value(matches!(
                            record.fields.get(&LocIdent::from(id.clone().into_inner())),
                            Some(field) if matches!(op_kind, RecordOpKind::ConsiderAllFields) || !field.is_empty_optional()
                        ),
                    self.context.pos_table.make_inline(pos_op_inh)
                ).into())
            }
            BinaryOp::RecordFieldIsDefined(op_kind) => {
                let Some(StringBody(id)) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let Some(RecordBody(record)) = value2.as_record() else {
                    return mk_type_error!("Record", 2, value2);
                };

                Ok(NickelValue::bool_value(
                        matches!(
                            record.fields.get(&LocIdent::from(id.clone().into_inner())),
                            Some(field @ Field { value: Some(_), ..}) if matches!(op_kind, RecordOpKind::ConsiderAllFields) || !field.is_empty_optional()
                        ),
                        self.context.pos_table.make_inline(pos_op_inh),
                ).into())
            }
            BinaryOp::ArrayConcat => {
                if value1.is_empty_array() {
                    // We are going to insert in the array, so we make sure that it's an allocated
                    // block and not an inline empty array.
                    value1 = NickelValue::empty_array_block(pos1);
                }

                let Some(array_data1) = value1.as_array() else {
                    return mk_type_error!("Array", 1, value1);
                };

                let Some(array_data2) = value2.as_array() else {
                    return mk_type_error!("Array", 2, value2);
                };

                // In all generality, we need to apply the pending contracts on both sides, as they
                // can differ. Even if some are common, the order of contracts is meaningful, so
                // deduplicating the common part is not trivial.
                //
                // Still, there's a simple common case that we can handle: if both arrays have only
                // one pending contract, and it's the same, we can keep it lazy.
                match (
                    array_data1.pending_contracts.as_slice(),
                    array_data2.pending_contracts.as_slice(),
                ) {
                    // We don't deduplicate polymorphic contracts, because
                    // they're not idempotent.
                    ([ctr1], [ctr2])
                        if !ctr1.can_have_poly_ctrs()
                            && contract_eq(&ctr1.contract, &env1, &ctr2.contract, &env2) =>
                    {
                        let result = array_data1
                            .array
                            .iter()
                            .chain(array_data2.array.iter())
                            .cloned()
                            .collect();

                        Ok(NickelValue::array(result, vec![ctr1.clone()], pos_op_inh)
                            .unwrap()
                            .into())
                    }
                    _ => {
                        // We need to collect in two phases, since the mapped closures capture
                        // `&mut self.cache`, so chaining the iterators first wouldn't work.
                        let mut result: Array = array_data1
                            .array
                            .iter()
                            .cloned()
                            .map(|elt| {
                                RuntimeContract::apply_all(
                                    elt,
                                    array_data1.pending_contracts.iter().cloned(),
                                    pos1,
                                )
                                .closurize(&mut self.context.cache, env1.clone())
                            })
                            .collect();

                        result.extend(array_data2.array.iter().cloned().map(|elt| {
                            RuntimeContract::apply_all(
                                elt,
                                array_data2.pending_contracts.iter().cloned(),
                                pos2,
                            )
                            .closurize(&mut self.context.cache, env2.clone())
                        }));

                        Ok(NickelValue::array(result, Vec::new(), pos_op_inh)
                            .unwrap()
                            .into())
                    }
                }
                //
                // let mut array1 = &array_data1.array;
                // //TODO[seen at RFC007]: this is unsound, because it might change the order in which
                // //contracts are applied (common contracts are delayed), but the order of
                // //application might be relied upon by the users.
                // //
                // // We have two sets of contracts from the LHS and RHS arrays.
                // // - Common contracts between the two sides can be put into
                // // `pending_contracts` of the resulting concatenation as they're
                // // shared by all elements: we don't have to apply them just yet.
                // // - Contracts thats are specific to the LHS or the RHS have to
                // // applied because we don't have a way of tracking which elements
                // // should take which contracts.
                //
                // // Separate contracts between the parts that aren't common, and
                // // must be applied right away, and the common part, which can be
                // // kept lazy.
                // let ctrs_left = &array_data1.pending_contracts;
                // // We use a vector of `Option` so that we can set the elements to
                // // remove to `None` and make a single pass at the end
                // // to retain the remaining ones.
                // let mut ctrs_right_sieve: Vec<_> =
                //     array_data2.pending_contracts.iter().map(Some).collect();
                // let mut ctrs_common = Vec::new();
                //
                // // We basically compute the intersection (`ctr_common`),
                // // `ctrs_left - ctr_common`, and `ctrs_right - ctr_common`.
                // let ctrs_left_dedup: Vec<_> = ctrs_left
                //     .into_iter()
                //     .filter(|ctr| {
                //         // We don't deduplicate polymorphic contracts, because
                //         // they're not idempotent.
                //         if ctr.can_have_poly_ctrs() {
                //             return true;
                //         }
                //
                //         // We check if there is a remaining contract in
                //         // `ctrs_right_sieve` which matches `ctr`: in this case,
                //         // `twin_index` will hold its index.
                //         let twin_index = ctrs_right_sieve.iter().position(|other_ctr| {
                //             other_ctr.as_ref().is_some_and(|other_ctr| {
                //                 contract_eq(&ctr.contract, &env1, &other_ctr.contract, &env2)
                //             })
                //         });
                //
                //         if let Some(index) = twin_index {
                //             // unwrap(): we know that the contract at this index is
                //             // `Some`, because all elements are initially some when
                //             // creating `ctrs_right_sieve` and then we don't
                //             // consider `None` values when computing a new `index`
                //             // in the `position` above.
                //             let common = ctrs_right_sieve[index].take().unwrap().clone();
                //             ctrs_common.push(common);
                //             false
                //         } else {
                //             true
                //         }
                //     })
                //     .collect();
                //
                // let ctrs_right_empty = ctrs_right_sieve.iter().all(Option::is_none);
                // let ctrs_left_empty = ctrs_left_dedup.is_empty();
                // let ctrs_right_dedup = ctrs_right_sieve.iter().flatten().map(|ctr| (*ctr).clone());
                //
                // let result = if ctrs_right_empty && ctrs_left_empty {
                //     array1.extend(array_data2.array.iter().cloned());
                //
                //     array1
                // } else if ctrs_left_empty {
                //     array1.extend(array_data2.array.iter().cloned().map(|t| {
                //         RuntimeContract::apply_all(t, ctrs_right_dedup.clone(), pos1)
                //             .closurize(&mut self.cache, env1.clone())
                //     }));
                //
                //     array1
                // } else {
                //     let mut array = Array::default();
                //
                //     array.extend(array1.iter().map(|t| {
                //         RuntimeContract::apply_all(t.clone(), ctrs_left_dedup.iter().cloned(), pos1)
                //             .closurize(&mut self.cache, env1.clone())
                //     }));
                //
                //     array.extend(array_data2.array.iter().map(|t| {
                //         RuntimeContract::apply_all(t.clone(), ctrs_right_dedup.clone(), pos2)
                //             .closurize(&mut self.cache, env2.clone())
                //     }));
                //
                //     array
                // };
                //
                // // unwrap(): will go away soon
                // Ok(NickelValue::array(result, ctrs_common, pos_op_inh)
                //     .unwrap()
                //     .into())
            }
            BinaryOp::ArrayAt => {
                let Some(array_data) = value1.as_array() else {
                    return mk_type_error!("Array", 1, value1);
                };

                let Some(NumberBody(n)) = value2.as_number() else {
                    return mk_type_error!("Number", 2, value2);
                };

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

                if n_as_usize >= array_data.array.len() {
                    return Err(EvalErrorData::Other(
                        format!(
                            "array/at: index out of bounds. \
                                Expected an index between 0 and {}, got {}",
                            array_data.array.len(),
                            n
                        ),
                        pos_op,
                    ));
                }

                let elem_with_ctr = RuntimeContract::apply_all(
                    array_data.array.get(n_as_usize).unwrap().clone(),
                    array_data.pending_contracts.iter().cloned(),
                    pos1.to_inherited_block(&mut self.context.pos_table),
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

                let Some(StringBody(s)) = value2.as_string() else {
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

                let Some(StringBody(s)) = value1.as_string() else {
                    return mk_type_error!("String", 2, value2);
                };

                let deser: NickelValue = match enum_data.tag.label() {
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
                    "Yaml" => crate::serialize::yaml::load_yaml_value(
                        &mut self.context.pos_table,
                        s,
                        None,
                    )
                    .map_err(|err| {
                        EvalErrorData::DeserializationErrorWithInner {
                            format: InputFormat::Yaml,
                            inner: err,
                            pos: pos_op,
                        }
                    })?,
                    "Toml" => toml::from_str(s).map_err(|err| {
                        EvalErrorData::DeserializationError(
                            String::from("toml"),
                            format!("{err}"),
                            pos_op,
                        )
                    })?,
                    _ => return mk_err_fst(),
                };

                Ok(deser
                    .with_pos_idx(&mut self.context.pos_table, pos_op_inh)
                    .into())
            }
            BinaryOp::StringSplit => self.binary_string_fn(
                |input, sep| {
                    //unwrap(): will go away soon
                    NickelValue::array(input.split(sep), Vec::new(), pos_op_inh).unwrap()
                },
                value1,
                value2,
                pos_op_inh,
                b_op.to_string(),
            ),
            BinaryOp::StringContains => {
                let pos_op_inline = self.context.pos_table.make_inline(pos_op_inh);

                self.binary_string_fn(
                    |s1, s2| NickelValue::bool_value(s1.contains(s2.as_str()), pos_op_inline),
                    value1,
                    value2,
                    pos_op_inh,
                    b_op.to_string(),
                )
            }
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
                    value1,
                    value2,
                    pos_op_inh,
                    b_op.to_string(),
                )
            }
            BinaryOp::ContractArrayLazyApp => {
                let (ctr, _) = self.stack.pop_arg(&self.context.cache).ok_or_else(|| {
                    EvalErrorData::NotEnoughArgs(3, String::from("contract/array_lazy_app"), pos_op)
                })?;

                let Closure {
                    value: ctr_val,
                    env: env_ctr,
                } = ctr;

                // FIXME: use match?
                let Some(LabelBody(label)) = value1.as_label() else {
                    return mk_type_error!("Label", 1, value1);
                };

                if value2.is_empty_array() {
                    // We are going to insert in the array, so we make sure that it's an allocated
                    // block and not an inline empty array.
                    value2 = NickelValue::empty_array_block(pos2);
                }

                let ValueContentRefMut::Array(array_data) = value2.content_make_mut() else {
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
                    EvalErrorData::NotEnoughArgs(
                        3,
                        String::from("contract/record_lazy_app"),
                        pos_op,
                    )
                })?;

                let Some(LabelBody(label)) = value1.as_label() else {
                    return mk_type_error!("Label", 1, value1);
                };

                let Some(RecordBody(record_data)) = value2.as_record() else {
                    return mk_type_error!("Record", 2, value2);
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
                        NickelValue::string(id, self.context.pos_table.push_block(id.pos))
                    )
                    .with_pos_idx(&mut self.context.pos_table, pos)
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
                let Some(StringBody(message)) = value1.as_string() else {
                    return mk_type_error!("String", 1, value1);
                };

                let ValueContentRefMut::Label(LabelBody(label)) = value2.content_make_mut() else {
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

                let ValueContentRefMut::Label(LabelBody(label)) = value2.content_make_mut() else {
                    return mk_type_error!("Label", 2, value2);
                };

                let notes = array_data
                    .array
                    .iter()
                    .map(|element| {
                        if let Some(StringBody(s)) = element.as_string() {
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

                let ValueContentRefMut::Label(LabelBody(label)) = value2.content_make_mut() else {
                    return mk_type_error!("Label", 2, value2);
                };

                label.append_diagnostic_note(&note.0);
                Ok(value2.into())
            }
            BinaryOp::LabelLookupTypeVar => {
                let Some(key) = value1.as_sealing_key() else {
                    return mk_type_error!("SealingKey", 1, value1);
                };

                let Some(LabelBody(label)) = value2.as_label() else {
                    return mk_type_error!("Label", 2, value2);
                };

                Ok(
                    NickelValue::from(label.type_environment.get(&key.0).unwrap())
                        .with_pos_idx(&mut self.context.pos_table, pos_op_inh)
                        .into(),
                )
            }
            BinaryOp::RecordSplitPair => {
                // We could take the record out as owned value, but it's not clear what we gain:
                // most of the content is `NickelValue`s, which are cheap to clone.
                let Some(RecordBody(record1)) = value1.as_record() else {
                    return mk_type_error!("Record", 1, value1);
                };

                let Some(RecordBody(record2)) = value2.as_record() else {
                    return mk_type_error!("Record", 2, value2);
                };

                let split::SplitResult {
                    left,
                    center,
                    right,
                } = split::split(record1.fields.clone(), record2.fields.clone());

                let left_only = NickelValue::record_posless(RecordData {
                    fields: left,
                    sealed_tail: record1.sealed_tail.clone(),
                    attrs: record1.attrs,
                });

                let right_only = NickelValue::record_posless(RecordData {
                    fields: right,
                    sealed_tail: record2.sealed_tail.clone(),
                    attrs: record2.attrs,
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

                Ok(NickelValue::record_force_pos(
                    &mut self.context.pos_table,
                    RecordData {
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
                        attrs: RecordAttrs::default(),
                        sealed_tail: None,
                    },
                    pos_op_inh,
                )
                .into())
            }
            BinaryOp::RecordDisjointMerge => {
                let ValueContentRefMut::Record(RecordBody(record1)) = value1.content_make_mut()
                else {
                    return mk_type_error!("Record", 1, value1);
                };

                let Some(RecordBody(record2)) = value2.as_record() else {
                    return mk_type_error!("Record", 2, value2);
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
                let sealed_tail = match (record1.sealed_tail.clone(), record2.sealed_tail.clone()) {
                    (Some(record::SealedTail { label, .. }), Some(_)) => {
                        return Err(EvalErrorData::IllegalPolymorphicTailAccess {
                            action: IllegalPolymorphicTailAction::Merge,
                            evaluated_arg: label.get_evaluated_arg(&self.context.cache),
                            label,
                        });
                    }
                    (tail1, tail2) => tail1.or(tail2),
                };

                // Note that because of record closurization, we assume here that the record data
                // of each record are already closurized, so we don't really care about
                // environments. Should that invariant change, we might get into trouble (trouble
                // meaning undue `UnboundIdentifier` errors).
                record1.fields.extend(record2.fields.clone());
                record1.attrs = Combine::combine(record1.attrs, record2.attrs);
                record1.sealed_tail = sealed_tail;

                Ok(value1
                    .with_pos_idx(&mut self.context.pos_table, pos_op_inh)
                    .into())
            }
        }
    }

    fn binary_number_cmp<Op>(
        &mut self,
        op: Op,
        value1: NickelValue,
        pos1: PosIdx,
        value2: NickelValue,
        pos2: PosIdx,
        pos_op: PosIdx,
        op_name: String,
    ) -> Result<Closure, EvalErrorData>
    where
        Op: Fn(&Number, &Number) -> bool,
    {
        let pos_op_inh = pos_op.to_inherited_inline(&mut self.context.pos_table);

        self.binary_number_fn(
            |n1, n2| NickelValue::bool_value(op(n1, n2), pos_op_inh),
            value1,
            pos1,
            value2,
            pos2,
            pos_op,
            op_name,
        )
    }

    fn binary_number_op<Op>(
        &mut self,
        op: Op,
        value1: NickelValue,
        pos1: PosIdx,
        value2: NickelValue,
        pos2: PosIdx,
        pos_op: PosIdx,
        op_name: String,
    ) -> Result<Closure, EvalErrorData>
    where
        Op: Fn(&Number, &Number) -> Number,
    {
        let pos_op_inh = pos_op.to_inherited_block(&mut self.context.pos_table);

        self.binary_number_fn(
            |n1, n2| NickelValue::number(op(n1, n2), pos_op_inh),
            value1,
            pos1,
            value2,
            pos2,
            pos_op,
            op_name,
        )
    }

    fn binary_number_fn<F>(
        &mut self,
        f: F,
        value1: NickelValue,
        pos1: PosIdx,
        value2: NickelValue,
        pos2: PosIdx,
        pos_op: PosIdx,
        op_name: String,
    ) -> Result<Closure, EvalErrorData>
    where
        F: Fn(&Number, &Number) -> NickelValue,
    {
        let Some(n1) = value1.as_number() else {
            return Err(EvalErrorData::NAryPrimopTypeError {
                primop: op_name,
                expected: "Number".to_owned(),
                arg_number: 1,
                pos_arg: pos1,
                arg_evaluated: value1,
                pos_op,
            });
        };

        let Some(n2) = value2.as_number() else {
            return Err(EvalErrorData::NAryPrimopTypeError {
                primop: op_name,
                expected: "Number".to_owned(),
                arg_number: 2,
                pos_arg: pos2,
                arg_evaluated: value2,
                pos_op,
            });
        };

        let n1 = &n1.0;
        let n2 = &n2.0;

        Ok(f(n1, n2).into())
    }

    fn binary_string_fn<F>(
        &mut self,
        f: F,
        value1: NickelValue,
        value2: NickelValue,
        pos_op: PosIdx,
        op_name: String,
    ) -> Result<Closure, EvalErrorData>
    where
        F: Fn(&NickelString, &NickelString) -> NickelValue,
    {
        let Some(StringBody(s1)) = value1.as_string() else {
            return Err(EvalErrorData::NAryPrimopTypeError {
                primop: op_name,
                expected: "String".to_owned(),
                arg_number: 1,
                pos_arg: value1.pos_idx(),
                arg_evaluated: value1,
                pos_op,
            });
        };

        let Some(StringBody(s2)) = value2.as_string() else {
            return Err(EvalErrorData::NAryPrimopTypeError {
                primop: op_name,
                expected: "String".to_owned(),
                arg_number: 2,
                pos_arg: value2.pos_idx(),
                arg_evaluated: value2,
                pos_op,
            });
        };

        Ok(f(s1, s2).into())
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
    ) -> Result<Closure, EvalErrorData> {
        increment!(format!("primop:{n_op}"));

        let pos_op_inh = pos_op.to_inherited_block(&mut self.context.pos_table);

        let mk_type_error =
            |expected: &str, arg_number: usize, pos_arg: PosIdx, arg_evaluated: NickelValue| {
                Err(EvalErrorData::NAryPrimopTypeError {
                    primop: n_op.to_string(),
                    expected: expected.to_owned(),
                    arg_number,
                    pos_arg,
                    arg_evaluated: arg_evaluated,
                    pos_op,
                })
            };

        // Currently, for fixed arity primitive operators, the parser must ensure that they get
        // exactly the right number of argument: if it is not the case, this is a bug, and we panic.
        match n_op {
            NAryOp::StringReplace | NAryOp::StringReplaceRegex => {
                let mut args_wo_env = args.into_iter().map(|(arg, pos)| (arg.value, pos));
                let (arg1, arg_pos1) = args_wo_env.next().unwrap();
                let (arg2, arg_pos2) = args_wo_env.next().unwrap();
                let (arg3, arg_pos3) = args_wo_env.next().unwrap();
                debug_assert!(args_wo_env.next().is_none());

                let Some(StringBody(s)) = arg1.as_string() else {
                    return mk_type_error("String", 1, arg_pos1, arg1);
                };

                let Some(StringBody(from)) = arg2.as_string() else {
                    return mk_type_error("String", 2, arg_pos2, arg2);
                };

                let Some(StringBody(to)) = arg3.as_string() else {
                    return mk_type_error("String", 3, arg_pos3, arg3);
                };

                let result = if let NAryOp::StringReplace = n_op {
                    s.replace(from.as_str(), to.as_str())
                } else {
                    let re = regex::Regex::new(from)
                        .map_err(|err| EvalErrorData::Other(err.to_string(), pos_op))?;

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

                let Some(StringBody(s)) = arg1.as_string() else {
                    return mk_type_error("String", 1, arg_pos1, arg1);
                };

                let Some(NumberBody(start)) = arg2.as_number() else {
                    return mk_type_error("Number", 2, arg_pos2, arg2);
                };

                let Some(NumberBody(end)) = arg3.as_number() else {
                    return mk_type_error("Number", 3, arg_pos3, arg3);
                };

                s.substring(start, end)
                    .map(|substr| NickelValue::string(substr, pos_op_inh).into())
                    .map_err(|e| EvalErrorData::Other(format!("{e}"), pos_op))
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

                let Some(LabelBody(label)) = arg1.as_label() else {
                    return Err(EvalErrorData::InternalError(
                        format!(
                            "The {n_op} operator was expecting \
                                a first argument of type Label, got {}",
                            arg1.type_of().unwrap_or("<unevaluated>")
                        ),
                        pos_op,
                    ));
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

                let Some(SealingKeyBody(s)) = arg1.as_sealing_key() else {
                    return mk_type_error("SealingKey", 1, arg_pos1, arg1);
                };

                let Some(LabelBody(label)) = arg2.as_label() else {
                    return mk_type_error("Label", 2, arg_pos2, arg2);
                };

                if arg3.is_empty_record() {
                    // We are going to insert in the record, so we make sure that it's an allocated
                    // block and not an inline empty record.
                    arg3 = NickelValue::empty_record_block(arg3.pos_idx());
                }

                let ValueContentRefMut::Record(RecordBody(r)) = arg3.content_make_mut() else {
                    return mk_type_error("Record", 3, arg_pos3, arg3);
                };

                let Some(RecordBody(tail)) = arg4.as_record() else {
                    return mk_type_error("Record", 4, arg_pos4, arg4);
                };

                let tail_closurized = NickelValue::record_posless(tail.clone())
                    .closurize(&mut self.context.cache, env4);
                let fields = tail.fields.keys().map(|s| s.ident()).collect();
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

                let Some(SealingKeyBody(s)) = arg1.as_sealing_key() else {
                    return mk_type_error("SealingKey", 1, arg_pos1, arg1);
                };

                let Some(LabelBody(label)) = arg2.as_label() else {
                    return mk_type_error("Label", 2, arg_pos2, arg2);
                };

                let Some(RecordBody(r)) = arg3.as_record() else {
                    return mk_type_error("Record", 3, arg_pos3, arg3);
                };

                r.sealed_tail
                    .as_ref()
                    .and_then(|tail| tail.unseal(s).cloned())
                    .ok_or_else(|| EvalErrorData::BlameError {
                        evaluated_arg: label.get_evaluated_arg(&self.context.cache),
                        label: label.clone(),
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

                let Some(SealingKeyBody(key)) = arg1.as_sealing_key() else {
                    return mk_type_error("SealingKey", 1, arg_pos1, arg1);
                };

                let Ok(polarity) = Polarity::try_from(&arg2) else {
                    return mk_type_error("Polarity", 2, arg_pos2, arg2);
                };

                let ValueContentRefMut::Label(LabelBody(label)) = arg3.content_make_mut() else {
                    return mk_type_error("Label", 3, arg_pos3, arg3);
                };

                label
                    .type_environment
                    .insert(*key, TypeVarData { polarity });

                Ok(arg3
                    .try_with_pos_idx(arg_pos3.to_inherited_block(&mut self.context.pos_table))
                    // unwrap(): arg3 is a label, which is a value block, so `try_with_pos_idx`
                    // cannot fail
                    .unwrap()
                    .into())
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

                let Some(NumberBody(start)) = arg1.as_number() else {
                    return mk_type_error("Number", 1, arg_pos1, arg1);
                };

                let Some(NumberBody(end)) = arg2.as_number() else {
                    return mk_type_error("Number", 2, arg_pos2, arg2);
                };

                if arg3.is_empty_array() {
                    // We are going to insert in the array, so we make sure that it's an allocated
                    // block and not an inline empty array.
                    arg3 = NickelValue::empty_array_block(arg3.pos_idx());
                }

                let ValueContentRefMut::Array(ArrayBody { array, .. }) = arg3.content_make_mut()
                else {
                    return mk_type_error("Array", 3, arg_pos3, arg3);
                };

                let Ok(start_as_usize) = usize::try_from(start) else {
                    return Err(EvalErrorData::Other(
                        format!(
                            "{n_op} expects its first argument (start) to be a \
                            positive integer smaller than {}, got {start}",
                            usize::MAX
                        ),
                        pos_op,
                    ));
                };

                let Ok(end_as_usize) = usize::try_from(end) else {
                    return Err(EvalErrorData::Other(
                        format!(
                            "{n_op} expects its second argument (end) to be a \
                            positive integer smaller than {}, got {end}",
                            usize::MAX
                        ),
                        pos_op,
                    ));
                };

                if end_as_usize < start_as_usize || end_as_usize > array.len() {
                    return Err(EvalErrorData::Other(
                        format!(
                            "{n_op}: index out of bounds. Expected `start <= end <= {}`, but \
                            got `start={start}` and `end={end}`.",
                            array.len()
                        ),
                        pos_op,
                    ));
                }

                array.slice(start_as_usize, end_as_usize);

                Ok(Closure {
                    value: arg3.with_pos_idx(&mut self.context.pos_table, pos_op_inh),
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
    pos_table: &mut PosTable,
    c1: Closure,
    c2: Closure,
    pos_op: PosIdx,
) -> Result<EqResult, EvalErrorData> {
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
        (ValueContentRef::Inline(inl_v1), ValueContentRef::Inline(inl_v2)) => {
            Ok(EqResult::Bool(inl_v1 == inl_v2))
        }
        (ValueContentRef::Number(NumberBody(n1)), ValueContentRef::Number(NumberBody(n2))) => {
            Ok(EqResult::Bool(n1 == n2))
        }
        (ValueContentRef::String(StringBody(s1)), ValueContentRef::String(StringBody(s2))) => {
            Ok(EqResult::Bool(s1 == s2))
        }
        (ValueContentRef::Label(LabelBody(l1)), ValueContentRef::Label(LabelBody(l2))) => {
            Ok(EqResult::Bool(l1 == l2))
        }
        (
            ValueContentRef::SealingKey(SealingKeyBody(k1)),
            ValueContentRef::SealingKey(SealingKeyBody(k2)),
        ) => Ok(EqResult::Bool(k1 == k2)),
        (
            ValueContentRef::EnumVariant(EnumVariantBody {
                tag: tag1,
                arg: None,
            }),
            ValueContentRef::EnumVariant(EnumVariantBody {
                tag: tag2,
                arg: None,
            }),
        ) => Ok(EqResult::Bool(tag1.ident() == tag2.ident())),
        (
            ValueContentRef::EnumVariant(EnumVariantBody {
                tag: tag1,
                arg: Some(arg1),
            }),
            ValueContentRef::EnumVariant(EnumVariantBody {
                tag: tag2,
                arg: Some(arg2),
            }),
        ) if tag1.ident() == tag2.ident() => Ok(gen_eqs(
            cache,
            std::iter::once((arg1.clone(), arg2.clone())),
            env1,
            env2,
        )),
        (ValueContentRef::Record(RecordBody(r1)), ValueContentRef::Record(RecordBody(r2))) => {
            let merge::split::SplitResult {
                left,
                center,
                right,
            } = merge::split::split(r1.fields.clone(), r2.fields.clone());

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
        (ValueContentRef::Array(array_data1), ValueContentRef::Array(array_data2))
            if array_data1.array.len() == array_data2.array.len() =>
        {
            // Equalities are tested in reverse order, but that shouldn't matter. If it
            // does, just do `eqs.rev()`

            // We should apply all contracts here, otherwise we risk having wrong values, think
            // record contracts with default values, wrapped terms, etc.

            let mut eqs = array_data1
                .array
                .iter()
                .cloned()
                .map(|elt| {
                    let pos = elt.pos_idx().to_inherited_block(pos_table);
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
                    let pos = elt.pos_idx().to_inherited_block(pos_table);
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
            ValueContentRef::Term(TermBody(Term::Fun(..) | Term::Match(_) | Term::FunPattern(..))),
            ValueContentRef::Term(TermBody(Term::Fun(..) | Term::Match(_) | Term::FunPattern(..))),
        ) => Err(EvalErrorData::IncomparableValues {
            eq_pos: pos_op,
            left: value1,
            right: value2,
        }),
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

    Term::Fun(
        param,
        NickelValue::term(
            Term::Op1(op, NickelValue::term(Term::Var(param), pos_op)),
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
