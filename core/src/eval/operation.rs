//! Implementation of primitive operations.
//!
//! Define functions which perform the evaluation of primitive operators. The machinery required
//! for the strict evaluation of the operands is mainly handled by [crate::eval], and marginally in
//! [`VirtualMachine::continuate_operation`].
//!
//! On the other hand, the functions `process_unary_operation` and `process_binary_operation`
//! receive evaluated operands and implement the actual semantics of operators.
use super::{
    cache::lazy::Thunk,
    merge::{self, MergeMode},
    stack::StrAccData,
    subst, Cache, Closure, Environment, ImportResolver, VirtualMachine,
};

#[cfg(feature = "nix-experimental")]
use crate::nix_ffi;

use crate::{
    closurize::Closurize,
    error::{EvalError, IllegalPolymorphicTailAction},
    identifier::LocIdent,
    label::{ty_path, Polarity, TypeVarData},
    match_sharedterm, mk_app, mk_fun, mk_opn, mk_record,
    parser::utils::parse_number_sci,
    position::TermPos,
    serialize,
    serialize::ExportFormat,
    stdlib::internals,
    term::{
        array::{Array, ArrayAttrs, OutOfBoundError},
        make as mk_term,
        record::{self, Field, FieldMetadata, RecordData},
        string::NickelString,
        *,
    },
    typecheck::eq::contract_eq,
};

use malachite::{
    num::{
        arithmetic::traits::Pow,
        basic::traits::Zero,
        conversion::traits::{RoundingFrom, ToSci},
    },
    rounding_modes::RoundingMode,
    Integer,
};

use md5::digest::Digest;
use simple_counter::*;
use unicode_segmentation::UnicodeSegmentation;

use std::{convert::TryFrom, iter::Extend, rc::Rc};

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
    Eqs(RichTerm, RichTerm, Vec<(Closure, Closure)>),
}

/// An operation continuation as stored on the stack.
#[derive(PartialEq, Clone)]
pub enum OperationCont {
    Op1(
        /* unary operation */ UnaryOp,
        /* original position of the argument before evaluation */ TermPos,
    ),
    // The last parameter saves the strictness mode before the evaluation of the operator
    Op2First(
        /* the binary operation */ BinaryOp,
        /* second argument, to evaluate next */ Closure,
        /* original position of the first argument */ TermPos,
    ),
    Op2Second(
        /* binary operation */ BinaryOp,
        /* first argument, evaluated */ Closure,
        /* original position of the first argument before evaluation */ TermPos,
        /* original position of the second argument before evaluation */ TermPos,
    ),
    OpN {
        op: NAryOp,                         /* the n-ary operation */
        evaluated: Vec<(Closure, TermPos)>, /* evaluated arguments and their original position */
        current_pos: TermPos, /* original position of the argument being currently evaluated */
        pending: Vec<Closure>, /* a stack (meaning the order of arguments is to be reversed)
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

impl<R: ImportResolver, C: Cache> VirtualMachine<R, C> {
    /// Process to the next step of the evaluation of an operation.
    ///
    /// Depending on the content of the stack, it either starts the evaluation of the first
    /// argument, starts the evaluation of the second argument, or finally process with the
    /// operation if both arguments are evaluated (for binary operators).
    pub fn continuate_operation(&mut self, mut clos: Closure) -> Result<Closure, EvalError> {
        let (cont, cs_len, pos) = self.stack.pop_op_cont().expect("Condition already checked");
        self.call_stack.truncate(cs_len);
        match cont {
            OperationCont::Op1(u_op, arg_pos) => {
                self.process_unary_operation(u_op, clos, arg_pos, pos)
            }
            OperationCont::Op2First(b_op, mut snd_clos, fst_pos) => {
                std::mem::swap(&mut clos, &mut snd_clos);
                self.stack.push_op_cont(
                    OperationCont::Op2Second(b_op, snd_clos, fst_pos, clos.body.pos),
                    cs_len,
                    pos,
                );
                Ok(clos)
            }
            OperationCont::Op2Second(b_op, fst_clos, fst_pos, snd_pos) => {
                self.process_binary_operation(b_op, fst_clos, fst_pos, clos, snd_pos, pos)
            }
            OperationCont::OpN {
                op,
                mut evaluated,
                current_pos,
                mut pending,
            } => {
                evaluated.push((clos, current_pos));

                if let Some(next) = pending.pop() {
                    let current_pos = next.body.pos;
                    self.stack.push_op_cont(
                        OperationCont::OpN {
                            op,
                            evaluated,
                            current_pos,
                            pending,
                        },
                        cs_len,
                        pos,
                    );

                    Ok(next)
                } else {
                    self.process_nary_operation(op, evaluated, pos)
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
        arg_pos: TermPos,
        pos_op: TermPos,
    ) -> Result<Closure, EvalError> {
        let Closure {
            body: RichTerm { term: t, pos },
            env,
        } = clos;
        let pos_op_inh = pos_op.into_inherited();

        macro_rules! mk_type_error {
            ($primop:expr, $expected:expr) => {
                EvalError::UnaryPrimopTypeError {
                    primop: String::from($primop),
                    expected: String::from($expected),
                    arg_pos,
                    arg_evaluated: RichTerm { term: t, pos },
                }
            };
            ($primop:expr, $expected:expr, $arg_number:expr) => {
                EvalError::NAryPrimopTypeError {
                    primop: String::from($primop),
                    expected: String::from($expected),
                    arg_number: $arg_number,
                    arg_pos,
                    arg_evaluated: RichTerm { term: t, pos },
                }
            };
        }

        match u_op {
            UnaryOp::IfThenElse => {
                if let Term::Bool(b) = *t {
                    if self.stack.count_args() >= 2 {
                        let (fst, ..) = self
                            .stack
                            .pop_arg(&self.cache)
                            .expect("Condition already checked.");
                        let (snd, ..) = self
                            .stack
                            .pop_arg(&self.cache)
                            .expect("Condition already checked.");

                        Ok(if b { fst } else { snd })
                    } else {
                        panic!("An If-Then-Else wasn't saturated")
                    }
                } else {
                    // Not using mk_type_error! because of a non-uniform message
                    Err(EvalError::TypeError(
                        String::from("Bool"),
                        String::from("the condition in an if expression must have type Bool"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::Typeof => {
                let result = match *t {
                    Term::Num(_) => "Number",
                    Term::Bool(_) => "Bool",
                    Term::Str(_) => "String",
                    Term::Enum(_) | Term::EnumVariant { .. } => "Enum",
                    Term::Fun(..) | Term::Match { .. } => "Function",
                    Term::Array(..) => "Array",
                    Term::Record(..) | Term::RecRecord(..) => "Record",
                    Term::Lbl(..) => "Label",
                    Term::ForeignId(_) => "ForeignId",
                    _ => "Other",
                };
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Enum(LocIdent::from(result)),
                    pos_op_inh,
                )))
            }
            UnaryOp::BoolAnd =>
            // The syntax should not allow partially applied boolean operators.
            {
                if let Some((next, ..)) = self.stack.pop_arg(&self.cache) {
                    match &*t {
                        Term::Bool(true) => Ok(next),
                        // FIXME: this does not check that the second argument is actually a
                        // boolean. This means `true && 2` silently evaluates to `2`. This is
                        // simpler and more efficient, but can make debugging harder. In any case,
                        // it should be solved only once primary operators have better support for
                        // laziness in some arguments.
                        Term::Bool(false) => Ok(Closure::atomic_closure(RichTerm {
                            term: t,
                            pos: pos_op_inh,
                        })),
                        _ => Err(mk_type_error!("(&&)", "Bool", 1)),
                    }
                } else {
                    Err(EvalError::NotEnoughArgs(2, String::from("&&"), pos_op))
                }
            }
            UnaryOp::BoolOr => {
                if let Some((next, ..)) = self.stack.pop_arg(&self.cache) {
                    match &*t {
                        Term::Bool(true) => Ok(Closure::atomic_closure(RichTerm {
                            term: t,
                            pos: pos_op_inh,
                        })),
                        // FIXME: this does not check that the second argument is actually a
                        // boolean. This means `false || 2` silently evaluates to `2`. This is
                        // simpler and more efficient, but can make debugging harder. In any case,
                        // it should be solved only once primary operators have better support for
                        // laziness in some arguments.
                        Term::Bool(false) => Ok(next),
                        _ => Err(mk_type_error!("(||)", "Bool", 1)),
                    }
                } else {
                    Err(EvalError::NotEnoughArgs(2, String::from("||"), pos_op))
                }
            }
            UnaryOp::BoolNot => {
                if let Term::Bool(b) = *t {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Bool(!b),
                        pos_op_inh,
                    )))
                } else {
                    Err(mk_type_error!("unary negation", "Bool"))
                }
            }
            UnaryOp::Blame => match_sharedterm!(match (t) {
                Term::Lbl(label) => Err(EvalError::BlameError {
                    evaluated_arg: label.get_evaluated_arg(&self.cache),
                    label,
                    call_stack: std::mem::take(&mut self.call_stack),
                }),
                _ => Err(mk_type_error!("blame", "Label")),
            }),
            UnaryOp::EnumEmbed(_id) => {
                if let Term::Enum(_) = &*t {
                    Ok(Closure::atomic_closure(RichTerm {
                        term: t,
                        pos: pos_op_inh,
                    }))
                } else {
                    Err(mk_type_error!("embed", "Enum"))
                }
            }
            UnaryOp::TagsOnlyMatch { has_default } => {
                let (cases_closure, ..) = self
                    .stack
                    .pop_arg(&self.cache)
                    .expect("missing arg for match");

                let default = if has_default {
                    Some(
                        self.stack
                            .pop_arg(&self.cache)
                            .map(|(clos, ..)| clos)
                            .expect("missing default case for match"),
                    )
                } else {
                    None
                };

                if let Term::Enum(en) = &*t {
                    let Closure {
                        body:
                            RichTerm {
                                term: cases_term, ..
                            },
                        env: cases_env,
                    } = cases_closure;

                    let mut cases = match cases_term.into_owned() {
                        Term::Record(r) => r.fields,
                        _ => panic!("invalid argument for %match%"),
                    };

                    cases
                        .remove(en)
                        .map(|field| Closure {
                            // The record containing the match cases, as well as the match primop
                            // itself, aren't accessible in the surface language. They are
                            // generated by the interpreter, and should never contain field without
                            // definition.
                            body: field.value.expect("%match% cases must have a definition"),
                            env: cases_env,
                        })
                        .or(default)
                        .ok_or_else(|| EvalError::NonExhaustiveEnumMatch {
                            expected: cases.keys().copied().collect(),
                            found: RichTerm::new(Term::Enum(*en), pos),
                            pos: pos_op_inh,
                        })
                } else if let Some(clos) = default {
                    Ok(clos)
                } else {
                    Err(mk_type_error!("match", "Enum", 2))
                }
            }
            UnaryOp::LabelFlipPol => match_sharedterm!(match (t) {
                Term::Lbl(l) => {
                    let mut l = l;
                    l.polarity = l.polarity.flip();
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Lbl(l),
                        pos_op_inh,
                    )))
                }
                _ => Err(mk_type_error!("chng_pol", "Label")),
            }),
            UnaryOp::LabelPol => {
                if let Term::Lbl(l) = &*t {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        l.polarity.into(),
                        pos_op_inh,
                    )))
                } else {
                    Err(mk_type_error!("polarity", "Label"))
                }
            }
            UnaryOp::LabelGoDom => match_sharedterm!(match (t) {
                Term::Lbl(l) => {
                    let mut l = l;
                    l.path.push(ty_path::Elem::Domain);
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Lbl(l),
                        pos_op_inh,
                    )))
                }
                _ => Err(mk_type_error!("go_dom", "Label")),
            }),
            UnaryOp::LabelGoCodom => match_sharedterm!(match (t) {
                Term::Lbl(l) => {
                    let mut l = l;
                    l.path.push(ty_path::Elem::Codomain);
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Lbl(l),
                        pos_op_inh,
                    )))
                }
                _ => Err(mk_type_error!("go_codom", "Label")),
            }),
            UnaryOp::LabelGoArray => match_sharedterm!(match (t) {
                Term::Lbl(l) => {
                    let mut l = l;
                    l.path.push(ty_path::Elem::Array);
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Lbl(l),
                        pos_op_inh,
                    )))
                }
                _ => Err(mk_type_error!("go_array", "Label")),
            }),
            UnaryOp::LabelGoDict => match_sharedterm!(match (t) {
                Term::Lbl(l) => {
                    let mut l = l;
                    l.path.push(ty_path::Elem::Dict);
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Lbl(l),
                        pos_op_inh,
                    )))
                }
                _ => Err(mk_type_error!("go_dict", "Label")),
            }),
            UnaryOp::RecordAccess(id) => {
                if let Term::Record(record) = &*t {
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
                            self.call_stack.enter_field(id, pos, value.pos, pos_op);
                            Ok(Closure { body: value, env })
                        }
                        None => match record.sealed_tail.as_ref() {
                            Some(t) if t.has_field(&id.ident()) => {
                                Err(EvalError::IllegalPolymorphicTailAccess {
                                    action: IllegalPolymorphicTailAction::FieldAccess {
                                        field: id.to_string(),
                                    },
                                    evaluated_arg: t.label.get_evaluated_arg(&self.cache),
                                    label: t.label.clone(),
                                    call_stack: std::mem::take(&mut self.call_stack),
                                })
                            }
                            _ => Err(EvalError::FieldMissing {
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
                    Err(EvalError::TypeError(
                        String::from("Record"),
                        String::from("field access only makes sense for records"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::RecordFields(op_kind) => match_sharedterm!(match (t) {
                Term::Record(record) => {
                    let fields_as_terms: Array = record
                        .field_names(op_kind)
                        .into_iter()
                        .map(mk_term::string)
                        .collect();

                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Array(fields_as_terms, ArrayAttrs::new().closurized()),
                        pos_op_inh,
                    )))
                }
                _ => Err(mk_type_error!("fields", "Record")),
            }),
            UnaryOp::RecordValues => match_sharedterm!(match (t) {
                Term::Record(record) => {
                    let mut values = record
                        .into_iter_without_opts()
                        .collect::<Result<Vec<_>, _>>()
                        .map_err(|missing_def_err| missing_def_err.into_eval_err(pos, pos_op))?;

                    values.sort_by_key(|(id, _)| *id);
                    let terms = values.into_iter().map(|(_, value)| value).collect();

                    Ok(Closure {
                        // TODO: once sure that the Record is properly closurized, we can safely
                        // assume that the extracted array here is, in turn, also closuried.
                        body: RichTerm::new(Term::Array(terms, ArrayAttrs::default()), pos_op_inh),
                        env,
                    })
                }
                _ => Err(mk_type_error!("values", "Record")),
            }),
            UnaryOp::ArrayMap => {
                let (f, ..) = self
                    .stack
                    .pop_arg(&self.cache)
                    .ok_or_else(|| EvalError::NotEnoughArgs(2, String::from("map"), pos_op))?;
                match_sharedterm!(match (t) {
                    Term::Array(ts, attrs) => {
                        let f_as_var = f.body.closurize(&mut self.cache, f.env);

                        // Array elements are closurized to preserve laziness of data
                        // structures. It maintains the invariant that any data structure only
                        // contain indices (that is, currently, variables).
                        let ts = ts
                            .into_iter()
                            .map(|t| {
                                let t_with_ctrs = RuntimeContract::apply_all(
                                    t,
                                    attrs.pending_contracts.iter().cloned(),
                                    pos.into_inherited(),
                                );

                                RichTerm::new(Term::App(f_as_var.clone(), t_with_ctrs), pos_op_inh)
                                    .closurize(&mut self.cache, env.clone())
                            })
                            .collect();

                        Ok(Closure {
                            body: RichTerm::new(
                                Term::Array(ts, attrs.contracts_cleared().closurized()),
                                pos_op_inh,
                            ),
                            env: Environment::new(),
                        })
                    }
                    _ => Err(mk_type_error!("map", "Array")),
                })
            }
            UnaryOp::ArrayGen => {
                let (f, _) = self
                    .stack
                    .pop_arg(&self.cache)
                    .ok_or_else(|| EvalError::NotEnoughArgs(2, String::from("generate"), pos_op))?;

                let Term::Num(ref n) = *t else {
                    return Err(mk_type_error!("generate", "Number"));
                };

                if n < &Number::ZERO {
                    return Err(EvalError::Other(
                        format!(
                            "generate expects its first argument to be a positive number, got {n}"
                        ),
                        pos_op,
                    ));
                }

                let Ok(n_int) = u32::try_from(n) else {
                    return Err(EvalError::Other(
                        format!(
                            "generate expects its first argument to be an integer \
                            smaller than {}, got {n}",
                            u32::MAX,
                        ),
                        pos_op,
                    ));
                };

                let f_closure = f.body.closurize(&mut self.cache, f.env);

                // Array elements are closurized to preserve laziness of data structures. It
                // maintains the invariant that any data structure only contain indices (that is,
                // currently, variables).
                let ts = (0..n_int)
                    .map(|n| {
                        mk_app!(f_closure.clone(), Term::Num(n.into()))
                            .closurize(&mut self.cache, env.clone())
                    })
                    .collect();

                Ok(Closure {
                    body: RichTerm::new(
                        Term::Array(ts, ArrayAttrs::new().closurized()),
                        pos_op_inh,
                    ),
                    env: Environment::new(),
                })
            }
            UnaryOp::RecordMap => {
                let (f, ..) = self.stack.pop_arg(&self.cache).ok_or_else(|| {
                    EvalError::NotEnoughArgs(2, String::from("record_map"), pos_op)
                })?;

                match_sharedterm!(match (t) {
                    Term::Record(record) => {
                        // While it's certainly possible to allow mapping over
                        // a record with a sealed tail, it's not entirely obvious
                        // how that should behave. It's also not clear that this
                        // is something users will actually need to do, so we've
                        // decided to prevent this until we have a clearer idea
                        // of potential use-cases.
                        if let Some(record::SealedTail { label, .. }) = record.sealed_tail {
                            return Err(EvalError::IllegalPolymorphicTailAccess {
                                action: IllegalPolymorphicTailAction::Map,
                                evaluated_arg: label.get_evaluated_arg(&self.cache),
                                label,
                                call_stack: std::mem::take(&mut self.call_stack),
                            });
                        }

                        let f_closure = f.body.closurize(&mut self.cache, f.env);

                        // As for `ArrayMap` (see above), we closurize the content of fields

                        let fields = record
                            .fields
                            .into_iter()
                            .filter(|(_, field)| !field.is_empty_optional())
                            .map_values_closurize(&mut self.cache, &env, |id, t| {
                                let pos = t.pos.into_inherited();

                                mk_app!(f_closure.clone(), mk_term::string(id.label()), t)
                                    .with_pos(pos)
                            })
                            .map_err(|missing_field_err| {
                                missing_field_err.into_eval_err(pos, pos_op)
                            })?;

                        Ok(Closure {
                            body: RichTerm::new(
                                Term::Record(RecordData { fields, ..record }),
                                pos_op_inh,
                            ),
                            env: Environment::new(),
                        })
                    }
                    _ => Err(mk_type_error!("record_map", "Record", 1)),
                })
            }
            UnaryOp::Seq => self
                .stack
                .pop_arg(&self.cache)
                .map(|(next, ..)| next)
                .ok_or_else(|| EvalError::NotEnoughArgs(2, String::from("seq"), pos_op)),
            UnaryOp::DeepSeq => {
                // Build a `RichTerm` that forces a given list of terms, and at the end resumes the
                // evaluation of the argument on the top of the stack.
                //
                // Requires its first argument to be non-empty.
                fn seq_terms<I>(mut it: I, pos_op_inh: TermPos) -> RichTerm
                where
                    I: Iterator<Item = RichTerm>,
                {
                    let first = it
                        .next()
                        .expect("expected the argument to be a non-empty iterator");

                    it.fold(
                        mk_term::op1(UnaryOp::DeepSeq, first).with_pos(pos_op_inh),
                        |acc, t| {
                            mk_app!(mk_term::op1(UnaryOp::DeepSeq, t), acc).with_pos(pos_op_inh)
                        },
                    )
                }

                match t.into_owned() {
                    Term::Record(record) if !record.fields.is_empty() => {
                        let defined = record
                            // into_iter_without_opts applies pending contracts as well
                            .into_iter_without_opts()
                            .collect::<Result<Vec<_>, _>>()
                            .map_err(|missing_def_err| {
                                missing_def_err.into_eval_err(pos, pos_op)
                            })?;

                        let terms = defined.into_iter().map(|(_, field)| field);

                        Ok(Closure {
                            body: seq_terms(terms, pos_op),
                            env,
                        })
                    }
                    Term::Array(ts, attrs) if !ts.is_empty() => {
                        let terms = seq_terms(
                            ts.into_iter().map(|t| {
                                let t_with_ctr = RuntimeContract::apply_all(
                                    t,
                                    attrs.pending_contracts.iter().cloned(),
                                    pos.into_inherited(),
                                )
                                .closurize(&mut self.cache, env.clone());
                                t_with_ctr
                            }),
                            pos_op,
                        );

                        Ok(Closure {
                            body: terms,
                            env: Environment::new(),
                        })
                    }
                    Term::EnumVariant { arg, .. } => Ok(Closure {
                        body: seq_terms(std::iter::once(arg), pos_op),
                        env,
                    }),
                    _ => {
                        if let Some((next, ..)) = self.stack.pop_arg(&self.cache) {
                            Ok(next)
                        } else {
                            Err(EvalError::NotEnoughArgs(
                                2,
                                String::from("deep_seq"),
                                pos_op,
                            ))
                        }
                    }
                }
            }
            UnaryOp::ArrayLength => {
                if let Term::Array(ts, _) = &*t {
                    // A num does not have any free variable so we can drop the environment
                    Ok(Closure {
                        body: RichTerm::new(Term::Num(ts.len().into()), pos_op_inh),
                        env: Environment::new(),
                    })
                } else {
                    Err(mk_type_error!("length", "Array"))
                }
            }
            UnaryOp::ChunksConcat => {
                let StrAccData {
                    mut acc,
                    curr_indent: indent,
                    env: env_chunks,
                    curr_pos,
                } = self.stack.pop_str_acc().unwrap();

                if let Term::Str(s) = &*t {
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
                            curr_pos: e.pos,
                        });

                        Ok(Closure {
                            body: RichTerm::new(Term::Op1(UnaryOp::ChunksConcat, e), pos_op_inh),
                            env: env_chunks,
                        })
                    } else {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Str(acc.into()),
                            pos_op_inh,
                        )))
                    }
                } else {
                    // Since the error halts the evaluation, we don't bother cleaning the stack of
                    // the remaining string chunks.
                    //
                    // Not using mk_type_error! because of a non-uniform message
                    Err(EvalError::TypeError(
                        String::from("String"),
                        String::from("interpolated values must be of type String"),
                        curr_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::StringTrim => {
                if let Term::Str(s) = &*t {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Str(s.trim().into()),
                        pos_op_inh,
                    )))
                } else {
                    Err(mk_type_error!("str_trim", "String"))
                }
            }
            UnaryOp::StringChars => {
                if let Term::Str(s) = &*t {
                    let ts = s.characters();
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Array(ts, ArrayAttrs::new().closurized()),
                        pos_op_inh,
                    )))
                } else {
                    Err(mk_type_error!("str_chars", "String"))
                }
            }
            UnaryOp::StringUppercase => {
                if let Term::Str(s) = &*t {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Str(s.to_uppercase().into()),
                        pos_op_inh,
                    )))
                } else {
                    Err(mk_type_error!("str_uppercase", "String"))
                }
            }
            UnaryOp::StringLowercase => {
                if let Term::Str(s) = &*t {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Str(s.to_lowercase().into()),
                        pos_op_inh,
                    )))
                } else {
                    Err(mk_type_error!("string_lowercase", "String"))
                }
            }
            UnaryOp::StringLength => {
                if let Term::Str(s) = &*t {
                    let length = s.graphemes(true).count();
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Num(length.into()),
                        pos_op_inh,
                    )))
                } else {
                    Err(mk_type_error!("str_length", "String"))
                }
            }
            UnaryOp::ToString => {
                let result = match_sharedterm!(match (t) {
                    Term::Num(n) => Ok(Term::Str(format!("{}", n.to_sci()).into())),
                    Term::Str(s) => Ok(Term::Str(s)),
                    Term::Bool(b) => Ok(Term::Str(b.to_string().into())),
                    Term::Enum(id) => Ok(Term::Str(id.into())),
                    Term::Null => Ok(Term::Str("null".into())),
                    _ => Err(EvalError::Other(
                        format!(
                            "to_string: can't convert an argument of type {} to string",
                            t.type_of().unwrap()
                        ),
                        pos,
                    )),
                })?;

                Ok(Closure::atomic_closure(RichTerm::new(result, pos_op_inh)))
            }
            UnaryOp::NumberFromString => {
                if let Term::Str(s) = &*t {
                    let n = parse_number_sci(s).map_err(|_| {
                        EvalError::Other(
                            format!("num_from_string: invalid number literal `{}`", s.as_str()),
                            pos,
                        )
                    })?;
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Num(n),
                        pos_op_inh,
                    )))
                } else {
                    Err(mk_type_error!("num_from_str", "String"))
                }
            }
            UnaryOp::EnumFromString => {
                if let Term::Str(s) = &*t {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Enum(LocIdent::from(s)),
                        pos_op_inh,
                    )))
                } else {
                    Err(mk_type_error!("enum_from_str", "String"))
                }
            }
            UnaryOp::StringIsMatch => {
                if let Term::Str(s) = &*t {
                    let re = regex::Regex::new(s)
                        .map_err(|err| EvalError::Other(err.to_string(), pos_op))?;

                    let param = LocIdent::fresh();
                    let matcher = Term::Fun(
                        param,
                        RichTerm::new(
                            Term::Op1(
                                UnaryOp::StringIsMatchCompiled(re.into()),
                                RichTerm::new(Term::Var(param), pos_op_inh),
                            ),
                            pos_op_inh,
                        ),
                    );

                    Ok(Closure::atomic_closure(RichTerm::new(matcher, pos)))
                } else {
                    Err(mk_type_error!("str_is_match", "String", 1))
                }
            }
            UnaryOp::StringFind => {
                if let Term::Str(s) = &*t {
                    let re = regex::Regex::new(s)
                        .map_err(|err| EvalError::Other(err.to_string(), pos_op))?;

                    let param = LocIdent::fresh();
                    let matcher = Term::Fun(
                        param,
                        RichTerm::new(
                            Term::Op1(
                                UnaryOp::StringFindCompiled(re.into()),
                                RichTerm::new(Term::Var(param), pos_op_inh),
                            ),
                            pos_op_inh,
                        ),
                    );

                    Ok(Closure::atomic_closure(RichTerm::new(matcher, pos)))
                } else {
                    Err(mk_type_error!("str_find", "String", 1))
                }
            }
            UnaryOp::StringFindAll => {
                if let Term::Str(s) = &*t {
                    let re = regex::Regex::new(s)
                        .map_err(|err| EvalError::Other(err.to_string(), pos_op))?;

                    let param = LocIdent::fresh();
                    let matcher = Term::Fun(
                        param,
                        RichTerm::new(
                            Term::Op1(
                                UnaryOp::StringFindAllCompiled(re.into()),
                                RichTerm::new(Term::Var(param), pos_op_inh),
                            ),
                            pos_op_inh,
                        ),
                    );

                    Ok(Closure::atomic_closure(RichTerm::new(matcher, pos_op_inh)))
                } else {
                    Err(mk_type_error!("str_find_all", "String", 1))
                }
            }
            UnaryOp::StringIsMatchCompiled(regex) => {
                if let Term::Str(s) = &*t {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        s.matches_regex(&regex),
                        pos_op_inh,
                    )))
                } else {
                    Err(mk_type_error!(
                        "a compiled regular expression match",
                        "String"
                    ))
                }
            }
            UnaryOp::StringFindCompiled(regex) => {
                if let Term::Str(s) = &*t {
                    use crate::term::string::RegexFindResult;
                    let result = match s.find_regex(&regex) {
                        None => mk_record!(
                            ("matched", RichTerm::from(Term::Str(NickelString::new()))),
                            ("index", RichTerm::from(Term::Num(Number::from(-1)))),
                            (
                                "groups",
                                RichTerm::from(Term::Array(
                                    Array::default(),
                                    ArrayAttrs::default()
                                ))
                            )
                        ),
                        Some(RegexFindResult {
                            matched: mtch,
                            index,
                            groups,
                        }) => mk_record!(
                            ("matched", RichTerm::from(Term::Str(mtch))),
                            ("index", RichTerm::from(Term::Num(index))),
                            (
                                "groups",
                                RichTerm::from(Term::Array(
                                    Array::from_iter(
                                        groups.into_iter().map(|s| Term::Str(s).into())
                                    ),
                                    ArrayAttrs::new().closurized()
                                ))
                            )
                        ),
                    };
                    Ok(Closure::atomic_closure(result))
                } else {
                    Err(mk_type_error!(
                        "a compiled regular expression match",
                        "String"
                    ))
                }
            }
            UnaryOp::StringFindAllCompiled(regex) => {
                if let Term::Str(s) = &*t {
                    let result = Term::Array(
                        Array::from_iter(s.find_all_regex(&regex).map(|found| {
                            mk_record!(
                                ("matched", RichTerm::from(Term::Str(found.matched))),
                                ("index", RichTerm::from(Term::Num(found.index))),
                                (
                                    "groups",
                                    RichTerm::from(Term::Array(
                                        Array::from_iter(
                                            found.groups.into_iter().map(|s| Term::Str(s).into())
                                        ),
                                        ArrayAttrs::new().closurized()
                                    ))
                                )
                            )
                        })),
                        ArrayAttrs::default(),
                    );

                    Ok(Closure::atomic_closure(RichTerm::new(result, pos_op_inh)))
                } else {
                    Err(mk_type_error!(
                        "a compiled regular expression match",
                        "String"
                    ))
                }
            }
            UnaryOp::Force {
                ignore_not_exported,
            } => {
                /// `Seq` the `terms` iterator and then resume evaluating the `cont` continuation.
                fn seq_terms<I>(terms: I, pos: TermPos, cont: RichTerm) -> RichTerm
                where
                    I: Iterator<Item = RichTerm>,
                {
                    terms
                        .fold(cont, |acc, t| mk_app!(mk_term::op1(UnaryOp::Seq, t), acc))
                        .with_pos(pos)
                }

                match_sharedterm!(match (t) {
                    Term::Record(record) if !record.fields.is_empty() => {
                        let fields = record
                            .fields
                            .into_iter()
                            .filter(|(_, field)| {
                                !(field.is_empty_optional()
                                    || (ignore_not_exported && field.metadata.not_exported))
                            })
                            .map_values_closurize(&mut self.cache, &env, |_, value| {
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

                        let cont = RichTerm::new(
                            Term::Record(RecordData { fields, ..record }),
                            pos.into_inherited(),
                        );

                        Ok(Closure {
                            body: seq_terms(terms, pos_op, cont),
                            env: Environment::new(),
                        })
                    }
                    Term::Array(ts, attrs) if !ts.is_empty() => {
                        let ts = ts
                            .into_iter()
                            .map(|t| {
                                mk_term::op1(
                                    UnaryOp::Force {
                                        ignore_not_exported,
                                    },
                                    RuntimeContract::apply_all(
                                        t,
                                        attrs.pending_contracts.iter().cloned(),
                                        pos.into_inherited(),
                                    ),
                                )
                                .closurize(&mut self.cache, env.clone())
                            })
                            // It's important to collect here, otherwise the two usages below
                            // will each do their own .closurize(...) calls and end up with
                            // different closures, which means that `cont` won't be properly
                            // updated.
                            .collect::<Array>();

                        let terms = ts.clone().into_iter();
                        let cont = RichTerm::new(Term::Array(ts, attrs), pos.into_inherited());

                        Ok(Closure {
                            body: seq_terms(terms, pos_op, cont),
                            env: Environment::new(),
                        })
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
                        .closurize(&mut self.cache, env.clone());
                        let cont = RichTerm::new(
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
                        body: RichTerm { term: t, pos },
                        env
                    }),
                })
            }
            UnaryOp::RecDefault => {
                Ok(RecPriority::Bottom.propagate_in_term(&mut self.cache, t, env, pos))
            }
            UnaryOp::RecForce => {
                Ok(RecPriority::Top.propagate_in_term(&mut self.cache, t, env, pos))
            }
            UnaryOp::RecordEmptyWithTail => match_sharedterm!(match (t) {
                Term::Record(r) => {
                    let mut empty = RecordData::empty();
                    empty.sealed_tail = r.sealed_tail;
                    Ok(Closure {
                        body: RichTerm::new(Term::Record(empty), pos_op.into_inherited()),
                        env,
                    })
                }
                _ => Err(mk_type_error!("record_empty_with_tail", "Record")),
            }),
            UnaryOp::Trace => {
                if let Term::Str(s) = &*t {
                    let _ = writeln!(self.trace, "std.trace: {s}");
                    Ok(())
                } else {
                    Err(mk_type_error!("trace", "String"))
                }?;

                self.stack
                    .pop_arg(&self.cache)
                    .map(|(next, ..)| next)
                    .ok_or_else(|| EvalError::NotEnoughArgs(2, String::from("trace"), pos_op))
            }
            UnaryOp::LabelPushDiag => {
                match_sharedterm!(match (t) {
                    Term::Lbl(label) => {
                        let mut label = label;
                        label.push_diagnostic();
                        Ok(Closure {
                            body: RichTerm::new(Term::Lbl(label), pos),
                            env,
                        })
                    }
                    _ => Err(mk_type_error!("label_push_diag", "Label")),
                })
            }
            #[cfg(feature = "nix-experimental")]
            UnaryOp::EvalNix => {
                if let Term::Str(s) = &*t {
                    let json = nix_ffi::eval_to_json(&String::from(s)).map_err(|e| {
                        EvalError::Other(
                            format!("nix code failed to evaluate:\n {}", e.what()),
                            pos,
                        )
                    })?;
                    Ok(Closure::atomic_closure(
                        serde_json::from_str(&json).map_err(|e| {
                            EvalError::Other(format!("Nix produced invalid json: {}", e), pos)
                        })?,
                    ))
                } else {
                    // Not using mk_type_error! because of a non-uniform message
                    Err(EvalError::TypeError(
                        String::from("String"),
                        String::from("eval_nix takes a string of nix code as an argument"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::EnumGetArg => {
                if let Term::EnumVariant { arg, .. } = &*t {
                    Ok(Closure {
                        body: arg.clone(),
                        env,
                    })
                } else {
                    Err(mk_type_error!("enum_unwrap_variant", "Enum variant"))
                }
            }
            UnaryOp::EnumMakeVariant => {
                let Term::Str(tag) = &*t else {
                    return Err(mk_type_error!("enum/make_variant", "String"));
                };

                let (arg_clos, _) = self.stack.pop_arg(&self.cache).ok_or_else(|| {
                    EvalError::NotEnoughArgs(2, String::from("enum/make_variant"), pos)
                })?;
                let arg_pos = arg_clos.body.pos;
                let arg = RichTerm::new(Term::Closure(Thunk::new(arg_clos)), arg_pos);

                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::EnumVariant {
                        tag: LocIdent::new(tag).with_pos(pos),
                        arg,
                        attrs: EnumVariantAttrs { closurized: true },
                    },
                    pos_op_inh,
                )))
            }
            UnaryOp::EnumGetTag => match &*t {
                Term::EnumVariant { tag, .. } | Term::Enum(tag) => Ok(Closure::atomic_closure(
                    RichTerm::new(Term::Enum(*tag), pos_op_inh),
                )),
                _ => Err(mk_type_error!("enum_get_tag", "Enum")),
            },
            UnaryOp::EnumIsVariant => {
                let result = matches!(&*t, Term::EnumVariant { .. });
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Bool(result),
                    pos_op_inh,
                )))
            }
            UnaryOp::PatternBranch => {
                // The continuation, that we must evaluate in the augmented environment.
                let (mut cont, _) = self
                    .stack
                    .pop_arg(&self.cache)
                    .ok_or_else(|| EvalError::NotEnoughArgs(2, String::from("with_env"), pos_op))?;

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
                                            self.cache.add(
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
                    _ => Err(mk_type_error!("with_env", "Record")),
                })
            }
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
        fst_pos: TermPos,
        clos: Closure,
        snd_pos: TermPos,
        pos_op: TermPos,
    ) -> Result<Closure, EvalError> {
        let Closure {
            body: RichTerm {
                term: t1,
                pos: pos1,
            },
            env: env1,
        } = fst_clos;
        let Closure {
            body: RichTerm {
                term: t2,
                pos: pos2,
            },
            env: env2,
        } = clos;
        let pos_op_inh = pos_op.into_inherited();

        macro_rules! mk_type_error {
            ($primop:expr, $expected:expr, $arg_number:expr, $term:expr, $pos:expr) => {
                EvalError::NAryPrimopTypeError {
                    primop: String::from($primop),
                    expected: String::from($expected),
                    arg_number: $arg_number,
                    arg_pos: {
                        match $arg_number {
                            1 => fst_pos,
                            2 => snd_pos,
                            _ => unimplemented!(),
                        }
                    },
                    arg_evaluated: RichTerm {
                        term: $term,
                        pos: $pos,
                    },
                }
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
                        Err(mk_type_error!("seal", "Label", 2, t2, pos2))
                    }
                } else {
                    Err(mk_type_error!("seal", "SealingKey", 1, t1, pos1))
                }
            }
            BinaryOp::Plus => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Num(n1 + n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(mk_type_error!("(+)", "Number", 2, t2, pos2))
                    }
                } else {
                    Err(mk_type_error!("(+)", "Number", 1, t1, pos1))
                }
            }
            BinaryOp::Sub => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Num(n1 - n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(mk_type_error!("(-)", "Number", 2, t2, pos2))
                    }
                } else {
                    Err(mk_type_error!("(-)", "Number", 1, t1, pos1))
                }
            }
            BinaryOp::Mult => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Num(n1 * n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(mk_type_error!("(*)", "Number", 2, t2, pos2))
                    }
                } else {
                    Err(mk_type_error!("(*)", "Number", 1, t1, pos1))
                }
            }
            BinaryOp::Div => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        if n2 == &Number::ZERO {
                            Err(EvalError::Other(String::from("division by zero"), pos_op))
                        } else {
                            Ok(Closure::atomic_closure(RichTerm::new(
                                Term::Num(n1 / n2),
                                pos_op_inh,
                            )))
                        }
                    } else {
                        Err(mk_type_error!("(/)", "Number", 2, t2, pos2))
                    }
                } else {
                    Err(mk_type_error!("(/)", "Number", 1, t1, pos1))
                }
            }
            BinaryOp::Modulo => {
                let Term::Num(ref n1) = *t1 else {
                    return Err(mk_type_error!("(%)", "Number", 1, t1, pos1));
                };

                let Term::Num(ref n2) = *t2 else {
                    return Err(mk_type_error!("(%)", "Number", 2, t2, pos2));
                };

                if n2 == &Number::ZERO {
                    return Err(EvalError::Other(String::from("division by zero (%)"), pos2));
                }

                // This is the equivalent of `truncate()` for `Number`
                let quotient = Number::from(Integer::rounding_from(n1 / n2, RoundingMode::Down).0);

                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Num(n1 - quotient * n2),
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
                                EvalError::Other(
                                    format!(
                                        "invalid arithmetic operation: \
                                        {n1}^{n2} returned {result_as_f64}, \
                                        but {result_as_f64} isn't representable in Nickel"
                                    ),
                                    pos_op,
                                )
                            })?
                        };

                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Num(result),
                            pos_op_inh,
                        )))
                    } else {
                        Err(mk_type_error!("pow", "Number", 2, t2, pos2))
                    }
                } else {
                    Err(mk_type_error!("pow", "Number", 1, t1, pos1))
                }
            }
            BinaryOp::StringConcat => {
                if let Term::Str(s1) = &*t1 {
                    if let Term::Str(s2) = &*t2 {
                        let ss: [&str; 2] = [s1, s2];
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Str(ss.concat().into()),
                            pos_op_inh,
                        )))
                    } else {
                        Err(mk_type_error!("(++)", "String", 2, t2, pos2))
                    }
                } else {
                    Err(mk_type_error!("(++)", "String", 1, t1, pos1))
                }
            }
            BinaryOp::ContractApply => {
                if let Term::Lbl(l) = &*t2 {
                    // Track the contract argument for better error reporting, and push back the
                    // label on the stack, so that it becomes the first argument of the contract.
                    let idx = self.stack.track_arg(&mut self.cache).ok_or_else(|| {
                        EvalError::NotEnoughArgs(3, String::from("apply_contract"), pos_op)
                    })?;
                    let mut l = l.clone();
                    l.arg_pos = self.cache.get_then(idx.clone(), |c| c.body.pos);
                    l.arg_idx = Some(idx);

                    self.stack.push_arg(
                        Closure::atomic_closure(RichTerm::new(Term::Lbl(l), pos2.into_inherited())),
                        pos2.into_inherited(),
                    );

                    match *t1 {
                        Term::Fun(..) | Term::Match { .. } => Ok(Closure {
                            body: RichTerm {
                                term: t1,
                                pos: pos1,
                            },
                            env: env1,
                        }),
                        Term::Record(..) => {
                            let closurized = RichTerm {
                                term: t1,
                                pos: pos1,
                            }
                            .closurize(&mut self.cache, env1);

                            // Convert the record to the function `fun l x => MergeContract l x t1
                            // contract`.
                            let body = mk_fun!(
                                "l",
                                "x",
                                mk_opn!(
                                    NAryOp::MergeContract,
                                    mk_term::var("l"),
                                    mk_term::var("x"),
                                    closurized
                                )
                            )
                            .with_pos(pos1.into_inherited());

                            Ok(Closure {
                                body,
                                env: Environment::new(),
                            })
                        }
                        _ => Err(mk_type_error!("apply_contract", "Contract", 1, t1, pos1)),
                    }
                } else {
                    Err(mk_type_error!("apply_contract", "Label", 2, t2, pos2))
                }
            }
            BinaryOp::Unseal => {
                if let Term::SealingKey(s1) = &*t1 {
                    // Return a function that either behaves like the identity or
                    // const unwrapped_term

                    Ok(if let Term::Sealed(s2, t, _) = t2.into_owned() {
                        if *s1 == s2 {
                            Closure {
                                body: mk_fun!("-invld", t),
                                env: env2,
                            }
                        } else {
                            Closure::atomic_closure(mk_term::id())
                        }
                    } else {
                        Closure::atomic_closure(mk_term::id())
                    })
                } else {
                    Err(mk_type_error!("unseal", "SealingKey", 1, t1, pos1))
                }
            }
            BinaryOp::Eq => {
                let c1 = Closure {
                    body: RichTerm {
                        term: t1,
                        pos: pos1,
                    },
                    env: env1,
                };
                let c2 = Closure {
                    body: RichTerm {
                        term: t2,
                        pos: pos2,
                    },
                    env: env2,
                };

                match eq(&mut self.cache, c1, c2, pos_op_inh)? {
                    EqResult::Bool(b) => match (b, self.stack.pop_eq()) {
                        (false, _) => {
                            self.stack.clear_eqs();
                            Ok(Closure::atomic_closure(RichTerm::new(
                                Term::Bool(false),
                                pos_op_inh,
                            )))
                        }
                        (true, None) => Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Bool(true),
                            pos_op_inh,
                        ))),
                        (true, Some((c1, c2))) => {
                            let t1 = c1.body.closurize(&mut self.cache, c1.env);
                            let t2 = c2.body.closurize(&mut self.cache, c2.env);

                            Ok(Closure {
                                body: RichTerm::new(Term::Op2(BinaryOp::Eq, t1, t2), pos_op),
                                env: Environment::new(),
                            })
                        }
                    },
                    EqResult::Eqs(t1, t2, subeqs) => {
                        self.stack.push_eqs(subeqs.into_iter());

                        Ok(Closure {
                            body: RichTerm::new(Term::Op2(BinaryOp::Eq, t1, t2), pos_op),
                            env: Environment::new(),
                        })
                    }
                }
            }
            BinaryOp::LessThan => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Bool(n1 < n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(mk_type_error!("(<)", "Number", 2, t2, pos2))
                    }
                } else {
                    Err(mk_type_error!("(<)", "Number", 1, t1, pos1))
                }
            }
            BinaryOp::LessOrEq => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Bool(n1 <= n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(mk_type_error!("(<=)", "Number", 2, t2, pos2))
                    }
                } else {
                    Err(mk_type_error!("(<=)", "Number", 1, t1, pos1))
                }
            }
            BinaryOp::GreaterThan => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Bool(n1 > n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(mk_type_error!("(>)", "Number", 2, t2, pos2))
                    }
                } else {
                    Err(mk_type_error!("(>)", "Number", 1, t1, pos1))
                }
            }
            BinaryOp::GreaterOrEq => {
                if let Term::Num(ref n1) = *t1 {
                    if let Term::Num(ref n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Bool(n1 >= n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(mk_type_error!("(>=)", "Number", 2, t2, pos2))
                    }
                } else {
                    Err(mk_type_error!("(>=)", "Number", 1, t1, pos1))
                }
            }
            BinaryOp::LabelGoField => match_sharedterm!(match (t1) {
                Term::Str(field) => match_sharedterm!(match (t2) {
                    Term::Lbl(l) => {
                        let mut l = l;
                        l.path.push(ty_path::Elem::Field(field.into_inner().into()));
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Lbl(l),
                            pos_op_inh,
                        )))
                    }
                    _ => Err(mk_type_error!("go_field", "Label", 2, t2, pos2)),
                }),
                _ => Err(mk_type_error!("go_field", "String", 1, t1, pos1)),
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
                                        Err(EvalError::IllegalPolymorphicTailAccess {
                                            action: IllegalPolymorphicTailAction::FieldAccess {
                                                field: id.to_string(),
                                            },
                                            evaluated_arg: t.label.get_evaluated_arg(&self.cache),
                                            label: t.label.clone(),
                                            call_stack: std::mem::take(&mut self.call_stack),
                                        })
                                    }
                                    _ => Err(EvalError::FieldMissing {
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
                            Err(EvalError::TypeError(
                                String::from("Record"),
                                String::from("field access only makes sense for records"),
                                snd_pos,
                                RichTerm {
                                    term: t2,
                                    pos: pos2,
                                },
                            ))
                        }
                    }
                    // This error should be impossible to trigger. The parser
                    // prevents a dynamic field access where the field name is not syntactically
                    // a string.
                    _ => Err(mk_type_error!(".$", "String", 1, t1, pos1)),
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
                                    self.stack.pop_arg(&self.cache).ok_or_else(|| {
                                        EvalError::NotEnoughArgs(3, String::from("insert"), pos_op)
                                    })?;

                                let closurized = value_closure
                                    .body
                                    .closurize(&mut self.cache, value_closure.env);
                                Some(closurized)
                            } else {
                                None
                            };

                            match fields.insert(
                                LocIdent::from(id),
                                Field {
                                    value,
                                    metadata,
                                    pending_contracts,
                                },
                            ) {
                                Some(t)
                                    if matches!(op_kind, RecordOpKind::ConsiderAllFields)
                                        || !t.is_empty_optional() =>
                                {
                                    Err(EvalError::Other(
                                        format!(
                                            "record_insert: \
                                            tried to extend a record with the field {id}, \
                                            but it already exists"
                                        ),
                                        pos_op,
                                    ))
                                }
                                _ => Ok(Closure {
                                    body: Term::Record(RecordData { fields, ..record }).into(),
                                    env: env2,
                                }),
                            }
                        }
                        _ => Err(mk_type_error!("record_insert", "Record", 2, t2, pos2)),
                    })
                } else {
                    Err(mk_type_error!("record_insert", "String", 1, t1, pos1))
                }
            }
            BinaryOp::RecordRemove(op_kind) => match_sharedterm!(match (t1) {
                Term::Str(id) => match_sharedterm!(match (t2) {
                    Term::Record(record) => {
                        let mut fields = record.fields;
                        let fetched = fields.remove(&LocIdent::from(&id));
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
                                    Err(EvalError::IllegalPolymorphicTailAccess {
                                        action: IllegalPolymorphicTailAction::RecordRemove {
                                            field: id.to_string(),
                                        },
                                        evaluated_arg: t.label.get_evaluated_arg(&self.cache),
                                        label: t.label.clone(),
                                        call_stack: std::mem::take(&mut self.call_stack),
                                    })
                                }
                                _ => {
                                    // We reconstruct the record's data to have access to
                                    // `data.field_names()`
                                    let record = RecordData { fields, ..record };

                                    Err(EvalError::FieldMissing {
                                        id: id.into(),
                                        field_names: record.field_names(op_kind),
                                        operator: String::from("record_remove"),
                                        pos_record: pos2,
                                        pos_op,
                                    })
                                }
                            }
                        } else {
                            Ok(Closure {
                                body: RichTerm::new(
                                    Term::Record(RecordData { fields, ..record }),
                                    pos_op_inh,
                                ),
                                env: env2,
                            })
                        }
                    }
                    _ => Err(mk_type_error!("record_remove", "Record", 2, t2, pos2)),
                }),
                _ => Err(mk_type_error!("record_remove", "String", 1, t1, pos1)),
            }),
            BinaryOp::RecordHasField(op_kind) => match_sharedterm!(match (t1) {
                Term::Str(id) => {
                    if let Term::Record(record) = &*t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Bool(matches!(
                                record.fields.get(&LocIdent::from(id.into_inner())),
                                Some(field) if matches!(op_kind, RecordOpKind::ConsiderAllFields) || !field.is_empty_optional()
                            )),
                            pos_op_inh,
                        )))
                    } else {
                        Err(mk_type_error!("has_field", "Record", 2, t2, pos2))
                    }
                }
                _ => Err(mk_type_error!("has_field", "String", 1, t1, pos1)),
            }),
            BinaryOp::RecordFieldIsDefined(op_kind) => match_sharedterm!(match (t1) {
                Term::Str(id) => {
                    if let Term::Record(record) = &*t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Bool(matches!(
                                record.fields.get(&LocIdent::from(id.into_inner())),
                                Some(field @ Field { value: Some(_), ..}) if matches!(op_kind, RecordOpKind::ConsiderAllFields) || !field.is_empty_optional()
                            )),
                            pos_op_inh,
                        )))
                    } else {
                        Err(mk_type_error!("field_is_defined", "Record", 2, t2, pos2))
                    }
                }
                _ => Err(mk_type_error!("field_is_defined", "String", 1, t1, pos1)),
            }),
            BinaryOp::ArrayConcat => match_sharedterm!(match (t1) {
                Term::Array(ts1, attrs1) => match_sharedterm!(match (t2) {
                    Term::Array(ts2, attrs2) => {
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

                        // NOTE: To avoid the extra Vec allocation, we could use
                        // Rc<[T]>::new_uninit_slice() and fill up the slice manually, but
                        // that's a nightly-only experimental API. Note that collecting into
                        // an Rc<[T]> will also allocate a intermediate vector, unless the
                        // input iterator implements the nightly-only API TrustedLen, and
                        // Array's iterator currently doesn't. Even if we could implement
                        // TrustedLen we would have to contend with the fact that .chain(..)
                        // tends to be slow.
                        // - Rc<[T]>::from_iter docs:
                        //   https://doc.rust-lang.org/std/rc/struct.Rc.html#impl-FromIterator%3CT%3E
                        // - chain issue: https://github.com/rust-lang/rust/issues/63340
                        let mut ts: Vec<RichTerm> = Vec::with_capacity(ts1.len() + ts2.len());

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
                                    other_ctr.as_ref().map_or(false, |other_ctr| {
                                        contract_eq(
                                            0,
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

                        let ctrs_right_dedup = ctrs_right_sieve.into_iter().flatten();

                        ts.extend(ts1.into_iter().map(|t| {
                            RuntimeContract::apply_all(t, ctrs_left_dedup.iter().cloned(), pos1)
                                .closurize(&mut self.cache, env1.clone())
                        }));

                        ts.extend(ts2.into_iter().map(|t| {
                            RuntimeContract::apply_all(t, ctrs_right_dedup.clone(), pos2)
                                .closurize(&mut self.cache, env2.clone())
                        }));

                        let attrs = ArrayAttrs {
                            closurized: true,
                            pending_contracts: ctrs_common,
                        };

                        Ok(Closure {
                            body: RichTerm::new(
                                Term::Array(Array::new(Rc::from(ts)), attrs),
                                pos_op_inh,
                            ),
                            env: Environment::new(),
                        })
                    }
                    _ => {
                        Err(mk_type_error!("(@)", "Array", 2, t2, pos2))
                    }
                }),
                _ => {
                    Err(mk_type_error!("(@)", "Array", 1, t1, pos1))
                }
            }),
            BinaryOp::ArrayAt => match (&*t1, &*t2) {
                (Term::Array(ts, attrs), Term::Num(n)) => {
                    let Ok(n_as_usize) = usize::try_from(n) else {
                        return Err(EvalError::Other(
                            format!(
                                "elem_at expects its second argument to be a \
                                positive integer smaller than {}, got {n}",
                                usize::MAX
                            ),
                            pos_op,
                        ));
                    };

                    if n_as_usize >= ts.len() {
                        return Err(EvalError::Other(
                            format!(
                                "elem_at: index out of bounds. \
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
                        body: elem_with_ctr,
                        env: env1,
                    })
                }
                (Term::Array(..), _) => Err(mk_type_error!("elem_at", "Number", 2, t2, pos2)),
                (_, _) => Err(mk_type_error!("elem_at", "Array", 1, t1, pos1)),
            },
            BinaryOp::Merge(merge_label) => merge::merge(
                &mut self.cache,
                RichTerm {
                    term: t1,
                    pos: pos1,
                },
                env1,
                RichTerm {
                    term: t2,
                    pos: pos2,
                },
                env2,
                pos_op,
                MergeMode::Standard(merge_label),
                &mut self.call_stack,
            ),
            BinaryOp::Hash => {
                let mk_err_fst = |t1| {
                    Err(mk_type_error!(
                        "hash",
                        "[| 'Md5, 'Sha1, 'Sha256, 'Sha512 |]",
                        1,
                        t1,
                        pos1
                    ))
                };

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

                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Str(result.into()),
                            pos_op_inh,
                        )))
                    } else {
                        Err(mk_type_error!("hash", "String", 2, t2, pos2))
                    }
                } else {
                    mk_err_fst(t1)
                }
            }
            BinaryOp::Serialize => {
                let mk_err_fst = |t1| {
                    Err(mk_type_error!(
                        "serialize",
                        "[| 'Json, 'Yaml, 'Toml |]",
                        1,
                        t1,
                        pos1
                    ))
                };

                if let Term::Enum(ref id) = t1.as_ref() {
                    // Serialization needs all variables term to be fully substituted
                    let initial_env = Environment::new();
                    let rt2 = subst(
                        &self.cache,
                        RichTerm {
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
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Str(serialize::to_string(format, &rt2)?.into()),
                        pos_op_inh,
                    )))
                } else {
                    mk_err_fst(t1)
                }
            }
            BinaryOp::Deserialize => {
                let mk_err_fst = |t1| {
                    Err(mk_type_error!(
                        "deserialize",
                        "[| 'Json, 'Yaml, 'Toml |]",
                        1,
                        t1,
                        pos1
                    ))
                };

                if let Term::Enum(id) = &*t1 {
                    if let Term::Str(s) = &*t2 {
                        let rt: RichTerm = match id.as_ref() {
                            "Json" => serde_json::from_str(s).map_err(|err| {
                                EvalError::DeserializationError(
                                    String::from("json"),
                                    format!("{err}"),
                                    pos_op,
                                )
                            })?,
                            "Yaml" => serde_yaml::from_str(s).map_err(|err| {
                                EvalError::DeserializationError(
                                    String::from("yaml"),
                                    format!("{err}"),
                                    pos_op,
                                )
                            })?,
                            "Toml" => toml::from_str(s).map_err(|err| {
                                EvalError::DeserializationError(
                                    String::from("toml"),
                                    format!("{err}"),
                                    pos_op,
                                )
                            })?,
                            _ => return mk_err_fst(t1),
                        };

                        Ok(Closure::atomic_closure(rt.with_pos(pos_op_inh)))
                    } else {
                        Err(mk_type_error!("deserialize", "String", 2, t2, pos2))
                    }
                } else {
                    mk_err_fst(t1)
                }
            }
            BinaryOp::StringSplit => match (&*t1, &*t2) {
                (Term::Str(input), Term::Str(separator)) => {
                    let result = input.split(separator);
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Array(result, ArrayAttrs::new().closurized()),
                        pos_op_inh,
                    )))
                }
                (Term::Str(_), _) => Err(mk_type_error!("str_split", "String", 2, t2, pos2)),
                (_, _) => Err(mk_type_error!("str_split", "String", 1, t1, pos1)),
            },
            BinaryOp::StringContains => match (&*t1, &*t2) {
                (Term::Str(s1), Term::Str(s2)) => {
                    let result = s1.contains(s2.as_str());
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Bool(result),
                        pos_op_inh,
                    )))
                }
                (Term::Str(_), _) => Err(mk_type_error!("str_contains", "String", 2, t2, pos2)),
                (_, _) => Err(mk_type_error!("str_contains", "String", 1, t1, pos1)),
            },
            BinaryOp::ContractArrayLazyApp => {
                let (ctr, _) = self.stack.pop_arg(&self.cache).ok_or_else(|| {
                    EvalError::NotEnoughArgs(3, String::from("array_lazy_app_ctr"), pos_op)
                })?;

                let Closure {
                    body: rt3,
                    env: env3,
                } = ctr;

                // FIXME: use match?
                let lbl = match_sharedterm!(match (t1) {
                    Term::Lbl(lbl) => lbl,
                    _ => return Err(mk_type_error!("array_lazy_app_ctr", "Label", 1, t1, pos1)),
                });

                match_sharedterm!(match (t2) {
                    Term::Array(ts, attrs) => {
                        let mut attrs = attrs;
                        let mut final_env = env2;

                        // Preserve the environment of the contract in the resulting array.
                        let contract = rt3.closurize(&mut self.cache, env3);
                        RuntimeContract::push_dedup(
                            &mut attrs.pending_contracts,
                            &final_env,
                            RuntimeContract::new(contract, lbl),
                            &final_env,
                        );

                        let array_with_ctr = Closure {
                            body: RichTerm::new(Term::Array(ts, attrs), pos2),
                            env: final_env,
                        };

                        Ok(array_with_ctr)
                    }
                    _ => Err(mk_type_error!("array_lazy_app_ctr", "Array", 2, t2, pos2)),
                })
            }
            BinaryOp::ContractRecordLazyApp => {
                // The contract is expected to be of type `String -> Contract`: it takes the name
                // of the field as a parameter, and returns a contract.
                let (
                    Closure {
                        body: contract_term,
                        env: contract_env,
                    },
                    _,
                ) = self.stack.pop_arg(&self.cache).ok_or_else(|| {
                    EvalError::NotEnoughArgs(3, String::from("record_lazy_app_ctr"), pos_op)
                })?;

                let label = match_sharedterm!(match (t1) {
                    Term::Lbl(label) => label,
                    _ => return Err(mk_type_error!("record_lazy_app_ctr", "Label", 1, t1, pos1)),
                });

                match_sharedterm!(match (t2) {
                    Term::Record(record_data) => {
                        // due to a limitation of `match_sharedterm`: see the macro's
                        // documentation
                        let mut record_data = record_data;

                        let mut contract_at_field = |id: LocIdent| {
                            let pos = contract_term.pos;
                            mk_app!(
                                contract_term.clone(),
                                RichTerm::new(Term::Str(id.into()), id.pos)
                            )
                            .with_pos(pos)
                            .closurize(&mut self.cache, contract_env.clone())
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
                        let reverted = super::fixpoint::revert(&mut self.cache, record_data);

                        Ok(Closure {
                            body: RichTerm::new(reverted, pos2),
                            env: Environment::new(),
                        })
                    }
                    _ => Err(mk_type_error!("record_lazy_app_ctr", "Record", 2, t2, pos2)),
                })
            }
            BinaryOp::LabelWithMessage => {
                let t1 = t1.into_owned();
                let t2 = t2.into_owned();

                let Term::Str(message) = t1 else {
                    return Err(mk_type_error!(
                        "label_with_message",
                        "String",
                        1,
                        t1.into(),
                        pos1
                    ));
                };

                let Term::Lbl(label) = t2 else {
                    return Err(mk_type_error!(
                        "label_with_message",
                        "String",
                        2,
                        t2.into(),
                        pos2
                    ));
                };

                Ok(Closure::atomic_closure(RichTerm::new(
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
                    &self.cache,
                    RichTerm {
                        term: t1,
                        pos: pos1,
                    },
                    &Environment::new(),
                    &env1,
                );
                let t1 = t1_subst.term.into_owned();

                let Term::Array(array, _) = t1 else {
                    return Err(mk_type_error!(
                        "label_with_notes",
                        "Array",
                        1,
                        t1.into(),
                        pos1
                    ));
                };

                let notes = array
                    .into_iter()
                    .map(|element| {
                        let term = element.term.into_owned();

                        if let Term::Str(s) = term {
                            Ok(s.into_inner())
                        } else {
                            Err(mk_type_error!(
                                "label_with_notes",
                                "String",
                                1,
                                term.into(),
                                element.pos
                            ))
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let Term::Lbl(label) = t2 else {
                    return Err(mk_type_error!(
                        "label_with_notes",
                        "Label",
                        2,
                        t2.into(),
                        pos2
                    ));
                };

                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Lbl(label.with_diagnostic_notes(notes)),
                    pos_op_inh,
                )))
            }
            BinaryOp::LabelAppendNote => {
                let t1 = t1.into_owned();
                let t2 = t2.into_owned();

                let Term::Str(note) = t1 else {
                    return Err(mk_type_error!(
                        "label_append_note",
                        "String",
                        1,
                        t1.into(),
                        pos1
                    ));
                };

                let Term::Lbl(label) = t2 else {
                    return Err(mk_type_error!(
                        "label_append_note",
                        "Label",
                        2,
                        t2.into(),
                        pos2
                    ));
                };

                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Lbl(label.append_diagnostic_note(note.into_inner())),
                    pos2.into_inherited(),
                )))
            }
            BinaryOp::LabelLookupTypeVar => {
                let t1 = t1.into_owned();
                let t2 = t2.into_owned();

                let Term::SealingKey(key) = t1 else {
                    return Err(mk_type_error!(
                        "lookup_type_variable",
                        "SealingKey",
                        1,
                        t1.into(),
                        pos1
                    ));
                };

                let Term::Lbl(label) = t2 else {
                    return Err(mk_type_error!(
                        "lookup_type_variable",
                        "Label",
                        2,
                        t2.into(),
                        pos2
                    ));
                };

                Ok(Closure::atomic_closure(RichTerm::new(
                    label.type_environment.get(&key).unwrap().into(),
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
        args: Vec<(Closure, TermPos)>,
        pos_op: TermPos,
    ) -> Result<Closure, EvalError> {
        let pos_op_inh = pos_op.into_inherited();

        // Currently, for fixed arity primitive operators, the parser must ensure that they get
        // exactly the right number of argument: if it is not the case, this is a bug, and we panic.
        match n_op {
            NAryOp::StringReplace | NAryOp::StringReplaceRegex => {
                let mut args_wo_env = args
                    .into_iter()
                    .map(|(clos, pos)| (clos.body.term, clos.body.pos, pos));
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
                                .map_err(|err| EvalError::Other(err.to_string(), pos_op))?;

                            s.replace_regex(&CompiledRegex(re), to)
                        };

                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Str(result),
                            pos_op_inh,
                        )))
                    }
                    (Term::Str(_), Term::Str(_), _) => Err(EvalError::NAryPrimopTypeError {
                        primop: format!("{n_op}"),
                        expected: String::from("String"),
                        arg_number: 3,
                        arg_pos: thd_pos,
                        arg_evaluated: RichTerm {
                            term: thd,
                            pos: pos3,
                        },
                    }),
                    (Term::Str(_), _, _) => Err(EvalError::NAryPrimopTypeError {
                        primop: format!("{n_op}"),
                        expected: String::from("String"),
                        arg_number: 2,
                        arg_pos: snd_pos,
                        arg_evaluated: RichTerm {
                            term: snd,
                            pos: pos2,
                        },
                    }),
                    (_, _, _) => Err(EvalError::NAryPrimopTypeError {
                        primop: format!("{n_op}"),
                        expected: String::from("String"),
                        arg_number: 1,
                        arg_pos: fst_pos,
                        arg_evaluated: RichTerm {
                            term: fst,
                            pos: pos1,
                        },
                    }),
                }
            }
            NAryOp::StringSubstr => {
                let mut args_wo_env = args
                    .into_iter()
                    .map(|(clos, pos)| (clos.body.term, clos.body.pos, pos));
                let (fst, pos1, fst_pos) = args_wo_env.next().unwrap();
                let (snd, pos2, snd_pos) = args_wo_env.next().unwrap();
                let (thd, pos3, thd_pos) = args_wo_env.next().unwrap();
                debug_assert!(args_wo_env.next().is_none());

                match (&*fst, &*snd, &*thd) {
                    (Term::Str(s), Term::Num(start), Term::Num(end)) => s
                        .substring(start, end)
                        .map(|substr| {
                            Closure::atomic_closure(RichTerm::new(Term::Str(substr), pos_op_inh))
                        })
                        .map_err(|e| EvalError::Other(format!("{}", e), pos_op)),
                    (Term::Str(_), Term::Num(_), _) => Err(EvalError::NAryPrimopTypeError {
                        primop: String::from("str_subst"),
                        expected: String::from("String"),
                        arg_number: 3,
                        arg_pos: thd_pos,
                        arg_evaluated: RichTerm {
                            term: thd,
                            pos: pos3,
                        },
                    }),
                    (Term::Str(_), _, _) => Err(EvalError::NAryPrimopTypeError {
                        primop: String::from("str_subst"),
                        expected: String::from("String"),
                        arg_number: 2,
                        arg_pos: snd_pos,
                        arg_evaluated: RichTerm {
                            term: snd,
                            pos: pos2,
                        },
                    }),
                    (_, _, _) => Err(EvalError::NAryPrimopTypeError {
                        primop: String::from("str_substr"),
                        expected: String::from("String"),
                        arg_number: 1,
                        arg_pos: fst_pos,
                        arg_evaluated: RichTerm {
                            term: fst,
                            pos: pos1,
                        },
                    }),
                }
            }
            NAryOp::MergeContract => {
                let mut args_iter = args.into_iter();
                let (
                    Closure {
                        body: RichTerm { term: t1, pos: _ },
                        env: _,
                    },
                    _,
                ) = args_iter.next().unwrap();
                let (
                    Closure {
                        body:
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        env: env2,
                    },
                    _,
                ) = args_iter.next().unwrap();
                let (
                    Closure {
                        body:
                            RichTerm {
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
                            &mut self.cache,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                            env2,
                            RichTerm {
                                term: t3,
                                pos: pos3,
                            },
                            env3,
                            pos_op,
                            MergeMode::Contract(lbl),
                            &mut self.call_stack,
                        )
                    }
                    _ => Err(EvalError::InternalError(
                        format!(
                            "The MergeContract() operator was expecting \
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
                        body:
                            RichTerm {
                                term: a1,
                                pos: pos1,
                            },
                        ..
                    },
                    fst_pos,
                ) = args.next().unwrap();
                let (
                    Closure {
                        body:
                            RichTerm {
                                term: a2,
                                pos: pos2,
                            },
                        ..
                    },
                    snd_pos,
                ) = args.next().unwrap();
                let (
                    Closure {
                        body:
                            RichTerm {
                                term: a3,
                                pos: pos3,
                            },
                        env: env3,
                    },
                    thd_pos,
                ) = args.next().unwrap();
                let (
                    Closure {
                        body:
                            RichTerm {
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

                        let tail_closurized = RichTerm::from(Term::Record(tail.clone()))
                            .closurize(&mut self.cache, env4);
                        let fields = tail.fields.keys().map(|s| s.ident()).collect();
                        r.sealed_tail = Some(record::SealedTail::new(
                            *s,
                            label.clone(),
                            tail_closurized,
                            fields,
                        ));

                        let body = RichTerm::from(Term::Record(r));
                        Ok(Closure { body, env: env3 })
                    }
                    (Term::SealingKey(_), Term::Lbl(_), Term::Record(_), _) => {
                        Err(EvalError::NAryPrimopTypeError {
                            primop: String::from("record_seal_tail"),
                            expected: String::from("Record"),
                            arg_number: 4,
                            arg_pos: frth_pos,
                            arg_evaluated: RichTerm {
                                term: a4,
                                pos: pos4,
                            },
                        })
                    }
                    (Term::SealingKey(_), Term::Lbl(_), _, _) => {
                        Err(EvalError::NAryPrimopTypeError {
                            primop: String::from("record_seal_tail"),
                            expected: String::from("Record"),
                            arg_number: 3,
                            arg_pos: thd_pos,
                            arg_evaluated: RichTerm {
                                term: a3,
                                pos: pos3,
                            },
                        })
                    }
                    (Term::SealingKey(_), _, _, _) => Err(EvalError::NAryPrimopTypeError {
                        primop: String::from("record_seal_tail"),
                        expected: String::from("Label"),
                        arg_number: 2,
                        arg_pos: snd_pos,
                        arg_evaluated: RichTerm {
                            term: a2,
                            pos: pos2,
                        },
                    }),
                    (_, _, _, _) => Err(EvalError::NAryPrimopTypeError {
                        primop: String::from("record_seal_tail"),
                        expected: String::from("SealingKey"),
                        arg_number: 1,
                        arg_pos: fst_pos,
                        arg_evaluated: RichTerm {
                            term: a1,
                            pos: pos1,
                        },
                    }),
                }
            }
            NAryOp::RecordUnsealTail => {
                let mut args = args.into_iter();
                let (
                    Closure {
                        body:
                            RichTerm {
                                term: a1,
                                pos: pos1,
                            },
                        ..
                    },
                    fst_pos,
                ) = args.next().unwrap();
                let (
                    Closure {
                        body:
                            RichTerm {
                                term: a2,
                                pos: pos2,
                            },
                        ..
                    },
                    snd_pos,
                ) = args.next().unwrap();
                let (
                    Closure {
                        body:
                            RichTerm {
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
                        .ok_or_else(|| EvalError::BlameError {
                            evaluated_arg: l.get_evaluated_arg(&self.cache),
                            label: l.clone(),
                            call_stack: std::mem::take(&mut self.call_stack),
                        })
                        .map(|t| Closure { body: t, env: env3 }),
                    (Term::SealingKey(_), Term::Lbl(_), _) => Err(EvalError::NAryPrimopTypeError {
                        primop: String::from("record_unseal_tail"),
                        expected: String::from("Record"),
                        arg_number: 3,
                        arg_pos: thd_pos,
                        arg_evaluated: RichTerm {
                            term: a3,
                            pos: pos3,
                        },
                    }),
                    (Term::SealingKey(_), _, _) => Err(EvalError::NAryPrimopTypeError {
                        primop: String::from("record_unseal_tail"),
                        expected: String::from("Label"),
                        arg_number: 2,
                        arg_pos: snd_pos,
                        arg_evaluated: RichTerm {
                            term: a2,
                            pos: pos2,
                        },
                    }),
                    (_, _, _) => Err(EvalError::NAryPrimopTypeError {
                        primop: String::from("record_unseal_tail"),
                        expected: String::from("SealingKey"),
                        arg_number: 1,
                        arg_pos: fst_pos,
                        arg_evaluated: RichTerm {
                            term: a1,
                            pos: pos1,
                        },
                    }),
                }
            }
            NAryOp::LabelInsertTypeVar => {
                let mut args = args.into_iter();

                let (
                    Closure {
                        body:
                            RichTerm {
                                term: key,
                                pos: key_pos,
                            },
                        ..
                    },
                    pos1,
                ) = args.next().unwrap();

                let (
                    Closure {
                        body:
                            RichTerm {
                                term: polarity,
                                pos: polarity_pos,
                            },
                        ..
                    },
                    pos2,
                ) = args.next().unwrap();

                let (
                    Closure {
                        body:
                            RichTerm {
                                term: label,
                                pos: label_pos,
                            },
                        ..
                    },
                    pos3,
                ) = args.next().unwrap();
                debug_assert!(args.next().is_none());

                let Term::SealingKey(key) = *key else {
                    return Err(EvalError::NAryPrimopTypeError {
                        primop: String::from("insert_type_variable"),
                        expected: String::from("SealingKey"),
                        arg_number: 1,
                        arg_pos: key_pos,
                        arg_evaluated: RichTerm {
                            term: key,
                            pos: pos1,
                        },
                    });
                };

                let Ok(polarity) = Polarity::try_from(polarity.as_ref()) else {
                    return Err(EvalError::NAryPrimopTypeError {
                        primop: String::from("insert_type_variable"),
                        expected: String::from("Polarity"),
                        arg_number: 2,
                        arg_pos: polarity_pos,
                        arg_evaluated: RichTerm {
                            term: polarity,
                            pos: pos2,
                        },
                    });
                };

                let Term::Lbl(label) = &*label else {
                    return Err(EvalError::NAryPrimopTypeError {
                        primop: String::from("insert_type_variable"),
                        expected: String::from("Label"),
                        arg_number: 3,
                        arg_pos: label_pos,
                        arg_evaluated: RichTerm {
                            term: label,
                            pos: pos3,
                        },
                    });
                };

                let mut new_label = label.clone();
                new_label
                    .type_environment
                    .insert(key, TypeVarData { polarity });

                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Lbl(new_label),
                    pos2.into_inherited(),
                )))
            }
            NAryOp::ArraySlice => {
                let mut args = args.into_iter();

                let (
                    Closure {
                        body:
                            RichTerm {
                                term: t1,
                                pos: pos1,
                            },
                        ..
                    },
                    fst_pos,
                ) = args.next().unwrap();

                let (
                    Closure {
                        body:
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ..
                    },
                    snd_pos,
                ) = args.next().unwrap();

                let (
                    Closure {
                        body:
                            RichTerm {
                                term: t3,
                                pos: pos3,
                            },
                        env: env3,
                    },
                    third_pos,
                ) = args.next().unwrap();
                debug_assert!(args.next().is_none());

                let Term::Num(ref start) = &*t1 else {
                    return Err(EvalError::NAryPrimopTypeError {
                        primop: String::from("array_slice"),
                        expected: String::from("Number"),
                        arg_number: 1,
                        arg_pos: fst_pos,
                        arg_evaluated: RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    });
                };

                let Term::Num(ref end) = &*t2 else {
                    return Err(EvalError::NAryPrimopTypeError {
                        primop: String::from("array_slice"),
                        expected: String::from("Number"),
                        arg_number: 2,
                        arg_pos: snd_pos,
                        arg_evaluated: RichTerm {
                            term: t2,
                            pos: pos2,
                        },
                    });
                };

                let t3_owned = t3.into_owned();

                let Term::Array(mut array, attrs) = t3_owned else {
                    return Err(EvalError::NAryPrimopTypeError {
                        primop: String::from("array_slice"),
                        expected: String::from("Array"),
                        arg_number: 3,
                        arg_pos: third_pos,
                        arg_evaluated: RichTerm::new(t3_owned, pos3),
                    });
                };

                let Ok(start_as_usize) = usize::try_from(start) else {
                    return Err(EvalError::Other(
                        format!(
                            "array_slice expects its first argument (start) to be a \
                            positive integer smaller than {}, got {start}",
                            usize::MAX
                        ),
                        pos_op,
                    ));
                };

                let Ok(end_as_usize) = usize::try_from(end) else {
                    return Err(EvalError::Other(
                        format!(
                            "array_slice expects its second argument (end) to be a \
                            positive integer smaller than {}, got {end}",
                            usize::MAX
                        ),
                        pos_op,
                    ));
                };

                let result = array.slice(start_as_usize, end_as_usize);

                if let Err(OutOfBoundError) = result {
                    return Err(EvalError::Other(
                        format!(
                            "array_slice: index out of bounds. Expected `start <= end <= {}`, but \
                            got `start={start}` and `end={end}`.",
                            array.len()
                        ),
                        pos_op,
                    ));
                };

                Ok(Closure {
                    body: RichTerm::new(Term::Array(array, attrs), pos_op_inh),
                    env: env3,
                })
            }
        }
    }
}

/// A merge priority that can be recursively pushed down to the leafs of a record. Currently only
/// `default` (`Bottom`) and `force` (`Top`) can be recursive.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RecPriority {
    Bottom,
    Top,
}

impl From<RecPriority> for MergePriority {
    fn from(rec_prio: RecPriority) -> Self {
        match rec_prio {
            RecPriority::Top => MergePriority::Top,
            RecPriority::Bottom => MergePriority::Bottom,
        }
    }
}

impl RecPriority {
    /// Return the recursive priority operator corresponding to this priority (`$rec_force` or
    /// `$rec_default`) applied to the given term.
    pub fn apply_rec_prio_op(&self, rt: RichTerm) -> RichTerm {
        let pos = rt.pos;

        let op = match self {
            RecPriority::Top => internals::rec_force(),
            RecPriority::Bottom => internals::rec_default(),
        };

        mk_app!(op, rt).with_pos(pos)
    }

    /// Propagate the priority down the fields of a record.
    fn propagate_in_record<C: Cache>(
        &self,
        cache: &mut C,
        mut record: RecordData,
        env: &Environment,
        pos: TermPos,
    ) -> Closure {
        let mut new_env = Environment::new();

        let update_priority = |meta: &mut FieldMetadata| {
            if let MergePriority::Neutral = meta.priority {
                meta.priority = (*self).into();
            }
        };

        record.fields = record
            .fields
            .into_iter()
            .map(|(id, mut field)| {
                // There is a subtlety with respect to overriding here. Take:
                //
                // ```nickel
                // ({foo = bar + 1, bar = 1} | rec default) & {bar = 2}
                // ```
                //
                // In the example above, if we just map `$rec_default` on the value of `foo` and
                // closurize it into a new, normal cache element (non revertible), we lose the
                // ability to override `foo` and we end up with the unexpected result `{foo = 2, bar
                // = 2}`.
                //
                // What we want is that:
                //
                // ```nickel
                // {foo = bar + 1, bar = 1} | rec default
                // ```
                //
                // is equivalent to writing:
                //
                // ```nickel
                // {foo | default = bar + 1, bar | default = 1}
                // ```
                //
                // For revertible elements, we don't want to only map the push operator on the
                // current cached value, but also on the original expression.
                //
                // To do so, we create a new independent copy of the original element by mapping the
                // function over both expressions (in the sense of both the original expression and
                // the cached expression). This logic is encapsulated by
                // [crate::eval::cache::Cache::map_at_index].

                field.value = field.value.take().map(|value| {
                    if let Term::Var(id_inner) = value.as_ref() {
                        let idx = env.get(&id_inner.ident()).unwrap();

                        let new_idx =
                            cache.map_at_index(idx, |cache, inner| match inner.body.as_ref() {
                                Term::Record(record_data) => self.propagate_in_record(
                                    cache,
                                    record_data.clone(),
                                    &inner.env,
                                    pos,
                                ),
                                t if t.is_whnf() => {
                                    update_priority(&mut field.metadata);
                                    inner.clone()
                                }
                                _ => panic!("rec_priority: expected an evaluated form"),
                            });

                        let fresh_id = LocIdent::fresh();
                        new_env.insert(fresh_id.ident(), new_idx);
                        RichTerm::new(Term::Var(fresh_id), pos)
                    } else {
                        // A record field that doesn't contain a variable is a constant (a number,
                        // a string, etc.). It can't be a record, and we can thus update its
                        // priority without recursing further.
                        update_priority(&mut field.metadata);
                        value
                    }
                });

                (id, field)
            })
            .collect();

        Closure {
            body: RichTerm::new(Term::Record(record), pos),
            env: new_env,
        }
    }

    /// Push the priority into an evaluated expression.
    fn propagate_in_term<C: Cache>(
        &self,
        cache: &mut C,
        st: SharedTerm,
        env: Environment,
        pos: TermPos,
    ) -> Closure {
        match st.into_owned() {
            Term::Record(record_data) => self.propagate_in_record(cache, record_data, &env, pos),
            t => Closure {
                body: RichTerm::new(t, pos),
                env,
            },
        }
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
/// otherwise returns an [`EvalError`] indicating that the values cannot be compared.
fn eq<C: Cache>(
    cache: &mut C,
    c1: Closure,
    c2: Closure,
    pos_op: TermPos,
) -> Result<EqResult, EvalError> {
    let Closure {
        body: RichTerm {
            term: t1,
            pos: pos1,
        },
        env: env1,
    } = c1;
    let Closure {
        body: RichTerm {
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
        I: Iterator<Item = (RichTerm, RichTerm)>,
    {
        if let Some((t1, t2)) = it.next() {
            let eqs = it
                .map(|(t1, t2)| {
                    (
                        Closure {
                            body: t1,
                            env: env1.clone(),
                        },
                        Closure {
                            body: t2,
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

                            let value1_with_ctr = RuntimeContract::apply_all(
                                value1,
                                pending_contracts1.into_iter(),
                                pos1,
                            );
                            let value2_with_ctr = RuntimeContract::apply_all(
                                value2,
                                pending_contracts2.into_iter(),
                                pos2,
                            );
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

                            Some(Err(EvalError::MissingFieldDef {
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
            // record contrats with default values, wrapped terms, etc.

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
                                    body: t1,
                                    env: Environment::new(),
                                },
                                Closure {
                                    body: t2,
                                    env: Environment::new(),
                                },
                            )
                        })
                        .collect::<Vec<_>>();

                    Ok(EqResult::Eqs(t1, t2, eqs))
                }
            }
        }
        (Term::Fun(i, rt), _) => Err(EvalError::EqError {
            eq_pos: pos_op,
            term: RichTerm::new(Term::Fun(i, rt), pos1),
        }),
        (_, Term::Fun(i, rt)) => Err(EvalError::EqError {
            eq_pos: pos_op,
            term: RichTerm::new(Term::Fun(i, rt), pos2),
        }),
        (Term::ForeignId(v), _) => Err(EvalError::EqError {
            eq_pos: pos_op,
            term: RichTerm::new(Term::ForeignId(v), pos1),
        }),
        (_, Term::ForeignId(v)) => Err(EvalError::EqError {
            eq_pos: pos_op,
            term: RichTerm::new(Term::ForeignId(v), pos2),
        }),
        (_, _) => Ok(EqResult::Bool(false)),
    }
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
        F: FnMut(LocIdent, RichTerm) -> RichTerm;
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
        F: FnMut(LocIdent, RichTerm) -> RichTerm,
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
    use crate::cache::resolvers::DummyResolver;
    use crate::eval::cache::CacheImpl;
    use crate::eval::Environment;

    #[test]
    fn ite_operation() {
        let cont: OperationCont = OperationCont::Op1(UnaryOp::IfThenElse, TermPos::None);
        let mut vm: VirtualMachine<DummyResolver, CacheImpl> =
            VirtualMachine::new(DummyResolver {}, std::io::sink());

        vm.stack
            .push_arg(Closure::atomic_closure(mk_term::integer(5)), TermPos::None);
        vm.stack
            .push_arg(Closure::atomic_closure(mk_term::integer(46)), TermPos::None);

        let mut clos = Closure {
            body: Term::Bool(true).into(),
            env: Environment::new(),
        };

        vm.stack.push_op_cont(cont, 0, TermPos::None);

        clos = vm.continuate_operation(clos).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: mk_term::integer(46),
                env: Environment::new()
            }
        );
        assert_eq!(0, vm.stack.count_args());
    }

    #[test]
    fn plus_first_term_operation() {
        let cont = OperationCont::Op2First(
            BinaryOp::Plus,
            Closure {
                body: mk_term::integer(6),
                env: Environment::new(),
            },
            TermPos::None,
        );

        let mut clos = Closure {
            body: mk_term::integer(7),
            env: Environment::new(),
        };
        let mut vm = VirtualMachine::new(DummyResolver {}, std::io::sink());
        vm.stack.push_op_cont(cont, 0, TermPos::None);

        clos = vm.continuate_operation(clos).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: mk_term::integer(6),
                env: Environment::new()
            }
        );

        assert_eq!(1, vm.stack.count_conts());
        assert_eq!(
            (
                OperationCont::Op2Second(
                    BinaryOp::Plus,
                    Closure {
                        body: mk_term::integer(7),
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
    }

    #[test]
    fn plus_second_term_operation() {
        let cont: OperationCont = OperationCont::Op2Second(
            BinaryOp::Plus,
            Closure {
                body: mk_term::integer(7),
                env: Environment::new(),
            },
            TermPos::None,
            TermPos::None,
        );

        let mut vm: VirtualMachine<DummyResolver, CacheImpl> =
            VirtualMachine::new(DummyResolver {}, std::io::sink());
        let mut clos = Closure {
            body: mk_term::integer(6),
            env: Environment::new(),
        };
        vm.stack.push_op_cont(cont, 0, TermPos::None);

        clos = vm.continuate_operation(clos).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: mk_term::integer(13),
                env: Environment::new()
            }
        );
    }
}
