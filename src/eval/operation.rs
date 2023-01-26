//! Implementation of primitive operations.
//!
//! Define functions which perform the evaluation of primitive operators. The machinery required
//! for the strict evaluation of the operands is mainly handled by [crate::eval], and marginally in
//! [`VirtualMachine::continuate_operation`].
//!
//! On the other hand, the functions `process_unary_operation` and `process_binary_operation`
//! receive evaluated operands and implement the actual semantics of operators.
use super::{
    callstack, merge,
    merge::{merge, MergeMode},
    subst, Cache, Closure, Environment, ImportResolver, VirtualMachine,
};

use crate::{
    error::{EvalError, IllegalPolymorphicTailAction},
    identifier::Ident,
    label::ty_path,
    match_sharedterm, mk_app, mk_fun, mk_opn, mk_record,
    position::TermPos,
    serialize,
    serialize::ExportFormat,
    stdlib::internals,
    term::{
        array::{Array, ArrayAttrs},
        make as mk_term,
        record::{self, Field, FieldMetadata, RecordData},
        BinaryOp, MergePriority, NAryOp, PendingContract, RecordExtKind, RichTerm, SharedTerm,
        StrChunk, Term, UnaryOp,
    },
    transform::{apply_contracts::apply_contracts, Closurizable},
};

use md5::digest::Digest;

use simple_counter::*;
use unicode_segmentation::UnicodeSegmentation;

use std::{collections::HashMap, iter::Extend, rc::Rc};

generate_counter!(FreshVariableCounter, usize);

/// Result of the equality of two terms.
///
/// The equality of two terms can either be computed directly for base types (`Num`, `Str`, etc.),
/// in which case `Bool` is returned. Otherwise, composite values such as arrays or records generate
/// new subequalities, as represented by the last variant as a vector of pairs of terms.  This list
/// should be non-empty (it if was empty, `eq` should have returned `Bool(true)` directly).  The
/// first element of this non-empty list is encoded as the two first parameters of `Eqs`, while the
/// last vector parameter is the (potentially empty) tail.
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
            OperationCont::Op1(op, _) => write!(f, "Op1 {:?}", op),
            OperationCont::Op2First(op, _, _) => write!(f, "Op2First {:?}", op),
            OperationCont::Op2Second(op, _, _, _) => write!(f, "Op2Second {:?}", op),
            OperationCont::OpN { op, .. } => write!(f, "OpN {:?}", op),
        }
    }
}

impl<R: ImportResolver, C: Cache> VirtualMachine<R, C> {
    /// Process to the next step of the evaluation of an operation.
    ///
    /// Depending on the content of the stack, it either starts the evaluation of the first argument,
    /// starts the evaluation of the second argument, or finally process with the operation if both
    /// arguments are evaluated (for binary operators).
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
            mut env,
        } = clos;
        let pos_op_inh = pos_op.into_inherited();

        match u_op {
            UnaryOp::Ite() => {
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
                    Err(EvalError::TypeError(
                        String::from("Bool"),
                        String::from("if"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::Typeof() => {
                let result = match *t {
                    Term::Num(_) => "Num",
                    Term::Bool(_) => "Bool",
                    Term::Str(_) => "Str",
                    Term::Enum(_) => "Enum",
                    Term::Fun(..) | Term::Match { .. } => "Fun",
                    Term::Array(..) => "Array",
                    Term::Record(..) | Term::RecRecord(..) => "Record",
                    Term::Lbl(..) => "Lbl",
                    _ => "Other",
                };
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Enum(Ident::from(result)),
                    pos_op_inh,
                )))
            }
            UnaryOp::BoolAnd() =>
            // The syntax should not allow partially applied boolean operators.
            {
                if let Some((next, ..)) = self.stack.pop_arg(&self.cache) {
                    match &*t {
                        Term::Bool(true) => Ok(next),
                        // FIXME: this does not check that the second argument is actually a boolean.
                        // This means `true && 2` silently evaluates to `2`. This is simpler and more
                        // efficient, but can make debugging harder. In any case, it should be solved
                        // only once primary operators have better support for laziness in some
                        // arguments.
                        Term::Bool(false) => Ok(Closure::atomic_closure(RichTerm {
                            term: t,
                            pos: pos_op_inh,
                        })),
                        _ => Err(EvalError::TypeError(
                            String::from("Bool"),
                            String::from("&&"),
                            arg_pos,
                            RichTerm { term: t, pos },
                        )),
                    }
                } else {
                    Err(EvalError::NotEnoughArgs(2, String::from("&&"), pos_op))
                }
            }
            UnaryOp::BoolOr() => {
                if let Some((next, ..)) = self.stack.pop_arg(&self.cache) {
                    match &*t {
                        Term::Bool(true) => Ok(Closure::atomic_closure(RichTerm {
                            term: t,
                            pos: pos_op_inh,
                        })),
                        // FIXME: this does not check that the second argument is actually a boolean.
                        // This means `false || 2` silently evaluates to `2`. This is simpler and more
                        // efficient, but can make debugging harder. In any case, it should be solved
                        // only once primary operators have better support for laziness in some
                        // arguments.
                        Term::Bool(false) => Ok(next),
                        _ => Err(EvalError::TypeError(
                            String::from("Bool"),
                            String::from("||"),
                            arg_pos,
                            RichTerm { term: t, pos },
                        )),
                    }
                } else {
                    Err(EvalError::NotEnoughArgs(2, String::from("||"), pos_op))
                }
            }
            UnaryOp::BoolNot() => {
                if let Term::Bool(b) = *t {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Bool(!b),
                        pos_op_inh,
                    )))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Bool"),
                        String::from("!"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::Blame() => match_sharedterm! { t, with {
                    Term::Lbl(label) => Err(
                        EvalError::BlameError {
                            evaluated_arg: label.get_evaluated_arg(&self.cache),
                            label,
                            call_stack: std::mem::take(&mut self.call_stack),
                        }),
                } else
                    Err(EvalError::TypeError(
                        String::from("Label"),
                        String::from("blame"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
            },
            UnaryOp::Embed(_id) => {
                if let Term::Enum(_) = &*t {
                    Ok(Closure::atomic_closure(RichTerm {
                        term: t,
                        pos: pos_op_inh,
                    }))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Enum"),
                        String::from("embed"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::Match { has_default } => {
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
                        _ => panic!("invalid argument for match"),
                    };

                    cases
                        .remove(en)
                        .map(|field| Closure {
                            // The record containing the match cases, as well as the match primop
                            // itself, aren't accessible in the surface language. They are
                            // generated by the interpreter, and should never contain field without
                            // definition.
                            body: field.value.expect("match cases must have a definition"),
                            env: cases_env,
                        })
                        .or(default)
                        .ok_or_else(||
                        // ? We should have a dedicated error for unmatched pattern
                        EvalError::TypeError(
                            String::from("Enum"),
                            String::from("match"),
                            arg_pos,
                            RichTerm {
                                term: t,
                                pos,
                            },
                        ))
                } else if let Some(clos) = default {
                    Ok(clos)
                } else {
                    Err(EvalError::TypeError(
                        String::from("Enum"),
                        String::from("match"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::ChangePolarity() => match_sharedterm! {t, with {
                    Term::Lbl(l) => {
                        let mut l = l;
                        l.polarity = !l.polarity;
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Lbl(l),
                            pos_op_inh,
                        )))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Label"),
                        String::from("changePolarity"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            },
            UnaryOp::Pol() => {
                if let Term::Lbl(l) = &*t {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Bool(l.polarity),
                        pos_op_inh,
                    )))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Label"),
                        String::from("polarity"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::GoDom() => match_sharedterm! {t, with {
                    Term::Lbl(l) => {
                        let mut l = l;
                        l.path.push(ty_path::Elem::Domain);
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Lbl(l),
                            pos_op_inh,
                        )))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Label"),
                        String::from("goDom"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            },
            UnaryOp::GoCodom() => match_sharedterm! {t, with {
                    Term::Lbl(l) => {
                        let mut l = l;
                        l.path.push(ty_path::Elem::Codomain);
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Lbl(l),
                            pos_op_inh,
                        )))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Label"),
                        String::from("goCodom"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            },
            UnaryOp::GoArray() => match_sharedterm! {t, with {
                    Term::Lbl(l) => {
                        let mut l = l;
                        l.path.push(ty_path::Elem::Array);
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Lbl(l),
                            pos_op_inh,
                        )))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Label"),
                        String::from("go_array"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            },
            UnaryOp::StaticAccess(id) => {
                if let Term::Record(record) = &*t {
                    match record.fields.get(&id) {
                        Some(Field {
                            value: Some(value), ..
                        }) => {
                            self.call_stack.enter_field(id, pos, value.pos, pos_op);
                            Ok(Closure {
                                body: value.clone(),
                                env,
                            })
                        }
                        Some(Field {
                            value: None,
                            metadata,
                        }) => Err(EvalError::MissingFieldDef {
                            id,
                            metadata: metadata.clone(),
                            pos_record: pos,
                            pos_access: pos_op,
                        }),
                        None => Err(EvalError::FieldMissing(
                            id.into_label(),
                            String::from("(.)"),
                            RichTerm { term: t, pos },
                            pos_op,
                        )), //TODO include the position of operators on the stack
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Record"),
                        String::from("field access"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::FieldsOf() => match_sharedterm! {t, with {
                    Term::Record(record) => {
                        let mut fields: Vec<String> = record
                            .fields
                            .into_iter()
                            .filter_map(|(id, field)| (!field.metadata.opt).then(|| id.to_string()))
                            // Ignore optional fields without definitions.
                            .collect();
                        fields.sort();
                        let terms = fields.into_iter().map(mk_term::string).collect();

                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Array(terms, ArrayAttrs::new().closurized()),
                            pos_op_inh,
                        )))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Record"),
                        String::from("fields"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            },
            UnaryOp::ValuesOf() => match_sharedterm! {t, with {
                    Term::Record(record) => {
                        let mut values = record
                            .into_iter_without_opts()
                            .collect::<Result<Vec<_>, _>>()
                            .map_err(|missing_def_err| missing_def_err.into_eval_err(pos, pos_op))?;

                        // Although it seems that sort_by_key would be easier here, it would actually
                        // require to copy the identifiers because of the lack of HKT. See
                        // https://github.com/rust-lang/rust/issues/34162.
                        values.sort_by(|(id1, _), (id2, _)| id1.cmp(id2));
                        let terms = values.into_iter().map(|(_, value)| value).collect();

                        Ok(Closure {
                            // TODO: once sure that the Record is properly closurized, we can
                            // safely assume that the extracted array here is, in turn, also closuried.
                            body: RichTerm::new(Term::Array(terms, ArrayAttrs::default()), pos_op_inh),
                            env,
                        })
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Record"),
                        String::from("valuesOf"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            },
            UnaryOp::ArrayMap() => {
                let (f, ..) = self
                    .stack
                    .pop_arg(&self.cache)
                    .ok_or_else(|| EvalError::NotEnoughArgs(2, String::from("map"), pos_op))?;
                match_sharedterm! {t, with {
                        Term::Array(ts, attrs) => {
                            let mut shared_env = Environment::new();
                            let f_as_var = f.body.closurize(&mut self.cache, &mut env, f.env);

                            // Array elements are closurized to preserve lazyness of data structures. It
                            // maintains the invariant that any data structure only contain thunks (that is,
                            // currently, variables).
                            let ts = ts
                                .into_iter()
                                .map(|t| {
                                    let t_with_ctrs = apply_contracts(
                                        t,
                                        attrs.pending_contracts.iter().cloned(),
                                        pos.into_inherited(),
                                    );

                                    RichTerm::new(Term::App(f_as_var.clone(), t_with_ctrs), pos_op_inh)
                                        .closurize(&mut self.cache, &mut shared_env, env.clone())
                                })
                                .collect();

                            Ok(Closure {
                                body: RichTerm::new(Term::Array(ts, attrs.contracts_cleared().closurized()), pos_op_inh),
                                env: shared_env,
                            })
                        }
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Array"),
                            String::from("map, 2nd argument"),
                            arg_pos,
                            RichTerm { term: t, pos },
                        ))
                    }
                }
            }
            UnaryOp::ArrayGen() => {
                let (f, _) = self
                    .stack
                    .pop_arg(&self.cache)
                    .ok_or_else(|| EvalError::NotEnoughArgs(2, String::from("generate"), pos_op))?;

                if let Term::Num(n) = *t {
                    let n_int = n as usize;
                    if n < 0.0 || n.fract() != 0.0 {
                        Err(EvalError::Other(
                            format!(
                            "generate: expected the 1st agument to be a positive integer, got {}",
                            n
                        ),
                            pos_op,
                        ))
                    } else {
                        let mut shared_env = Environment::new();
                        let f_as_var = f.body.closurize(&mut self.cache, &mut env, f.env);

                        // Array elements are closurized to preserve lazyness of data structures. It
                        // maintains the invariant that any data structure only contain thunks (that is,
                        // currently, variables).
                        let ts = (0..n_int)
                            .map(|n| {
                                mk_app!(f_as_var.clone(), Term::Num(n as f64)).closurize(
                                    &mut self.cache,
                                    &mut shared_env,
                                    env.clone(),
                                )
                            })
                            .collect();

                        Ok(Closure {
                            body: RichTerm::new(
                                Term::Array(ts, ArrayAttrs::new().closurized()),
                                pos_op_inh,
                            ),
                            env: shared_env,
                        })
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Num"),
                        String::from("generate, 1st argument"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::RecordMap() => {
                let (f, ..) = self.stack.pop_arg(&self.cache).ok_or_else(|| {
                    EvalError::NotEnoughArgs(2, String::from("recordMap"), pos_op)
                })?;

                match_sharedterm! {t, with {
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
                                })
                            }

                            let mut shared_env = Environment::new();
                            let f_as_var = f.body.closurize(&mut self.cache, &mut env, f.env);

                            // As for `ArrayMap` (see above), we closurize the content of fields
                            let record = record.map_fields_without_optionals(&mut self.cache, &mut shared_env, &env, |id, t| {
                                let pos = t.pos.into_inherited();

                                mk_app!(f_as_var.clone(), mk_term::string(id.label()), t)
                                    .with_pos(pos)
                            }).map_err(|missing_field_err| missing_field_err.into_eval_err(pos, pos_op))?;

                            Ok(Closure {
                                body: RichTerm::new(Term::Record(record), pos_op_inh),
                                env: shared_env,
                            })
                        }
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Record"),
                            String::from("map on record"),
                            arg_pos,
                            RichTerm { term: t, pos },
                        ))
                    }
                }
            }
            UnaryOp::Seq() => self
                .stack
                .pop_arg(&self.cache)
                .map(|(next, ..)| next)
                .ok_or_else(|| EvalError::NotEnoughArgs(2, String::from("seq"), pos_op)),
            UnaryOp::DeepSeq(_) => {
                /// Build a RichTerm that forces a given list of terms, and at the end resumes the
                /// evaluation of the argument on the top of the stack. The argument must iterate over
                /// a tuple, which first element is an optional call stack element to add to the
                /// callstack before starting evaluation. This is a temporary fix to have reasonable
                /// missing definition error when deepsequing a record.
                ///
                /// Requires its first argument to be non-empty.
                fn seq_terms<I>(mut it: I, pos_op_inh: TermPos) -> RichTerm
                where
                    I: Iterator<Item = (Option<callstack::StackElem>, RichTerm)>,
                {
                    let (first_elem, first) = it
                        .next()
                        .expect("expected the argument to be a non-empty iterator");

                    it.fold(
                        mk_term::op1(UnaryOp::DeepSeq(first_elem), first).with_pos(pos_op_inh),
                        |acc, (elem, t)| {
                            mk_app!(mk_term::op1(UnaryOp::DeepSeq(elem), t), acc)
                                .with_pos(pos_op_inh)
                        },
                    )
                }

                match t.into_owned() {
                    Term::Record(record) if !record.fields.is_empty() => {
                        let pos_record = pos;
                        let pos_access = pos_op;
                        let defined = record
                            .into_iter_without_opts()
                            .collect::<Result<Vec<_>, _>>()
                            .map_err(|missing_def_err| {
                                missing_def_err.into_eval_err(pos, pos_op)
                            })?;

                        let terms = defined.into_iter().map(|(id, value)| {
                            (
                                Some(callstack::StackElem::Field {
                                    id,
                                    pos_record,
                                    pos_field: value.pos,
                                    pos_access,
                                }),
                                value,
                            )
                        });

                        Ok(Closure {
                            body: seq_terms(terms, pos_op),
                            env,
                        })
                    }
                    Term::Array(ts, attrs) if !ts.is_empty() => {
                        let mut shared_env = Environment::new();
                        let terms =
                            seq_terms(
                                ts.into_iter().map(|t| {
                                    let t_with_ctr = apply_contracts(
                                        t,
                                        attrs.pending_contracts.iter().cloned(),
                                        pos.into_inherited(),
                                    )
                                    .closurize(&mut self.cache, &mut shared_env, env.clone());
                                    (None, t_with_ctr)
                                }),
                                pos_op,
                            );

                        Ok(Closure {
                            body: terms,
                            env: shared_env,
                        })
                    }
                    _ => {
                        if let Some((next, ..)) = self.stack.pop_arg(&self.cache) {
                            Ok(next)
                        } else {
                            Err(EvalError::NotEnoughArgs(2, String::from("deepSeq"), pos_op))
                        }
                    }
                }
            }
            UnaryOp::ArrayHead() => {
                if let Term::Array(ts, attrs) = &*t {
                    if let Some(head) = ts.get(0) {
                        let head_with_ctr = apply_contracts(
                            head.clone(),
                            attrs.pending_contracts.iter().cloned(),
                            pos.into_inherited(),
                        );

                        Ok(Closure {
                            body: head_with_ctr,
                            env,
                        })
                    } else {
                        Err(EvalError::Other(String::from("head: empty array"), pos_op))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Array"),
                        String::from("head"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::ArrayTail() => match_sharedterm! {t, with {
                        Term::Array(ts, attrs) => {
                            if !ts.is_empty() {
                                Ok(Closure {
                                    body: RichTerm::new(Term::Array(ts.advance_by(1), attrs), pos_op_inh),
                                    env,
                                })
                            } else {
                                Err(EvalError::Other(String::from("tail: empty array"), pos_op))
                            }
                        }
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Array"),
                            String::from("tail"),
                            arg_pos,
                            RichTerm { term: t, pos },
                        ))
                    }
            },
            UnaryOp::ArrayLength() => {
                if let Term::Array(ts, _) = &*t {
                    // A num does not have any free variable so we can drop the environment
                    Ok(Closure {
                        body: RichTerm::new(Term::Num(ts.len() as f64), pos_op_inh),
                        env: Environment::new(),
                    })
                } else {
                    Err(EvalError::TypeError(
                        String::from("Array"),
                        String::from("length"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::ChunksConcat() => {
                let (mut acc, indent, env_chunks) = self.stack.pop_str_acc().unwrap();

                if let Term::Str(s) = &*t {
                    let s = if indent != 0 {
                        let indent_str: String = std::iter::once('\n')
                            .chain((0..indent).map(|_| ' '))
                            .collect();
                        s.replace('\n', &indent_str)
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
                        self.stack.push_str_acc(acc, indent, env_chunks.clone());

                        Ok(Closure {
                            body: RichTerm::new(Term::Op1(UnaryOp::ChunksConcat(), e), pos_op_inh),
                            env: env_chunks,
                        })
                    } else {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Str(acc),
                            pos_op_inh,
                        )))
                    }
                } else {
                    // Since the error halts the evaluation, we don't bother cleaning the stack of the
                    // remaining string chunks.
                    Err(EvalError::TypeError(
                        String::from("String"),
                        String::from("interpolated string"),
                        pos_op,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::StrTrim() => {
                if let Term::Str(s) = &*t {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Str(String::from(s.trim())),
                        pos_op_inh,
                    )))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("trim"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::StrChars() => {
                if let Term::Str(s) = &*t {
                    let ts = s
                        .chars()
                        .map(|c| RichTerm::from(Term::Str(c.to_string())))
                        .collect();

                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Array(ts, ArrayAttrs::new().closurized()),
                        pos_op_inh,
                    )))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("chars"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::CharCode() => {
                if let Term::Str(s) = &*t {
                    if s.len() == 1 {
                        let code = (s.chars().next().unwrap() as u32) as f64;
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Num(code),
                            pos_op_inh,
                        )))
                    } else {
                        Err(EvalError::Other(
                            format!("charCode: expected 1-char string, got `{}`", s.len()),
                            pos,
                        ))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("charCode"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::CharFromCode() => {
                if let Term::Num(code) = *t {
                    if code.fract() != 0.0 {
                        Err(EvalError::Other(format!("charFromCode: expected the agument to be an integer, got the floating-point value {}", code), pos_op))
                    } else if code < 0.0 || code > (u32::MAX as f64) {
                        Err(EvalError::Other(format!("charFromCode: code out of bounds. Expected a value between 0 and {}, got {}", u32::MAX, code), pos_op))
                    } else if let Some(car) = std::char::from_u32(code as u32) {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Str(String::from(car)),
                            pos_op_inh,
                        )))
                    } else {
                        Err(EvalError::Other(
                            format!("charFromCode: invalid character code {}", code),
                            pos_op,
                        ))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Num"),
                        String::from("charFromCode"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::StrUppercase() => {
                if let Term::Str(s) = &*t {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Str(s.to_uppercase()),
                        pos_op_inh,
                    )))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("strUppercase"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::StrLowercase() => {
                if let Term::Str(s) = &*t {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Str(s.to_lowercase()),
                        pos_op_inh,
                    )))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("strLowercase"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::StrLength() => {
                if let Term::Str(s) = &*t {
                    let length = s.graphemes(true).count();
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Num(length as f64),
                        pos_op_inh,
                    )))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("strLength"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::ToStr() => {
                let result = match_sharedterm! {t, with {
                    Term::Num(n) => Ok(Term::Str(n.to_string())),
                    Term::Str(s) => Ok(Term::Str(s)),
                    Term::Bool(b) => Ok(Term::Str(b.to_string())),
                    Term::Enum(id) => Ok(Term::Str(id.to_string())),
                    Term::Null => Ok(Term::Str(String::from("null"))),
                } else {
                    Err(EvalError::Other(
                        format!(
                            "strFrom: can't convert the argument of type {} to string",
                            t.type_of().unwrap()
                        ),
                        pos,
                    ))
                }}?;
                Ok(Closure::atomic_closure(RichTerm::new(result, pos_op_inh)))
            }
            UnaryOp::NumFromStr() => {
                if let Term::Str(s) = &*t {
                    let n = s.parse::<f64>().map_err(|_| {
                        EvalError::Other(format!("numFrom: invalid num literal `{}`", s), pos)
                    })?;
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Num(n),
                        pos_op_inh,
                    )))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("strLength"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::EnumFromStr() => {
                if let Term::Str(s) = &*t {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Enum(s.into()),
                        pos_op_inh,
                    )))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("strLength"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::StrIsMatch() => {
                if let Term::Str(s) = &*t {
                    let re = regex::Regex::new(s)
                        .map_err(|err| EvalError::Other(err.to_string(), pos_op))?;

                    let param = Ident::fresh();
                    let matcher = Term::Fun(
                        param,
                        RichTerm::new(
                            Term::Op1(
                                UnaryOp::StrIsMatchCompiled(re.into()),
                                RichTerm::new(Term::Var(param), pos_op_inh),
                            ),
                            pos_op_inh,
                        ),
                    );

                    Ok(Closure::atomic_closure(RichTerm::new(matcher, pos)))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("str_is_match"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::StrFind() => {
                if let Term::Str(s) = &*t {
                    let re = regex::Regex::new(s)
                        .map_err(|err| EvalError::Other(err.to_string(), pos_op))?;

                    let param = Ident::fresh();
                    let matcher = Term::Fun(
                        param,
                        RichTerm::new(
                            Term::Op1(
                                UnaryOp::StrFindCompiled(re.into()),
                                RichTerm::new(Term::Var(param), pos_op_inh),
                            ),
                            pos_op_inh,
                        ),
                    );

                    Ok(Closure::atomic_closure(RichTerm::new(matcher, pos)))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("str_match"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::StrIsMatchCompiled(regex) => {
                if let Term::Str(s) = &*t {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Bool(regex.is_match(s)),
                        pos_op_inh,
                    )))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("str_is_match_compiled"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::StrFindCompiled(regex) => {
                if let Term::Str(s) = &*t {
                    let capt = regex.captures(s);
                    let result = if let Some(capt) = capt {
                        let first_match = capt.get(0).unwrap();
                        let groups = capt
                            .iter()
                            .skip(1)
                            .filter_map(|s_opt| {
                                s_opt.map(|s| RichTerm::from(Term::Str(String::from(s.as_str()))))
                            })
                            .collect();

                        mk_record!(
                            ("matched", Term::Str(String::from(first_match.as_str()))),
                            ("index", Term::Num(first_match.start() as f64)),
                            (
                                "groups",
                                Term::Array(groups, ArrayAttrs::new().closurized())
                            )
                        )
                    } else {
                        //FIXME: what should we return when there's no match?
                        mk_record!(
                            ("matched", Term::Str(String::new())),
                            ("index", Term::Num(-1.)),
                            (
                                "groups",
                                Term::Array(Array::default(), ArrayAttrs::default())
                            )
                        )
                    };

                    Ok(Closure::atomic_closure(result))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("str_match_compiled"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }
            }
            UnaryOp::Force(_) => {
                /// `Seq` the `terms` iterator and then resume evaluating the `cont` continuation.
                fn seq_terms<I>(terms: I, pos: TermPos, cont: RichTerm) -> RichTerm
                where
                    I: Iterator<Item = RichTerm>,
                {
                    terms
                        .fold(cont, |acc, t| mk_app!(mk_term::op1(UnaryOp::Seq(), t), acc))
                        .with_pos(pos)
                }

                match_sharedterm! {t,
                    with {
                        Term::Record(record) if !record.fields.is_empty() => {
                            let mut shared_env = Environment::new();

                            let record = record.map_fields_without_optionals(&mut self.cache, &mut shared_env, &env, |id, t| {
                                let stack_elem = Some(callstack::StackElem::Field {
                                    id,
                                    pos_record: pos,
                                    pos_field: t.pos,
                                    pos_access: pos_op,
                                });

                                mk_term::op1(UnaryOp::Force(stack_elem), t)
                            }).map_err(|missing_field_err| missing_field_err.into_eval_err(pos, pos_op))?;

                            // unwrap: the call to map_fields_without_optionals must ensure that
                            // the fields all have a definition, so `field.value` must be `Some`.
                            let terms = record.fields.clone().into_values().map(|field| field.value.unwrap());
                            let cont = RichTerm::new(Term::Record(record), pos.into_inherited());

                            Ok(Closure {
                                body: seq_terms(terms, pos_op, cont),
                                env: shared_env,
                            })
                        },
                        Term::Array(ts, attrs) if !ts.is_empty() => {
                            let mut shared_env = Environment::new();
                            let ts = ts
                                .into_iter()
                                .map(|t| {
                                    mk_term::op1(
                                        UnaryOp::Force(None),
                                        apply_contracts(
                                            t,
                                            attrs.pending_contracts.iter().cloned(),
                                            pos.into_inherited(),
                                        ),
                                    )
                                    .closurize(&mut self.cache, &mut shared_env, env.clone())
                                })
                                // It's important to collect here, otherwise the two usages below
                                // will each do their own .closurize(...) calls and end up with
                                // different variables, which means that `cont` won't be properly updated.
                                .collect::<Array>();

                            let terms = ts.clone().into_iter();
                            let cont = RichTerm::new(Term::Array(ts, attrs), pos.into_inherited());

                            Ok(Closure {
                                body: seq_terms(terms, pos_op, cont),
                                env: shared_env,
                            })
                        }
                    } else Ok(Closure {
                        body: RichTerm { term : t, pos},
                        env
                    })
                }
            }
            UnaryOp::RecDefault() => {
                Ok(RecPriority::Bottom.propagate_in_term(&mut self.cache, t, env, pos))
            }
            UnaryOp::RecForce() => {
                Ok(RecPriority::Top.propagate_in_term(&mut self.cache, t, env, pos))
            }
            UnaryOp::RecordEmptyWithTail() => match_sharedterm! { t,
                with {
                    Term::Record(r) => {
                        let mut empty = RecordData::empty();
                        empty.sealed_tail = r.sealed_tail;
                        Ok(Closure {
                            body: RichTerm::new(Term::Record(empty), pos_op.into_inherited()),
                            env
                        })
                    },
                } else {
                    Err(EvalError::TypeError(
                        String::from("Record"),
                        String::from("%record_empty_with_tail%, 1st arg"),
                        arg_pos,
                        RichTerm { term: t, pos }
                    ))
                }
            },
            UnaryOp::Trace() => {
                if let Term::Str(s) = &*t {
                    eprintln!("builtin.trace: {s}");
                    Ok(())
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("trace"),
                        arg_pos,
                        RichTerm { term: t, pos },
                    ))
                }?;

                self.stack
                    .pop_arg(&self.cache)
                    .map(|(next, ..)| next)
                    .ok_or_else(|| EvalError::NotEnoughArgs(2, String::from("trace"), pos_op))
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
            env: mut env2,
        } = clos;
        let pos_op_inh = pos_op.into_inherited();

        match b_op {
            BinaryOp::Seal() => {
                if let Term::SealingKey(s) = &*t1 {
                    if let Term::Lbl(lbl) = &*t2 {
                        Ok(Closure::atomic_closure(
                            mk_fun!("x", Term::Sealed(*s, mk_term::var("x"), lbl.clone()))
                                .with_pos(pos_op_inh),
                        ))
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Lbl"),
                            String::from("%seal%, 2nd argument"),
                            snd_pos,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Sym"),
                        String::from("%seal%, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            }
            BinaryOp::Plus() => {
                if let Term::Num(n1) = *t1 {
                    if let Term::Num(n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Num(n1 + n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Num"),
                            String::from("+, 2nd argument"),
                            snd_pos,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Num"),
                        String::from("+, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            }
            BinaryOp::Sub() => {
                if let Term::Num(n1) = *t1 {
                    if let Term::Num(n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Num(n1 - n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Num"),
                            String::from("-, 2nd argument"),
                            snd_pos,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Num"),
                        String::from("-, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            }
            BinaryOp::Mult() => {
                if let Term::Num(n1) = *t1 {
                    if let Term::Num(n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Num(n1 * n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Num"),
                            String::from("*, 2nd argument"),
                            snd_pos,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Num"),
                        String::from("*, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            }
            BinaryOp::Div() => {
                if let Term::Num(n1) = *t1 {
                    if let Term::Num(n2) = *t2 {
                        if n2 == 0.0 {
                            Err(EvalError::Other(String::from("division by zero"), pos_op))
                        } else {
                            Ok(Closure::atomic_closure(RichTerm::new(
                                Term::Num(n1 / n2),
                                pos_op_inh,
                            )))
                        }
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Num"),
                            String::from("/, 2nd argument"),
                            snd_pos,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Num"),
                        String::from("/, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            }
            BinaryOp::Modulo() => {
                if let Term::Num(n1) = *t1 {
                    if let Term::Num(n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Num(n1 % n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Num"),
                            String::from("%, 2nd argument"),
                            snd_pos,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Num"),
                        String::from("%, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            }
            BinaryOp::Pow() => {
                if let Term::Num(n1) = *t1 {
                    if let Term::Num(n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Num(n1.powf(n2)),
                            pos_op_inh,
                        )))
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Num"),
                            String::from("pow, 2nd argument"),
                            snd_pos,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Num"),
                        String::from("pow, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            }
            BinaryOp::StrConcat() => {
                if let Term::Str(s1) = &*t1 {
                    if let Term::Str(s2) = &*t2 {
                        let ss: [&str; 2] = [s1, s2];
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Str(ss.concat()),
                            pos_op_inh,
                        )))
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Str"),
                            String::from("++, 2nd argument"),
                            snd_pos,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("++, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            }
            BinaryOp::Assume() => {
                if let Term::Lbl(l) = &*t2 {
                    // Track the contract argument for better error reporting, and push back the label
                    // on the stack, so that it becomes the first argument of the contract.
                    let idx = self.stack.track_arg(&mut self.cache).ok_or_else(|| {
                        EvalError::NotEnoughArgs(3, String::from("assume"), pos_op)
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
                            let mut new_env = Environment::new();
                            let closurized = RichTerm {
                                term: t1,
                                pos: pos1,
                            }
                            .closurize(
                                &mut self.cache,
                                &mut new_env,
                                env1,
                            );

                            // Convert the record to the function `fun l x => MergeContract l x t1
                            // contract`.
                            let body = mk_fun!(
                                "l",
                                "x",
                                mk_opn!(
                                    NAryOp::MergeContract(),
                                    mk_term::var("l"),
                                    mk_term::var("x"),
                                    closurized
                                )
                            )
                            .with_pos(pos1.into_inherited());

                            Ok(Closure { body, env: new_env })
                        }
                        _ => Err(EvalError::TypeError(
                            String::from("Function or Record"),
                            String::from("assume, 1st argument"),
                            fst_pos,
                            RichTerm {
                                term: t1,
                                pos: pos1,
                            },
                        )),
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Label"),
                        String::from("assume, 2nd argument"),
                        snd_pos,
                        RichTerm {
                            term: t2,
                            pos: pos2,
                        },
                    ))
                }
            }
            BinaryOp::Unseal() => {
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
                    Err(EvalError::TypeError(
                        String::from("Sym"),
                        String::from("unwrap, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            }
            BinaryOp::Tag() => match_sharedterm! {t1, with {
                    Term::Str(s) => match_sharedterm!{t2, with {
                                Term::Lbl(l) => {
                                    let mut l = l;
                                    l.tag = s;
                                    Ok(Closure::atomic_closure(RichTerm::new(
                                        Term::Lbl(l),
                                        pos_op_inh,
                                    )))
                                }
                            } else {
                                Err(EvalError::TypeError(
                                    String::from("Label"),
                                    String::from("tag, 2nd argument"),
                                    snd_pos,
                                    RichTerm {
                                        term: t2,
                                        pos: pos2,
                                    },
                                ))
                            }
                        }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("tag, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            },
            BinaryOp::Eq() => {
                let mut env = Environment::new();

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

                match eq(&mut self.cache, &mut env, c1, c2, pos_op_inh)? {
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
                            let t1 = c1.body.closurize(&mut self.cache, &mut env, c1.env);
                            let t2 = c2.body.closurize(&mut self.cache, &mut env, c2.env);

                            Ok(Closure {
                                body: RichTerm::new(Term::Op2(BinaryOp::Eq(), t1, t2), pos_op),
                                env,
                            })
                        }
                    },
                    EqResult::Eqs(t1, t2, subeqs) => {
                        self.stack.push_eqs(subeqs.into_iter());

                        Ok(Closure {
                            body: RichTerm::new(Term::Op2(BinaryOp::Eq(), t1, t2), pos_op),
                            env,
                        })
                    }
                }
            }
            BinaryOp::LessThan() => {
                if let Term::Num(n1) = *t1 {
                    if let Term::Num(n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Bool(n1 < n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Num"),
                            String::from("<, 2nd argument"),
                            snd_pos,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Num"),
                        String::from("<, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            }
            BinaryOp::LessOrEq() => {
                if let Term::Num(n1) = *t1 {
                    if let Term::Num(n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Bool(n1 <= n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Num"),
                            String::from("<, 2nd argument"),
                            snd_pos,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Num"),
                        String::from("<, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            }
            BinaryOp::GreaterThan() => {
                if let Term::Num(n1) = *t1 {
                    if let Term::Num(n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Bool(n1 > n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Num"),
                            String::from(">, 2nd argument"),
                            snd_pos,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Num"),
                        String::from(">, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            }
            BinaryOp::GreaterOrEq() => {
                if let Term::Num(n1) = *t1 {
                    if let Term::Num(n2) = *t2 {
                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Bool(n1 >= n2),
                            pos_op_inh,
                        )))
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Num"),
                            String::from(">=, 2nd argument"),
                            snd_pos,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ))
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Num"),
                        String::from(">=, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            }
            BinaryOp::GoField() => match_sharedterm! {t1, with {
                    Term::Str(field) => match_sharedterm! {t2, with {
                            Term::Lbl(l) => {
                                let mut l = l;
                                l.path.push(ty_path::Elem::Field(Ident::from(field)));
                                Ok(Closure::atomic_closure(RichTerm::new(
                                    Term::Lbl(l),
                                    pos_op_inh,
                                )))
                            }
                        } else {
                            Err(EvalError::TypeError(
                                String::from("Label"),
                                String::from("goField, 2nd argument"),
                                snd_pos,
                                RichTerm {
                                    term: t2,
                                    pos: pos2,
                                },
                            ))
                        }
                    },
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("goField, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            },
            BinaryOp::DynAccess() => match_sharedterm! {t1, with {
                    Term::Str(id) => {
                        if let Term::Record(record) = &*t2 {
                            match record.get_value(&Ident::from(&id)).map_err(|missing_field_err| missing_field_err.into_eval_err(pos2, pos_op))? {
                                Some(value) => {
                                    self.call_stack.enter_field(Ident::from(id), pos2, value.pos, pos_op);
                                    Ok(Closure {
                                        body: value.clone(),
                                        env: env2,
                                    })
                                }
                                None => Err(EvalError::FieldMissing(
                                    id,
                                    String::from("(.$)"),
                                    RichTerm {
                                        term: t2,
                                        pos: pos2,
                                    },
                                    pos_op,
                                )),
                            }
                        } else {
                            Err(EvalError::TypeError(
                                String::from("Record"),
                                String::from(".$"),
                                snd_pos,
                                RichTerm {
                                    term: t2,
                                    pos: pos2,
                                },
                            ))
                        }
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from(".$"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            },
            BinaryOp::DynExtend(metadata, extension_kind) => {
                if let Term::Str(id) = &*t1 {
                    match_sharedterm! {t2, with {
                            Term::Record(record) => {
                                let mut fields = record.fields;

                                // If a defined value is expected for this field, it must be
                                // provided as an additional argument, so we pop it from the stack
                                let value = if let RecordExtKind::WithValue = extension_kind {
                                    let (value_closure, _) = self
                                        .stack
                                        .pop_arg(&self.cache)
                                        .ok_or_else(|| EvalError::NotEnoughArgs(3, String::from("insert"), pos_op))?;

                                    let as_var = value_closure.body.closurize(&mut self.cache, &mut env2, value_closure.env);
                                    Some(as_var)
                                }
                                else {
                                    None
                                };

                                match fields.insert(Ident::from(id), Field {value, metadata }) {
                                    //TODO: what to do on insertion where an empty optional field
                                    //exists? Temporary: we fail with existing field exception
                                    Some(t) => Err(EvalError::Other(format!("insert: tried to extend a record with the field {}, but it already exists", id), pos_op)),
                                    _ => Ok(Closure {
                                        body: Term::Record(RecordData { fields, ..record }).into(),
                                        env: env2,
                                    }),
                                }
                            }
                        } else {
                            Err(EvalError::TypeError(
                                String::from("Record"),
                                String::from("insert"),
                                snd_pos,
                                RichTerm {
                                    term: t2,
                                    pos: pos2,
                                },
                            ))
                        }
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("insert"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            }
            BinaryOp::DynRemove() => match_sharedterm! {t1, with {
                    Term::Str(id) => match_sharedterm! {t2, with {
                            Term::Record(record) => {
                                let mut fields = record.fields;
                                let fetched = fields.remove(&Ident::from(&id));
                                match fetched {
                                    None
                                    | Some(Field {
                                        value: None,
                                        metadata: FieldMetadata { opt: true, ..},
                                      }) =>
                                    {
                                        Err(EvalError::FieldMissing(
                                            id,
                                            String::from("remove"),
                                            RichTerm::new(
                                                Term::Record(RecordData { fields, ..record }),
                                                pos2,
                                            ),
                                            pos_op,
                                        ))
                                    }
                                    _ => {
                                        Ok(Closure {
                                            body: RichTerm::new(
                                                Term::Record(RecordData { fields, ..record }), pos_op_inh
                                            ),
                                            env: env2,
                                        })
                                    }
                                }
                            }
                        } else {
                            Err(EvalError::TypeError(
                                String::from("Record"),
                                String::from("remove"),
                                snd_pos,
                                RichTerm {
                                    term: t2,
                                    pos: pos2,
                                },
                            ))
                        }
                    },
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("remove"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            },
            BinaryOp::HasField() => match_sharedterm! {t1, with {
                    Term::Str(id) => {
                        if let Term::Record(record) = &*t2 {
                            Ok(Closure::atomic_closure(RichTerm::new(
                                Term::Bool(matches!(record.fields.get(&Ident::from(id)), Some(field) if !field.is_empty_optional())),
                                pos_op_inh,
                            )))
                        } else {
                            Err(EvalError::TypeError(
                                String::from("Record"),
                                String::from("has_field, 2nd argument"),
                                snd_pos,
                                RichTerm {
                                    term: t2,
                                    pos: pos2,
                                },
                            ))
                        }
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("has_field, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            },
            BinaryOp::ArrayConcat() => match_sharedterm! {t1,
                with {
                    Term::Array(ts1, attrs1) => match_sharedterm! {t2,
                        with {
                            Term::Array(ts2, attrs2) => {
                                // NOTE: the [eval_closure] function in [eval] should've made sure
                                // that the array is closurized. We leave a debug_assert! here just
                                // in case something goes wrong in the future.
                                // If the assert failed, you may need to map closurize over `ts1` and `ts2`.
                                debug_assert!(attrs1.closurized, "the left-hand side of ArrayConcat (@) is not closurized.");
                                debug_assert!(attrs2.closurized, "the right-hand side of ArrayConcat (@) is not closurized.");

                                // NOTE: To avoid the extra Vec allocation, we could use Rc<[T]>::new_uninit_slice()
                                // and fill up the slice manually, but that's a nightly-only experimental API.
                                // Note that collecting into an Rc<[T]> will also allocate a intermediate vector,
                                // unless the input iterator implements the nightly-only API TrustedLen, and Array's iterator currently doesn't.
                                // Even if we could implement TrustedLen we would have to contend with the fact that .chain(..) tends to be slow.
                                // - Rc<[T]>::from_iter docs: https://doc.rust-lang.org/std/rc/struct.Rc.html#impl-FromIterator%3CT%3E
                                // - chain issue: https://github.com/rust-lang/rust/issues/63340
                                let mut ts: Vec<RichTerm> = Vec::with_capacity(ts1.len() + ts2.len());

                                let mut env = env1.clone();
                                // TODO: Is there a cheaper way to "merge" two environements?
                                env.extend(env2.iter_elems().map(|(k, v)| (*k, v.clone())));

                                // We have two sets of contracts from the LHS and RHS arrays.
                                // - Common contracts between the two sides can be put into
                                // `pending_contracts` of the resulting concatenation as they're
                                // shared by all elements: we don't have to apply them just yet.
                                // - Contracts thats are specific to the LHS or the RHS have to
                                // applied because we don't have a way of tracking which elements
                                // should take which contracts.

                                let (ctrs_left, ctrs_common) : (Vec<_>, Vec<_>) = attrs1
                                    .pending_contracts
                                    .into_iter()
                                    .partition(|ctr| !attrs2.pending_contracts.contains(ctr));

                                let ctrs_right = attrs2
                                    .pending_contracts
                                    .into_iter()
                                    .filter(|ctr| !ctrs_left.contains(ctr) && !ctrs_common.contains(ctr));

                                ts.extend(ts1.into_iter().map(|t|
                                    apply_contracts(t, ctrs_left.iter().cloned(), pos1)
                                    .closurize(&mut self.cache, &mut env, env1.clone())
                                ));

                                ts.extend(ts2.into_iter().map(|t|
                                    apply_contracts(t, ctrs_right.clone(), pos2)
                                    .closurize(&mut self.cache, &mut env, env2.clone())
                                ));

                                let attrs = ArrayAttrs {
                                    closurized: true,
                                    pending_contracts: ctrs_common,
                                };

                                Ok(Closure {
                                    body: RichTerm::new(Term::Array(Array::new(Rc::from(ts)), attrs), pos_op_inh),
                                    env,
                                })
                            }
                        } else {
                            Err(EvalError::TypeError(
                                String::from("Array"),
                                String::from("@, 2nd operand"),
                                snd_pos,
                                RichTerm {
                                    term: t2,
                                    pos: pos2,
                                },
                            ))

                        }
                    },
                } else {
                    Err(EvalError::TypeError(
                        String::from("Array"),
                        String::from("@, 1st operand"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                }
            },
            BinaryOp::ArrayElemAt() => match (&*t1, &*t2) {
                (Term::Array(ts, attrs), Term::Num(n)) => {
                    let n_int = *n as usize;
                    if n.fract() != 0.0 {
                        Err(EvalError::Other(format!("elemAt: expected the 2nd agument to be an integer, got the floating-point value {}", n), pos_op))
                    } else if *n < 0.0 || n_int >= ts.len() {
                        Err(EvalError::Other(format!("elemAt: index out of bounds. Expected a value between 0 and {}, got {}", ts.len(), n), pos_op))
                    } else {
                        let elem_with_ctr = apply_contracts(
                            ts.get(n_int).unwrap().clone(),
                            attrs.pending_contracts.iter().cloned(),
                            pos1.into_inherited(),
                        );
                        Ok(Closure {
                            body: elem_with_ctr,
                            env: env1,
                        })
                    }
                }
                (Term::Array(..), _) => Err(EvalError::TypeError(
                    String::from("Num"),
                    String::from("elemAt, 2nd argument"),
                    snd_pos,
                    RichTerm {
                        term: t2,
                        pos: pos2,
                    },
                )),
                (_, _) => Err(EvalError::TypeError(
                    String::from("Array"),
                    String::from("elemAt, 1st argument"),
                    fst_pos,
                    RichTerm {
                        term: t1,
                        pos: pos1,
                    },
                )),
            },
            BinaryOp::Merge() => merge(
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
                MergeMode::Standard,
                &mut self.call_stack,
            ),

            BinaryOp::Hash() => {
                let mk_err_fst = |t1| {
                    Err(EvalError::TypeError(
                        String::from("Enum <Md5, Sha1, Sha256, Sha512>"),
                        String::from("hash, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                };

                if let Term::Enum(id) = &*t1 {
                    if let Term::Str(s) = &*t2 {
                        let result = match id.as_ref() {
                            "Md5" => {
                                let mut hasher = md5::Md5::new();
                                hasher.update(s);
                                format!("{:x}", hasher.finalize())
                            }
                            "Sha1" => {
                                let mut hasher = sha1::Sha1::new();
                                hasher.update(s);
                                format!("{:x}", hasher.finalize())
                            }
                            "Sha256" => {
                                let mut hasher = sha2::Sha256::new();
                                hasher.update(s);
                                format!("{:x}", hasher.finalize())
                            }
                            "Sha512" => {
                                let mut hasher = sha2::Sha512::new();
                                hasher.update(s);
                                format!("{:x}", hasher.finalize())
                            }
                            _ => return mk_err_fst(t1),
                        };

                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Str(result),
                            pos_op_inh,
                        )))
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Str"),
                            String::from("hash, 2nd argument"),
                            snd_pos,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ))
                    }
                } else {
                    mk_err_fst(t1)
                }
            }
            BinaryOp::Serialize() => {
                let mk_err_fst = |t1| {
                    Err(EvalError::TypeError(
                        String::from("Enum <Json, Yaml, Toml>"),
                        String::from("serialize, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
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
                        Term::Str(serialize::to_string(format, &rt2)?),
                        pos_op_inh,
                    )))
                } else {
                    mk_err_fst(t1)
                }
            }
            BinaryOp::Deserialize() => {
                let mk_err_fst = |t1| {
                    Err(EvalError::TypeError(
                        String::from("Enum <Json, Yaml, Toml>"),
                        String::from("deserialize, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: t1,
                            pos: pos1,
                        },
                    ))
                };

                if let Term::Enum(id) = &*t1 {
                    if let Term::Str(s) = &*t2 {
                        let rt: RichTerm = match id.as_ref() {
                            "Json" => serde_json::from_str(s).map_err(|err| {
                                EvalError::DeserializationError(
                                    String::from("json"),
                                    format!("{}", err),
                                    pos_op,
                                )
                            })?,
                            "Yaml" => serde_yaml::from_str(s).map_err(|err| {
                                EvalError::DeserializationError(
                                    String::from("yaml"),
                                    format!("{}", err),
                                    pos_op,
                                )
                            })?,
                            "Toml" => toml::from_str(s).map_err(|err| {
                                EvalError::DeserializationError(
                                    String::from("toml"),
                                    format!("{}", err),
                                    pos_op,
                                )
                            })?,
                            _ => return mk_err_fst(t1),
                        };

                        Ok(Closure::atomic_closure(rt.with_pos(pos_op_inh)))
                    } else {
                        Err(EvalError::TypeError(
                            String::from("Str"),
                            String::from("deserialize, 2nd argument"),
                            snd_pos,
                            RichTerm {
                                term: t2,
                                pos: pos2,
                            },
                        ))
                    }
                } else {
                    mk_err_fst(t1)
                }
            }
            BinaryOp::StrSplit() => match (&*t1, &*t2) {
                (Term::Str(s1), Term::Str(s2)) => {
                    let array = s1
                        .split(s2)
                        .map(|s| Term::Str(String::from(s)).into())
                        .collect();

                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Array(array, ArrayAttrs::new().closurized()),
                        pos_op_inh,
                    )))
                }
                (Term::Str(_), _) => Err(EvalError::TypeError(
                    String::from("Str"),
                    String::from("strSplit, 2nd argument"),
                    snd_pos,
                    RichTerm {
                        term: t2,
                        pos: pos2,
                    },
                )),
                (_, _) => Err(EvalError::TypeError(
                    String::from("Str"),
                    String::from("strSplit, 1st argument"),
                    fst_pos,
                    RichTerm {
                        term: t1,
                        pos: pos1,
                    },
                )),
            },
            BinaryOp::StrContains() => match (&*t1, &*t2) {
                (Term::Str(s1), Term::Str(s2)) => Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Bool(s1.contains(s2)),
                    pos_op_inh,
                ))),
                (Term::Str(_), _) => Err(EvalError::TypeError(
                    String::from("Str"),
                    String::from("strContains, 2nd argument"),
                    snd_pos,
                    RichTerm {
                        term: t2,
                        pos: pos2,
                    },
                )),
                (_, _) => Err(EvalError::TypeError(
                    String::from("Str"),
                    String::from("strContains, 1st argument"),
                    fst_pos,
                    RichTerm {
                        term: t1,
                        pos: pos1,
                    },
                )),
            },
            BinaryOp::ArrayLazyAssume() => {
                let (ctr, _) = self.stack.pop_arg(&self.cache).ok_or_else(|| {
                    EvalError::NotEnoughArgs(3, String::from("arrayLazyAssume"), pos_op)
                })?;

                let Closure {
                    body: rt3,
                    env: env3,
                } = ctr;

                // FIXME: use match?
                let lbl = match_sharedterm! {t1, with {
                        Term::Lbl(lbl) => lbl
                    } else return Err(EvalError::TypeError(
                        String::from("Lbl"),
                        String::from("arrayLazyAssume, 2nd argument"),
                        fst_pos,
                        RichTerm {
                            term: t2,
                            pos: pos2,
                        },
                    ))
                };

                match_sharedterm! {t2,
                    with {
                        Term::Array(ts, attrs) => {
                            // Preserve the environment of the contract in the resulting array.
                            let rt3 = rt3.closurize(&mut self.cache, &mut env2, env3);

                            let array_with_ctr = Closure {
                                body: RichTerm::new(
                                    Term::Array(ts, attrs.with_extra_contracts([PendingContract::new(rt3, lbl)])),
                                    pos2,
                                ),
                                env: env2,
                            };

                            Ok(array_with_ctr)
                        }
                    } else Err(EvalError::TypeError(
                        String::from("Array"),
                        String::from("arrayLazyAssume, 2nd argument"),
                        snd_pos,
                        RichTerm {
                            term: t2,
                            pos: pos2,
                        },
                    ))
                }
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

        // Currently, for fixed arity primitive operators, the parser must ensure that they get exactly
        // the right number of argument: if it is not the case, this is a bug, and we panic.
        match n_op {
            NAryOp::StrReplace() | NAryOp::StrReplaceRegex() => {
                let mut args_wo_env = args
                    .into_iter()
                    .map(|(clos, pos)| (clos.body.term, clos.body.pos, pos));
                let (fst, pos1, fst_pos) = args_wo_env.next().unwrap();
                let (snd, pos2, snd_pos) = args_wo_env.next().unwrap();
                let (thd, pos3, thd_pos) = args_wo_env.next().unwrap();
                debug_assert!(args_wo_env.next().is_none());

                match (&*fst, &*snd, &*thd) {
                    (Term::Str(s), Term::Str(from), Term::Str(to)) => {
                        let result = if let NAryOp::StrReplace() = n_op {
                            str::replace(s, from, to)
                        } else {
                            let re = regex::Regex::new(from)
                                .map_err(|err| EvalError::Other(err.to_string(), pos_op))?;

                            re.replace_all(s, to.as_str()).into_owned()
                        };

                        Ok(Closure::atomic_closure(RichTerm::new(
                            Term::Str(result),
                            pos_op_inh,
                        )))
                    }
                    (Term::Str(_), Term::Str(_), _) => Err(EvalError::TypeError(
                        String::from("Str"),
                        format!("{}, 3rd argument", n_op),
                        thd_pos,
                        RichTerm {
                            term: thd,
                            pos: pos3,
                        },
                    )),
                    (Term::Str(_), _, _) => Err(EvalError::TypeError(
                        String::from("Str"),
                        format!("{}, 2nd argument", n_op),
                        snd_pos,
                        RichTerm {
                            term: snd,
                            pos: pos2,
                        },
                    )),
                    (_, _, _) => Err(EvalError::TypeError(
                        String::from("Str"),
                        format!("{}, 1st argument", n_op),
                        fst_pos,
                        RichTerm {
                            term: fst,
                            pos: pos1,
                        },
                    )),
                }
            }
            NAryOp::StrSubstr() => {
                let mut args_wo_env = args
                    .into_iter()
                    .map(|(clos, pos)| (clos.body.term, clos.body.pos, pos));
                let (fst, pos1, fst_pos) = args_wo_env.next().unwrap();
                let (snd, pos2, snd_pos) = args_wo_env.next().unwrap();
                let (thd, pos3, thd_pos) = args_wo_env.next().unwrap();
                debug_assert!(args_wo_env.next().is_none());

                match (&*fst, &*snd, &*thd) {
                    (Term::Str(s), Term::Num(start), Term::Num(end)) => {
                        let start_int = *start as usize;
                        let end_int = *end as usize;

                        if start.fract() != 0.0 {
                            Err(EvalError::Other(format!("substring: expected the 2nd agument (start) to be an integer, got the floating-point value {}", start), pos_op))
                        } else if !s.is_char_boundary(start_int) {
                            Err(EvalError::Other(format!("substring: index out of bounds. Expected the 2nd argument (start) to be between 0 and {}, got {}", s.len(), start), pos_op))
                        } else if end.fract() != 0.0 {
                            Err(EvalError::Other(format!("substring: expected the 3nd argument (end) to be an integer, got the floating-point value {}", end), pos_op))
                        } else if end <= start || !s.is_char_boundary(end_int) {
                            Err(EvalError::Other(format!("substring: index out of bounds. Expected the 3rd argument (end) to be between {} and {}, got {}", start+1., s.len(), end), pos_op))
                        } else {
                            Ok(Closure::atomic_closure(RichTerm::new(
                                Term::Str(s[start_int..end_int].to_owned()),
                                pos_op_inh,
                            )))
                        }
                    }
                    (Term::Str(_), Term::Num(_), _) => Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("strReplace, 3rd argument"),
                        thd_pos,
                        RichTerm {
                            term: thd,
                            pos: pos3,
                        },
                    )),
                    (Term::Str(_), _, _) => Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("strReplace, 2nd argument"),
                        snd_pos,
                        RichTerm {
                            term: snd,
                            pos: pos2,
                        },
                    )),
                    (_, _, _) => Err(EvalError::TypeError(
                        String::from("Str"),
                        String::from("strReplace, 1st argument"),
                        fst_pos,
                        RichTerm {
                            term: fst,
                            pos: pos1,
                        },
                    )),
                }
            }
            NAryOp::MergeContract() => {
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

                match_sharedterm! {t1, with {
                        Term::Lbl(lbl) => {
                            merge(
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
                                &mut self.call_stack
                            )
                        }
                    } else {
                        Err(EvalError::InternalError(format!("The MergeContract() operator was expecting a first argument of type Label, got {}", t1.type_of().unwrap_or_else(|| String::from("<unevaluated>"))), pos_op))
                    }
                }
            }
            NAryOp::RecordSealTail() => {
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
                        let mut env = env3;

                        let tail_as_var = RichTerm::from(Term::Record(tail.clone())).closurize(
                            &mut self.cache,
                            &mut env,
                            env4,
                        );
                        r.sealed_tail =
                            Some(record::SealedTail::new(*s, label.clone(), tail_as_var));

                        let body = RichTerm::from(Term::Record(r));
                        Ok(Closure { body, env })
                    }
                    (Term::SealingKey(_), Term::Lbl(_), Term::Record(_), _) => {
                        Err(EvalError::TypeError(
                            String::from("Record"),
                            String::from("%record_seal_tail%, 4th arg"),
                            frth_pos,
                            RichTerm {
                                term: a4,
                                pos: pos4,
                            },
                        ))
                    }
                    (Term::SealingKey(_), Term::Lbl(_), _, _) => Err(EvalError::TypeError(
                        String::from("Record"),
                        String::from("%record_seal_tail%, 3rd arg"),
                        thd_pos,
                        RichTerm {
                            term: a3,
                            pos: pos3,
                        },
                    )),
                    (Term::SealingKey(_), _, _, _) => Err(EvalError::TypeError(
                        String::from("Label"),
                        String::from("%record_seal_tail%, 2nd arg"),
                        snd_pos,
                        RichTerm {
                            term: a2,
                            pos: pos2,
                        },
                    )),
                    (_, _, _, _) => Err(EvalError::TypeError(
                        String::from("SealingKey"),
                        String::from("%record_seal_tail%, 1st arg"),
                        fst_pos,
                        RichTerm {
                            term: a1,
                            pos: pos1,
                        },
                    )),
                }
            }
            NAryOp::RecordUnsealTail() => {
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
                        body: RichTerm { term: a3, .. },
                        env: env3,
                    },
                    _,
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
                    (Term::SealingKey(..), _, _) => Err(EvalError::TypeError(
                        String::from("Label"),
                        String::from("%record_unseal_tail%, 2nd arg"),
                        snd_pos,
                        RichTerm {
                            term: a2,
                            pos: pos2,
                        },
                    )),
                    (_, _, _) => Err(EvalError::TypeError(
                        String::from("Record"),
                        String::from("%record_unseal_tail%, 3rd arg"),
                        fst_pos,
                        RichTerm {
                            term: a1,
                            pos: pos1,
                        },
                    )),
                }
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
                // closurize it into a new, normal thunk (non revertible), we lose the ability to
                // override `foo` and we end up with the unexpected result `{foo = 2, bar = 2}`.
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
                // For revertible thunks, we don't want to only map the push operator on the
                // current cached value, but also on the original expression.
                //
                // To do so, we create a new independent copy of the original thunk by mapping the
                // function over both expressions (in the sense of both the original expression and
                // the cached expression). This logic is encapsulated by `Thunk::map`.

                field.value = field.value.take().map(|value| {
                    if let Term::Var(id_inner) = value.as_ref() {
                        let idx = env.get(id_inner).unwrap();

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

                        let fresh_id = Ident::fresh();
                        new_env.insert(fresh_id, new_idx);
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
/// - `env`: the final environment in which to closurize the operands of potential subequalities.
/// - `c1`: the closure of the first operand.
/// - `c2`: the closure of the second operand.
/// - `pos_op`: the position of the equality operation, used for error diagnostics.
///
/// # Return
///
/// If the comparison is successful, returns a bool indicating whether the values were equal,
/// otherwise returns an [`EvalError`] indiciating that the values cannot be compared.
fn eq<C: Cache>(
    cache: &mut C,
    env: &mut Environment,
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
    // generate an approriate `EqResult::Eqs` variant with closurized terms in it.
    fn gen_eqs<I, C: Cache>(
        cache: &mut C,
        mut it: I,
        env: &mut Environment,
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

            EqResult::Eqs(
                t1.closurize(cache, env, env1),
                t2.closurize(cache, env, env2),
                eqs,
            )
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
        (Term::Enum(id1), Term::Enum(id2)) => Ok(EqResult::Bool(id1 == id2)),
        (Term::Record(r1), Term::Record(r2)) => {
            let merge::hashmap::SplitResult {
                left,
                center,
                right,
            } = merge::hashmap::split(r1.fields, r2.fields);

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
                                ..
                            },
                            Field {
                                value: Some(value2),
                                ..
                            },
                        ) => Some(Ok((value1, value2))),
                        (Field { value: None, .. }, Field { value: None, .. }) => None,
                        (
                            Field {
                                value: value1 @ None,
                                metadata,
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
                Ok(gen_eqs(cache, eqs?.into_iter(), env, env1, env2))
            }
        }
        (Term::Array(l1, a1), Term::Array(l2, a2)) if l1.len() == l2.len() => {
            // Equalities are tested in reverse order, but that shouldn't matter. If it
            // does, just do `eqs.rev()`

            // We should apply all contracts here, otherwise we risk having wrong values, think
            // record contrats with default values, wrapped terms, etc.
            let mut shared_env1 = env1.clone();
            let mut shared_env2 = env2.clone();

            let mut eqs = l1
                .into_iter()
                .map(|t| {
                    let pos = t.pos.into_inherited();
                    apply_contracts(t, a1.pending_contracts.iter().cloned(), pos).closurize(
                        cache,
                        &mut shared_env1,
                        env1.clone(),
                    )
                })
                .collect::<Vec<_>>()
                .into_iter()
                .zip(l2.into_iter().map(|t| {
                    let pos = t.pos.into_inherited();
                    apply_contracts(t, a2.pending_contracts.iter().cloned(), pos).closurize(
                        cache,
                        &mut shared_env2,
                        env2.clone(),
                    )
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
                                    env: shared_env1.clone(),
                                },
                                Closure {
                                    body: t2,
                                    env: shared_env2.clone(),
                                },
                            )
                        })
                        .collect::<Vec<_>>();

                    Ok(EqResult::Eqs(
                        t1.closurize(cache, env, shared_env1),
                        t2.closurize(cache, env, shared_env2),
                        eqs,
                    ))
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
        (_, _) => Ok(EqResult::Bool(false)),
    }
}

trait RecordDataExt {
    fn map_fields_without_optionals<F, C: Cache>(
        self,
        cache: &mut C,
        shared_env: &mut Environment,
        env: &Environment,
        f: F,
    ) -> Result<Self, record::MissingFieldDefError>
    where
        F: FnMut(Ident, RichTerm) -> RichTerm,
        Self: Sized;
}

impl RecordDataExt for RecordData {
    /// Returns the record resulting from applying the provided function
    /// to each field, and removing any field which is an empty optional
    /// in the provided environment. The resulting values are then closurized
    /// into the shared environment.
    ///
    /// Note that `f` is taken as `mut` in order to allow it to mutate
    /// external state while iterating.
    fn map_fields_without_optionals<F, C: Cache>(
        self,
        cache: &mut C,
        shared_env: &mut Environment,
        env: &Environment,
        mut f: F,
    ) -> Result<Self, record::MissingFieldDefError>
    where
        F: FnMut(Ident, RichTerm) -> RichTerm,
    {
        let fields: Result<HashMap<_, _>, _> = self
            .fields
            .into_iter()
            .filter_map(|(id, field)| {
                (!field.is_empty_optional()).then(|| {
                    let value = field.value.map(|value| f(id, value)).ok_or(
                        record::MissingFieldDefError {
                            id,
                            metadata: field.metadata.clone(),
                        },
                    )?;

                    let field = Field {
                        value: Some(value),
                        ..field
                    }
                    .closurize(cache, shared_env, env.clone());

                    Ok((id, field))
                })
            })
            .collect();
        Ok(Self {
            fields: fields?,
            ..self
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cache::resolvers::DummyResolver;
    use crate::eval::cache::CBNCache;
    use crate::eval::Environment;

    type EC = CBNCache;

    #[test]
    fn ite_operation() {
        let cont: OperationCont = OperationCont::Op1(UnaryOp::Ite(), TermPos::None);
        let mut vm: VirtualMachine<DummyResolver, EC> = VirtualMachine::new(DummyResolver {});

        vm.stack.push_arg(
            Closure::atomic_closure(Term::Num(5.0).into()),
            TermPos::None,
        );
        vm.stack.push_arg(
            Closure::atomic_closure(Term::Num(46.0).into()),
            TermPos::None,
        );

        let mut clos = Closure {
            body: Term::Bool(true).into(),
            env: Environment::new(),
        };

        vm.stack.push_op_cont(cont, 0, TermPos::None);

        clos = vm.continuate_operation(clos).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(46.0).into(),
                env: Environment::new()
            }
        );
        assert_eq!(0, vm.stack.count_args());
    }

    #[test]
    fn plus_first_term_operation() {
        let cont = OperationCont::Op2First(
            BinaryOp::Plus(),
            Closure {
                body: Term::Num(6.0).into(),
                env: Environment::new(),
            },
            TermPos::None,
        );

        let mut clos = Closure {
            body: Term::Num(7.0).into(),
            env: Environment::new(),
        };
        let mut vm = VirtualMachine::new(DummyResolver {});
        vm.stack.push_op_cont(cont, 0, TermPos::None);

        clos = vm.continuate_operation(clos).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(6.0).into(),
                env: Environment::new()
            }
        );

        assert_eq!(1, vm.stack.count_conts());
        assert_eq!(
            (
                OperationCont::Op2Second(
                    BinaryOp::Plus(),
                    Closure {
                        body: Term::Num(7.0).into(),
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
            BinaryOp::Plus(),
            Closure {
                body: Term::Num(7.0).into(),
                env: Environment::new(),
            },
            TermPos::None,
            TermPos::None,
        );

        let mut vm: VirtualMachine<DummyResolver, EC> = VirtualMachine::new(DummyResolver {});
        let mut clos = Closure {
            body: Term::Num(6.0).into(),
            env: Environment::new(),
        };
        vm.stack.push_op_cont(cont, 0, TermPos::None);

        clos = vm.continuate_operation(clos).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(13.0).into(),
                env: Environment::new()
            }
        );
    }
}
