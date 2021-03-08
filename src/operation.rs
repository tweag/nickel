//! Implementation of primitive operations.
//!
//! Define functions which perform the evaluation of primitive operators. The machinery required
//! for the strict evaluation of the operands is mainly handled by [`eval`](../eval/index.html),
//! and marginally in [`continuate_operation`](fn.continuate_operation.html). On the other hand,
//! the functions [`process_unary_operation`](fn.process_unary_operation.html) and
//! [`process_binary_operation`](fn.process_binary_operation.html) receive evaluated operands and
//! implement the actual semantics of operators.
use crate::error::EvalError;
use crate::eval::{subst, CallStack, Closure, Environment};
use crate::identifier::Ident;
use crate::label::ty_path;
use crate::merge;
use crate::merge::merge;
use crate::position::TermPos;
use crate::stack::Stack;
use crate::term::make as mk_term;
use crate::term::{BinaryOp, RichTerm, StrChunk, Term, UnaryOp};
use crate::transformations::Closurizable;
use crate::{mk_app, mk_fun};
use crate::{serialize, serialize::ExportFormat};
use md5::digest::Digest;
use simple_counter::*;
use std::iter::Extend;

generate_counter!(FreshVariableCounter, usize);

/// Result of the equality of two terms.
///
/// The equality of two terms can either be computed directly for base types (`Num`, `Str`, etc.),
/// in which case `Bool` is returned. Otherwise, composite values such as lists or records generate
/// new subequalities, as represented by the last variant as a vector of pairs of terms.  This list
/// should be non-empty (it if was empty, `eq` should have returned `Bool(true)` directly).  The
/// first element of this non-empty list is encoded as the two first parameters of `Eqs`, while the
/// last vector parameter is the (potentially empty) tail.
///
/// See [`eq`](./fn.eq.html).
enum EqResult {
    Bool(bool),
    Eqs(RichTerm, RichTerm, Vec<(Closure, Closure)>),
}

/// An operation continuation as stored on the stack.
#[derive(Debug, PartialEq)]
pub enum OperationCont {
    Op1(
        /* unary operation */ UnaryOp,
        /* original position of the argument before evaluation */ TermPos,
        /* previous value of enriched_strict */ bool,
    ),
    // The last parameter saves the strictness mode before the evaluation of the operator
    Op2First(
        /* the binary operation */ BinaryOp,
        /* second argument, to evaluate next */ Closure,
        /* original position of the first argument */ TermPos,
        /* previous value of enriched_strict */ bool,
    ),
    Op2Second(
        /* binary operation */ BinaryOp,
        /* first argument, evaluated */ Closure,
        /* original position of the first argument before evaluation */ TermPos,
        /* original position of the second argument before evaluation */ TermPos,
        /* previous value of enriched_strict */ bool,
    ),
}

/// Process to the next step of the evaluation of an operation.
///
/// Depending on the content of the stack, it either starts the evaluation of the first argument,
/// starts the evaluation of the second argument, or finally process with the operation if both
/// arguments are evaluated (for binary operators).
pub fn continuate_operation(
    mut clos: Closure,
    stack: &mut Stack,
    call_stack: &mut CallStack,
    enriched_strict: &mut bool,
) -> Result<Closure, EvalError> {
    let (cont, cs_len, pos) = stack.pop_op_cont().expect("Condition already checked");
    call_stack.truncate(cs_len);
    match cont {
        OperationCont::Op1(u_op, arg_pos, prev_strict) => {
            let result = process_unary_operation(u_op, clos, arg_pos, stack, call_stack, pos);
            *enriched_strict = prev_strict;
            result
        }
        OperationCont::Op2First(b_op, mut snd_clos, fst_pos, prev_strict) => {
            std::mem::swap(&mut clos, &mut snd_clos);
            stack.push_op_cont(
                OperationCont::Op2Second(b_op, snd_clos, fst_pos, clos.body.pos, prev_strict),
                cs_len,
                pos,
            );
            Ok(clos)
        }
        OperationCont::Op2Second(b_op, fst_clos, fst_pos, snd_pos, prev_strict) => {
            let result =
                process_binary_operation(b_op, fst_clos, fst_pos, clos, snd_pos, stack, pos);
            *enriched_strict = prev_strict;
            result
        }
    }
}

/// Evaluate a unary operation.
///
/// The argument is expected to be evaluated (in WHNF). `pos_op` corresponds to the whole
/// operation position, that may be needed for error reporting.
fn process_unary_operation(
    u_op: UnaryOp,
    clos: Closure,
    arg_pos: TermPos,
    stack: &mut Stack,
    call_stack: &mut CallStack,
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
                if stack.count_args() >= 2 {
                    let (fst, ..) = stack.pop_arg().expect("Condition already checked.");
                    let (snd, ..) = stack.pop_arg().expect("Condition already checked.");

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
        UnaryOp::IsNum() => Ok(Closure::atomic_closure(RichTerm::new(
            Term::Bool(matches!(*t, Term::Num(..))),
            pos_op_inh,
        ))),
        UnaryOp::IsBool() => Ok(Closure::atomic_closure(RichTerm::new(
            Term::Bool(matches!(*t, Term::Bool(..))),
            pos_op_inh,
        ))),
        UnaryOp::IsStr() => Ok(Closure::atomic_closure(RichTerm::new(
            Term::Bool(matches!(*t, Term::Str(..))),
            pos_op_inh,
        ))),
        UnaryOp::IsFun() => Ok(Closure::atomic_closure(RichTerm::new(
            Term::Bool(matches!(*t, Term::Fun(..))),
            pos_op_inh,
        ))),
        UnaryOp::IsList() => Ok(Closure::atomic_closure(RichTerm::new(
            Term::Bool(matches!(*t, Term::List(..))),
            pos_op_inh,
        ))),
        UnaryOp::IsRecord() => Ok(Closure::atomic_closure(RichTerm::new(
            Term::Bool(matches!(*t, Term::Record(..) | Term::RecRecord(..))),
            pos_op_inh,
        ))),
        UnaryOp::BoolAnd() =>
        // The syntax should not allow partially applied boolean operators.
        {
            if let Some((next, ..)) = stack.pop_arg() {
                match *t {
                    Term::Bool(true) => Ok(next),
                    // FIXME: this does not check that the second argument is actually a boolean.
                    // This means `true && 2` silently evaluates to `2`. This is simpler and more
                    // efficient, but can make debugging harder. In any case, it should be solved
                    // only once primary operators have better support for laziness in some
                    // arguments.
                    b @ Term::Bool(false) => {
                        Ok(Closure::atomic_closure(RichTerm::new(b, pos_op_inh)))
                    }
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
            if let Some((next, ..)) = stack.pop_arg() {
                match *t {
                    b @ Term::Bool(true) => {
                        Ok(Closure::atomic_closure(RichTerm::new(b, pos_op_inh)))
                    }
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
        UnaryOp::Blame() => {
            if let Term::Lbl(label) = *t {
                Err(EvalError::BlameError(
                    label,
                    std::mem::replace(call_stack, Vec::new()),
                ))
            } else {
                Err(EvalError::TypeError(
                    String::from("Label"),
                    String::from("blame"),
                    arg_pos,
                    RichTerm { term: t, pos },
                ))
            }
        }
        UnaryOp::Assume() => {
            match *t {
                Term::Fun(..) => Ok(Closure {
                    body: RichTerm { term: t, pos },
                    env,
                }),
                Term::Record(..) => {
                    let mut new_env = Environment::new();
                    let closurized = RichTerm { term: t, pos }.closurize(&mut new_env, env);

                    // Convert the record to the function `fun l x => contract & x`.
                    let body = mk_fun!(
                        "l",
                        "x",
                        mk_term::op2(BinaryOp::Merge(), closurized, mk_term::var("x"))
                    )
                    .with_pos(pos.into_inherited());

                    Ok(Closure { body, env: new_env })
                }
                _ => Err(EvalError::TypeError(
                    String::from("Function or record"),
                    String::from("contract application"),
                    arg_pos,
                    RichTerm { term: t, pos },
                )),
            }
        }
        UnaryOp::Embed(_id) => {
            if let en @ Term::Enum(_) = *t {
                Ok(Closure::atomic_closure(RichTerm::new(en, pos_op_inh)))
            } else {
                Err(EvalError::TypeError(
                    String::from("Enum"),
                    String::from("embed"),
                    arg_pos,
                    RichTerm { term: t, pos },
                ))
            }
        }
        UnaryOp::Switch(has_default) => {
            let (cases_closure, ..) = stack.pop_arg().expect("missing arg for switch");
            let default = if has_default {
                Some(
                    stack
                        .pop_arg()
                        .map(|(clos, ..)| clos)
                        .expect("missing default case for switch"),
                )
            } else {
                None
            };

            if let Term::Enum(en) = *t {
                let Closure {
                    body:
                        RichTerm {
                            term: cases_term, ..
                        },
                    env: cases_env,
                } = cases_closure;

                let mut cases = match *cases_term {
                    Term::Record(map) => map,
                    _ => panic!("invalid argument for switch"),
                };

                cases
                    .remove(&en)
                    .map(|body| Closure {
                        body,
                        env: cases_env,
                    })
                    .or(default)
                    .ok_or_else(||
                        // ? We should have a dedicated error for unmatched pattern
                        EvalError::TypeError(
                            String::from("Enum"),
                            String::from("switch"),
                            arg_pos,
                            RichTerm {
                                term: Box::new(Term::Enum(en)),
                                pos,
                            },
                        ))
            } else if let Some(clos) = default {
                Ok(clos)
            } else {
                Err(EvalError::TypeError(
                    String::from("Enum"),
                    String::from("switch"),
                    arg_pos,
                    RichTerm { term: t, pos },
                ))
            }
        }
        UnaryOp::ChangePolarity() => {
            if let Term::Lbl(mut l) = *t {
                l.polarity = !l.polarity;
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Lbl(l),
                    pos_op_inh,
                )))
            } else {
                Err(EvalError::TypeError(
                    String::from("Label"),
                    String::from("changePolarity"),
                    arg_pos,
                    RichTerm { term: t, pos },
                ))
            }
        }
        UnaryOp::Pol() => {
            if let Term::Lbl(l) = *t {
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
        UnaryOp::GoDom() => {
            if let Term::Lbl(mut l) = *t {
                l.path.push(ty_path::Elem::Domain);
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Lbl(l),
                    pos_op_inh,
                )))
            } else {
                Err(EvalError::TypeError(
                    String::from("Label"),
                    String::from("goDom"),
                    arg_pos,
                    RichTerm { term: t, pos },
                ))
            }
        }
        UnaryOp::GoCodom() => {
            if let Term::Lbl(mut l) = *t {
                l.path.push(ty_path::Elem::Codomain);
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Lbl(l),
                    pos_op_inh,
                )))
            } else {
                Err(EvalError::TypeError(
                    String::from("Label"),
                    String::from("goCodom"),
                    arg_pos,
                    RichTerm { term: t, pos },
                ))
            }
        }
        UnaryOp::GoList() => {
            if let Term::Lbl(mut l) = *t {
                l.path.push(ty_path::Elem::List);
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Lbl(l),
                    pos_op_inh,
                )))
            } else {
                Err(EvalError::TypeError(
                    String::from("Label"),
                    String::from("goList"),
                    arg_pos,
                    RichTerm { term: t, pos },
                ))
            }
        }
        UnaryOp::Wrap() => {
            if let Term::Sym(s) = *t {
                Ok(Closure::atomic_closure(
                    mk_fun!("x", Term::Wrapped(s, mk_term::var("x"))).with_pos(pos_op_inh),
                ))
            } else {
                Err(EvalError::TypeError(
                    String::from("Sym"),
                    String::from("wrap"),
                    arg_pos,
                    RichTerm { term: t, pos },
                ))
            }
        }
        UnaryOp::StaticAccess(id) => {
            if let Term::Record(mut static_map) = *t {
                match static_map.remove(&id) {
                    Some(e) => Ok(Closure { body: e, env }),

                    None => Err(EvalError::FieldMissing(
                        id.0,
                        String::from("(.)"),
                        RichTerm {
                            term: Box::new(Term::Record(static_map)),
                            pos,
                        },
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
        UnaryOp::FieldsOf() => {
            if let Term::Record(map) = *t {
                let mut fields: Vec<String> = map.keys().map(|Ident(id)| id.clone()).collect();
                fields.sort();
                let terms = fields.into_iter().map(mk_term::string).collect();
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::List(terms),
                    pos_op_inh,
                )))
            } else {
                Err(EvalError::TypeError(
                    String::from("Record"),
                    String::from("fieldsOf"),
                    arg_pos,
                    RichTerm { term: t, pos },
                ))
            }
        }
        UnaryOp::ListMap() => {
            let (f, ..) = stack
                .pop_arg()
                .ok_or_else(|| EvalError::NotEnoughArgs(2, String::from("map"), pos_op))?;

            if let Term::List(ts) = *t {
                let mut shared_env = Environment::new();
                let f_as_var = f.body.closurize(&mut env, f.env);

                // List elements are closurized to preserve lazyness of data structures. It
                // maintains the invariant that any data structure only contain thunks (that is,
                // currently, variables).
                let ts = ts
                    .into_iter()
                    .map(|t| {
                        RichTerm::new(Term::App(f_as_var.clone(), t), pos_op_inh)
                            .closurize(&mut shared_env, env.clone())
                    })
                    .collect();

                Ok(Closure {
                    body: RichTerm::new(Term::List(ts), pos_op_inh),
                    env: shared_env,
                })
            } else {
                Err(EvalError::TypeError(
                    String::from("List"),
                    String::from("map, 2nd argument"),
                    arg_pos,
                    RichTerm { term: t, pos },
                ))
            }
        }
        UnaryOp::RecordMap() => {
            let (f, ..) = stack
                .pop_arg()
                .ok_or_else(|| EvalError::NotEnoughArgs(2, String::from("recordMap"), pos_op))?;

            if let Term::Record(rec) = *t {
                let mut shared_env = Environment::new();
                let f_as_var = f.body.closurize(&mut env, f.env);

                // As for `ListMap` (see above), we closurize the content of fields
                let rec = rec
                    .into_iter()
                    .map(|e| {
                        let (Ident(s), t) = e;
                        let pos = t.pos.into_inherited();
                        (
                            Ident(s.clone()),
                            mk_app!(f_as_var.clone(), mk_term::string(s), t)
                                .closurize(&mut shared_env, env.clone())
                                .with_pos(pos),
                        )
                    })
                    .collect();

                Ok(Closure {
                    body: RichTerm::new(Term::Record(rec), pos_op_inh),
                    env: shared_env,
                })
            } else {
                Err(EvalError::TypeError(
                    String::from("Record"),
                    String::from("map on record"),
                    arg_pos,
                    RichTerm { term: t, pos },
                ))
            }
        }
        UnaryOp::Seq() => {
            if stack.count_args() >= 1 {
                let (next, ..) = stack.pop_arg().expect("Condition already checked.");
                Ok(next)
            } else {
                Err(EvalError::NotEnoughArgs(2, String::from("seq"), pos_op))
            }
        }
        UnaryOp::DeepSeq() => {
            /// Build a closure that forces a given list of terms, and at the end resumes the
            /// evaluation of the argument on the top of the stack.
            ///
            /// Requires its first argument to be non-empty.
            fn seq_terms<I>(
                mut terms: I,
                env: Environment,
                pos_op_inh: TermPos,
            ) -> Result<Closure, EvalError>
            where
                I: Iterator<Item = RichTerm>,
            {
                let first = terms
                    .next()
                    .expect("expected the argument to be a non-empty iterator");
                let body = terms.fold(
                    mk_term::op1(UnaryOp::DeepSeq(), first).with_pos(pos_op_inh),
                    |acc, t| mk_app!(mk_term::op1(UnaryOp::DeepSeq(), t), acc).with_pos(pos_op_inh),
                );

                Ok(Closure { body, env })
            };

            match *t {
                Term::Record(map) if !map.is_empty() => {
                    let terms = map.into_iter().map(|(_, t)| t);
                    seq_terms(terms, env, pos_op)
                }
                Term::List(ts) if !ts.is_empty() => seq_terms(ts.into_iter(), env, pos_op),
                _ => {
                    if let Some((next, ..)) = stack.pop_arg() {
                        Ok(next)
                    } else {
                        Err(EvalError::NotEnoughArgs(2, String::from("deepSeq"), pos_op))
                    }
                }
            }
        }
        UnaryOp::ListHead() => {
            if let Term::List(ts) = *t {
                let mut ts_it = ts.into_iter();
                if let Some(head) = ts_it.next() {
                    Ok(Closure { body: head, env })
                } else {
                    Err(EvalError::Other(String::from("head: empty list"), pos_op))
                }
            } else {
                Err(EvalError::TypeError(
                    String::from("List"),
                    String::from("head"),
                    arg_pos,
                    RichTerm { term: t, pos },
                ))
            }
        }
        UnaryOp::ListTail() => {
            if let Term::List(ts) = *t {
                let mut ts_it = ts.into_iter();
                if ts_it.next().is_some() {
                    Ok(Closure {
                        body: RichTerm::new(Term::List(ts_it.collect()), pos_op_inh),
                        env,
                    })
                } else {
                    Err(EvalError::Other(String::from("tail: empty list"), pos_op))
                }
            } else {
                Err(EvalError::TypeError(
                    String::from("List"),
                    String::from("tail"),
                    arg_pos,
                    RichTerm { term: t, pos },
                ))
            }
        }
        UnaryOp::ListLength() => {
            if let Term::List(ts) = *t {
                // A num does not have any free variable so we can drop the environment
                Ok(Closure {
                    body: RichTerm::new(Term::Num(ts.len() as f64), pos_op_inh),
                    env: Environment::new(),
                })
            } else {
                Err(EvalError::TypeError(
                    String::from("List"),
                    String::from("length"),
                    arg_pos,
                    RichTerm { term: t, pos },
                ))
            }
        }
        UnaryOp::ChunksConcat() => {
            let (mut acc, indent, env_chunks) = stack.pop_str_acc().unwrap();

            if let Term::Str(s) = *t {
                let s = if indent != 0 {
                    let indent_str: String = std::iter::once('\n')
                        .chain((0..indent).map(|_| ' '))
                        .collect();
                    s.replace("\n", &indent_str)
                } else {
                    s
                };

                acc.push_str(&s);

                let mut next_opt = stack.pop_str_chunk();

                // Pop consecutive string literals to find the next expression to evaluate
                while let Some(StrChunk::Literal(s)) = next_opt {
                    acc.push_str(&s);
                    next_opt = stack.pop_str_chunk();
                }

                if let Some(StrChunk::Expr(e, indent)) = next_opt {
                    stack.push_str_acc(acc, indent, env_chunks.clone());

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
    }
}

/// Evaluate a binary operation.
///
/// Both arguments are expected to be evaluated (in WHNF). `pos_op` corresponds to the whole
/// operation position, that may be needed for error reporting.
fn process_binary_operation(
    b_op: BinaryOp,
    fst_clos: Closure,
    fst_pos: TermPos,
    clos: Closure,
    snd_pos: TermPos,
    stack: &mut Stack,
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
        BinaryOp::PlusStr() => {
            if let Term::Str(s1) = *t1 {
                if let Term::Str(s2) = *t2 {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Str(s1 + &s2),
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
        BinaryOp::Unwrap() => {
            if let Term::Sym(s1) = *t1 {
                // Return a function that either behaves like the identity or
                // const unwrapped_term

                Ok(if let Term::Wrapped(s2, t) = *t2 {
                    if s1 == s2 {
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
        BinaryOp::Tag() => {
            if let Term::Str(s) = *t1 {
                if let Term::Lbl(mut l) = *t2 {
                    l.tag = s;
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Lbl(l),
                        pos_op_inh,
                    )))
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
        }
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

            match eq(&mut env, c1, c2) {
                EqResult::Bool(b) => match (b, stack.pop_eq()) {
                    (false, _) => {
                        stack.clear_eqs();
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
                        let t1 = c1.body.closurize(&mut env, c1.env);
                        let t2 = c2.body.closurize(&mut env, c2.env);

                        Ok(Closure {
                            body: RichTerm::new(Term::Op2(BinaryOp::Eq(), t1, t2), pos_op),
                            env,
                        })
                    }
                },
                EqResult::Eqs(t1, t2, subeqs) => {
                    stack.push_eqs(subeqs.into_iter());

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
        BinaryOp::GoField() => {
            if let Term::Str(field) = *t1 {
                if let Term::Lbl(mut l) = *t2 {
                    l.path.push(ty_path::Elem::Field(Ident(field)));
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Lbl(l),
                        pos_op_inh,
                    )))
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
        }

        BinaryOp::DynAccess() => {
            if let Term::Str(id) = *t1 {
                if let Term::Record(mut static_map) = *t2 {
                    match static_map.remove(&Ident(id.clone())) {
                        Some(e) => Ok(Closure { body: e, env: env2 }),
                        None => Err(EvalError::FieldMissing(
                            id,
                            String::from("(.$)"),
                            RichTerm {
                                term: Box::new(Term::Record(static_map)),
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
        }
        BinaryOp::DynExtend() => {
            let (clos, _) = stack
                .pop_arg()
                .ok_or_else(|| EvalError::NotEnoughArgs(3, String::from("$[ .. ]"), pos_op))?;

            if let Term::Str(id) = *t1 {
                if let Term::Record(mut static_map) = *t2 {
                    let as_var = clos.body.closurize(&mut env2, clos.env);
                    match static_map.insert(Ident(id.clone()), as_var) {
                        Some(_) => Err(EvalError::Other(format!("$[ .. ]: tried to extend record with the field {}, but it already exists", id), pos_op)),
                        None => Ok(Closure {
                            body: Term::Record(static_map).into(),
                            env: env2,
                        }),
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Record"),
                        String::from("$[ .. ]"),
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
                    String::from("$[ .. ]"),
                    fst_pos,
                    RichTerm {
                        term: t1,
                        pos: pos1,
                    },
                ))
            }
        }
        BinaryOp::DynRemove() => {
            if let Term::Str(id) = *t1 {
                if let Term::Record(mut static_map) = *t2 {
                    match static_map.remove(&Ident(id.clone())) {
                        None => Err(EvalError::FieldMissing(
                            id,
                            String::from("(-$)"),
                            RichTerm {
                                term: Box::new(Term::Record(static_map)),
                                pos: pos2,
                            },
                            pos_op,
                        )),
                        Some(_) => Ok(Closure {
                            body: RichTerm::new(Term::Record(static_map), pos_op_inh),
                            env: env2,
                        }),
                    }
                } else {
                    Err(EvalError::TypeError(
                        String::from("Record"),
                        String::from("-$"),
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
                    String::from("-$"),
                    fst_pos,
                    RichTerm {
                        term: t1,
                        pos: pos1,
                    },
                ))
            }
        }
        BinaryOp::HasField() => {
            if let Term::Str(id) = *t1 {
                if let Term::Record(static_map) = *t2 {
                    Ok(Closure::atomic_closure(RichTerm::new(
                        Term::Bool(static_map.contains_key(&Ident(id))),
                        pos_op_inh,
                    )))
                } else {
                    Err(EvalError::TypeError(
                        String::from("Record"),
                        String::from("hasField, 2nd argument"),
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
                    String::from("hasField, 1st argument"),
                    fst_pos,
                    RichTerm {
                        term: t1,
                        pos: pos1,
                    },
                ))
            }
        }
        BinaryOp::ListConcat() => match (*t1, *t2) {
            (Term::List(ts1), Term::List(ts2)) => {
                let mut env = Environment::new();
                let mut ts: Vec<RichTerm> = ts1
                    .into_iter()
                    .map(|t| t.closurize(&mut env, env1.clone()))
                    .collect();
                ts.extend(ts2.into_iter().map(|t| t.closurize(&mut env, env2.clone())));

                Ok(Closure {
                    body: RichTerm::new(Term::List(ts), pos_op_inh),
                    env,
                })
            }
            (Term::List(_), t2) => Err(EvalError::TypeError(
                String::from("List"),
                String::from("@, 2nd operand"),
                snd_pos,
                RichTerm {
                    term: Box::new(t2),
                    pos: pos2,
                },
            )),
            (t1, _) => Err(EvalError::TypeError(
                String::from("List"),
                String::from("@, 1st operand"),
                fst_pos,
                RichTerm {
                    term: Box::new(t1),
                    pos: pos1,
                },
            )),
        },
        BinaryOp::ListElemAt() => match (*t1, *t2) {
            (Term::List(mut ts), Term::Num(n)) => {
                let n_int = n as usize;
                if n.fract() != 0.0 {
                    Err(EvalError::Other(format!("elemAt: expected the 2nd agument to be an integer, got the floating-point value {}", n), pos_op))
                } else if n < 0.0 || n_int >= ts.len() {
                    Err(EvalError::Other(format!("elemAt: index out of bounds. Expected a value between 0 and {}, got {}", ts.len(), n), pos_op))
                } else {
                    Ok(Closure {
                        body: ts.swap_remove(n_int),
                        env: env1,
                    })
                }
            }
            (Term::List(_), t2) => Err(EvalError::TypeError(
                String::from("Num"),
                String::from("elemAt, 2nd argument"),
                snd_pos,
                RichTerm {
                    term: Box::new(t2),
                    pos: pos2,
                },
            )),
            (t1, _) => Err(EvalError::TypeError(
                String::from("List"),
                String::from("elemAt, 1st argument"),
                fst_pos,
                RichTerm {
                    term: Box::new(t1),
                    pos: pos1,
                },
            )),
        },
        BinaryOp::Merge() => merge(
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

            if let Term::Enum(ref id) = t1.as_ref() {
                if let Term::Str(s) = *t2 {
                    let result = match id.to_string().as_str() {
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
                let global_env = Environment::new();
                let rt2 = subst(
                    RichTerm {
                        term: t2,
                        pos: pos2,
                    },
                    &global_env,
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

            if let Term::Enum(ref id) = t1.as_ref() {
                if let Term::Str(s) = *t2 {
                    let rt: RichTerm = match id.to_string().as_str() {
                        "Json" => serde_json::from_str(&s).map_err(|err| {
                            EvalError::DeserializationError(
                                String::from("json"),
                                format!("{}", err),
                                pos_op,
                            )
                        })?,
                        "Yaml" => serde_yaml::from_str(&s).map_err(|err| {
                            EvalError::DeserializationError(
                                String::from("yaml"),
                                format!("{}", err),
                                pos_op,
                            )
                        })?,
                        "Toml" => toml::from_str(&s).map_err(|err| {
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
    }
}

/// Compute the equality of two terms, represented as closures.
///
/// # Parameters
///
/// - env: the final environment in which to closurize the operands of potential subequalities.
/// - c1: the closure of the first operand.
/// - c2: the closure of the second operand.
///
/// # Return
///
/// Return either a boolean when the equality can be computed directly, or a new non-empty list of equalities to be checked if
/// operands are composite values.
fn eq(env: &mut Environment, c1: Closure, c2: Closure) -> EqResult {
    let Closure {
        body: RichTerm { term: t1, .. },
        env: env1,
    } = c1;
    let Closure {
        body: RichTerm { term: t2, .. },
        env: env2,
    } = c2;

    // Take a list of subequalities, and either return `EqResult::Bool(true)` if it is empty, or
    // generate an approriate `EqResult::Eqs` variant with closurized terms in it.
    fn gen_eqs<I>(
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

            EqResult::Eqs(t1.closurize(env, env1), t2.closurize(env, env2), eqs)
        } else {
            EqResult::Bool(true)
        }
    }

    match (*t1, *t2) {
        (Term::Null, Term::Null) => EqResult::Bool(true),
        (Term::Bool(b1), Term::Bool(b2)) => EqResult::Bool(b1 == b2),
        (Term::Num(n1), Term::Num(n2)) => EqResult::Bool(n1 == n2),
        (Term::Str(s1), Term::Str(s2)) => EqResult::Bool(s1 == s2),
        (Term::Lbl(l1), Term::Lbl(l2)) => EqResult::Bool(l1 == l2),
        (Term::Sym(s1), Term::Sym(s2)) => EqResult::Bool(s1 == s2),
        (Term::Enum(id1), Term::Enum(id2)) => EqResult::Bool(id1 == id2),
        (Term::Record(m1), Term::Record(m2)) => {
            let (left, center, right) = merge::hashmap::split(m1, m2);

            if !left.is_empty() || !right.is_empty() {
                EqResult::Bool(false)
            } else if center.is_empty() {
                EqResult::Bool(true)
            } else {
                let eqs = center.into_iter().map(|(_, (t1, t2))| (t1, t2));
                gen_eqs(eqs, env, env1, env2)
            }
        }
        (Term::List(l1), Term::List(l2)) if l1.len() == l2.len() => {
            // Equalities are tested in reverse order, but that shouldn't matter. If it
            // does, just do `eqs.rev()`
            let eqs = l1.into_iter().zip(l2.into_iter());
            gen_eqs(eqs, env, env1, env2)
        }
        (_, _) => EqResult::Bool(false),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{CallStack, Environment};

    #[test]
    fn ite_operation() {
        let cont = OperationCont::Op1(UnaryOp::Ite(), TermPos::None, true);
        let mut stack = Stack::new();
        stack.push_arg(
            Closure::atomic_closure(Term::Num(5.0).into()),
            TermPos::None,
        );
        stack.push_arg(
            Closure::atomic_closure(Term::Num(46.0).into()),
            TermPos::None,
        );

        let mut clos = Closure {
            body: Term::Bool(true).into(),
            env: Environment::new(),
        };

        stack.push_op_cont(cont, 0, TermPos::None);
        let mut call_stack = CallStack::new();
        let mut strict = true;

        clos = continuate_operation(clos, &mut stack, &mut call_stack, &mut strict).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(46.0).into(),
                env: Environment::new()
            }
        );
        assert_eq!(0, stack.count_args());
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
            true,
        );

        let mut clos = Closure {
            body: Term::Num(7.0).into(),
            env: Environment::new(),
        };
        let mut stack = Stack::new();
        stack.push_op_cont(cont, 0, TermPos::None);
        let mut call_stack = CallStack::new();
        let mut strict = true;

        clos = continuate_operation(clos, &mut stack, &mut call_stack, &mut strict).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(6.0).into(),
                env: Environment::new()
            }
        );

        assert_eq!(1, stack.count_conts());
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
                    true
                ),
                0,
                TermPos::None
            ),
            stack.pop_op_cont().expect("Condition already checked.")
        );
    }

    #[test]
    fn plus_second_term_operation() {
        let cont = OperationCont::Op2Second(
            BinaryOp::Plus(),
            Closure {
                body: Term::Num(7.0).into(),
                env: Environment::new(),
            },
            TermPos::None,
            TermPos::None,
            true,
        );
        let mut clos = Closure {
            body: Term::Num(6.0).into(),
            env: Environment::new(),
        };
        let mut stack = Stack::new();
        stack.push_op_cont(cont, 0, TermPos::None);
        let mut call_stack = CallStack::new();
        let mut strict = false;

        clos = continuate_operation(clos, &mut stack, &mut call_stack, &mut strict).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(13.0).into(),
                env: Environment::new()
            }
        );
    }
}
