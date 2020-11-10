//! Evaluation of a Nickel term.
//!
//! The implementation of the Nickel abstract machine which evaluates a term. Note that this
//! machine is not currently formalized somewhere and is just a convenient name to designate the
//! current implementation.
//!
//! # The Nickel Abstract Machine
//! The abstract machine is a stack machine composed of the following elements:
//! - The term being currently evaluated
//! - The main stack, storing arguments, thunks and pending computations
//! - A pair of [environments](type.Environment.html), mapping identifiers to [closures](type.Closure.html):
//!     * The global environment contains builtin functions accessible from anywhere, and alive
//!     during the whole evaluation
//!     * The local environment contains the variables in scope of the current term and is subject
//!     to garbage collection (currently reference counting based)
//! - A [callstack](type.CallStack.html), mainly for error reporting purpose
//!
//! Depending on the shape of the current term, the following actions are preformed:
//!
//! ## Core calculus
//! - **Var(id)**: the term bound to `id` in the environment is fetched, and an update thunk is
//! pushed on the stack to indicate that once this term has been evaluated, the content of the
//! variable must be updated
//! - **App(func, arg)**: a closure containing the argument and the current environment is pushed
//! on the stack, and the applied term `func` is evaluated
//! - **Let(id, term, body)**: `term` is bound to `id` in the environment, and the machine proceeds with the evaluation of the body
//! - **Fun(id, body)**: Try to pop an argument from the stack. If there is some, we bound it to
//! `id` in the environment, and proceed with the body of the function. Otherwise, we are done: the
//! end result is an unapplied function
//! - **Thunk on stack**: If the evaluation of the current term is done, and there is one (or
//! several) thunk on the stack, this means we have to perform an update. Consecutive thunks are
//! popped from the stack and are updated to point to the current evaluated term.
//! - **Import**: Import must have been resolved before the evaluation starts. An unresolved import
//! causes an [`InternalError`](../error/enum.EvalError.html#variant.InternalError). A resolved
//! import, identified by a `FileId`, is retrieved from the import resolver and evaluation proceeds.
//!
//! ## Contracts
//!
//! - **`Assume(type, label, term)`** (or `Promise(type, label, term)`): replace the current term
//! with the contract corresponding to `types`, applied to label and term (`contract label term`).
//!
//! ## Operators
//!
//! Operators are strict by definition. To evaluate say `exp1 + exp2`, the following steps
//! have to be performed:
//! - `exp1` needs to be evaluated. The result must be saved somewhere, together with the resulting
//! environment
//! - `exp2`: same thing for `exp2`
//! - Finally, the implementation of `+` can proceed with the computation
//!
//! We detail the case of binary operators, as the case of unary ones is similar and simpler.
//!
//! - **Op(op, first, second)**: push an `OpFirst` element on the stack, which saves the operator
//! `op`, the second argument `second` and the current environment, and proceed with the evaluation
//! of `first`
//! - **OpFirst on stack**: if the evaluation of the current term is done and there is an `OpFirst`
//! marker on the stack, then:
//!     1. Extract the saved operator, the second argument and the environment `env2` from the marker
//!     2. Push an `OpSecond` marker, saving the operator and the evaluated form of the first
//!        argument with its environment
//!     3. Proceed with the evaluation of the second argument in environment `env2`
//! - **OpSecond on stack**: once the second term is evaluated, we can get back the operator and
//! the first term evaluated, and forward all both arguments evaluated and their respective
//! environment to the specific implementation of the operator (located in
//! [operation](../operation/index.html), or in [merge](../merge/index.html) for `merge`).
//!
//! ## Enriched values
//!
//! The evaluation of enriched values is controlled by the parameter `enriched_strict`. If it is
//! set to true (which is usually the case), the machine tries to extract a simple value from it:
//!  - **Contract**: raise an error. This usually means that an access to a field was attempted,
//!  and that this field had a contract to satisfy, but it was never defined.
//!  - **Default(value)**: an access to a field which has a default value. Proceed with the
//!  evaluation of this value
//!  - **ContractDefault(type, label, value)**: same as above, but the field also has an attached
//!  contract.  Proceed with the evaluation of `Assume(type, label, value)` to ensure that the
//!  default value satisfies this contract.
//!
//!  If `enriched_strict` is set to false, as it is when evaluating `merge`, the machine does not
//!  evaluate enriched values further, and consider the term evaluated.
//!
//! # Garbage collection
//!
//! Currently the machine relies on Rust's reference counting to manage memory. Precisely, the
//! environment stores `Rc<RefCell<Closure>>` objects, which are reference-counted pointers to a
//! mutable memory cell. This means that we do not deep copy everything everywhere, but this is
//! probably suboptimal for a functional language and is unable to collect cyclic data, which may
//! appear inside recursive records in the future. An adapted garbage collector is probably
//! something to consider at some point.
use crate::error::EvalError;
use crate::identifier::Ident;
use crate::mk_app;
use crate::operation::{continuate_operation, OperationCont};
use crate::position::RawSpan;
use crate::program::ImportResolver;
use crate::stack::Stack;
use crate::term::{make as mk_term, RichTerm, StrChunk, Term, UnaryOp};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

/// An environment, which is a mapping from identifiers to closures.
pub type Environment = HashMap<Ident, (Rc<RefCell<Closure>>, IdentKind)>;

/// A call stack, saving the history of function calls.
///
/// In a lazy language as Nickel, there are no well delimited stack frames due to how function
/// application is evaluated. This can make things hard to debug for the user, hence additional
/// information about the history of function calls is stored in the call stack, for error
/// reporting and debugging purposes.
pub type CallStack = Vec<StackElem>;

/// A call stack element.
#[derive(Debug, PartialEq, Clone)]
pub enum StackElem {
    App(Option<RawSpan>),
    Var(IdentKind, Ident, Option<RawSpan>),
}

/// Kind of an identifier.
#[derive(Debug, PartialEq, Clone)]
pub enum IdentKind {
    Let(),
    Lam(),
    Record(),
}

/// A closure, a term together with an environment.
#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    pub body: RichTerm,
    pub env: Environment,
}

impl Closure {
    pub fn atomic_closure(body: RichTerm) -> Closure {
        Closure {
            body,
            env: HashMap::new(),
        }
    }
}

/// Determine if a thunk is worth being put on the stack for future update.
///
/// Typically, WHNFs and enriched values will not be evaluated to a simpler expression and are not
/// worth updating.
fn should_update(t: &Term) -> bool {
    !t.is_whnf() && !t.is_enriched()
}

/// Evaluate a Nickel term. Wrapper around [eval_](fn.eval_.html) that drops the final environment.
pub fn eval<R>(t0: RichTerm, global_env: &Environment, resolver: &mut R) -> Result<Term, EvalError>
where
    R: ImportResolver,
{
    eval_(t0, global_env, resolver).map(|(term, _)| term)
}

/// Fully evaluate a Nickel term, that is not a to WHNF, but to a value, with all variable fully substituted.
pub fn eval_full<R>(
    t0: RichTerm,
    global_env: &Environment,
    resolver: &mut R,
) -> Result<Term, EvalError>
where
    R: ImportResolver,
{
    // Desugar to let x = term in deepSeq x x
    let wrapper = mk_term::let_in(
        "x",
        t0,
        mk_app!(
            mk_term::op1(UnaryOp::DeepSeq(), mk_term::var("x")),
            mk_term::var("x")
        ),
    );
    eval_(wrapper, global_env, resolver)
        .map(|(term, env)| substitute(term.into(), &global_env, &env).into())
}

/// The main loop of evaluation.
///
/// Implement the evaluation of the core language, which includes application, thunk update,
/// evaluation of the arguments of operations, and a few others. The specific implementations of
/// primitive operations is delegated to the modules [operation](../operation/index.html) and
/// [merge](../merge/index.html).
///
/// # Arguments
///
/// - `t0`: the term to evaluate
/// - `global_env`: the global environment containing the builtin functions of the language. Accessible from anywhere in the
/// program.
/// - `resolver`: the interface to fetch imports.
///
/// # Return
///
/// Either:
///  - an evaluation error
///  - the evaluated term with its final environment
pub fn eval_<R>(
    t0: RichTerm,
    global_env: &Environment,
    resolver: &mut R,
) -> Result<(Term, Environment), EvalError>
where
    R: ImportResolver,
{
    let mut clos = Closure::atomic_closure(t0);
    let mut call_stack = CallStack::new();
    let mut stack = Stack::new();
    let mut enriched_strict = true;

    loop {
        let Closure {
            body: RichTerm {
                term: boxed_term,
                pos,
            },
            mut env,
        } = clos;
        let term = *boxed_term;
        clos = match term {
            Term::Var(x) => {
                let (thunk, id_kind) = env
                    .remove(&x)
                    .or_else(|| {
                        global_env
                            .get(&x)
                            .map(|(rc, id_kind)| (rc.clone(), id_kind.clone()))
                    })
                    .ok_or(EvalError::UnboundIdentifier(x.clone(), pos.clone()))?;
                std::mem::drop(env); // thunk may be a 1RC pointer
                if should_update(&thunk.borrow().body.term) {
                    stack.push_thunk(Rc::downgrade(&thunk));
                }
                call_stack.push(StackElem::Var(id_kind, x, pos));
                match Rc::try_unwrap(thunk) {
                    Ok(c) => {
                        // thunk was the only strong ref to the closure
                        c.into_inner()
                    }
                    Err(rc) => {
                        // We need to clone it, there are other strong refs
                        rc.borrow().clone()
                    }
                }
            }
            Term::App(t1, t2) => {
                stack.push_arg(
                    Closure {
                        body: t2,
                        env: env.clone(),
                    },
                    pos,
                );
                Closure { body: t1, env }
            }
            Term::Let(x, s, t) => {
                let thunk = Rc::new(RefCell::new(Closure {
                    body: s,
                    env: env.clone(),
                }));
                env.insert(x, (Rc::clone(&thunk), IdentKind::Let()));
                Closure { body: t, env }
            }
            Term::Op1(op, t) => {
                let op = op.map(|t| Closure {
                    body: t,
                    env: env.clone(),
                });

                stack.push_op_cont(OperationCont::Op1(op, t.pos.clone()), call_stack.len(), pos);
                Closure { body: t, env }
            }
            Term::Op2(op, fst, snd) => {
                let op = op.map(|t| Closure {
                    body: t,
                    env: env.clone(),
                });

                let prev_strict = enriched_strict;
                enriched_strict = op.is_strict();
                stack.push_op_cont(
                    OperationCont::Op2First(
                        op,
                        Closure {
                            body: snd,
                            env: env.clone(),
                        },
                        fst.pos.clone(),
                        prev_strict,
                    ),
                    call_stack.len(),
                    pos,
                );
                Closure { body: fst, env }
            }
            Term::StrChunks(mut chunks) => match chunks.pop() {
                None => Closure {
                    body: Term::Str(String::new()).into(),
                    env: HashMap::new(),
                },
                Some(chunk) => {
                    let (arg, indent) = match chunk {
                        StrChunk::Literal(s) => (Term::Str(s).into(), 0),
                        StrChunk::Expr(e, indent) => (e, indent),
                    };

                    Closure {
                        body: RichTerm {
                            term: Box::new(Term::Op1(
                                UnaryOp::ChunksConcat(indent, String::new(), chunks),
                                arg,
                            )),
                            pos,
                        },
                        env,
                    }
                }
            },
            Term::Promise(ty, l, t) | Term::Assume(ty, l, t) => {
                stack.push_arg(
                    Closure {
                        body: t,
                        env: env.clone(),
                    },
                    None,
                );
                stack.push_arg(Closure::atomic_closure(Term::Lbl(l).into()), None);
                Closure {
                    body: ty.contract(),
                    env,
                }
            }
            Term::RecRecord(ts) => {
                // Thanks to the share normal form transformation, the content is either a constant or a
                // variable.
                let rec_env =
                    ts.iter()
                        .try_fold(HashMap::new(), |mut rec_env, (id, rt)| match rt.as_ref() {
                            &Term::Var(ref var_id) => {
                                let (thunk, id_kind) = env.get(var_id).ok_or(
                                    EvalError::UnboundIdentifier(var_id.clone(), rt.pos.clone()),
                                )?;
                                rec_env.insert(id.clone(), (thunk.clone(), id_kind.clone()));
                                Ok(rec_env)
                            }
                            _ => {
                                // If we are in this branch, the term must be a constant after the
                                // share normal form transformation, hence it should not need an
                                // environment, which is it is dropped.
                                let closure = Closure {
                                    body: rt.clone(),
                                    env: HashMap::new(),
                                };
                                rec_env.insert(
                                    id.clone(),
                                    (Rc::new(RefCell::new(closure)), IdentKind::Let()),
                                );
                                Ok(rec_env)
                            }
                        })?;

                let new_ts = ts.into_iter().map(|(id, rt)| {
                    let RichTerm { term, pos } = rt;
                    match *term {
                        Term::Var(var_id) => {
                            // We already checked for unbound identifier in the previous fold, so this
                            // get should always succeed.
                            let (thunk, _) = env.get(&var_id).unwrap();
                            thunk.borrow_mut().env.extend(rec_env.clone());
                            (
                                id,
                                RichTerm {
                                    term: Box::new(Term::Var(var_id)),
                                    pos,
                                },
                            )
                        }
                        _ => (id, RichTerm { term, pos }),
                    }
                });
                Closure {
                    body: RichTerm {
                        term: Box::new(Term::Record(new_ts.collect())),
                        pos,
                    },
                    env,
                }
            }
            // Unwrapping of enriched terms
            Term::Contract(_, _) if enriched_strict => {
                return Err(EvalError::Other(
                    String::from(
                        "Expected a simple term, got a Contract. Contracts cannot be evaluated",
                    ),
                    pos,
                ));
            }
            enriched @ Term::DefaultValue(_) | enriched @ Term::Docstring(_, _)
                if enriched_strict =>
            {
                /* Since we are forcing an enriched value, we are morally breaking subject
                 * reduction (i.e., the type of the current term changes from `enriched something`
                 * to just `something`). Updating a thunk after having performed this forcing may
                 * alter the semantics of the program in an unexpected way (see issue
                 * https://github.com/tweag/nickel/issues/123): we update potential thunks now so
                 * that their content remains an enriched value.
                 */
                let update_closure = Closure {
                    body: RichTerm {
                        term: Box::new(enriched),
                        pos,
                    },
                    env,
                };
                update_thunks(&mut stack, &update_closure);

                let Closure {
                    body:
                        RichTerm {
                            term: enriched_box,
                            pos: _,
                        },
                    env,
                } = update_closure;
                let t = match *enriched_box {
                    Term::DefaultValue(t) | Term::Docstring(_, t) => t,
                    _ => panic!("eval::eval(): previous match enforced that a term is a default or a docstring, but matched something else")
                };
                Closure { body: t, env }
            }
            Term::ContractWithDefault(ty, label, t) if enriched_strict => Closure {
                body: Term::Assume(ty, label, t).into(),
                env,
            },
            Term::ResolvedImport(id) => {
                if let Some(t) = resolver.get(id) {
                    Closure::atomic_closure(t)
                } else {
                    return Err(EvalError::InternalError(
                        format!("Resolved import not found ({:?})", id),
                        pos,
                    ));
                }
            }
            Term::Import(path) => {
                return Err(EvalError::InternalError(
                    format!("Unresolved import ({})", path),
                    pos,
                ))
            }
            // Continuation of operations and thunk update
            _ if stack.is_top_thunk() || stack.is_top_cont() => {
                clos = Closure {
                    body: RichTerm {
                        term: Box::new(term),
                        pos,
                    },
                    env,
                };
                if stack.is_top_thunk() {
                    update_thunks(&mut stack, &clos);
                    clos
                } else {
                    let cont_result = continuate_operation(
                        clos,
                        &mut stack,
                        &mut call_stack,
                        &mut enriched_strict,
                    );

                    if let Err(EvalError::BlameError(l, _)) = cont_result {
                        return Err(EvalError::BlameError(l, Some(call_stack)));
                    }
                    cont_result?
                }
            }
            // Function call
            Term::Fun(x, t) => {
                if 0 < stack.count_args() {
                    let (arg, pos_app) = stack.pop_arg().expect("Condition already checked.");
                    call_stack.push(StackElem::App(pos_app));
                    let thunk = Rc::new(RefCell::new(arg));
                    env.insert(x, (thunk, IdentKind::Lam()));
                    Closure { body: t, env }
                } else {
                    return Ok((Term::Fun(x, t), env));
                }
            }
            // Otherwise, this is either an ill-formed application, or we are done
            t => {
                if 0 < stack.count_args() {
                    let (arg, pos_app) = stack.pop_arg().expect("Condition already checked.");
                    return Err(EvalError::NotAFunc(
                        RichTerm {
                            term: Box::new(t),
                            pos,
                        },
                        arg.body,
                        pos_app,
                    ));
                } else {
                    return Ok((t, env));
                }
            }
        }
    }
}

/// Pop and update all the thunks on the top of the stack with the given closure.
fn update_thunks(stack: &mut Stack, closure: &Closure) {
    while let Some(thunk) = stack.pop_thunk() {
        if let Some(safe_thunk) = Weak::upgrade(&thunk) {
            *safe_thunk.borrow_mut() = closure.clone();
        }
    }
}

fn substitute(rt: RichTerm, global_env: &Environment, env: &Environment) -> RichTerm {
    use crate::types::{AbsType, Types};

    let RichTerm { term, pos } = rt;
    match *term {
        Term::Var(id) => env
            .get(&id)
            .or_else(|| global_env.get(&id))
            .map(|(rc, _)| {
                let closure = rc.borrow().clone();
                substitute(closure.body, global_env, &closure.env)
            })
            .unwrap_or_else(|| RichTerm::new(Term::Var(id), pos)),
        v @ Term::Bool(_)
        | v @ Term::Num(_)
        | v @ Term::Str(_)
        | v @ Term::Lbl(_)
        | v @ Term::Sym(_)
        | v @ Term::Enum(_)
        | v @ Term::Import(_)
        | v @ Term::ResolvedImport(_) => RichTerm::new(v, pos),
        Term::Fun(id, t) => {
            let t = substitute(t, global_env, env);

            RichTerm::new(Term::Fun(id, t), pos)
        }
        Term::Let(id, t1, t2) => {
            let t1 = substitute(t1, global_env, env);
            let t2 = substitute(t2, global_env, env);

            RichTerm::new(Term::Let(id, t1, t2), pos)
        }
        Term::App(t1, t2) => {
            let t1 = substitute(t1, global_env, env);
            let t2 = substitute(t2, global_env, env);

            RichTerm::new(Term::App(t1, t2), pos)
        }
        Term::Op1(op, t) => {
            let t = substitute(t, global_env, env);
            let op = op.map(|t| substitute(t, global_env, env));

            RichTerm::new(Term::Op1(op, t), pos)
        }
        Term::Op2(op, t1, t2) => {
            let t1 = substitute(t1, global_env, env);
            let t2 = substitute(t2, global_env, env);

            RichTerm::new(Term::Op2(op, t1, t2), pos)
        }
        Term::Promise(ty, l, t) => {
            let t = substitute(t, global_env, env);

            RichTerm::new(Term::Promise(ty, l, t), pos)
        }
        Term::Assume(ty, l, t) => {
            let t = substitute(t, global_env, env);

            RichTerm::new(Term::Assume(ty, l, t), pos)
        }
        Term::Wrapped(i, t) => {
            let t = substitute(t, global_env, env);

            RichTerm::new(Term::Wrapped(i, t), pos)
        }
        Term::Record(map) => {
            let map = map
                .into_iter()
                .map(|(id, t)| (id, substitute(t, global_env, env)))
                .collect();

            RichTerm::new(Term::Record(map), pos)
        }
        Term::RecRecord(map) => {
            let map = map
                .into_iter()
                .map(|(id, t)| (id, substitute(t, global_env, env)))
                .collect();

            RichTerm::new(Term::RecRecord(map), pos)
        }
        Term::List(ts) => {
            let ts = ts
                .into_iter()
                .map(|t| substitute(t, global_env, env))
                .collect();

            RichTerm::new(Term::List(ts), pos)
        }
        Term::StrChunks(chunks) => {
            let chunks = chunks
                .into_iter()
                .map(|chunk| match chunk {
                    chunk @ StrChunk::Literal(_) => chunk,
                    StrChunk::Expr(t) => StrChunk::Expr(substitute(t, global_env, env)),
                })
                .collect();

            RichTerm::new(Term::StrChunks(chunks), pos)
        }
        Term::Contract(ty, label) => {
            let ty = match ty {
                Types(AbsType::Flat(t)) => Types(AbsType::Flat(substitute(t, global_env, env))),
                ty => ty,
            };

            RichTerm::new(Term::Contract(ty, label), pos)
        }
        Term::DefaultValue(t) => {
            let t = substitute(t, global_env, env);

            RichTerm::new(Term::DefaultValue(t), pos)
        }
        Term::ContractWithDefault(ty, lbl, t) => {
            let ty = match ty {
                Types(AbsType::Flat(t)) => Types(AbsType::Flat(substitute(t, global_env, env))),
                ty => ty,
            };
            let t = substitute(t, global_env, env);

            RichTerm::new(Term::ContractWithDefault(ty, lbl, t), pos)
        }
        Term::Docstring(s, t) => {
            let t = substitute(t, global_env, env);

            RichTerm::new(Term::Docstring(s, t), pos)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::ImportError;
    use crate::label::Label;
    use crate::parser::{grammar, lexer};
    use crate::program::resolvers::{DummyResolver, SimpleResolver};
    use crate::term::make as mk_term;
    use crate::term::StrChunk;
    use crate::term::{BinaryOp, UnaryOp};
    use crate::transformations::transform;
    use crate::{mk_app, mk_fun};
    use codespan::Files;

    /// Evaluate a term without import support.
    fn eval_no_import(t: RichTerm) -> Result<Term, EvalError> {
        eval(t, &HashMap::new(), &mut DummyResolver {})
    }

    fn parse(s: &str) -> Option<RichTerm> {
        let id = Files::new().add("<test>", String::from(s));

        grammar::TermParser::new()
            .parse(id, lexer::Lexer::new(&s))
            .map(|mut t| {
                t.clean_pos();
                t
            })
            .map_err(|err| println!("{:?}", err))
            .ok()
    }

    #[test]
    fn identity_over_values() {
        let num = Term::Num(45.3);
        assert_eq!(Ok(num.clone()), eval_no_import(num.into()));

        let boolean = Term::Bool(true);
        assert_eq!(Ok(boolean.clone()), eval_no_import(boolean.into()));

        let lambda = mk_fun!("x", mk_app!(mk_term::var("x"), mk_term::var("x")));
        assert_eq!(Ok(lambda.as_ref().clone()), eval_no_import(lambda.into()));
    }

    #[test]
    fn blame_panics() {
        let label = Label::dummy();
        if let Err(EvalError::BlameError(l, _)) =
            eval_no_import(mk_term::op1(UnaryOp::Blame(), Term::Lbl(label.clone())))
        {
            assert_eq!(l, label);
        } else {
            panic!("This evaluation should've returned a BlameError!");
        }
    }

    #[test]
    #[should_panic]
    fn lone_var_panics() {
        eval_no_import(mk_term::var("unbound")).unwrap();
    }

    #[test]
    fn only_fun_are_applicable() {
        eval_no_import(mk_app!(Term::Bool(true), Term::Num(45.))).unwrap_err();
    }

    #[test]
    fn simple_app() {
        let t = mk_app!(mk_term::id(), Term::Num(5.0));
        assert_eq!(Ok(Term::Num(5.0)), eval_no_import(t));
    }

    #[test]
    fn simple_let() {
        let t = mk_term::let_in("x", Term::Num(5.0), mk_term::var("x"));
        assert_eq!(Ok(Term::Num(5.0)), eval_no_import(t));
    }

    #[test]
    fn simple_ite() {
        let t = mk_term::if_then_else(Term::Bool(true), Term::Num(5.0), Term::Bool(false));
        assert_eq!(Ok(Term::Num(5.0)), eval_no_import(t));
    }

    #[test]
    fn simple_plus() {
        let t = mk_term::op2(BinaryOp::Plus(), Term::Num(5.0), Term::Num(7.5));
        assert_eq!(Ok(Term::Num(12.5)), eval_no_import(t));
    }

    #[test]
    fn simple_is_zero() {
        let t = mk_term::op1(UnaryOp::IsZero(), Term::Num(7.0));
        assert_eq!(Ok(Term::Bool(false)), eval_no_import(t));
    }

    #[test]
    fn asking_for_various_types() {
        let num = mk_term::op1(UnaryOp::IsNum(), Term::Num(45.3));
        assert_eq!(Ok(Term::Bool(true)), eval_no_import(num));

        let boolean = mk_term::op1(UnaryOp::IsBool(), Term::Bool(true));
        assert_eq!(Ok(Term::Bool(true)), eval_no_import(boolean));

        let lambda = mk_term::op1(
            UnaryOp::IsFun(),
            mk_fun!("x", mk_app!(mk_term::var("x"), mk_term::var("x"))),
        );
        assert_eq!(Ok(Term::Bool(true)), eval_no_import(lambda));
    }

    #[test]
    fn enriched_terms_unwrapping() {
        let t = Term::DefaultValue(
            Term::DefaultValue(Term::Docstring("a".to_string(), Term::Bool(false).into()).into())
                .into(),
        )
        .into();
        assert_eq!(Ok(Term::Bool(false)), eval_no_import(t));
    }

    #[test]
    fn merge_enriched_default() {
        let t = mk_term::op2(
            BinaryOp::Merge(),
            Term::Num(1.0),
            Term::DefaultValue(Term::Num(2.0).into()),
        );
        assert_eq!(Ok(Term::Num(1.0)), eval_no_import(t));
    }

    #[test]
    fn merge_incompatible_defaults() {
        let t = mk_term::op2(
            BinaryOp::Merge(),
            Term::DefaultValue(Term::Num(1.0).into()),
            Term::DefaultValue(Term::Num(2.0).into()),
        );

        eval_no_import(t).unwrap_err();
    }

    #[test]
    fn imports() {
        let mut resolver = SimpleResolver::new();
        resolver.add_source(String::from("two"), String::from("1 + 1"));
        resolver.add_source(String::from("lib"), String::from("{ f = true }"));
        resolver.add_source(String::from("bad"), String::from("^$*/.23ab 0°@"));
        resolver.add_source(
            String::from("nested"),
            String::from("let x = import \"two\" in x + 1"),
        );
        resolver.add_source(
            String::from("cycle"),
            String::from("let x = import \"cycle_b\" in {a = 1; b = x.a}"),
        );
        resolver.add_source(
            String::from("cycle_b"),
            String::from("let x = import \"cycle\" in {a = x.a}"),
        );

        fn mk_import<R>(
            var: &str,
            import: &str,
            body: RichTerm,
            resolver: &mut R,
        ) -> Result<RichTerm, ImportError>
        where
            R: ImportResolver,
        {
            transform(
                mk_term::let_in(var, Term::Import(String::from(import)), body),
                resolver,
            )
        };

        // let x = import "does_not_exist" in x
        match mk_import("x", "does_not_exist", mk_term::var("x"), &mut resolver).unwrap_err() {
            ImportError::IOError(_, _, _) => (),
            _ => assert!(false),
        };

        // let x = import "bad" in x
        match mk_import("x", "bad", mk_term::var("x"), &mut resolver).unwrap_err() {
            ImportError::ParseError(_, _) => (),
            _ => assert!(false),
        };

        // let x = import "two" in x
        assert_eq!(
            eval(
                mk_import("x", "two", mk_term::var("x"), &mut resolver).unwrap(),
                &HashMap::new(),
                &mut resolver
            )
            .unwrap(),
            Term::Num(2.0)
        );

        // let x = import "nested" in x
        assert_eq!(
            eval(
                mk_import("x", "nested", mk_term::var("x"), &mut resolver).unwrap(),
                &HashMap::new(),
                &mut resolver
            )
            .unwrap(),
            Term::Num(3.0)
        );

        // let x = import "lib" in x.f
        assert_eq!(
            eval(
                mk_import(
                    "x",
                    "lib",
                    mk_term::op1(UnaryOp::StaticAccess(Ident::from("f")), mk_term::var("x")),
                    &mut resolver,
                )
                .unwrap(),
                &HashMap::new(),
                &mut resolver
            )
            .unwrap(),
            Term::Bool(true)
        );

        // let x = import "cycle" in x.b
        assert_eq!(
            eval(
                mk_import(
                    "x",
                    "cycle",
                    mk_term::op1(UnaryOp::StaticAccess(Ident::from("b")), mk_term::var("x")),
                    &mut resolver,
                )
                .unwrap(),
                &HashMap::new(),
                &mut resolver
            )
            .unwrap(),
            Term::Num(1.0)
        );
    }

    #[test]
    fn interpolation_simple() {
        let mut chunks = vec![
            StrChunk::Literal(String::from("Hello")),
            StrChunk::expr(
                mk_term::op2(
                    BinaryOp::PlusStr(),
                    mk_term::string(", "),
                    mk_term::string("World!"),
                )
                .into(),
            ),
            StrChunk::Literal(String::from(" How")),
            StrChunk::expr(mk_term::if_then_else(
                Term::Bool(true),
                mk_term::string(" are"),
                mk_term::string(" is"),
            )),
            StrChunk::Literal(String::from(" you?")),
        ];
        chunks.reverse();

        let t: RichTerm = Term::StrChunks(chunks).into();
        assert_eq!(
            eval_no_import(t),
            Ok(Term::Str(String::from("Hello, World! How are you?")))
        );
    }

    #[test]
    fn interpolation_nested() {
        let mut inner_chunks = vec![
            StrChunk::Literal(String::from(" How")),
            StrChunk::expr(
                Term::Op2(
                    BinaryOp::PlusStr(),
                    mk_term::string(" ar"),
                    mk_term::string("e"),
                )
                .into(),
            ),
            StrChunk::expr(mk_term::if_then_else(
                Term::Bool(true),
                mk_term::string(" you"),
                mk_term::string(" me"),
            )),
        ];
        inner_chunks.reverse();

        let mut chunks = vec![
            StrChunk::Literal(String::from("Hello, World!")),
            StrChunk::expr(Term::StrChunks(inner_chunks).into()),
            StrChunk::Literal(String::from("?")),
        ];
        chunks.reverse();

        let t: RichTerm = Term::StrChunks(chunks).into();
        assert_eq!(
            eval_no_import(t),
            Ok(Term::Str(String::from("Hello, World! How are you?")))
        );
    }

    #[test]
    fn global_env() {
        let mut global_env = HashMap::new();
        let mut resolver = DummyResolver {};
        let thunk = Rc::new(RefCell::new(Closure::atomic_closure(Term::Num(1.0).into())));
        global_env.insert(Ident::from("g"), (Rc::clone(&thunk), IdentKind::Let()));

        let t = mk_term::let_in("x", Term::Num(2.0), mk_term::var("x"));
        assert_eq!(eval(t, &global_env, &mut resolver), Ok(Term::Num(2.0)));

        let t = mk_term::let_in("x", Term::Num(2.0), mk_term::var("g"));
        assert_eq!(eval(t, &global_env, &mut resolver), Ok(Term::Num(1.0)));

        // Shadowing of global environment
        let t = mk_term::let_in("g", Term::Num(2.0), mk_term::var("g"));
        assert_eq!(eval(t, &global_env, &mut resolver), Ok(Term::Num(2.0)));
    }

    fn mk_env(bindings: Vec<(&str, RichTerm)>) -> Environment {
        bindings
            .into_iter()
            .map(|(id, t)| {
                (
                    id.into(),
                    (
                        Rc::new(RefCell::new(Closure::atomic_closure(t))),
                        IdentKind::Let(),
                    ),
                )
            })
            .collect()
    }

    #[test]
    fn substitution() {
        let global_env = mk_env(vec![
            ("glob1", Term::Num(1.0).into()),
            ("glob2", parse("\"Glob2\"").unwrap()),
            ("glob3", Term::Bool(false).into()),
        ]);
        let env = mk_env(vec![
            ("loc1", Term::Bool(true).into()),
            ("loc2", parse("if glob3 then glob1 else glob2").unwrap()),
        ]);

        let t = parse("let x = 1 in if loc1 then 1 + loc2 else glob3").unwrap();
        assert_eq!(
            substitute(t, &global_env, &env),
            parse("let x = 1 in if true then 1 + (if false then 1 else \"Glob2\") else false")
                .unwrap()
        );

        let t = parse("switch {x => [1, glob1], y => loc2, z => {id = true; other = glob3},} loc1")
            .unwrap();
        assert_eq!(
            substitute(t, &global_env, &env),
            parse("switch {x => [1, 1], y => (if false then 1 else \"Glob2\"), z => {id = true; other = false},} true").unwrap()
        );
    }
}
