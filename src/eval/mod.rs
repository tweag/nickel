//! Evaluation of a Nickel term.
//!
//! The implementation of the Nickel abstract machine which evaluates a term. Note that this
//! machine is not currently formalized somewhere and is just a convenient name to designate the
//! current implementation.
//!
//! # The Nickel Abstract Machine
//! The abstract machine is a stack machine composed of the following elements:
//! - The term being currently evaluated
//! - The main stack, storing arguments, cache indices and pending computations
//! - A pair of [environment][Environment], mapping identifiers to [closures][Closure]:
//!     * The initial environment contains builtin functions accessible from anywhere, and alive
//!     during the whole evaluation
//!     * The local environment contains the variables in scope of the current term and is subject
//!     to garbage collection (currently reference counting based)
//! - A [callstack], mainly for error reporting purpose
//!
//! Depending on the shape of the current term, the following actions are performed:
//!
//! ## Core calculus
//! - **Var(id)**: the term bound to `id` in the environment is fetched, and an update index is
//!   pushed on the stack to indicate that once this term has been evaluated, the content of the
//!   variable must be updated
//! - **App(func, arg)**: a closure containing the argument and the current environment is pushed
//!   on the stack, and the applied term `func` is evaluated
//! - **Let(id, term, body)**: `term` is bound to `id` in the environment, and the machine proceeds with the evaluation of the body
//! - **Fun(id, body)**: Try to pop an argument from the stack. If there is some, we bound it to
//!   `id` in the environment, and proceed with the body of the function. Otherwise, we are done: the
//!   end result is an unapplied function
//! - **Index on stack**: If the evaluation of the current term is done, and there is one (or
//!   several) index on the stack, this means we have to perform an update. Consecutive indices are
//!   popped from the stack and are updated to point to the current evaluated term.
//! - **Import**: Import must have been resolved before the evaluation starts. An unresolved import
//!   causes an [`crate::error::EvalError::InternalError`]. A resolved import, identified by a
//!   `FileId`, is retrieved from the import resolver and evaluation proceeds.
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
//!   marker on the stack, then:
//!     1. Extract the saved operator, the second argument and the environment `env2` from the marker
//!     2. Push an `OpSecond` marker, saving the operator and the evaluated form of the first
//!        argument with its environment
//!     3. Proceed with the evaluation of the second argument in environment `env2`
//! - **OpSecond on stack**: once the second term is evaluated, we can get back the operator and
//!   the first term evaluated, and forward all both arguments evaluated and their respective
//!   environment to the specific implementation of the operator (located in [operation], or in
//!   [merge] for `merge`).
//!
//! # Garbage collection
//!
//! Currently the machine relies on Rust's reference counting to manage memory. Precisely, the
//! environment stores `Rc<RefCell<Closure>>` objects, which are reference-counted pointers to a
//! mutable memory cell. This means that we do not deep copy everything everywhere, but this is
//! probably suboptimal for a functional language and is unable to collect cyclic data, which may
//! appear inside recursive records in the future. An adapted garbage collector is probably
//! something to consider at some point.

use crate::term::record::FieldMetadata;
use crate::{
    cache::{Cache as ImportCache, Envs, ImportResolver},
    environment::Environment as GenericEnvironment,
    error::{Error, EvalError},
    identifier::Ident,
    match_sharedterm,
    position::TermPos,
    program::QueryPath,
    term::{
        array::ArrayAttrs,
        make as mk_term,
        record::{Field, RecordData},
        BinaryOp, BindingType, LetAttrs, PendingContract, RichTerm, StrChunk, Term, UnaryOp,
    },
    transform::Closurizable,
};

pub mod cache;
pub mod callstack;
pub mod fixpoint;
pub mod merge;
pub mod operation;
pub mod stack;

use callstack::*;
use codespan::FileId;
use operation::OperationCont;
use stack::{Stack, StrAccData};

use self::cache::{Cache, CacheIndex};

impl AsRef<Vec<StackElem>> for CallStack {
    fn as_ref(&self) -> &Vec<StackElem> {
        &self.0
    }
}

// The current state of the Nickel virtual machine.
pub struct VirtualMachine<R: ImportResolver, C: Cache> {
    // The main stack, storing arguments, cache indices and pending computations.
    stack: Stack<C>,
    // The call stack, for error reporting.
    call_stack: CallStack,
    // The interface used to fetch imports.
    import_resolver: R,
    // The evaluation cache.
    pub cache: C,
}

impl<R: ImportResolver, C: Cache> VirtualMachine<R, C> {
    pub fn new(import_resolver: R) -> Self {
        VirtualMachine {
            import_resolver,
            call_stack: Default::default(),
            stack: Stack::new(),
            cache: Cache::new(),
        }
    }

    pub fn new_with_cache(import_resolver: R, cache: C) -> Self {
        VirtualMachine {
            import_resolver,
            call_stack: Default::default(),
            stack: Stack::new(),
            cache,
        }
    }

    /// Reset the state of the machine (stacks, eval mode and state of cached elements) to prepare
    /// for another evaluation round.
    pub fn reset(&mut self) {
        self.call_stack.0.clear();
        self.stack.reset(&mut self.cache);
    }

    pub fn import_resolver(&self) -> &R {
        &self.import_resolver
    }

    pub fn import_resolver_mut(&mut self) -> &mut R {
        &mut self.import_resolver
    }

    /// Evaluate a Nickel term. Wrapper around [VirtualMachine::eval_closure] that starts from an empty local
    /// environment and drops the final environment.
    pub fn eval(&mut self, t0: RichTerm, initial_env: &Environment) -> Result<RichTerm, EvalError> {
        self.eval_closure(Closure::atomic_closure(t0), initial_env)
            .map(|(term, _)| term)
    }

    /// Fully evaluate a Nickel term: the result is not a WHNF but to a value with all variables substituted.
    pub fn eval_full(
        &mut self,
        t0: RichTerm,
        initial_env: &Environment,
    ) -> Result<RichTerm, EvalError> {
        self.eval_deep_closure(t0, initial_env)
            .map(|(term, env)| subst(&self.cache, term, initial_env, &env))
    }

    /// Fully evaluates a Nickel term like `eval_full`, but does not substitute all variables.
    pub fn eval_deep(
        &mut self,
        t0: RichTerm,
        initial_env: &Environment,
    ) -> Result<RichTerm, EvalError> {
        self.eval_deep_closure(t0, initial_env)
            .map(|(term, _)| term)
    }

    fn eval_deep_closure(
        &mut self,
        rt: RichTerm,
        initial_env: &Environment,
    ) -> Result<(RichTerm, Environment), EvalError> {
        let wrapper = mk_term::op1(UnaryOp::Force(), rt);
        self.eval_closure(Closure::atomic_closure(wrapper), initial_env)
    }

    /// Query the value and the metadata of a record field in an expression.
    ///
    /// Querying `foo.bar.baz` on a term `exp` will evaluate `exp.foo.bar` and extract the field
    /// `baz`. The content of `baz` is evaluated as well, and variables are substituted, in order
    /// to obtain a value that can be printed. The metadata and the evaluated value are returned as
    /// a new field.
    pub fn query(
        &mut self,
        t: RichTerm,
        path: QueryPath,
        initial_env: &Environment,
    ) -> Result<Field, EvalError> {
        let (rt, mut env) = self.eval_closure(Closure::atomic_closure(t), initial_env)?;

        let mut prev_pos = rt.pos;
        let mut field: Field = rt.into();

        for id in path.0 {
            match field.value.as_ref().map(|rt| (rt.as_ref(), rt.pos)) {
                None => {
                    return Err(EvalError::MissingFieldDef {
                        id,
                        metadata: FieldMetadata::default(),
                        pos_record: prev_pos,
                        pos_access: id.pos,
                    })
                }
                Some((Term::Record(record_data), pos_record)) => {
                    let mut next_field = record_data.fields.get(&id).cloned();
                    prev_pos = pos_record;

                    if let Some(Field {
                        metadata: metadata_next,
                        value: mut value_next,
                        pending_contracts,
                    }) = next_field.take()
                    {
                        // We evaluate the fields' value, either to handle the next ident of the
                        // path, or to show the value if we are treating the last ident of the path

                        value_next = value_next
                            .map(|value| -> Result<RichTerm, EvalError> {
                                let pos_value = value.pos;
                                let value_with_ctr = PendingContract::apply_all(
                                    value,
                                    pending_contracts.into_iter(),
                                    pos_value,
                                );
                                let (new_value, new_env) = self.eval_closure(
                                    Closure {
                                        body: value_with_ctr,
                                        env: env.clone(),
                                    },
                                    initial_env,
                                )?;
                                env = new_env;

                                Ok(new_value)
                            })
                            .transpose()?;

                        field = Field {
                            metadata: metadata_next.clone(),
                            value: value_next,
                            pending_contracts: Default::default(),
                        };
                    } else {
                        return Err(EvalError::FieldMissing(
                            id.to_string(),
                            String::from("query"),
                            RichTerm::new(Term::Record(record_data.clone()), pos_record),
                            id.pos,
                        ));
                    }
                }
                Some(_) => {
                    //unwrap(): if we enter this pattern branch, `field.value` must be `Some(_)`
                    return Err(EvalError::TypeError(
                        String::from("Record"),
                        String::from("query"),
                        prev_pos,
                        field.value.unwrap(),
                    ));
                }
            }
        }

        Ok(field)
    }

    /// The main loop of evaluation.
    ///
    /// Implement the evaluation loop of the core language. The specific implementations of
    /// primitive operations is delegated to the modules [operation] and [merge].
    ///
    /// # Arguments
    ///
    /// - `clos`: the closure to evaluate
    /// - `initial_env`: the initial environment containing the stdlib items.
    ///   Accessible from anywhere in the program.
    ///
    /// # Return
    ///
    /// Either:
    ///  - an evaluation error
    ///  - the evaluated term with its final environment
    pub fn eval_closure(
        &mut self,
        mut clos: Closure,
        initial_env: &Environment,
    ) -> Result<(RichTerm, Environment), EvalError> {
        loop {
            let Closure {
                body:
                    RichTerm {
                        term: shared_term,
                        pos,
                    },
                mut env,
            } = clos;

            clos = match &*shared_term {
                Term::Sealed(_, inner, lbl) => {
                    let stack_item = self.stack.peek_op_cont();
                    let closure = Closure {
                        body: RichTerm {
                            term: shared_term.clone(),
                            pos,
                        },
                        env: env.clone(),
                    };
                    // Update at the original index (the index which holds the result of the op) in both cases,
                    // even if we continue with a seq.
                    // We do this because  we are on a `Sealed` term, and this is in WHNF, and if we don't,
                    // we will be unwrapping a `Sealed` term and assigning the "unsealed" value to the result
                    // of the `Seq` operation. See also: https://github.com/tweag/nickel/issues/123
                    update_at_indices(&mut self.cache, &mut self.stack, &closure);
                    match stack_item {
                        Some(OperationCont::Op2Second(BinaryOp::Unseal(), _, _, _)) => {
                            self.continuate_operation(closure)?
                        }
                        Some(OperationCont::Op1(UnaryOp::Seq(), _)) => {
                            // Then, evaluate / `Seq` the inner value.
                            Closure {
                                body: inner.clone(),
                                env: env.clone(),
                            }
                        }
                        None | Some(..) => {
                            // This operation should not be allowed to evaluate a sealed term
                            return Err(EvalError::BlameError {
                                evaluated_arg: lbl.get_evaluated_arg(&self.cache),
                                label: lbl.clone(),
                                call_stack: self.call_stack.clone(),
                            });
                        }
                    }
                }
                Term::Var(x) => {
                    let mut idx = env
                        .get(x)
                        .or_else(|| initial_env.get(x))
                        .cloned()
                        .ok_or(EvalError::UnboundIdentifier(*x, pos))?;
                    std::mem::drop(env); // idx may be a 1RC pointer

                    match self.cache.get_update_index(&mut idx) {
                        Ok(Some(idx_upd)) => self.stack.push_update_index(idx_upd),
                        Ok(None) => {}
                        Err(_blackholed_error) => {
                            return Err(EvalError::InfiniteRecursion(self.call_stack.clone(), pos))
                        }
                    }

                    self.call_stack
                        .enter_var(self.cache.ident_kind(&idx), *x, pos);

                    // If we are fetching a recursive field from the environment that doesn't have
                    // a definition, we complete the error with the additional information of where
                    // it was accessed:
                    let Closure { body, env } = self.cache.get(idx);
                    let body = match_sharedterm! {body.term, with {
                            Term::RuntimeError(EvalError::MissingFieldDef {
                                id,
                                metadata,
                                pos_record,
                                pos_access: TermPos::None,
                            }) => RichTerm::new(
                                Term::RuntimeError(EvalError::MissingFieldDef {
                                    id,
                                    metadata,
                                    pos_record,
                                    pos_access: pos,
                                }),
                                pos,
                            ),
                        } else {
                            body
                        }
                    };

                    Closure { body, env }
                }
                Term::App(t1, t2) => {
                    self.call_stack.enter_app(pos);

                    self.stack.push_arg(
                        Closure {
                            body: t2.clone(),
                            env: env.clone(),
                        },
                        pos,
                    );
                    Closure {
                        body: t1.clone(),
                        env,
                    }
                }
                Term::Let(x, s, t, LetAttrs { binding_type, rec }) => {
                    let closure: Closure = Closure {
                        body: s.clone(),
                        env: env.clone(),
                    };

                    let idx = self
                        .cache
                        .add(closure, IdentKind::Let, binding_type.clone());

                    // Patch the environment with the (x <- closure) binding
                    if *rec {
                        let idx_ = idx.clone();
                        self.cache
                            .patch(idx_.clone(), |cl| cl.env.insert(*x, idx_.clone()));
                    }

                    env.insert(*x, idx);
                    Closure {
                        body: t.clone(),
                        env,
                    }
                }
                Term::Op1(op, t) => {
                    self.stack.push_op_cont(
                        OperationCont::Op1(op.clone(), t.pos),
                        self.call_stack.len(),
                        pos,
                    );

                    Closure {
                        body: t.clone(),
                        env,
                    }
                }
                Term::Op2(op, fst, snd) => {
                    self.stack.push_op_cont(
                        OperationCont::Op2First(
                            op.clone(),
                            Closure {
                                body: snd.clone(),
                                env: env.clone(),
                            },
                            fst.pos,
                        ),
                        self.call_stack.len(),
                        pos,
                    );
                    Closure {
                        body: fst.clone(),
                        env,
                    }
                }
                Term::OpN(op, args) => {
                    // Arguments are passed as a stack to the operation continuation, so we reverse the
                    // original list.
                    let mut args_iter = args.iter();
                    let fst = args_iter
                        .next()
                        .cloned()
                        .ok_or_else(|| EvalError::NotEnoughArgs(op.arity(), op.to_string(), pos))?;

                    let pending: Vec<Closure> = args_iter
                        .rev()
                        .map(|t| Closure {
                            body: t.clone(),
                            env: env.clone(),
                        })
                        .collect();

                    self.stack.push_op_cont(
                        OperationCont::OpN {
                            op: op.clone(),
                            evaluated: Vec::with_capacity(pending.len() + 1),
                            pending,
                            current_pos: fst.pos,
                        },
                        self.call_stack.len(),
                        pos,
                    );

                    Closure { body: fst, env }
                }
                Term::StrChunks(chunks) => {
                    let mut chunks_iter = chunks.iter();
                    match chunks_iter.next_back() {
                        None => Closure {
                            body: Term::Str(String::new()).into(),
                            env: Environment::new(),
                        },
                        Some(chunk) => {
                            let (arg, indent) = match chunk {
                                StrChunk::Literal(s) => (Term::Str(s.clone()).into(), 0),
                                StrChunk::Expr(e, indent) => (e.clone(), *indent),
                            };

                            self.stack.push_str_chunks(chunks_iter.cloned());
                            self.stack.push_str_acc(StrAccData {
                                acc: String::new(),
                                env: env.clone(),
                                curr_indent: indent,
                                curr_pos: arg.pos,
                            });

                            Closure {
                                body: RichTerm::new(Term::Op1(UnaryOp::ChunksConcat(), arg), pos),
                                env,
                            }
                        }
                    }
                }
                Term::RecRecord(record, dyn_fields, _) => {
                    let rec_env =
                        fixpoint::rec_env(&mut self.cache, record.fields.iter(), &env, pos)?;

                    record.fields.iter().try_for_each(|(_, rt)| {
                        fixpoint::patch_field(&mut self.cache, rt, &rec_env, &env)
                    })?;

                    //TODO: We should probably avoid cloning the record, using `match_sharedterm`
                    //instead of `match` in the main eval loop, if possible
                    let static_part = RichTerm::new(Term::Record(record.clone()), pos);

                    // Transform the static part `{stat1 = val1, ..., statn = valn}` and the
                    // dynamic part `{exp1 = dyn_val1, ..., expm = dyn_valm}` to a sequence of
                    // extensions
                    //
                    // ```
                    // %record_insert% exp1
                    //   (...
                    //     (%record_insert% expn {stat1 = val1, ..., statn = valn} dyn_valn)
                    //   ...)
                    //   dyn_val1
                    //
                    // ```
                    //
                    // The `dyn_val` are given access to the recursive environment, but the
                    // recursive environment only contains the static fields, and not the dynamic
                    // fields.
                    let extended = dyn_fields
                        .iter()
                        .try_fold::<_, _, Result<RichTerm, EvalError>>(
                            static_part,
                            |acc, (name_as_term, field)| {
                                let pos = if let Some(ref value) = field.value {
                                    value.pos
                                } else {
                                    name_as_term.pos
                                };

                                fixpoint::patch_field(&mut self.cache, field, &rec_env, &env)?;

                                let ext_kind = field.extension_kind();
                                let Field {
                                    metadata,
                                    value,
                                    pending_contracts,
                                } = field;

                                //TODO[LAZYPROP]: we should probably closurize the pending
                                //contracts. It seems to work currently, but looks a bit fragile
                                //with respect to refactoring/changes.
                                let extend = mk_term::op2(
                                    BinaryOp::DynExtend {
                                        metadata: metadata.clone(),
                                        pending_contracts: pending_contracts.clone(),
                                        ext_kind,
                                    },
                                    name_as_term.clone(),
                                    acc,
                                );

                                let result = match value {
                                    Some(value) => RichTerm::new(
                                        Term::App(extend, value.clone()),
                                        pos.into_inherited(),
                                    ),
                                    None => extend,
                                };

                                Ok(result)
                            },
                        )?;

                    Closure {
                        body: extended.with_pos(pos),
                        env,
                    }
                }
                Term::ResolvedImport(id) => {
                    if let Some(t) = self.import_resolver.get(*id) {
                        Closure::atomic_closure(t)
                    } else {
                        return Err(EvalError::InternalError(
                            format!("Resolved import not found ({id:?})"),
                            pos,
                        ));
                    }
                }
                Term::Import(path) => {
                    return Err(EvalError::InternalError(
                        format!("Unresolved import ({})", path.to_string_lossy()),
                        pos,
                    ))
                }
                // Closurize the array if it's not already done.
                // This *should* make it unnecessary to call closurize in [operation].
                // See the comment on the `BinaryOp::ArrayConcat` match arm.
                Term::Array(terms, attrs) if !attrs.closurized => {
                    let mut local_env = Environment::new();

                    let closurized_array = terms
                        .clone()
                        .into_iter()
                        .map(|t| t.closurize(&mut self.cache, &mut local_env, env.clone()))
                        .collect();

                    let closurized_ctrs = attrs
                        .pending_contracts
                        .iter()
                        .map(|ctr| {
                            PendingContract::new(
                                ctr.contract.clone().closurize(
                                    &mut self.cache,
                                    &mut local_env,
                                    env.clone(),
                                ),
                                ctr.label.clone(),
                            )
                        })
                        .collect();

                    Closure {
                        body: RichTerm::new(
                            Term::Array(
                                closurized_array,
                                ArrayAttrs {
                                    closurized: true,
                                    pending_contracts: closurized_ctrs,
                                },
                            ),
                            pos,
                        ),
                        env: local_env,
                    }
                }
                Term::ParseError(parse_error) => {
                    return Err(EvalError::ParseError(parse_error.clone()));
                }
                Term::RuntimeError(error) => {
                    return Err(error.clone());
                }
                // For now, we simply erase annotations at runtime. They aren't accessible anyway
                // (as opposed to field metadata) and don't change the operational semantics, as
                // long as we generate the corresponding contract application when consuming it.
                //
                // The situation could change if we want to implement optimizations such as
                // avoiding repeated contract application. Annotations could then be a good way of
                // remembering which contracts have been applied to a value.
                Term::Annotated(annot, inner) => {
                    let contracts = annot.as_pending_contracts()?;
                    let pos = inner.pos;
                    let inner_with_ctr =
                        PendingContract::apply_all(inner.clone(), contracts.into_iter(), pos);

                    Closure {
                        body: inner_with_ctr,
                        env,
                    }
                }
                // Continuation of operations and element update
                _ if self.stack.is_top_idx() || self.stack.is_top_cont() => {
                    clos = Closure {
                        body: RichTerm {
                            term: shared_term,
                            pos,
                        },
                        env,
                    };
                    if self.stack.is_top_idx() {
                        update_at_indices(&mut self.cache, &mut self.stack, &clos);
                        clos
                    } else {
                        self.continuate_operation(clos)?
                    }
                }
                // Function call
                Term::Fun(x, t) => {
                    if let Some((idx, pos_app)) = self.stack.pop_arg_as_idx(&mut self.cache) {
                        self.call_stack.enter_fun(pos_app);
                        env.insert(*x, idx);
                        Closure {
                            body: t.clone(),
                            env,
                        }
                    } else {
                        return Ok((RichTerm::new(Term::Fun(*x, t.clone()), pos), env));
                    }
                }
                // A match expression acts as a function (in Nickel, a match expression corresponds
                // to the cases, and doesn't include the examined value).
                //
                // The behavior is the same as for a function: we look for an argument on the
                // stack, and proceed to the evaluation of the match, or stop here otherwise. If
                // found (let's call it `arg`), we evaluate `%match% arg cases default`, where
                // `%match%` is the primitive operation `UnaryOp::Match` taking care of forcing the
                // argument `arg` and doing the actual matching operation.
                Term::Match { cases, default } => {
                    if let Some((arg, pos_app)) = self.stack.pop_arg(&self.cache) {
                        // Setting the stack to be as if we would have evaluated an application
                        // `_ cases default`, where `default` is optional, and `_` is not relevant.

                        let has_default = default.is_some();

                        if let Some(t) = default {
                            self.stack.push_arg(
                                Closure {
                                    body: t.clone(),
                                    env: env.clone(),
                                },
                                pos,
                            );
                        }

                        self.stack.push_arg(
                            Closure {
                                body: RichTerm::new(
                                    Term::Record(RecordData::with_field_values(cases.clone())),
                                    pos,
                                ),
                                env: env.clone(),
                            },
                            pos,
                        );

                        // Now evaluating `%match% arg`, the left-most part of the application `%match%
                        // arg cases default`, which is in fact a primop application.
                        self.stack.push_op_cont(
                            OperationCont::Op1(UnaryOp::Match { has_default }, pos_app),
                            self.call_stack.len(),
                            pos,
                        );

                        arg
                    } else {
                        return Ok((
                            RichTerm::new(
                                Term::Match {
                                    cases: cases.clone(),
                                    default: default.clone(),
                                },
                                pos,
                            ),
                            env,
                        ));
                    }
                }
                // Otherwise, this is either an ill-formed application, or we are done
                t => {
                    if let Some((arg, pos_app)) = self.stack.pop_arg(&self.cache) {
                        return Err(EvalError::NotAFunc(
                            RichTerm {
                                term: shared_term.clone(),
                                pos,
                            },
                            arg.body,
                            pos_app,
                        ));
                    } else {
                        return Ok((RichTerm::new(t.clone(), pos), env));
                    }
                }
            }
        }
    }
}

impl<C: Cache> VirtualMachine<ImportCache, C> {
    pub fn prepare_eval(&mut self, main_id: FileId) -> Result<(RichTerm, Environment), Error> {
        let Envs {
            eval_env,
            type_ctxt,
        }: Envs = self.import_resolver.prepare_stdlib(&mut self.cache)?;
        self.import_resolver.prepare(main_id, &type_ctxt)?;
        Ok((self.import_resolver().get(main_id).unwrap(), eval_env))
    }

    pub fn prepare_stdlib(&mut self) -> Result<Envs, Error> {
        self.import_resolver.prepare_stdlib(&mut self.cache)
    }
}

/// Kind of an identifier.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IdentKind {
    Let,
    Lambda,
    Record,
}

/// A closure, a term together with an environment.
#[derive(PartialEq)]
pub struct Closure {
    pub body: RichTerm,
    pub env: Environment,
}

impl std::fmt::Debug for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Closure")
            .field("body", &self.body)
            .finish_non_exhaustive()
    }
}

impl Clone for Closure {
    fn clone(&self) -> Self {
        Closure {
            body: self.body.clone(),
            env: self.env.clone(),
        }
    }
}

impl Closure {
    pub fn atomic_closure(body: RichTerm) -> Closure {
        let env: Environment = Environment::new();
        Closure { body, env }
    }
}

#[allow(type_alias_bounds)] // TODO: Look into this warning.
pub type Environment = GenericEnvironment<Ident, CacheIndex>;

/// Raised when trying to build an environment from a term which is not a record.
#[derive(Clone, Debug)]
pub enum EnvBuildError {
    NotARecord(RichTerm),
}

/// Add the bindings of a record to an environment. Ignore the fields defined by interpolation as
/// well as fields without definition.
pub fn env_add_term<C: Cache>(
    cache: &mut C,
    env: &mut Environment,
    rt: RichTerm,
) -> Result<(), EnvBuildError> {
    match_sharedterm! {rt.term, with {
            Term::Record(record) | Term::RecRecord(record, ..) => {
                let ext = record.fields.into_iter().filter_map(|(id, field)| {
                    field.value.map(|value|
                    (
                        id,
                        cache.add(
                            Closure::atomic_closure(value),
                            IdentKind::Record,
                            BindingType::Normal
                        ),
                    ))
                });

                env.extend(ext);
                Ok(())
            },
        } else Err(EnvBuildError::NotARecord(rt))
    }
}

/// Bind a closure in an environment.
pub fn env_add<C: Cache>(
    cache: &mut C,
    env: &mut Environment,
    id: Ident,
    rt: RichTerm,
    local_env: Environment,
) {
    let closure = Closure {
        body: rt,
        env: local_env,
    };
    env.insert(id, cache.add(closure, IdentKind::Let, BindingType::Normal));
}

/// Pop and update all the indices on the top of the stack with the given closure.
fn update_at_indices<C: Cache>(cache: &mut C, stack: &mut Stack<C>, closure: &Closure) {
    while let Some(idx) = stack.pop_update_index() {
        cache.update(closure.clone(), idx);
    }
}

/// Recursively substitute each variable occurrence of a term for its value in the environment.
pub fn subst<C: Cache>(
    cache: &C,
    rt: RichTerm,
    initial_env: &Environment,
    env: &Environment,
) -> RichTerm {
    let RichTerm { term, pos } = rt;

    match term.into_owned() {
        Term::Var(id) => env
            .get(&id)
            .or_else(|| initial_env.get(&id))
            .map(|idx| {
                let closure = cache.get(idx.clone());
                subst(cache, closure.body, initial_env, &closure.env)
            })
            .unwrap_or_else(|| RichTerm::new(Term::Var(id), pos)),
        v @ Term::Null
        | v @ Term::ParseError(_)
        | v @ Term::RuntimeError(_)
        | v @ Term::Bool(_)
        | v @ Term::Num(_)
        | v @ Term::Str(_)
        // Do not substitute under lambdas: mutually recursive function could cause an infinite
        // loop. Although avoidable, this requires some care and is not currently needed.
        | v @ Term::Fun(..)
        | v @ Term::Lbl(_)
        | v @ Term::SealingKey(_)
        | v @ Term::Enum(_)
        | v @ Term::Import(_)
        | v @ Term::ResolvedImport(_) => RichTerm::new(v, pos),
        Term::Let(id, t1, t2, attrs) => {
            let t1 = subst(cache, t1, initial_env, env);
            let t2 = subst(cache, t2, initial_env, env);

            RichTerm::new(Term::Let(id, t1, t2, attrs), pos)
        }
        p @ Term::LetPattern(..) => panic!("Pattern {p:?} has not been transformed before evaluation"),
        p @ Term::FunPattern(..) => panic!("Pattern {p:?} has not been transformed before evaluation"),
        Term::App(t1, t2) => {
            let t1 = subst(cache, t1, initial_env, env);
            let t2 = subst(cache, t2, initial_env, env);

            RichTerm::new(Term::App(t1, t2), pos)
        }
        Term::Match {cases, default} => {
            let default =
                default.map(|d| subst(cache, d, initial_env, env));
            let cases = cases
                .into_iter()
                .map(|(id, t)| {
                    (
                        id,
                        subst(cache, t, initial_env, env),
                    )
                })
                .collect();

            RichTerm::new(Term::Match {cases, default}, pos)
        }
        Term::Op1(op, t) => {
            let t = subst(cache, t, initial_env, env);

            RichTerm::new(Term::Op1(op, t), pos)
        }
        Term::Op2(op, t1, t2) => {
            let t1 = subst(cache, t1, initial_env, env);
            let t2 = subst(cache, t2, initial_env, env);

            RichTerm::new(Term::Op2(op, t1, t2), pos)
        }
        Term::OpN(op, ts) => {
            let ts = ts
                .into_iter()
                .map(|t| subst(cache, t, initial_env, env))
                .collect();

            RichTerm::new(Term::OpN(op, ts), pos)
        }
        Term::Sealed(i, t, lbl) => {
            let t = subst(cache, t, initial_env, env);
            RichTerm::new(Term::Sealed(i, t, lbl), pos)
        }
        Term::Record(record) => {
            let record = record.map_defined_values(|_, value| subst(cache, value, initial_env, env));

            RichTerm::new(Term::Record(record), pos)
        }
        Term::RecRecord(record, dyn_fields, deps) => {
            let record = record.map_defined_values(|_, value| subst(cache, value, initial_env, env));

            let dyn_fields = dyn_fields
                .into_iter()
                .map(|(id_t, field)| {
                    (
                        subst(cache, id_t, initial_env, env),
                        field.map_value(|v| subst(cache, v, initial_env, env)),
                    )
                })
                .collect();

            RichTerm::new(Term::RecRecord(record, dyn_fields, deps), pos)
        }
        Term::Array(ts, attrs) => {
            let ts = ts
                .into_iter()
                .map(|t| subst(cache, t, initial_env, env))
                .collect();

            RichTerm::new(Term::Array(ts, attrs), pos)
        }
        Term::StrChunks(chunks) => {
            let chunks = chunks
                .into_iter()
                .map(|chunk| match chunk {
                    chunk @ StrChunk::Literal(_) => chunk,
                    StrChunk::Expr(t, indent) => StrChunk::Expr(
                        subst(cache, t, initial_env, env),
                        indent,
                    ),
                })
                .collect();

            RichTerm::new(Term::StrChunks(chunks), pos)
        }
        Term::Annotated(annot, t) => {
            // Currently, there is no interest in replacing variables inside contracts, thus we
            // limit the work of `subst`.
            RichTerm::new(Term::Annotated(annot, subst(cache, t, initial_env, env)), pos)
        }
    }
}

#[cfg(test)]
mod tests;
