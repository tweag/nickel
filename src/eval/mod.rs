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
//! - A pair of [environment][Environment], mapping identifiers to [closures][Closure]:
//!     * The initial environment contains builtin functions accessible from anywhere, and alive
//!     during the whole evaluation
//!     * The local environment contains the variables in scope of the current term and is subject
//!     to garbage collection (currently reference counting based)
//! - A [callstack], mainly for error reporting purpose
//!
//! Depending on the shape of the current term, the following actions are preformed:
//!
//! ## Core calculus
//! - **Var(id)**: the term bound to `id` in the environment is fetched, and an update thunk is
//!   pushed on the stack to indicate that once this term has been evaluated, the content of the
//!   variable must be updated
//! - **App(func, arg)**: a closure containing the argument and the current environment is pushed
//!   on the stack, and the applied term `func` is evaluated
//! - **Let(id, term, body)**: `term` is bound to `id` in the environment, and the machine proceeds with the evaluation of the body
//! - **Fun(id, body)**: Try to pop an argument from the stack. If there is some, we bound it to
//!   `id` in the environment, and proceed with the body of the function. Otherwise, we are done: the
//!   end result is an unapplied function
//! - **Thunk on stack**: If the evaluation of the current term is done, and there is one (or
//!   several) thunk on the stack, this means we have to perform an update. Consecutive thunks are
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
//! ## Enriched values
//!
//! The evaluation of enriched values is controlled by the parameter `eval_mode`. If it is
//! set to `UnwrapMeta` (which is usually the case), the machine tries to extract a simple
//! value from it:
//!  - **Contract**: raise an error. This usually means that an access to a field was attempted,
//!    and that this field had a contract to satisfy, but it was never defined.
//!  - **Default(value)**: an access to a field which has a default value. Proceed with the
//!    evaluation of this value
//!  - **ContractDefault(type, label, value)**: same as above, but the field also has an attached
//!    contract.  Proceed with the evaluation of `Assume(type, label, value)` to ensure that the
//!    default value satisfies this contract.
//!
//! If `eval_mode` is set to StopAtMeta, as it is when evaluating `merge`, the machine does not
//! evaluate enriched values further, and consider the term evaluated.
//!
//! # Garbage collection
//!
//! Currently the machine relies on Rust's reference counting to manage memory. Precisely, the
//! environment stores `Rc<RefCell<Closure>>` objects, which are reference-counted pointers to a
//! mutable memory cell. This means that we do not deep copy everything everywhere, but this is
//! probably suboptimal for a functional language and is unable to collect cyclic data, which may
//! appear inside recursive records in the future. An adapted garbage collector is probably
//! something to consider at some point.

use crate::{
    cache::ImportResolver,
    environment::Environment as GenericEnvironment,
    error::EvalError,
    identifier::Ident,
    match_sharedterm,
    term::{
        make as mk_term, record::RecordData, ArrayAttrs, BinaryOp, BindingType, LetAttrs,
        MetaValue, PendingContract, RichTerm, SharedTerm, StrChunk, Term, UnaryOp,
    },
    transform::Closurizable,
};

pub mod callstack;
pub mod fixpoint;
pub mod lazy;
pub mod merge;
pub mod operation;
pub mod stack;

use callstack::*;
use lazy::*;
use operation::OperationCont;
use stack::Stack;

impl AsRef<Vec<StackElem>> for CallStack {
    fn as_ref(&self) -> &Vec<StackElem> {
        &self.0
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum EvalMode {
    UnwrapMeta,
    StopAtMeta,
}

impl Default for EvalMode {
    fn default() -> Self {
        EvalMode::UnwrapMeta
    }
}

// The current state of the Nickel virtual machine.
pub struct VirtualMachine<R: ImportResolver> {
    // Behavior of evaluation with respect to metavalues.
    eval_mode: EvalMode,
    // The main stack, storing arguments, thunks and pending computations.
    stack: Stack,
    // The call stack, for error reporting.
    call_stack: CallStack,
    // The interface used to fetch imports.
    import_resolver: R,
}

impl<R: ImportResolver> VirtualMachine<R> {
    pub fn new(import_resolver: R) -> Self {
        VirtualMachine {
            import_resolver,
            eval_mode: Default::default(),
            call_stack: Default::default(),
            stack: Default::default(),
        }
    }

    /// Reset the state of the machine (stacks, eval mode and thunk state) to prepare for another evaluation round.
    pub fn reset(&mut self) {
        self.eval_mode = Default::default();
        self.call_stack.0.clear();
        self.stack.reset();
    }

    fn set_mode(&mut self, new_mode: EvalMode) {
        if self.eval_mode != new_mode {
            self.stack.push_strictness(self.eval_mode);
        }

        self.eval_mode = new_mode;
    }

    pub fn import_resolver(&self) -> &R {
        &self.import_resolver
    }

    pub fn import_resolver_mut(&mut self) -> &mut R {
        &mut self.import_resolver
    }

    /// Evaluate a Nickel term. Wrapper around [eval_closure] that starts from an empty local
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
            .map(|(term, env)| subst(term, initial_env, &env))
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
        let wrapper = mk_term::op1(UnaryOp::Force(None), rt);
        self.eval_closure(Closure::atomic_closure(wrapper), initial_env)
    }

    /// Evaluate a Nickel Term, stopping when a meta value is encountered at the top-level without
    /// unwrapping it. Then evaluate the underlying value, and substitute variables in order to obtain
    /// a WHNF that is printable.
    ///
    /// Used to query the metadata of a value.
    pub fn eval_meta(
        &mut self,
        t: RichTerm,
        initial_env: &Environment,
    ) -> Result<RichTerm, EvalError> {
        self.eval_mode = EvalMode::StopAtMeta;
        let (mut rt, env) = self.eval_closure(Closure::atomic_closure(t), initial_env)?;

        if let Term::MetaValue(ref mut meta) = *SharedTerm::make_mut(&mut rt.term) {
            if let Some(t) = meta.value.take() {
                self.reset();
                let (evaluated, env) = self.eval_closure(Closure { body: t, env }, initial_env)?;
                let substituted = subst(evaluated, initial_env, &env);

                meta.value = Some(substituted);
            }
        };

        Ok(rt)
    }

    /// The main loop of evaluation.
    ///
    /// Implement the evaluation of the core language, which includes application, thunk update,
    /// evaluation of the arguments of operations, and a few others. The specific implementations of
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

            if let Some(eval_mode) = self.stack.pop_strictness_marker() {
                self.eval_mode = eval_mode;
            }

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
                    // Update the original thunk (the thunk which holds the result of the op) in both cases,
                    // even if we continue with a seq.
                    // We do this because  we are on a `Sealed` term, and this is in WHNF, and if we don't,
                    // we will be unwrapping a `Sealed` term and assigning the "unsealed" value to the result
                    // of the `Seq` operation. See also: https://github.com/tweag/nickel/issues/123
                    update_thunks(&mut self.stack, &closure);
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
                            return Err(EvalError::BlameError(
                                lbl.clone(),
                                self.call_stack.clone(),
                            ));
                        }
                    }
                }
                Term::Var(x) => {
                    let mut thunk = env
                        .get(x)
                        .or_else(|| initial_env.get(x))
                        .cloned()
                        .ok_or_else(|| EvalError::UnboundIdentifier(x.clone(), pos))?;
                    std::mem::drop(env); // thunk may be a 1RC pointer

                    if thunk.state() != ThunkState::Evaluated {
                        if thunk.should_update() {
                            match thunk.mk_update_frame() {
                                Ok(thunk_upd) => self.stack.push_thunk(thunk_upd),
                                Err(BlackholedError) => {
                                    return Err(EvalError::InfiniteRecursion(
                                        self.call_stack.clone(),
                                        pos,
                                    ))
                                }
                            }
                        }
                        // If the thunk isn't to be updated, directly set the evaluated flag.
                        else {
                            thunk.set_evaluated();
                        }
                    }
                    self.call_stack
                        .enter_var(thunk.ident_kind(), x.clone(), pos);
                    thunk.into_closure()
                }
                Term::App(t1, t2) => {
                    self.call_stack.enter_app(pos);
                    self.set_mode(EvalMode::UnwrapMeta);

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
                    let closure = Closure {
                        body: s.clone(),
                        env: env.clone(),
                    };

                    let mut thunk = match binding_type {
                        BindingType::Normal => Thunk::new(closure, IdentKind::Let),
                        BindingType::Revertible(deps) => {
                            Thunk::new_rev(closure, IdentKind::Let, deps.clone())
                        }
                    };

                    // Patch the environment with the (x <- closure) binding
                    if *rec {
                        let thunk_ = thunk.clone();
                        thunk.borrow_mut().env.insert(x.clone(), thunk_);
                    }

                    env.insert(x.clone(), thunk);
                    Closure {
                        body: t.clone(),
                        env,
                    }
                }
                Term::Switch(exp, cases, default) => {
                    self.set_mode(EvalMode::UnwrapMeta);

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
                                Term::Record(RecordData::with_fields(cases.clone())),
                                pos,
                            ),
                            env: env.clone(),
                        },
                        pos,
                    );

                    Closure {
                        body: RichTerm::new(
                            Term::Op1(UnaryOp::Switch(has_default), exp.clone()),
                            pos,
                        ),
                        env,
                    }
                }
                Term::Op1(op, t) => {
                    self.set_mode(op.eval_mode());

                    self.stack.push_op_cont(
                        OperationCont::Op1(op.clone(), t.pos),
                        self.call_stack.len(),
                        pos,
                    );

                    // Special casing (hopefully temporary) due to the need for `DeepSeq` to produce
                    // acceptable error message for missing field definition occurring when sequencing
                    // a record. See the definition of `UnaryOp::DeepSeq`.
                    if let UnaryOp::DeepSeq(Some(stack_elem)) | UnaryOp::Force(Some(stack_elem)) =
                        op
                    {
                        self.call_stack.0.push(stack_elem.clone());
                    }

                    Closure {
                        body: t.clone(),
                        env,
                    }
                }
                Term::Op2(op, fst, snd) => {
                    self.set_mode(op.eval_mode());

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
                    self.set_mode(op.eval_mode());

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

                            self.set_mode(EvalMode::UnwrapMeta);
                            self.stack.push_str_chunks(chunks_iter.cloned());
                            self.stack.push_str_acc(String::new(), indent, env.clone());

                            Closure {
                                body: RichTerm::new(Term::Op1(UnaryOp::ChunksConcat(), arg), pos),
                                env,
                            }
                        }
                    }
                }
                Term::RecRecord(record, dyn_fields, _) => {
                    let rec_env = fixpoint::rec_env(record.fields.iter(), &env)?;

                    record
                        .fields
                        .iter()
                        .try_for_each(|(_, rt)| fixpoint::patch_field(rt, &rec_env, &env))?;

                    //TODO: We should probably avoid cloning the record, using `match_sharedterm`
                    //instead of `match` in the main eval loop, if possible
                    let static_part = RichTerm::new(Term::Record(record.clone()), pos);

                    // Transform the static part `{stat1 = val1, ..., statn = valn}` and the dynamic
                    // part `{exp1 = dyn_val1, ..., expm = dyn_valm}` to a sequence of extensions
                    // `%record_insert% exp1 (... (%record_insert% expn {stat1 = val1, ..., statn = valn} dyn_valn)) dyn_val1`
                    // The `dyn_val` are given access to the recursive environment, but the recursive
                    // environment only contains the static fields, and not the dynamic fields.
                    let extended = dyn_fields
                        .iter()
                        .try_fold::<_, _, Result<RichTerm, EvalError>>(
                            static_part,
                            |acc, (id_t, t)| {
                                let id_t = id_t.clone();
                                let pos = t.pos;

                                fixpoint::patch_field(t, &rec_env, &env)?;
                                Ok(RichTerm::new(
                                    Term::App(
                                        mk_term::op2(BinaryOp::DynExtend(), id_t, acc),
                                        t.clone(),
                                    ),
                                    pos.into_inherited(),
                                ))
                            },
                        )?;

                    Closure {
                        body: extended.with_pos(pos),
                        env,
                    }
                }
                // Unwrapping of enriched terms
                Term::MetaValue(meta) if self.eval_mode == EvalMode::UnwrapMeta => {
                    if meta.value.is_some() {
                        /* Since we are forcing a metavalue, we are morally evaluating `force t` rather
                         * than `t` itself. Updating a thunk after having performed this forcing may
                         * alter the semantics of the program in an unexpected way (see issue
                         * https://github.com/tweag/nickel/issues/123): we update potential thunks now
                         * so that their content remains a meta value.
                         */
                        let update_closure = Closure {
                            body: RichTerm {
                                term: shared_term.clone(),
                                pos,
                            },
                            env,
                        };
                        update_thunks(&mut self.stack, &update_closure);

                        let Closure {
                            body: RichTerm { term, .. },
                            env,
                        } = update_closure;

                        match term.into_owned() {
                            Term::MetaValue(MetaValue {
                                value: Some(inner), ..
                            }) => Closure { body: inner, env },
                            _ => unreachable!(),
                        }
                    } else {
                        let label = meta
                            .contracts
                            .last()
                            .or(meta.types.as_ref())
                            .map(|ctr| ctr.label.clone());
                        return Err(EvalError::MissingFieldDef(label, self.call_stack.clone()));
                    }
                }
                Term::ResolvedImport(id) => {
                    if let Some(t) = self.import_resolver.get(*id) {
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
                        format!("Unresolved import ({})", path.to_string_lossy()),
                        pos,
                    ))
                }
                // Closurize the array if it's not already done.
                // This *should* make it unecessary to call closurize in [operation].
                // See the comment on the `BinaryOp::ArrayConcat` match arm.
                Term::Array(terms, attrs) if !attrs.closurized => {
                    let mut local_env = Environment::new();

                    let closurized_array = terms
                        .clone()
                        .into_iter()
                        .map(|t| t.closurize(&mut local_env, env.clone()))
                        .collect();

                    let closurized_ctrs = attrs
                        .pending_contracts
                        .iter()
                        .map(|ctr| {
                            PendingContract::new(
                                ctr.contract.clone().closurize(&mut local_env, env.clone()),
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
                // Continuation of operations and thunk update
                _ if self.stack.is_top_thunk() || self.stack.is_top_cont() => {
                    clos = Closure {
                        body: RichTerm {
                            term: shared_term,
                            pos,
                        },
                        env,
                    };
                    if self.stack.is_top_thunk() {
                        update_thunks(&mut self.stack, &clos);
                        clos
                    } else {
                        self.continuate_operation(clos)?
                    }
                }
                // Function call
                Term::Fun(x, t) => {
                    if let Some((thunk, pos_app)) = self.stack.pop_arg_as_thunk() {
                        self.call_stack.enter_fun(pos_app);
                        env.insert(x.clone(), thunk);
                        Closure {
                            body: t.clone(),
                            env,
                        }
                    } else {
                        return Ok((RichTerm::new(Term::Fun(x.clone(), t.clone()), pos), env));
                    }
                }
                // Otherwise, this is either an ill-formed application, or we are done
                t => {
                    if let Some((arg, pos_app)) = self.stack.pop_arg() {
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

/// Kind of an identifier.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IdentKind {
    Let,
    Lambda,
    Record,
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
            env: Environment::new(),
        }
    }
}

pub type Environment = GenericEnvironment<Ident, Thunk>;

/// Raised when trying to build an environment from a term which is not a record.
#[derive(Clone, Debug)]
pub enum EnvBuildError {
    NotARecord(RichTerm),
}

/// Add the bindings of a record to an environment. Ignore the fields defined by interpolation.
pub fn env_add_term(env: &mut Environment, rt: RichTerm) -> Result<(), EnvBuildError> {
    match_sharedterm! {rt.term, with {
            Term::Record(record) | Term::RecRecord(record, ..) => {
                let ext = record.fields.into_iter().map(|(id, t)| {
                    (
                        id,
                        Thunk::new(Closure::atomic_closure(t), IdentKind::Record),
                    )
                });

                env.extend(ext);
                Ok(())
            },
        } else Err(EnvBuildError::NotARecord(rt))
    }
}

/// Bind a closure in an environment.
pub fn env_add(env: &mut Environment, id: Ident, rt: RichTerm, local_env: Environment) {
    let closure = Closure {
        body: rt,
        env: local_env,
    };
    env.insert(id, Thunk::new(closure, IdentKind::Let));
}

/// Pop and update all the thunks on the top of the stack with the given closure.
fn update_thunks(stack: &mut Stack, closure: &Closure) {
    while let Some(thunk) = stack.pop_thunk() {
        thunk.update(closure.clone());
    }
}

/// Recursively substitute each variable occurrence of a term for its value in the environment.
pub fn subst(rt: RichTerm, initial_env: &Environment, env: &Environment) -> RichTerm {
    let RichTerm { term, pos } = rt;

    match term.into_owned() {
        Term::Var(id) => env
            .get(&id)
            .or_else(|| initial_env.get(&id))
            .map(|thunk| {
                let closure = thunk.get_owned();
                subst(closure.body, initial_env, &closure.env)
            })
            .unwrap_or_else(|| RichTerm::new(Term::Var(id), pos)),
        v @ Term::Null
        | v @ Term::ParseError(_)
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
            let t1 = subst(t1, initial_env, env);
            let t2 = subst(t2, initial_env, env);

            RichTerm::new(Term::Let(id, t1, t2, attrs), pos)
        }
        p @ Term::LetPattern(..) => panic!("Pattern {:?} has not been transformed before evaluation", p),
        p @ Term::FunPattern(..) => panic!("Pattern {:?} has not been transformed before evaluation", p),
        Term::App(t1, t2) => {
            let t1 = subst(t1, initial_env, env);
            let t2 = subst(t2, initial_env, env);

            RichTerm::new(Term::App(t1, t2), pos)
        }
        Term::Switch(t, cases, default) => {
            let default =
                default.map(|d| subst(d, initial_env, env));
            let cases = cases
                .into_iter()
                .map(|(id, t)| {
                    (
                        id,
                        subst(t, initial_env, env),
                    )
                })
                .collect();
            let t = subst(t, initial_env, env);

            RichTerm::new(Term::Switch(t, cases, default), pos)
        }
        Term::Op1(op, t) => {
            let t = subst(t, initial_env, env);

            RichTerm::new(Term::Op1(op, t), pos)
        }
        Term::Op2(op, t1, t2) => {
            let t1 = subst(t1, initial_env, env);
            let t2 = subst(t2, initial_env, env);

            RichTerm::new(Term::Op2(op, t1, t2), pos)
        }
        Term::OpN(op, ts) => {
            let ts = ts
                .into_iter()
                .map(|t| subst(t, initial_env, env))
                .collect();

            RichTerm::new(Term::OpN(op, ts), pos)
        }
        Term::Sealed(i, t, lbl) => {
            let t = subst(t, initial_env, env);
            RichTerm::new(Term::Sealed(i, t, lbl), pos)
        }
        Term::Record(record) => {
            let fields = record.fields
                .into_iter()
                .map(|(id, t)| {
                    (
                        id,
                        subst(t, initial_env, env),
                    )
                })
                .collect();

            RichTerm::new(Term::Record(RecordData { fields, attrs: record.attrs }), pos)
        }
        Term::RecRecord(record, dyn_fields, deps) => {
            let fields = record.fields
                .into_iter()
                .map(|(id, t)| {
                    (
                        id,
                        subst(t, initial_env, env),
                    )
                })
                .collect();

            let dyn_fields = dyn_fields
                .into_iter()
                .map(|(id_t, t)| {
                    (
                        subst(id_t, initial_env, env),
                        subst(t, initial_env, env),
                    )
                })
                .collect();

            RichTerm::new(Term::RecRecord(RecordData { fields, attrs: record.attrs }, dyn_fields, deps), pos)
        }
        Term::Array(ts, attrs) => {
            let ts = ts
                .into_iter()
                .map(|t| subst(t, initial_env, env))
                .collect();

            RichTerm::new(Term::Array(ts, attrs), pos)
        }
        Term::StrChunks(chunks) => {
            let chunks = chunks
                .into_iter()
                .map(|chunk| match chunk {
                    chunk @ StrChunk::Literal(_) => chunk,
                    StrChunk::Expr(t, indent) => StrChunk::Expr(
                        subst(t, initial_env, env),
                        indent,
                    ),
                })
                .collect();

            RichTerm::new(Term::StrChunks(chunks), pos)
        }
        Term::MetaValue(meta) => {
            // Currently, there is no interest in replacing variables inside contracts, thus we
            // limit the work of `subst`. If this is needed at some point, just uncomment the
            // following code.

            // let contracts: Vec<_> = meta
            //     .contracts
            //     .into_iter()
            //     .map(|ctr| {
            //         let types = match ctr.types {
            //             Types(AbsType::Flat(t)) => Types(AbsType::Flat(subst(
            //                 t,
            //                 initial_env,
            //                 env,
            //                 Cow::Borrowed(bound.as_ref()),
            //             ))),
            //             ty => ty,
            //         };
            //
            //         Contract { types, ..ctr }
            //     })
            //     .collect();
            //
            // let types = meta.types.map(|ctr| {
            //     let types = match ctr.types {
            //         Types(AbsType::Flat(t)) => Types(AbsType::Flat(subst(
            //             t,
            //             initial_env,
            //             env,
            //             Cow::Borrowed(bound.as_ref()),
            //         ))),
            //         ty => ty,
            //     };
            //
            //     Contract { types, ..ctr }
            // });

            let value = meta.value.map(|t| subst(t, initial_env, env));

            let meta = MetaValue {
                doc: meta.doc,
                value,
                ..meta
            };

            RichTerm::new(Term::MetaValue(meta), pos)
        }
    }
}

/// Checks if the given term is a chain of nested and/or merged metavalues such that:
///
/// 1. At least one metavalue has the `optional` flag set
/// 2. The final value is undefined
///
/// This function is used to determine if a record field is an optional field without definition,
/// and should thus be ignored by record operations.
///
/// Beware: correctly checking that a value is an empty optional can incur arbitrary computations
/// in principle, but this function is supposed to be a quick peek. It's a terminating
/// approximation and hence is necessarily incomplete. In practice, this function is able to
/// traverse merge expressions and follow variables links, but within a limit (in the number of
/// variables followed).
pub fn is_empty_optional(rt: &RichTerm, env: &Environment) -> bool {
    fn is_empty_optional_aux(rt: &RichTerm, env: &Environment, is_opt: bool, gas: &mut u8) -> bool {
        match rt.as_ref() {
            Term::MetaValue(meta) => {
                let is_opt = is_opt || meta.opt;

                if let Some(ref next) = meta.value {
                    is_empty_optional_aux(next, env, is_opt, gas)
                } else {
                    is_opt
                }
            }
            Term::Op2(BinaryOp::Merge(), ref t1, ref t2) => {
                // The aggregated value for is_opt must follow the same logic as in the
                // implementation of merge: a field is optional if both operands define it as
                // optional.
                //
                // Thus the resulting value is an empty optional if either:
                // - both branch are empty optionals
                // - a wrapping metavalue is optional (is_opt is true at this point) and both
                // branch are empty.
                is_empty_optional_aux(t1, env, is_opt, gas)
                    && is_empty_optional_aux(t2, env, is_opt, gas)
            }
            Term::Var(id) if *gas > 0 => {
                if let Some(closure) = env.get(id).map(Thunk::borrow) {
                    *gas -= 1;
                    is_empty_optional_aux(&closure.body, &closure.env, is_opt, gas)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    // The total amount of gas is rather abritrary, but in any case, it ought to stay low: remember
    // that is_empty_optional may be called on each field of a record when evaluatinog some record
    // operations.
    is_empty_optional_aux(rt, env, false, &mut 8)
}

#[cfg(test)]
mod tests;
