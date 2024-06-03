//! Evaluation of a Nickel term.
//!
//! Implementation of the Nickel abstract machine. Note that this machine is not currently
//! formalized somewhere and is just a convenient name to designate the current implementation.
//!
//! # The Nickel Abstract Machine
//!
//! The abstract machine is a stack machine composed of the following elements:
//!
//! - The term being currently evaluated
//! - The main stack, storing arguments, cache indices and pending computations (continuations)
//! - A pair of [environment][Environment], mapping identifiers to [closures][Closure]:
//!     - The initial environment contains builtin functions accessible from anywhere, is immutable
//!       and alive during the whole evaluation
//!     - The local environment contains the variables in scope of the current term and is subject
//!       to garbage collection (currently reference counting based)
//! - A [callstack], mainly for error reporting purpose
//!
//! Depending on the shape of the current term, the following actions are performed:
//!
//! ## Core calculus
//!
//! - **variable (`id`)**: the term bound to `id` in the environment is fetched, and an update index is
//!   pushed on the stack to indicate that once this term has been evaluated, the cached value for
//!   this variable must be updated
//! - **application `func arg`**: a closure containing the argument and the current environment is pushed on
//!   the stack and the VM proceed with the evaluation of `func`
//! - **`let id = term in body`**: `term` is bound to `id` in the environment and the VM proceeds
//!   with the evaluation of `body`
//! - **`fun id => body`**: tries to pop an argument from the stack. If there is one, we bind it to
//!   `id` in the environment and proceed with the body of the function. Otherwise, we are done:
//!   the end result is a (unapplied) function
//! - **Index on stack**: if the evaluation of the current term is done, and there is one (or
//!   several) index on the stack, this means we have to perform an evaluation cache update.
//!   Consecutive indices are popped from the stack and are updated to point to the current evaluated
//!   term.
//! - **Import**: import must have been resolved before the evaluation starts. An unresolved import
//!   causes an [`crate::error::EvalError::InternalError`]. A resolved import, identified by a
//!   `FileId`, is retrieved from the import resolver and evaluation proceeds.
//!
//! ## Operators
//!
//! Operators are strict by definition. To evaluate say `exp1 + exp2`, the following steps have to
//! be performed:
//!
//! - `exp1` needs to be evaluated. The result must be saved somewhere, together with the resulting
//! environment
//! - same thing for `exp2`
//! - Finally, the implementation of `+` can proceed with the computation
//!
//! We detail the case of binary operators, as the case of unary ones is similar and simpler.
//!
//! - **Op(op, first, second)**: pushes an `OpFirst` element on the stack, which saves the operator
//! `op`, the second argument `second` and the current environment, and proceed with the evaluation
//! of `first`
//! - **OpFirst on stack**: if the evaluation of the current term is done and there is an `OpFirst`
//!   marker on the stack, then:
//!     1. Extract the saved operator, the second argument and the environment `env2` from the
//!        marker
//!     2. Push an `OpSecond` marker, saving the operator and the evaluated form of the first
//!        argument with its environment
//!     3. Proceed with the evaluation of the second argument in environment `env2`
//! - **OpSecond on stack**: once the second term is evaluated, we can get back the operator and the
//!   first term evaluated, and forward all both arguments evaluated and their respective
//!   environment to the specific implementation of the operator (located in [operation], or in
//!   [merge] for `merge`).
//!
//! # Garbage collection
//!
//! Currently the machine relies on Rust's reference counting to manage memory. Precisely, the
//! environment stores `Rc<RefCell<Closure>>` objects, which are reference-counted pointers to a
//! mutable memory cell. This means that we do not deep copy everything everywhere, but this is
//! probably suboptimal for a functional language and is unable to collect cyclic data, which may
//! appear inside recursive records. A dedicated garbage collector is probably something to
//! consider at some point.

use crate::identifier::Ident;
use crate::term::string::NickelString;
use crate::{
    cache::{Cache as ImportCache, Envs, ImportResolver},
    closurize::{closurize_rec_record, Closurize},
    environment::Environment as GenericEnvironment,
    error::{Error, EvalError},
    identifier::LocIdent,
    match_sharedterm,
    position::TermPos,
    program::FieldPath,
    term::{
        array::ArrayAttrs,
        make as mk_term,
        pattern::compile::Compile,
        record::{Field, RecordData},
        BinaryOp, BindingType, LetAttrs, MatchBranch, MatchData, RecordOpKind, RichTerm,
        RuntimeContract, StrChunk, Term, UnaryOp,
    },
};

use std::io::Write;

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
    // The initial environment containing stdlib and builtin functions accessible from anywhere
    initial_env: Environment,
    // The stream for writing trace output.
    trace: Box<dyn Write>,
}

impl<R: ImportResolver, C: Cache> VirtualMachine<R, C> {
    pub fn new(import_resolver: R, trace: impl Write + 'static) -> Self {
        VirtualMachine {
            import_resolver,
            call_stack: Default::default(),
            stack: Stack::new(),
            cache: Cache::new(),
            initial_env: Environment::new(),
            trace: Box::new(trace),
        }
    }

    pub fn new_with_cache(import_resolver: R, cache: C, trace: impl Write + 'static) -> Self {
        VirtualMachine {
            import_resolver,
            call_stack: Default::default(),
            stack: Stack::new(),
            cache,
            trace: Box::new(trace),
            initial_env: Environment::new(),
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

    /// Evaluate a Nickel term. Wrapper around [VirtualMachine::eval_closure] that starts from an
    /// empty local environment and drops the final environment.
    pub fn eval(&mut self, t: RichTerm) -> Result<RichTerm, EvalError> {
        self.eval_closure(Closure::atomic_closure(t))
            .map(|closure| closure.body)
    }

    /// Fully evaluate a Nickel term: the result is not a WHNF but to a value with all variables
    /// substituted.
    pub fn eval_full(&mut self, t0: RichTerm) -> Result<RichTerm, EvalError> {
        self.eval_full_closure(Closure::atomic_closure(t0))
            .map(|result| result.body)
    }

    /// Same as [Self::eval_full], but takes a closure as an argument instead of a term.
    pub fn eval_full_closure(&mut self, t0: Closure) -> Result<Closure, EvalError> {
        self.eval_deep_closure_impl(t0, false)
            .map(|result| Closure {
                body: subst(&self.cache, result.body, &self.initial_env, &result.env),
                env: result.env,
            })
    }

    /// Like [Self::eval_full], but skips evaluating record fields marked `not_exported`.
    pub fn eval_full_for_export(&mut self, t0: RichTerm) -> Result<RichTerm, EvalError> {
        self.eval_deep_closure_impl(Closure::atomic_closure(t0), true)
            .map(|result| subst(&self.cache, result.body, &self.initial_env, &result.env))
    }

    /// Same as [Self::eval_full_for_export], but takes a closure as an argument instead of a term.
    pub fn eval_full_for_export_closure(
        &mut self,
        closure: Closure,
    ) -> Result<RichTerm, EvalError> {
        self.eval_deep_closure_impl(closure, true)
            .map(|result| subst(&self.cache, result.body, &self.initial_env, &result.env))
    }

    /// Fully evaluates a Nickel term like `eval_full`, but does not substitute all variables.
    pub fn eval_deep(&mut self, t0: RichTerm) -> Result<RichTerm, EvalError> {
        self.eval_deep_closure_impl(Closure::atomic_closure(t0), false)
            .map(|result| result.body)
    }

    /// Same as [Self::eval_deep], but take a closure as an argument instead of a term.
    pub fn eval_deep_closure(&mut self, closure: Closure) -> Result<RichTerm, EvalError> {
        self.eval_deep_closure_impl(closure, false)
            .map(|result| result.body)
    }

    /// Use a specific initial environment for evaluation. Usually, [VirtualMachine::prepare_eval]
    /// is populating the initial environment. But in some cases, such as testing or benchmarks, we
    /// might want to use a different one.
    ///
    /// Return the new virtual machine with the updated initial environment.
    pub fn with_initial_env(mut self, env: Environment) -> Self {
        self.initial_env = env;
        self
    }

    fn eval_deep_closure_impl(
        &mut self,
        mut closure: Closure,
        for_export: bool,
    ) -> Result<Closure, EvalError> {
        closure.body = mk_term::op1(
            UnaryOp::Force {
                ignore_not_exported: for_export,
            },
            closure.body,
        );

        self.eval_closure(closure)
    }

    /// Take a term and a field path, and evaluate until the corresponding field can be extracted.
    /// Return the resulting field in its final environment.
    ///
    /// Note that this method doesn't evaluate the content of the field itself. Calling it with an
    /// empty path simply returns the original expression unevaluated in an empty environment. If
    /// you need to evaluate the value later, don't forget to apply pending contracts stored in the
    /// field.
    ///
    /// For example, extracting `foo.bar.baz` on a term `exp` will evaluate `exp` to a record and
    /// try to extract the field `foo`. If anything goes wrong (the result isn't a record or the
    /// field `bar` doesn't exist), a proper error is raised. Otherwise,
    /// [Self::extract_field_closure] applies the same recipe recursively and evaluate the content
    /// of the `foo` field extracted from `exp` to a record, tries to extract `bar`, and so on.
    pub fn extract_field_closure(
        &mut self,
        closure: Closure,
        path: &FieldPath,
    ) -> Result<(Field, Environment), EvalError> {
        self.extract_field_impl(closure, path, false)
    }

    /// Same as [Self::extract_field_closure], but also requires that the field value is defined
    /// and returns the value directly.
    ///
    /// This method also applies potential pending contracts to the value.
    ///
    /// In theory, extracting the value could be done manually after calling to
    /// [Self::extract_field_closure], instead of needing a separate method.
    ///
    /// However, once [Self::extract_field_closure] returns, most contextual information required
    /// to raise a proper error if the field is missing (e.g. positions) has been lost. So, if the
    /// returned field's value is `None`, we would have a hard time reporting a good error message.
    /// On the other hand, [Self::extract_field_value_closure] raises the error earlier, when the
    /// context is still available.
    pub fn extract_field_value_closure(
        &mut self,
        closure: Closure,
        path: &FieldPath,
    ) -> Result<Closure, EvalError> {
        let (field, env) = self.extract_field_impl(closure, path, true)?;

        // unwrap(): by definition, extract_field_impl(_, _, true) ensure that
        // `field.value.is_some()`
        let value = field.value.unwrap();
        let pos = value.pos;

        let value_with_ctr =
            RuntimeContract::apply_all(value, field.pending_contracts.iter().cloned(), pos);

        let Closure { body, env } = self.eval_closure(Closure {
            body: value_with_ctr,
            env,
        })?;

        Ok(Closure { body, env })
    }

    fn extract_field_impl(
        &mut self,
        closure: Closure,
        path: &FieldPath,
        require_defined: bool,
    ) -> Result<(Field, Environment), EvalError> {
        let mut prev_pos = closure.body.pos;

        let mut field: Field = closure.body.into();
        let mut path = path.0.iter().peekable();
        let mut env = closure.env;

        let Some(mut prev_id) = path.peek().cloned() else {
            return Ok((field, env));
        };

        for id in path {
            let Some(current_value) = field.value else {
                return Err(EvalError::MissingFieldDef {
                    id: *prev_id,
                    metadata: field.metadata,
                    pos_record: prev_pos,
                    pos_access: TermPos::None,
                });
            };

            // We evaluate the fields' value, either to handle the next ident of the
            // path, or to show the value if we are treating the last ident of the path

            prev_pos = current_value.pos;

            let curr_value_with_ctr = RuntimeContract::apply_all(
                current_value,
                field.pending_contracts.into_iter(),
                prev_pos,
            );

            let current_evaled = self.eval_closure(Closure {
                body: curr_value_with_ctr,
                env,
            })?;

            env = current_evaled.env;

            match current_evaled.body.term.into_owned() {
                Term::Record(mut record_data) => {
                    let Some(next_field) = record_data.fields.remove(id) else {
                        return Err(EvalError::FieldMissing {
                            id: *id,
                            field_names: record_data.field_names(RecordOpKind::IgnoreEmptyOpt),
                            operator: String::from("extract_field"),
                            pos_record: prev_pos,
                            pos_op: id.pos,
                        });
                    };

                    field = next_field;
                }
                other => {
                    //unwrap(): if we enter this pattern branch, `field.value` must be `Some(_)`
                    return Err(EvalError::QueryNonRecord {
                        pos: prev_pos,
                        id: *id,
                        value: RichTerm::new(other, prev_pos),
                    });
                }
            }

            prev_id = id;
        }

        if field.value.is_none() && require_defined {
            return Err(EvalError::MissingFieldDef {
                id: *prev_id,
                metadata: field.metadata,
                pos_record: prev_pos,
                pos_access: TermPos::None,
            });
        }

        Ok((field, env))
    }

    pub fn query(&mut self, t: RichTerm, path: &FieldPath) -> Result<Field, EvalError> {
        self.query_closure(Closure::atomic_closure(t), path)
    }

    /// Same as [VirtualMachine::query], but starts from a closure instead of a term in an empty
    /// environment.
    pub fn query_closure(
        &mut self,
        closure: Closure,
        path: &FieldPath,
    ) -> Result<Field, EvalError> {
        // extract_field does almost what we want, but for querying, we evaluate the content of the
        // field as well in order to print a potential default value.
        let (mut field, env) = self.extract_field_closure(closure, path)?;

        if let Some(value) = field.value.take() {
            let pos = value.pos;

            let value_with_ctr =
                RuntimeContract::apply_all(value, field.pending_contracts.iter().cloned(), pos);

            field.value = Some(
                self.eval_closure(Closure {
                    body: value_with_ctr,
                    env,
                })?
                .body,
            );
        }

        Ok(field)
    }

    fn enter_cache_index(
        &mut self,
        var: Option<LocIdent>,
        mut idx: CacheIndex,
        pos: TermPos,
        env: Environment,
    ) -> Result<Closure, EvalError> {
        // idx may be a 1-counted RC, so we make sure we drop any reference to it from `env`, which
        // is going to be discarded anyway
        std::mem::drop(env);

        match self.cache.get_update_index(&mut idx) {
            Ok(Some(idx_upd)) => self.stack.push_update_index(idx_upd),
            Ok(None) => {}
            Err(_blackholed_error) => {
                return Err(EvalError::InfiniteRecursion(self.call_stack.clone(), pos))
            }
        }

        if let Some(var) = var {
            self.call_stack.enter_var(var, pos);
        }

        // If we are fetching a recursive field from the environment that doesn't have
        // a definition, we complete the error with the additional information of where
        // it was accessed:
        let Closure { body, env } = self.cache.get(idx);
        let body = match_sharedterm!(match (body.term) {
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
            _ => {
                body
            }
        });

        Ok(Closure { body, env })
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
    pub fn eval_closure(&mut self, mut clos: Closure) -> Result<Closure, EvalError> {
        loop {
            let Closure {
                body:
                    RichTerm {
                        term: shared_term,
                        pos,
                    },
                mut env,
            } = clos;

            let has_cont_on_stack = self.stack.is_top_idx() || self.stack.is_top_cont();

            clos = match_sharedterm!(match (shared_term) {
                Term::Sealed(key, inner, label) => {
                    let stack_item = self.stack.peek_op_cont();
                    let closure = Closure {
                        body: RichTerm::new(Term::Sealed(key, inner.clone(), label.clone()), pos),
                        env: env.clone(),
                    };

                    // Update at the original index (the index which holds the result of the op) in
                    // both cases, even if we continue with a seq.
                    //
                    // We do this because we are on a `Sealed` term which is in weak head normal
                    // form, and if we don't, we will be unwrapping a `Sealed` term and assigning
                    // the "unsealed" value to the result of the `Seq` operation. See also:
                    // https://github.com/tweag/nickel/issues/123
                    update_at_indices(&mut self.cache, &mut self.stack, &closure);

                    // We have to peek the stack to see what operation is coming next and decide
                    // what to do.
                    //
                    // - If it's `unseal`, then we proceed with its evaluation, as `unseal` legitly
                    //   operates on sealed terms.
                    // - `seq` is the only primitive operation allowed to see through a sealed
                    //   term. Indeed, `seq`-ing doesn't violate parametricity, `seq`-ing shouldn't
                    //   be observable (that is, adding seq shouldn't change the semantics and
                    //   suddenly make a program blame), and it's useful in practice to seq sealed
                    //   terms such as in the implementation of `std.fold_left` to ensure we don't
                    //   accumulate thunks in memory.
                    // - If it's anything else, we raise an error right away because the
                    //   corresponding polymorphic contract has been violated: a function tried to
                    //   use a polymorphic sealed value.
                    match stack_item {
                        Some(OperationCont::Op2Second(BinaryOp::Unseal, _, _, _)) => {
                            self.continuate_operation(closure)?
                        }
                        Some(OperationCont::Op1(UnaryOp::Seq, _)) => {
                            // Then, evaluate / `Seq` the inner value.
                            Closure { body: inner, env }
                        }
                        None | Some(..) => {
                            // This operation should not be allowed to evaluate a sealed term
                            return Err(EvalError::BlameError {
                                evaluated_arg: label.get_evaluated_arg(&self.cache),
                                label,
                                call_stack: self.call_stack.clone(),
                            });
                        }
                    }
                }
                Term::Var(x) => {
                    let idx = env
                        .get(&x.ident())
                        .or_else(|| self.initial_env.get(&x.ident()))
                        .cloned()
                        .ok_or(EvalError::UnboundIdentifier(x, pos))?;

                    self.enter_cache_index(Some(x), idx, pos, env)?
                }
                Term::Closure(idx) => self.enter_cache_index(None, idx, pos, env)?,
                Term::App(t1, t2) => {
                    self.call_stack.enter_app(pos);

                    self.stack.push_arg(
                        Closure {
                            body: t2,
                            env: env.clone(),
                        },
                        pos,
                    );
                    Closure { body: t1, env }
                }
                Term::Let(x, bound, body, LetAttrs { binding_type, rec }) => {
                    let bound_closure: Closure = Closure {
                        body: bound,
                        env: env.clone(),
                    };

                    let idx = self.cache.add(bound_closure, binding_type);

                    // Patch the environment with the (x <- closure) binding
                    if rec {
                        self.cache
                            .patch(idx.clone(), |cl| cl.env.insert(x.ident(), idx.clone()));
                    }

                    env.insert(x.ident(), idx);

                    Closure { body, env }
                }
                Term::Op1(op, arg) => {
                    self.stack.push_op_cont(
                        OperationCont::Op1(op, arg.pos),
                        self.call_stack.len(),
                        pos,
                    );

                    Closure { body: arg, env }
                }
                Term::Op2(op, fst, snd) => {
                    self.stack.push_op_cont(
                        OperationCont::Op2First(
                            op,
                            Closure {
                                body: snd,
                                env: env.clone(),
                            },
                            fst.pos,
                        ),
                        self.call_stack.len(),
                        pos,
                    );
                    Closure { body: fst, env }
                }
                Term::OpN(op, args) => {
                    // Arguments are passed as a stack to the operation continuation, so we reverse
                    // the original list.
                    let mut args_iter = args.into_iter();
                    let fst = args_iter
                        .next()
                        .ok_or_else(|| EvalError::NotEnoughArgs(op.arity(), op.to_string(), pos))?;

                    let pending: Vec<Closure> = args_iter
                        .rev()
                        .map(|t| Closure {
                            body: t,
                            env: env.clone(),
                        })
                        .collect();

                    self.stack.push_op_cont(
                        OperationCont::OpN {
                            op,
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
                    let mut chunks_iter = chunks.into_iter();
                    match chunks_iter.next_back() {
                        None => Closure {
                            body: Term::Str(NickelString::new()).into(),
                            env: Environment::new(),
                        },
                        Some(chunk) => {
                            let (arg, indent) = match chunk {
                                StrChunk::Literal(s) => (Term::Str(s.into()).into(), 0),
                                StrChunk::Expr(e, indent) => (e, indent),
                            };

                            self.stack.push_str_chunks(chunks_iter);
                            self.stack.push_str_acc(StrAccData {
                                acc: String::new(),
                                env: env.clone(),
                                curr_indent: indent,
                                curr_pos: arg.pos,
                            });

                            Closure {
                                body: RichTerm::new(Term::Op1(UnaryOp::ChunksConcat, arg), pos),
                                env,
                            }
                        }
                    }
                }
                // Closurize the argument of an enum variant if it's not already done. Usually this is done at the first
                // time the variant is evaluated.
                Term::EnumVariant { tag, arg, attrs } if !attrs.closurized => {
                    Closure {
                        body: RichTerm::new(
                            Term::EnumVariant {
                                tag,
                                arg: arg.closurize(&mut self.cache, env),
                                attrs: attrs.closurized(),
                            },
                            pos,
                        ),
                        env: Environment::new(),
                    }
                }
                // Closurize the record if it's not already done. Usually this is done at the first
                // time this record is evaluated.
                Term::Record(data) if !data.attrs.closurized => {
                    Closure {
                        body: RichTerm::new(
                            Term::Record(data.closurize(&mut self.cache, env)),
                            pos,
                        ),
                        env: Environment::new(),
                    }
                }
                Term::RecRecord(data, dyn_fields, deps) => {
                    // We start by closurizing the fields, which might not be if the record is
                    // coming out of the parser.

                    // We must avoid re-closurizing a recursive record that is already closurized
                    // (coming from `merge`, for example), as the current representation is broken
                    // if we add a new indirection. This should ideally be encoded in the Rust
                    // type, once we have a different representation for runtime evaluation,
                    // instead of relying on invariants. But for now, we have to live with it.
                    let (mut static_part, dyn_fields) = if !data.attrs.closurized {
                        closurize_rec_record(&mut self.cache, data, dyn_fields, deps, env)
                    } else {
                        (data, dyn_fields)
                    };

                    let rec_env =
                        fixpoint::rec_env(&mut self.cache, static_part.fields.iter(), pos);

                    for rt in static_part.fields.values_mut() {
                        fixpoint::patch_field(&mut self.cache, rt, &rec_env);
                    }

                    // Transform the static part `{stat1 = val1, ..., statn = valn}` and the
                    // dynamic part `{exp1 = dyn_val1, ..., expm = dyn_valm}` to a sequence of
                    // extensions
                    //
                    // ```
                    // %record/insert% exp1
                    //   (...
                    //     (%record/insert% expn {stat1 = val1, ..., statn = valn} dyn_valn)
                    //   ...)
                    //   dyn_val1
                    //
                    // ```
                    //
                    // The `dyn_val` are given access to the recursive environment, but the
                    // recursive environment only contains the static fields, and not the dynamic
                    // fields.
                    let extended = dyn_fields.into_iter().fold(
                        RichTerm::new(Term::Record(static_part), pos),
                        |acc, (name_as_term, mut field)| {
                            let pos = field
                                .value
                                .as_ref()
                                .map(|v| v.pos)
                                .unwrap_or(name_as_term.pos);

                            fixpoint::patch_field(&mut self.cache, &mut field, &rec_env);

                            let ext_kind = field.extension_kind();
                            let Field {
                                metadata,
                                value,
                                pending_contracts,
                            } = field;

                            let extend = mk_term::op2(
                                BinaryOp::RecordInsert {
                                    metadata,
                                    pending_contracts,
                                    ext_kind,
                                    op_kind: RecordOpKind::ConsiderAllFields,
                                },
                                name_as_term,
                                acc,
                            );

                            match value {
                                Some(value) => {
                                    RichTerm::new(Term::App(extend, value), pos.into_inherited())
                                }
                                None => extend,
                            }
                        },
                    );

                    Closure {
                        body: extended.with_pos(pos),
                        env: Environment::new(),
                    }
                }
                Term::ResolvedImport(id) => {
                    if let Some(t) = self.import_resolver.get(id) {
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
                    ));
                }
                // Closurize the array if it's not already done.
                // This *should* make it unnecessary to call closurize in [operation].
                // See the comment on the `BinaryOp::ArrayConcat` match arm.
                Term::Array(terms, attrs) if !attrs.closurized => {
                    let closurized_array = terms
                        .into_iter()
                        .map(|t| t.closurize(&mut self.cache, env.clone()))
                        .collect();

                    let closurized_ctrs = attrs
                        .pending_contracts
                        .into_iter()
                        .map(|ctr| {
                            RuntimeContract::new(
                                ctr.contract.closurize(&mut self.cache, env.clone()),
                                ctr.label,
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
                        env: Environment::new(),
                    }
                }
                Term::ParseError(parse_error) => {
                    return Err(EvalError::ParseError(parse_error));
                }
                Term::RuntimeError(error) => {
                    return Err(error);
                }
                // For now, we simply erase annotations at runtime. They aren't accessible anyway
                // (as opposed to field metadata) and don't change the operational semantics, as
                // long as we generate the corresponding contract application when consuming it.
                //
                // The situation could change if we want to implement optimizations such as
                // avoiding repeated contract application. Annotations could then be a good way of
                // remembering which contracts have been applied to a value.
                Term::Annotated(annot, inner) => {
                    // We apply the contract coming from the static type annotation separately as
                    // it is optimized.
                    let static_contract = annot.static_contract();
                    let contracts = annot.pending_contracts()?;
                    let pos = inner.pos;

                    let inner_with_static = if let Some(static_ctr) = static_contract {
                        static_ctr?.apply(inner, pos)
                    } else {
                        inner
                    };

                    let inner_with_ctr =
                        RuntimeContract::apply_all(inner_with_static, contracts.into_iter(), pos);

                    Closure {
                        body: inner_with_ctr,
                        env,
                    }
                }
                // Evaluating a type turns it into a contract.
                Term::Type(ty) => Closure {
                    body: ty.contract()?,
                    env,
                },
                // Function call if there's no continuation on the stack (otherwise, the function
                // is just an argument to a primop or to put in the eval cache)
                Term::Fun(x, t) if !has_cont_on_stack => {
                    if let Some((idx, pos_app)) = self.stack.pop_arg_as_idx(&mut self.cache) {
                        self.call_stack.enter_fun(pos_app);
                        env.insert(x.ident(), idx);
                        Closure { body: t, env }
                    } else {
                        return Ok(Closure {
                            body: RichTerm::new(Term::Fun(x, t), pos),
                            env,
                        });
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
                Term::Match(data) if !has_cont_on_stack => {
                    if let Some((arg, pos_app)) = self.stack.pop_arg(&self.cache) {
                        Closure {
                            body: data.compile(arg.body.closurize(&mut self.cache, arg.env), pos),
                            env,
                        }
                    } else {
                        return Ok(Closure {
                            body: RichTerm::new(Term::Match(data), pos),
                            env,
                        });
                    }
                }
                // At this point, we've evaluated the current term to a weak head normal form.
                _ => {
                    let evaluated = Closure {
                        body: RichTerm {
                            term: shared_term,
                            pos,
                        },
                        env,
                    };

                    // If there is a cache index update frame on the stack, we proceed with the
                    // update of the corresponding cached value.
                    if self.stack.is_top_idx() {
                        update_at_indices(&mut self.cache, &mut self.stack, &evaluated);
                        evaluated
                    }
                    // If there is a primitive operator continuation on the stack, we proceed with
                    // the continuation.
                    else if self.stack.is_top_cont() {
                        self.continuate_operation(evaluated)?
                    }
                    // Otherwise, if the stack is non-empty, this is an ill-formed application (we
                    // are supposed to evaluate an application, but the left hand side isn't a
                    // function)
                    else if let Some((arg, pos_app)) = self.stack.pop_arg(&self.cache) {
                        return Err(EvalError::NotAFunc(evaluated.body, arg.body, pos_app));
                    }
                    // Finally, if the stack is empty, it's all good: it just means we are done
                    // evaluating.
                    else {
                        return Ok(evaluated);
                    }
                }
            })
        }
    }

    /// Evaluate a term, but attempt to continue on errors.
    ///
    /// This differs from `VirtualMachine::eval_full` in 2 ways:
    /// - We try to accumulate errors instead of bailing out. When recursing into record
    ///   fields and array elements, we keep evaluating subsequent elements even if one
    ///   fails.
    /// - We only return the accumulated errors; we don't return the eval'ed term.
    /// - We support a recursion limit, to limit the number of times we recurse into
    ///   arrays or records.
    pub fn eval_permissive(&mut self, rt: RichTerm, recursion_limit: usize) -> Vec<EvalError> {
        fn inner<R: ImportResolver, C: Cache>(
            slf: &mut VirtualMachine<R, C>,
            acc: &mut Vec<EvalError>,
            rt: RichTerm,
            recursion_limit: usize,
        ) {
            if recursion_limit == 0 {
                return;
            }

            let pos = rt.pos;
            match slf.eval(rt) {
                Err(e) => {
                    acc.push(e);
                    slf.reset();
                }
                Ok(t) => match t.as_ref() {
                    Term::Array(ts, attrs) => {
                        for t in ts.iter() {
                            // After eval_closure, all the array elements  are
                            // closurized already, so we don't need to do any tracking
                            // of the env.
                            let value_with_ctr = RuntimeContract::apply_all(
                                t.clone(),
                                attrs.pending_contracts.iter().cloned(),
                                t.pos,
                            );
                            inner(slf, acc, value_with_ctr, recursion_limit.saturating_sub(1));
                        }
                    }
                    Term::Record(data) => {
                        for (id, field) in &data.fields {
                            if let Some(v) = &field.value {
                                let value_with_ctr = RuntimeContract::apply_all(
                                    v.clone(),
                                    field.pending_contracts.iter().cloned(),
                                    v.pos,
                                );
                                inner(slf, acc, value_with_ctr, recursion_limit.saturating_sub(1));
                            } else {
                                acc.push(EvalError::MissingFieldDef {
                                    id: *id,
                                    metadata: field.metadata.clone(),
                                    pos_record: pos,
                                    pos_access: TermPos::None,
                                });
                            }
                        }
                    }
                    _ => {}
                },
            }
        }
        let mut ret = Vec::new();
        inner(self, &mut ret, rt, recursion_limit);
        ret
    }
}

impl<C: Cache> VirtualMachine<ImportCache, C> {
    /// Prepare the underlying program for evaluation (load the stdlib, typecheck, transform,
    /// etc.). Sets the initial environment of the virtual machine.
    pub fn prepare_eval(&mut self, main_id: FileId) -> Result<RichTerm, Error> {
        let Envs {
            eval_env,
            type_ctxt,
        } = self.import_resolver.prepare_stdlib(&mut self.cache)?;
        self.import_resolver.prepare(main_id, &type_ctxt)?;
        self.initial_env = eval_env;
        Ok(self.import_resolver().get(main_id).unwrap())
    }

    /// Prepare the stdlib for evaluation. Sets the initial environment of the virtual machine. As
    /// opposed to [VirtualMachine::prepare_eval], [VirtualMachine::prepare_stdlib] doesn't prepare
    /// the main program yet (typechecking, transformations, etc.).
    ///
    /// # Returns
    ///
    /// The initial evaluation and typing environments, containing the stdlib items.
    pub fn prepare_stdlib(&mut self) -> Result<Envs, Error> {
        let envs = self.import_resolver.prepare_stdlib(&mut self.cache)?;
        self.initial_env = envs.eval_env.clone();
        Ok(envs)
    }
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
pub fn env_add_record<C: Cache>(
    cache: &mut C,
    env: &mut Environment,
    closure: Closure,
) -> Result<(), EnvBuildError> {
    match_sharedterm!(match (closure.body.term) {
        Term::Record(record) | Term::RecRecord(record, ..) => {
            let ext = record.fields.into_iter().filter_map(|(id, field)| {
                field.value.map(|value| {
                    (
                        id.ident(),
                        cache.add(
                            Closure {
                                body: value,
                                env: closure.env.clone(),
                            },
                            BindingType::Normal,
                        ),
                    )
                })
            });

            env.extend(ext);
            Ok(())
        }
        _ => Err(EnvBuildError::NotARecord(closure.body)),
    })
}

/// Bind a closure in an environment.
pub fn env_add<C: Cache>(
    cache: &mut C,
    env: &mut Environment,
    id: LocIdent,
    rt: RichTerm,
    local_env: Environment,
) {
    let closure = Closure {
        body: rt,
        env: local_env,
    };
    env.insert(id.ident(), cache.add(closure, BindingType::Normal));
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
            .get(&id.ident())
            .or_else(|| initial_env.get(&id.ident()))
            .map(|idx| {
                let closure = cache.get(idx.clone());
                subst(cache, closure.body, initial_env, &closure.env)
            })
            .unwrap_or_else(|| RichTerm::new(Term::Var(id), pos)),
        Term::Closure(idx) => {
                let closure = cache.get(idx.clone());
                subst(cache, closure.body, initial_env, &closure.env)
        },
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
        | v @ Term::ForeignId(_)
        | v @ Term::SealingKey(_)
        | v @ Term::Enum(_)
        | v @ Term::Import(_)
        | v @ Term::ResolvedImport(_)
        // We could recurse here, because types can contain terms which would then be subject to
        // substitution. Not recursing should be fine, though, because a type in term position
        // turns into a contract, and we don't substitute inside contracts either currently.
        | v @ Term::Type(_) => RichTerm::new(v, pos),
        Term::EnumVariant { tag, arg, attrs } => {
            let arg = subst(cache, arg, initial_env, env);

            RichTerm::new(Term::EnumVariant { tag, arg, attrs }, pos)
        }
        Term::Let(id, t1, t2, attrs) => {
            let t1 = subst(cache, t1, initial_env, env);
            let t2 = subst(cache, t2, initial_env, env);

            RichTerm::new(Term::Let(id, t1, t2, attrs), pos)
        }
        p @ Term::LetPattern(..) => panic!(
            "Pattern {p:?} has not been transformed before evaluation"
        ),
        p @ Term::FunPattern(..) => panic!(
            "Pattern {p:?} has not been transformed before evaluation"
        ),
        Term::App(t1, t2) => {
            let t1 = subst(cache, t1, initial_env, env);
            let t2 = subst(cache, t2, initial_env, env);

            RichTerm::new(Term::App(t1, t2), pos)
        }
        Term::Match(data) => {
            let branches = data.branches
                .into_iter()
                .map(|MatchBranch { pattern, guard, body} | {
                    MatchBranch {
                        pattern,
                        guard: guard.map(|cond| subst(cache, cond, initial_env, env)),
                        body: subst(cache, body, initial_env, env),
                    }
                })
                .collect();

            RichTerm::new(Term::Match(MatchData { branches }), pos)
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
            let mut record = record
                .map_defined_values(|_, value| subst(cache, value, initial_env, env));

            // [^subst-closurized-false]: After substitution, there's no closure in here anymore.
            // It's a detail but it comes handy in tests, where we abuse partial equality over
            // terms - keeping closurized to `true` would require to do the same when building the
            // expected result, which is annoying, as closurized is initialized to false by default
            // by term builders.
            record.attrs.closurized = false;

            RichTerm::new(Term::Record(record), pos)
        }
        Term::RecRecord(record, dyn_fields, deps) => {
            let mut record = record
                .map_defined_values(|_, value| subst(cache, value, initial_env, env));

            // see [^subst-closurized-false]
            record.attrs.closurized = false;

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
        Term::Array(ts, mut attrs) => {
            let ts = ts
                .into_iter()
                .map(|t| subst(cache, t, initial_env, env))
                .collect();

            // cd [^subst-closurized-false]
            attrs.closurized = false;
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
