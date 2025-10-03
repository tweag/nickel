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
//!   environment
//! - same thing for `exp2`
//! - Finally, the implementation of `+` can proceed with the computation
//!
//! We detail the case of binary operators, as the case of unary ones is similar and simpler.
//!
//! - **Op(op, first, second)**: pushes an `OpFirst` element on the stack, which saves the operator
//!   `op`, the second argument `second` and the current environment, and proceed with the evaluation
//!   of `first`
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
use crate::{
    bytecode::value::{
        EnumVariantBody, NickelValue, RecordBody, TermBody, ValueContentRef,
        ValueContentRefMut,
    },
    cache::{CacheHub as ImportCaches, ImportResolver},
    closurize::{closurize_rec_record, Closurize},
    environment::Environment as GenericEnvironment,
    error::{warning::Warning, Error, EvalError, Reporter},
    files::{FileId, Files},
    identifier::{Ident, LocIdent},
    metrics::{increment, measure_runtime},
    position::{TermPos, PosTable, PosIdx},
    program::FieldPath,
    term::{
        make as mk_term,
        pattern::compile::Compile,
        record::{Field, RecordData},
        string::NickelString,
        BinaryOp, BindingType, Import, LetAttrs, MatchBranch, MatchData, RecordOpKind,
        RuntimeContract, StrChunk, Term, UnaryOp,
    },
    transform::gen_pending_contracts,
};

use std::io::Write;

pub mod cache;
pub mod callstack;
pub mod contract_eq;
pub mod fixpoint;
pub mod merge;
pub mod operation;
pub mod stack;

use callstack::*;
use operation::OperationCont;
use stack::{Stack, StrAccData};

use self::cache::{Cache, CacheIndex};

impl AsRef<Vec<StackElem>> for CallStack {
    fn as_ref(&self) -> &Vec<StackElem> {
        &self.0
    }
}

/// The current state of the Nickel virtual machine.
pub struct VirtualMachine<R: ImportResolver, C: Cache> {
    /// The main stack, storing arguments, cache indices and pending computations.
    stack: Stack<C>,
    /// The call stack, for error reporting.
    call_stack: CallStack,
    /// The interface used to fetch imports.
    import_resolver: R,
    /// The evaluation cache.
    pub cache: C,
    /// The initial environment containing stdlib and builtin functions accessible from anywhere
    initial_env: Environment,
    /// The position table, mapping position indices embedded into values to full fledged positions.
    pos_table: PosTable,
    /// The stream for writing trace output.
    trace: Box<dyn Write>,
    /// A collector for warnings. Currently we only collect warnings and not errors; errors
    /// terminate evaluation (or typechecking, or whatever) immediately, and so they just
    /// get early-returned in a `Result`.
    pub reporter: Box<dyn Reporter<(Warning, Files)>>,
}

impl<R: ImportResolver, C: Cache> VirtualMachine<R, C> {
    pub fn new(
        pos_table: PosTable,
        import_resolver: R,
        trace: impl Write + 'static,
        reporter: impl Reporter<(Warning, Files)> + 'static,
    ) -> Self {
        VirtualMachine {
            import_resolver,
            call_stack: Default::default(),
            stack: Stack::new(),
            cache: Cache::new(),
            initial_env: Environment::new(),
            pos_table,
            trace: Box::new(trace),
            reporter: Box::new(reporter),
        }
    }

    pub fn new_with_cache(
        pos_table: PosTable,
        import_resolver: R,
        cache: C,
        trace: impl Write + 'static,
        reporter: impl Reporter<(Warning, Files)> + 'static,
    ) -> Self {
        VirtualMachine {
            import_resolver,
            call_stack: Default::default(),
            stack: Stack::new(),
            cache,
            pos_table,
            trace: Box::new(trace),
            initial_env: Environment::new(),
            reporter: Box::new(reporter),
        }
    }

    pub fn with_reporter(
        self,
        reporter: impl Reporter<(Warning, Files)> + 'static,
    ) -> VirtualMachine<R, C> {
        VirtualMachine {
            reporter: Box::new(reporter),
            ..self
        }
    }

    pub fn warn(&mut self, warning: Warning) {
        self.reporter
            .report((warning, self.import_resolver.files().clone()));
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

    /// Evaluate a Nickel expression. Wrapper around [VirtualMachine::eval_closure] that starts
    /// from an empty local environment and drops the final environment.
    pub fn eval(&mut self, value: NickelValue) -> Result<NickelValue, EvalError> {
        self.eval_closure(value.into()).map(|closure| closure.value)
    }

    /// Fully evaluate a Nickel term: the result is not a WHNF but to a value with all variables
    /// substituted.
    pub fn eval_full(&mut self, value: NickelValue) -> Result<NickelValue, EvalError> {
        self.eval_full_closure(value.into())
            .map(|result| result.value)
    }

    /// Same as [Self::eval_full], but takes a closure as an argument instead of a term.
    pub fn eval_full_closure(&mut self, closure: Closure) -> Result<Closure, EvalError> {
        self.eval_deep_closure_impl(closure, false)
            .map(|result| Closure {
                value: subst(&self.cache, result.value, &self.initial_env, &result.env),
                env: result.env,
            })
    }

    /// Like [Self::eval_full], but skips evaluating record fields marked `not_exported`.
    pub fn eval_full_for_export(&mut self, value: NickelValue) -> Result<NickelValue, EvalError> {
        self.eval_deep_closure_impl(value.into(), true)
            .map(|result| subst(&self.cache, result.value, &self.initial_env, &result.env))
    }

    /// Same as [Self::eval_full_for_export], but takes a closure as an argument instead of a term.
    pub fn eval_full_for_export_closure(
        &mut self,
        closure: Closure,
    ) -> Result<NickelValue, EvalError> {
        self.eval_deep_closure_impl(closure, true)
            .map(|result| subst(&self.cache, result.value, &self.initial_env, &result.env))
    }

    /// Fully evaluates a Nickel term like `eval_full`, but does not substitute all variables.
    pub fn eval_deep(&mut self, value: NickelValue) -> Result<NickelValue, EvalError> {
        self.eval_deep_closure_impl(value.into(), false)
            .map(|result| result.value)
    }

    /// Same as [Self::eval_deep], but take a closure as an argument instead of a term.
    pub fn eval_deep_closure(&mut self, closure: Closure) -> Result<NickelValue, EvalError> {
        self.eval_deep_closure_impl(closure, false)
            .map(|result| result.value)
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
        closure.value = mk_term::op1(
            UnaryOp::Force {
                ignore_not_exported: for_export,
            },
            closure.value,
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
        let pos_idx = value.pos_idx();

        let value_with_ctr =
            RuntimeContract::apply_all(value, field.pending_contracts.iter().cloned(), pos_idx);

        let Closure { value: body, env } = self.eval_closure(Closure {
            value: value_with_ctr,
            env,
        })?;

        Ok(Closure { value: body, env })
    }

    fn extract_field_impl(
        &mut self,
        closure: Closure,
        path: &FieldPath,
        require_defined: bool,
    ) -> Result<(Field, Environment), EvalError> {
        let mut prev_pos_idx = closure.value.pos_idx();

        let mut field: Field = closure.value.into();
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
                    pos_record: self.pos_table.get(prev_pos_idx),
                    pos_access: TermPos::None,
                });
            };

            // We evaluate the fields' value, either to handle the next ident of the
            // path, or to show the value if we are treating the last ident of the path

            prev_pos_idx = current_value.pos_idx();

            let curr_value_with_ctr =
                RuntimeContract::apply_all(current_value, field.pending_contracts, prev_pos_idx);

            let current_evaled = self.eval_closure(Closure {
                value: curr_value_with_ctr,
                env,
            })?;

            env = current_evaled.env;

            match current_evaled.value.content() {
                ValueContentRef::Record(RecordBody(record_data)) => {
                    let Some(next_field) = record_data.fields.get(id).cloned() else {
                        return Err(EvalError::FieldMissing {
                            id: *id,
                            field_names: record_data.field_names(RecordOpKind::IgnoreEmptyOpt),
                            operator: String::from("extract_field"),
                            pos_record: self.pos_table.get(prev_pos_idx),
                            pos_op: id.pos,
                        });
                    };

                    field = next_field;
                }
                other => {
                    //unwrap(): if we enter this pattern branch, `field.value` must be `Some(_)`
                    return Err(EvalError::QueryNonRecord {
                        pos: self.pos_table.get(prev_pos_idx),
                        id: *id,
                        value: current_evaled.value,
                    });
                }
            }

            prev_id = id;
        }

        if field.value.is_none() && require_defined {
            return Err(EvalError::MissingFieldDef {
                id: *prev_id,
                metadata: field.metadata,
                pos_record: prev_pos_idx,
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
            let pos_idx = value.pos_idx();

            let value_with_ctr =
                RuntimeContract::apply_all(value, field.pending_contracts.iter().cloned(), pos_idx);

            field.value = Some(
                self.eval_closure(Closure {
                    value: value_with_ctr,
                    env,
                })?
                .value,
            );
        }

        Ok(field)
    }

    fn enter_cache_index(
        &mut self,
        var: Option<LocIdent>,
        mut idx: CacheIndex,
        pos_idx: PosIdx,
        env: Environment,
    ) -> Result<Closure, EvalError> {
        // idx may be a 1-counted RC, so we make sure we drop any reference to it from `env`, which
        // is going to be discarded anyway
        std::mem::drop(env);

        match self.cache.get_update_index(&mut idx) {
            Ok(Some(idx_upd)) => self.stack.push_update_index(idx_upd),
            Ok(None) => {}
            Err(_blackholed_error) => {
                return Err(EvalError::InfiniteRecursion(self.call_stack.clone(), self.pos_table.get(pos_idx)))
            }
        }

        if let Some(var) = var {
            self.call_stack.enter_var(var, pos_idx);
        }

        // If we are fetching a recursive field from the environment that doesn't have
        // a definition, we complete the error with the additional information of where
        // it was accessed:
        let mut closure = self.cache.get(idx);

        if let ValueContentRef::Term(term) = closure.value.content() {
            if let Term::RuntimeError(EvalError::MissingFieldDef {
                id,
                metadata,
                pos_record,
                pos_access: TermPos::None,
            }) = term.0
            {
                closure.value = NickelValue::term(
                    Term::RuntimeError(EvalError::MissingFieldDef {
                        id,
                        metadata,
                        pos_record,
                        pos_access: pos_idx,
                    }),
                    todo!("should we use an index or an inline pos here"),
                );
            }
        }

        Ok(closure)
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
    pub fn eval_closure(&mut self, mut closure: Closure) -> Result<Closure, EvalError> {
        #[cfg(feature = "metrics")]
        let start_time = std::time::Instant::now();

        let result = loop {
            let Closure { value, env } = closure;
            let pos_idx = closure.value.pos_idx();
            let has_cont_on_stack = self.stack.is_top_idx() || self.stack.is_top_cont();

            closure = match value.content() {
                ValueContentRef::Thunk(thunk_body) => todo!(),
                ValueContentRef::Term(TermBody(Term::Value(value))) => Closure { value, env },
                ValueContentRef::Term(TermBody(Term::Sealed(key, inner, label))) => {
                    let stack_item = self.stack.peek_op_cont();
                    let closure = Closure {
                        value: NickelValue::term(
                            Term::Sealed(key, inner.clone(), label.clone()),
                            pos_idx,
                        ),
                        env,
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
                            Closure { value: inner, env }
                        }
                        None | Some(..) => {
                            // This operation should not be allowed to evaluate a sealed term
                            break Err(EvalError::BlameError {
                                evaluated_arg: label.get_evaluated_arg(&self.cache),
                                label,
                                call_stack: self.call_stack.clone(),
                            });
                        }
                    }
                }
                ValueContentRef::Term(TermBody(Term::Var(id))) => {
                    let idx = get_var(id, &closure.env, &self.initial_env, todo!("pos_idx?"))?;
                    self.enter_cache_index(Some(id), idx, todo!("pos_idx?"), env)?
                }
                ValueContentRef::Term(TermBody(Term::App(head, arg))) => {
                    self.call_stack.enter_app(todo!("pos_idx?"));

                    self.stack.push_arg(
                        Closure {
                            value: arg,
                            env: env.clone(),
                        },
                        pos_idx,
                    );
                    Closure { value: head, env }
                }
                ValueContentRef::Term(TermBody(Term::Let(
                    bindings,
                    body,
                    LetAttrs { binding_type, rec },
                ))) => {
                    let mut indices = Vec::new();
                    let Closure { value: body, env } = closure;
                    let init_env = closure.env.clone();

                    for (x, bound) in bindings {
                        let bound_closure = Closure {
                            value: bound,
                            env: init_env.clone(),
                        };

                        let idx = self.cache.add(bound_closure, binding_type.clone());

                        // Patch the environment with the (x <- closure) binding
                        if rec {
                            indices.push(idx.clone());
                        }

                        env.insert(x.ident(), idx);
                    }

                    for idx in indices {
                        self.cache.patch(idx, |cl| cl.env = env.clone());
                    }

                    Closure { value: body, env }
                }
                ValueContentRef::Term(TermBody(Term::Op1(op, arg))) => {
                    self.stack.push_op_cont(
                        OperationCont::Op1(op, todo!("pos_idx?")),
                        self.call_stack.len(),
                        pos_idx,
                    );

                    Closure { value: arg, env }
                }
                ValueContentRef::Term(TermBody(Term::Op2(op, fst_arg, snd_arg))) => {
                    self.stack.push_op_cont(
                        OperationCont::Op2First(
                            op,
                            Closure {
                                value: snd_arg,
                                env: env.clone(),
                            },
                            todo!("pos_idx?"),
                        ),
                        self.call_stack.len(),
                        todo!("pos_idx?"),
                    );

                    Closure {
                        value: fst_arg,
                        env,
                    }
                }
                ValueContentRef::Term(TermBody(Term::OpN(op, args))) => {
                    // Arguments are passed as a stack to the operation continuation, so we reverse
                    // the original list.
                    let mut args_iter = args.into_iter();
                    let fst_arg = args_iter.next().ok_or_else(|| {
                        EvalError::NotEnoughArgs(op.arity(), op.to_string(), todo!("pos_idx?"))
                    })?;

                    let pending: Vec<Closure> = args_iter
                        .rev()
                        .map(|value| Closure {
                            value,
                            env: env.clone(),
                        })
                        .collect();

                    self.stack.push_op_cont(
                        OperationCont::OpN {
                            op,
                            evaluated: Vec::with_capacity(pending.len() + 1),
                            pending,
                            current_pos: todo!("pos_idx?"),
                        },
                        self.call_stack.len(),
                        todo!("pos_idx?"),
                    );

                    Closure {
                        value: fst_arg,
                        env,
                    }
                }
                ValueContentRef::Term(TermBody(Term::StrChunks(chunks))) => {
                    let mut chunks_iter = chunks.into_iter();
                    match chunks_iter.next_back() {
                        None => Closure {
                            value: NickelValue::string(NickelString::new(), pos_idx),
                            env: Environment::new(),
                        },
                        Some(chunk) => {
                            let (arg, indent) = match chunk {
                                StrChunk::Literal(s) => (NickelValue::string_posless(s), 0),
                                StrChunk::Expr(e, indent) => (e, indent),
                            };

                            self.stack.push_str_chunks(chunks_iter);
                            self.stack.push_str_acc(StrAccData {
                                acc: String::new(),
                                env: env.clone(),
                                curr_indent: indent,
                                curr_pos: todo!("pos_idx?"),
                            });

                            Closure {
                                value: NickelValue::term(
                                    Term::Op1(UnaryOp::ChunksConcat, arg),
                                    pos_idx,
                                ),
                                env,
                            }
                        }
                    }
                }
                ValueContentRef::Term(TermBody(Term::Closurize(value))) => {
                    // Closurization is done the first time we see a value, so under normal
                    // conditions, this value should not be shared and we should be able to
                    // mutate it directly.
                    match value.content_make_mut() {
                        ValueContentRefMut::Inline(nickel_value) => {}
                        ValueContentRefMut::Array(array_body) => {
                            // This *should* make it unnecessary to call closurize in [operation].
                            // See the comment on the `BinaryOp::ArrayConcat` match arm.
                            array_body.array = array_body
                                .array
                                .iter()
                                .cloned()
                                .map(|t| t.closurize(&mut self.cache, env.clone()))
                                .collect();

                            array_body.pending_contracts = array_body
                                .pending_contracts
                                .iter()
                                .cloned()
                                .map(|ctr| {
                                    RuntimeContract::new(
                                        ctr.contract.closurize(&mut self.cache, env.clone()),
                                        ctr.label,
                                    )
                                })
                                .collect();
                        }
                        ValueContentRefMut::Record(record_body) => {
                            record_body.0 = record_body.0.closurize(&mut self.cache, env)
                        }
                        ValueContentRefMut::EnumVariant(EnumVariantBody { tag: _, arg }) => {
                            *arg = arg.map(|arg| arg.closurize(&mut self.cache, env))
                        }
                        _ => {
                            // This case is a red flag (it should be unreachable), but
                            // isn't a blocker per se, so we only fail in debug mode.
                            debug_assert!(false, "trying to closurize a non-container value");
                        }
                    }

                    // We can use an empty environment for a freshly closurized value
                    value.into()
                }
                ValueContentRef::Term(TermBody(Term::RecRecord(
                    data,
                    includes,
                    dyn_fields,
                    deps,
                ))) => {
                    // We start by closurizing the fields, which might not be if the record is
                    // coming out of the parser.

                    // We must avoid re-closurizing a recursive record that is already closurized
                    // (coming from `merge`, for example), as the current representation is broken
                    // if we add a new indirection. This should ideally be encoded in the Rust
                    // type, once we have a different representation for runtime evaluation,
                    // instead of relying on invariants. But for now, we have to live with it.
                    let (mut static_part, dyn_fields) = if !data.attrs.closurized {
                        let includes_as_terms: Result<Vec<_>, _> = includes
                            .into_iter()
                            .map(|incl| -> Result<_, EvalError> {
                                let field = Field {
                                    value: Some(RichTerm::new(
                                        Term::Closure(get_var(
                                            incl.ident,
                                            &env,
                                            &self.initial_env,
                                            TermPos::None,
                                        )?),
                                        incl.ident.pos,
                                    )),
                                    metadata: incl.metadata,
                                    pending_contracts: Vec::new(),
                                };

                                Ok((
                                    incl.ident,
                                    gen_pending_contracts::with_pending_contracts(field)?,
                                ))
                            })
                            .collect();

                        // We assume that the parser doesn't allow conflicts between field
                        // definitions and includes (the same field is defined in both). This
                        // restriction might be lifted in the future (we would probably merge the
                        // included field and the other definition pieces), but for now it's
                        // simpler this way.
                        let mut data = data;
                        data.fields.extend(includes_as_terms?);
                        closurize_rec_record(&mut self.cache, data, dyn_fields, deps, env)
                    } else {
                        // In a record that has been already closurized, we expect include
                        // expressions to be evaluated away.
                        debug_assert!(includes.is_empty());
                        (data, dyn_fields)
                    };

                    let rec_env = fixpoint::rec_env(
                        &mut self.cache,
                        static_part.fields.iter(),
                        todo!("pos_idx"),
                    );

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
                        NickelValue::record(static_part, pos_idx),
                        |acc, (name_as_term, mut field)| {
                            let pos_idx = field
                                .value
                                .as_ref()
                                .map(NickelValue::pos_idx)
                                .unwrap_or_else(|| name_as_term.pos_idx());

                            fixpoint::patch_field(&mut self.cache, &mut field, &rec_env);

                            let ext_kind = field.extension_kind();
                            let Field {
                                metadata,
                                value,
                                pending_contracts,
                            } = field;

                            let extend = mk_term::op2(
                                BinaryOp::RecordInsert {
                                    metadata: Box::new(metadata),
                                    pending_contracts,
                                    ext_kind,
                                    op_kind: RecordOpKind::ConsiderAllFields,
                                },
                                name_as_term,
                                acc,
                            );

                            match value {
                                Some(value) => NickelValue::term(
                                    Term::App(extend, value),
                                    todo!("pos_idx.into_inherited?()"),
                                ),
                                None => extend,
                            }
                        },
                    );

                    Closure {
                        value: extended.with_pos(pos_idx),
                        env: Environment::new(),
                    }
                }
                ValueContentRef::Term(TermBody(Term::ResolvedImport(id))) => {
                    increment!(format!("import:{id:?}"));

                    if let Some(val) = self.import_resolver.get(id) {
                        val.into()
                    } else {
                        break Err(EvalError::InternalError(
                            format!("Resolved import not found ({id:?})"),
                            self.pos_table.get(pos_idx),
                        ));
                    }
                }
                ValueContentRef::Term(TermBody(Term::Import(Import::Path { path, .. }))) => {
                    break Err(EvalError::InternalError(
                        format!("Unresolved import ({})", path.to_string_lossy()),
                        pos,
                    ));
                }
                ValueContentRef::Term(TermBody(Term::Import(Import::Package { id }))) => {
                    return Err(EvalError::InternalError(
                        format!("Unresolved package import ({id})"),
                        pos,
                    ));
                }
                ValueContentRef::Term(TermBody(Term::ParseError(parse_error))) => {
                    break Err(EvalError::ParseError(parse_error));
                }
                ValueContentRef::Term(TermBody(Term::RuntimeError(error))) => {
                    break Err(error);
                }
                // For now, we simply erase annotations at runtime. They aren't accessible anyway
                // (as opposed to field metadata) and don't change the operational semantics, as
                // long as we generate the corresponding contract application when consuming it.
                //
                // The situation could change if we want to implement optimizations such as
                // avoiding repeated contract application. Annotations could then be a good way of
                // remembering which contracts have been applied to a value.
                ValueContentRef::Term(TermBody(Term::Annotated(annot, inner))) => {
                    increment!("contract:free-standing(annotated)");

                    // We apply the contract coming from the static type annotation separately as
                    // it is optimized.
                    let static_contract = annot.static_contract();
                    let contracts = annot.pending_contracts()?;
                    let pos_idx = inner.pos_idx();
                    let inner = inner.clone();

                    let inner_with_static = if let Some(static_ctr) = static_contract {
                        static_ctr?.apply(inner, pos_idx)
                    } else {
                        inner
                    };

                    let inner_with_ctr =
                        RuntimeContract::apply_all(inner_with_static, contracts, pos_idx);

                    Closure {
                        value: inner_with_ctr,
                        env,
                    }
                }
                // Function call if there's no continuation on the stack (otherwise, the function
                // is just an argument to a primop or to put in the eval cache)
                ValueContentRef::Term(TermBody(Term::Fun(arg, body))) if !has_cont_on_stack => {
                    if let Some((idx, pos_app)) = self.stack.pop_arg_as_idx(&mut self.cache) {
                        self.call_stack.enter_fun(pos_app);
                        env.insert(arg.ident(), idx);
                        Closure { value: body, env }
                    } else {
                        break Ok(Closure {
                            value: NickelValue::term(Term::Fun(arg, body), pos_idx),
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
                ValueContentRef::Term(TermBody(Term::Match(data))) if !has_cont_on_stack => {
                    if let Some((arg, pos_app)) = self.stack.pop_arg(&self.cache) {
                        Closure {
                            value: data.compile(
                                arg.value.closurize(&mut self.cache, arg.env),
                                todo!("pos_idx?"),
                            ),
                            env,
                        }
                    } else {
                        break Ok(Closure {
                            value: NickelValue::term(Term::Match(data), pos_idx),
                            env,
                        });
                    }
                }
                ValueContentRef::Term(TermBody(Term::FunPattern(..) | Term::LetPattern(..))) => {
                    break Err(EvalError::InternalError(
                        "unexpected let-pattern or fun-pattern during evaluation".into_owned(),
                        todo!("pos_idx?"),
                    ));
                }
                // At this point, we've evaluated the current term to a weak head normal form.
                _ => {
                    let evaluated = Closure { value, env };

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
                        break Err(EvalError::NotAFunc(evaluated.value, arg.value, pos_app));
                    }
                    // Finally, if the stack is empty, it's all good: it just means we are done
                    // evaluating.
                    else {
                        break Ok(evaluated);
                    }
                }
            };
        };

        increment!("runtime:eval", start_time.elapsed().as_millis() as u64);

        result
    }

    /// Evaluate a term, but attempt to continue on errors.
    ///
    /// This differs from `VirtualMachine::eval_full` in three ways:
    /// - We try to accumulate errors instead of bailing out. When recursing into record
    ///   fields and array elements, we keep evaluating subsequent elements even if one
    ///   fails.
    /// - We only return the accumulated errors; we don't return the eval'ed term.
    /// - We support a recursion limit, to limit the number of times we recurse into
    ///   arrays or records.
    pub fn eval_permissive(
        &mut self,
        value: NickelValue,
        recursion_limit: usize,
    ) -> Vec<EvalError> {
        fn inner<R: ImportResolver, C: Cache>(
            this: &mut VirtualMachine<R, C>,
            acc: &mut Vec<EvalError>,
            value: NickelValue,
            recursion_limit: usize,
        ) {
            if recursion_limit == 0 {
                return;
            }

            let pos_idx = value.pos_idx();
            match this.eval(value) {
                Err(e) => {
                    acc.push(e);
                    this.reset();
                }
                Ok(val) => match val.content() {
                    ValueContentRef::Array(data) => {
                        for elt in data.array.iter() {
                            // After eval_closure, all the array elements  are
                            // closurized already, so we don't need to do any tracking
                            // of the env.
                            let value_with_ctr = RuntimeContract::apply_all(
                                elt.clone(),
                                data.pending_contracts.iter().cloned(),
                                todo!("t.pos_idx()?"),
                            );
                            inner(this, acc, value_with_ctr, recursion_limit.saturating_sub(1));
                        }
                    }
                    ValueContentRef::Record(RecordBody(data)) => {
                        for (id, field) in &data.fields {
                            if let Some(v) = &field.value {
                                let value_with_ctr = RuntimeContract::apply_all(
                                    v.clone(),
                                    field.pending_contracts.iter().cloned(),
                                    todo!("v.pos_idx?"),
                                );
                                inner(this, acc, value_with_ctr, recursion_limit.saturating_sub(1));
                            } else {
                                acc.push(EvalError::MissingFieldDef {
                                    id: *id,
                                    metadata: field.metadata.clone(),
                                    pos_record: todo!("pos_idx?"),
                                    pos_access: TermPos::None,
                                });
                            }
                        }
                    }
                    ValueContentRef::EnumVariant(EnumVariantBody { arg, .. }) => {
                        inner(this, acc, arg.clone(), recursion_limit.saturating_sub(1));
                    }
                    _ => {}
                },
            }
        }

        let mut ret = Vec::new();
        inner(self, &mut ret, value, recursion_limit);
        ret
    }
}

impl<C: Cache> VirtualMachine<ImportCaches, C> {
    /// Prepare the underlying program for evaluation (load the stdlib, typecheck, transform,
    /// etc.). Sets the initial environment of the virtual machine.
    pub fn prepare_eval(&mut self, main_id: FileId) -> Result<RichTerm, Error> {
        self.prepare_eval_impl(main_id, true)
    }

    /// Same as [Self::prepare_eval], but skip typechecking.
    pub fn prepare_eval_only(&mut self, main_id: FileId) -> Result<RichTerm, Error> {
        self.prepare_eval_impl(main_id, false)
    }

    fn prepare_eval_impl(&mut self, main_id: FileId, typecheck: bool) -> Result<RichTerm, Error> {
        measure_runtime!(
            "runtime:prepare_stdlib",
            self.import_resolver.prepare_stdlib()?
        );

        measure_runtime!(
            "runtime:prepare_main",
            if typecheck {
                self.import_resolver.prepare(main_id)
            } else {
                self.import_resolver.prepare_eval_only(main_id)
            }
        )?;

        // Unwrap: closurization only fails if the input wasn't parsed, and we just
        // parsed it.
        self.import_resolver
            .closurize(&mut self.cache, main_id)
            .unwrap();
        self.initial_env = self.import_resolver.mk_eval_env(&mut self.cache);
        Ok(self.import_resolver().get(main_id).unwrap())
    }

    /// Prepare the stdlib for evaluation. Sets the initial environment of the virtual machine. As
    /// opposed to [VirtualMachine::prepare_eval], [VirtualMachine::prepare_stdlib] doesn't prepare
    /// the main program yet (typechecking, transformations, etc.).
    ///
    /// # Returns
    ///
    /// The initial evaluation and typing environments, containing the stdlib items.
    pub fn prepare_stdlib(&mut self) -> Result<(), Error> {
        self.import_resolver.prepare_stdlib()?;
        self.initial_env = self.import_resolver.mk_eval_env(&mut self.cache);
        Ok(())
    }

    /// Generate an initial evaluation environment from the stdlib from the underlying import
    /// cache and eval cache.
    pub fn mk_eval_env(&mut self) -> Environment {
        self.import_resolver.mk_eval_env(&mut self.cache)
    }
}

/// A closure, which is a value packed with its environment.
#[derive(PartialEq, Clone)]
pub struct Closure {
    pub value: NickelValue,
    pub env: Environment,
}

impl std::fmt::Debug for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Closure")
            .field("body", &self.value)
            .finish_non_exhaustive()
    }
}

impl From<NickelValue> for Closure {
    fn from(value: NickelValue) -> Self {
        Closure {
            value,
            env: Environment::new(),
        }
    }
}

#[allow(type_alias_bounds)] // TODO: Look into this warning.
pub type Environment = GenericEnvironment<Ident, CacheIndex>;

/// Raised when trying to build an environment from a term which is not a record.
#[derive(Clone, Debug)]
pub enum EnvBuildError {
    NotARecord(NickelValue),
}

/// Add the bindings of a record to an environment. Ignore the fields defined by interpolation as
/// well, fields without definition and `include` expressions.
pub fn env_add_record<C: Cache>(
    cache: &mut C,
    env: &mut Environment,
    closure: Closure,
) -> Result<(), EnvBuildError> {
    if closure.value.is_empty_record() {
        return Ok(());
    }

    let record = closure
        .value
        .as_record()
        .map(|record| record.0)
        .or_else(|| {
            closure.value.as_term().and_then(|term| {
                if let Term::RecRecord(record, ..) = term {
                    Some(record)
                } else {
                    None
                }
            })
        });

    if let Some(record) = record {
        let ext = record.fields.iter().cloned().filter_map(|(id, field)| {
            field.value.map(|value| {
                (
                    id.ident(),
                    cache.add(
                        Closure {
                            value,
                            env: closure.env.clone(),
                        },
                        BindingType::Normal,
                    ),
                )
            })
        });

        env.extend(ext);
        Ok(())
    } else {
        Err(EnvBuildError::NotARecord(closure.value))
    }
}

/// Bind a closure in an environment.
pub fn env_add<C: Cache>(
    cache: &mut C,
    env: &mut Environment,
    id: LocIdent,
    value: NickelValue,
    local_env: Environment,
) {
    let closure = Closure {
        value,
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

/// Fetches a closure from the local or the initial environment, or fails with
/// [crate::error::EvalError::UnboundIdentifier] with either the variable position, or the
/// provided fallback position if the former isn't defined.
fn get_var(
    id: LocIdent,
    env: &Environment,
    initial_env: &Environment,
    pos: TermPos,
) -> Result<CacheIndex, EvalError> {
    env.get(&id.ident())
        .or_else(|| initial_env.get(&id.ident()))
        .cloned()
        .ok_or(EvalError::UnboundIdentifier(id, id.pos.or(pos)))
}

/// Recursively substitute each variable occurrence of a term for its value in the environment.
pub fn subst<C: Cache>(
    cache: &C,
    value: NickelValue,
    initial_env: &Environment,
    env: &Environment,
) -> NickelValue {
    match term.into_owned() {
        Term::Var(id) => get_var(id, env, initial_env, TermPos::None)
            .map(|idx| {
                let closure = cache.get(idx);
                subst(cache, closure.value, initial_env, &closure.env)
            })
            .unwrap_or_else(|_| RichTerm::new(Term::Var(id), pos)),
        Term::Closure(idx) => {
                let closure = cache.get(idx.clone());
                subst(cache, closure.value, initial_env, &closure.env)
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
        | v @ Term::CustomContract(_)
        | v @ Term::Lbl(_)
        | v @ Term::ForeignId(_)
        | v @ Term::SealingKey(_)
        | v @ Term::Enum(_)
        | v @ Term::Import(_)
        | v @ Term::ResolvedImport(_)
        // We could recurse here, because types can contain terms which would then be subject to
        // substitution. Not recursing should be fine, though, because a type in term position
        // turns into a contract, and we don't substitute inside contracts either currently.
        | v @ Term::Type {..} => RichTerm::new(v, pos),
        Term::EnumVariant { tag, arg, attrs } => {
            let arg = subst(cache, arg, initial_env, env);

            RichTerm::new(Term::EnumVariant { tag, arg, attrs }, pos)
        }
        Term::Let(bindings, body, attrs) => {
            let bindings = bindings.into_iter().map(|(key, val)| (key, subst(cache, val, initial_env, env))).collect();
            let body = subst(cache, body, initial_env, env);

            RichTerm::new(Term::Let(bindings, body, attrs), pos)
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
        // Currently, we downright ignore `include` expressions. However, one could argue that
        // substituting `foo` for `bar` in `{include foo}` should result in `{foo = bar}`.
        Term::RecRecord(record, includes, dyn_fields, deps) => {
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

            RichTerm::new(Term::RecRecord(record, includes, dyn_fields, deps), pos)
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
