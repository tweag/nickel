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
//!   causes an [`crate::error::EvalErrorData::InternalError`]. A resolved import, identified by a
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
    cache::{CacheHub as ImportCaches, ImportResolver},
    closurize::{Closurize, closurize_rec_record},
    environment::Environment as GenericEnvironment,
    error::{Error, EvalCtxt, EvalError, EvalErrorData, Reporter, warning::Warning},
    files::{FileId, Files},
    identifier::{Ident, LocIdent},
    metrics::{increment, measure_runtime},
    position::{PosIdx, PosTable},
    program::FieldPath,
    term::{
        BinaryOp, BindingType, FunData, Import, MatchBranch, MatchData, RecordOpKind,
        RuntimeContract, StrChunk, Term, UnaryOp, make as mk_term,
        pattern::compile::Compile,
        record::{Field, RecordData},
        string::NickelString,
    },
    transform::gen_pending_contracts,
};

use std::{
    io::Write,
    mem::ManuallyDrop,
    ops::{Deref, DerefMut},
};

pub mod cache;
pub mod callstack;
pub mod contract_eq;
pub mod fixpoint;
pub mod merge;
pub mod operation;
pub mod stack;
pub mod value;

use callstack::*;
use operation::OperationCont;
use stack::{Stack, StrAccData};
use value::{
    Container, EnumVariantData, NickelValue, ValueContent, ValueContentRef, ValueContentRefMut,
};

use self::cache::{Cache, CacheIndex};

impl AsRef<Vec<StackElem>> for CallStack {
    fn as_ref(&self) -> &Vec<StackElem> {
        &self.0
    }
}

/// The context of the Nickel virtual machine. The context stores external state that might need to
/// outlive one VM instance. The virtual machine typically borrows the context mutably.
pub struct VmContext<R: ImportResolver, C: Cache> {
    /// The interface used to resolve and fetch imports.
    pub import_resolver: R,
    /// The stream for writing trace output.
    pub trace: Box<dyn Write>,
    /// A collector for warnings. Currently we only collect warnings and not errors; errors
    /// terminate evaluation (or typechecking, or whatever) immediately, and so they just
    /// get early-returned in a `Result`.
    pub reporter: Box<dyn Reporter<(Warning, Files)>>,
    /// The evaluation cache.
    pub cache: C,
    /// The position table, mapping position indices to spans.
    pub pos_table: PosTable,
}

impl<R: ImportResolver, C: Cache> VmContext<R, C> {
    /// Creates a new VM context from a resolver, a trace sink and a reporter. Creates a new empty
    /// position table and eval cache.
    pub fn new(
        import_resolver: R,
        trace: impl Write + 'static,
        reporter: impl Reporter<(Warning, Files)> + 'static,
    ) -> Self {
        Self::new_with_pos_table(import_resolver, PosTable::new(), trace, reporter)
    }

    /// Creates a new VM context from a resolver, a position table, a trace sink and a reporter.
    /// Creates a new empty eval cache.
    pub fn new_with_pos_table(
        import_resolver: R,
        pos_table: PosTable,
        trace: impl Write + 'static,
        reporter: impl Reporter<(Warning, Files)> + 'static,
    ) -> Self {
        VmContext {
            import_resolver,
            trace: Box::new(trace),
            reporter: Box::new(reporter),
            cache: C::new(),
            pos_table,
        }
    }
}

impl<C: Cache> VmContext<ImportCaches, C> {
    /// Prepares the underlying program for evaluation (load the stdlib, typecheck, transform,
    /// etc.).
    pub fn prepare_eval(&mut self, main_id: FileId) -> Result<NickelValue, Error> {
        self.prepare_eval_impl(main_id, true)
    }

    /// Same as [Self::prepare_eval], but skip typechecking.
    pub fn prepare_eval_only(&mut self, main_id: FileId) -> Result<NickelValue, Error> {
        self.prepare_eval_impl(main_id, false)
    }

    fn prepare_eval_impl(
        &mut self,
        main_id: FileId,
        typecheck: bool,
    ) -> Result<NickelValue, Error> {
        measure_runtime!(
            "runtime:prepare_stdlib",
            self.import_resolver.prepare_stdlib(&mut self.pos_table)?
        );

        measure_runtime!(
            "runtime:prepare_main",
            if typecheck {
                self.import_resolver.prepare(&mut self.pos_table, main_id)
            } else {
                self.import_resolver
                    .prepare_eval_only(&mut self.pos_table, main_id)
            }
        )?;

        // Unwrap: closurization only fails if the input wasn't parsed, and we just
        // parsed it.
        self.import_resolver
            .closurize(&mut self.cache, main_id)
            .unwrap();

        Ok(self.import_resolver.get(main_id).unwrap())
    }
}

/// The Nickel virtual machine.
///
/// # Drop
///
/// The virtual machine implements [Drop]. The stack is unwinded by default upon dropping, which
/// amounts to calling [VirtualMachine::reset()], and avoids leaving thunks in the blackholed state
/// on abort. If you don't need unwinding and don't want to pay for it (though it doesn't cost
/// anything for successful executions), see  [NoUnwindVirtualMachine].
pub struct VirtualMachine<'ctxt, R: ImportResolver, C: Cache> {
    context: &'ctxt mut VmContext<R, C>,
    /// The main stack, storing arguments, cache indices and pending computations.
    stack: Stack<C>,
    /// The call stack, for error reporting.
    call_stack: CallStack,
    /// The initial environment containing stdlib and builtin functions accessible from anywhere
    initial_env: Environment,
}

impl<'ctxt, R: ImportResolver, C: Cache> Drop for VirtualMachine<'ctxt, R, C> {
    fn drop(&mut self) {
        self.reset();
    }
}

impl<'ctxt, R: ImportResolver, C: Cache> VirtualMachine<'ctxt, R, C> {
    /// Creates a new VM with an empty initial environment. See [Self::new] for initialization of
    /// the initial environment when `R` is instantiated to [crate::cache::CacheHub].
    pub fn new_empty_env(context: &'ctxt mut VmContext<R, C>) -> Self {
        VirtualMachine {
            context,
            call_stack: Default::default(),
            stack: Stack::new(),
            initial_env: Environment::new(),
        }
    }

    pub fn warn(&mut self, warning: Warning) {
        self.context
            .reporter
            .report((warning, self.context.import_resolver.files().clone()));
    }

    /// Reset the state of the machine (stacks, eval mode and state of cached elements) to prepare
    /// for another evaluation round.
    pub fn reset(&mut self) {
        self.call_stack.0.clear();
        self.stack.reset(&mut self.context.cache);
    }

    pub fn import_resolver(&self) -> &R {
        &self.context.import_resolver
    }

    pub fn import_resolver_mut(&mut self) -> &mut R {
        &mut self.context.import_resolver
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
                value: subst(
                    &self.context.pos_table,
                    &self.context.cache,
                    result.value,
                    &self.initial_env,
                    &result.env,
                ),
                env: result.env,
            })
    }

    /// Like [Self::eval_full], but skips evaluating record fields marked `not_exported`.
    pub fn eval_full_for_export(&mut self, value: NickelValue) -> Result<NickelValue, EvalError> {
        self.eval_deep_closure_impl(value.into(), true)
            .map(|result| {
                subst(
                    &self.context.pos_table,
                    &self.context.cache,
                    result.value,
                    &self.initial_env,
                    &result.env,
                )
            })
    }

    /// Same as [Self::eval_full_for_export], but takes a closure as an argument instead of a term.
    pub fn eval_full_for_export_closure(
        &mut self,
        closure: Closure,
    ) -> Result<NickelValue, EvalError> {
        self.eval_deep_closure_impl(closure, true).map(|result| {
            subst(
                &self.context.pos_table,
                &self.context.cache,
                result.value,
                &self.initial_env,
                &result.env,
            )
        })
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

    /// Use a specific initial environment for evaluation. Usually, [VmContext::prepare_eval] is
    /// populating the initial environment. But in some cases, such as testing or benchmarks, we
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

        let Closure { value, env } = self.eval_closure(Closure {
            value: value_with_ctr,
            env,
        })?;

        Ok(Closure { value, env })
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
                return self.throw_with_ctxt(EvalErrorData::MissingFieldDef {
                    id: *prev_id,
                    metadata: field.metadata,
                    pos_record: prev_pos_idx,
                    pos_access: PosIdx::NONE,
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

            match current_evaled.value.content_ref() {
                ValueContentRef::Record(container) => {
                    let Some(next_field) = container.get(*id).cloned() else {
                        let pos_op = self.context.pos_table.push(id.pos);

                        return self.throw_with_ctxt(EvalErrorData::FieldMissing {
                            id: *id,
                            field_names: container.field_names(RecordOpKind::IgnoreEmptyOpt),
                            operator: "extract_field".to_owned(),
                            pos_record: prev_pos_idx,
                            // TODO: we need to push back the position in the table, which isn't
                            // too bad, but is a bit useless. Maybe we should have a runtime
                            // version of identifiers with `PosIdx` instead of `TermPos`?
                            pos_op,
                        });
                    };

                    field = next_field;
                }
                _ => {
                    //unwrap(): if we enter this pattern branch, `field.value` must be `Some(_)`
                    return self.throw_with_ctxt(EvalErrorData::QueryNonRecord {
                        pos: prev_pos_idx,
                        id: *id,
                        value: current_evaled.value,
                    });
                }
            }

            prev_id = id;
        }

        if field.value.is_none() && require_defined {
            return self.throw_with_ctxt(EvalErrorData::MissingFieldDef {
                id: *prev_id,
                metadata: field.metadata,
                pos_record: prev_pos_idx,
                pos_access: PosIdx::NONE,
            });
        }

        Ok((field, env))
    }

    pub fn query(&mut self, v: NickelValue, path: &FieldPath) -> Result<Field, EvalError> {
        self.query_closure(v.into(), path)
    }

    /// Generates an error context from this virtual machine. This consumes the callstack, which
    /// doesn't have to be preserved after an error. However, we have to preserve the position
    /// table, since the VM could be reset and used for another evaluation round. The latter is
    /// copied.
    fn eval_ctxt(&mut self) -> EvalCtxt {
        EvalCtxt {
            call_stack: std::mem::take(&mut self.call_stack),
            pos_table: self.context.pos_table.clone(),
        }
    }

    /// Wraps [Self::err_with_ctxt] in the `Err` variant.
    fn throw_with_ctxt<T>(&mut self, error: EvalErrorData) -> Result<T, EvalError> {
        Err(self.err_with_ctxt(error))
    }

    /// Wraps an evaluation error [crate::error::EvalErrorData] with the current evaluation context
    /// ([Self::eval_ctxt]) to make a [crate::error::EvalError].
    fn err_with_ctxt(&mut self, error: EvalErrorData) -> EvalError {
        EvalError {
            error,
            ctxt: self.eval_ctxt(),
        }
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
    ) -> Result<Closure, EvalErrorData> {
        // idx may be a 1-counted RC, so we make sure we drop any reference to it from `env`, which
        // is going to be discarded anyway
        std::mem::drop(env);

        match self.context.cache.get_update_index(&mut idx) {
            Ok(Some(idx_upd)) => self.stack.push_update_index(idx_upd),
            Ok(None) => {}
            Err(_blackholed_error) => {
                return Err(EvalErrorData::InfiniteRecursion(
                    self.call_stack.clone(),
                    pos_idx,
                ));
            }
        }

        if let Some(var) = var {
            self.call_stack.enter_var(var, pos_idx);
        }

        // If we are fetching a recursive field from the environment that doesn't have
        // a definition, we complete the error with the additional information of where
        // it was accessed:
        let closure = self.context.cache.get(idx);

        let value = match closure.value.content() {
            ValueContent::Term(lens)
                if matches!(
                    lens.term(),
                    Term::RuntimeError(EvalErrorData::MissingFieldDef { .. })
                ) =>
            {
                let Term::RuntimeError(EvalErrorData::MissingFieldDef {
                    id,
                    metadata,
                    pos_record,
                    pos_access: PosIdx::NONE,
                }) = lens.take()
                else {
                    unreachable!();
                };

                NickelValue::term(
                    Term::RuntimeError(EvalErrorData::MissingFieldDef {
                        id,
                        metadata,
                        pos_record,
                        pos_access: pos_idx,
                    }),
                    pos_idx,
                )
            }
            lens => lens.restore(),
        };

        Ok(Closure { value, ..closure })
    }

    /// Fetches a closure from the local or the initial environment, or fails with
    /// [crate::error::EvalErrorData::UnboundIdentifier] with either the variable position, or the
    /// provided fallback position if the former isn't defined.
    fn get_var(
        &self,
        id: LocIdent,
        env: &Environment,
        pos_idx: PosIdx,
    ) -> Result<CacheIndex, EvalErrorData> {
        get_var(&self.context.pos_table, id, &self.initial_env, env, pos_idx)
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
    pub fn eval_closure(&mut self, closure: Closure) -> Result<Closure, EvalError> {
        self.eval_closure_impl(closure)
            .map_err(|err| self.err_with_ctxt(err))
    }

    /// Actual implementation of [Self::eval_closure]. We use this indirection mostly to use the
    /// `?` operator on [crate::error::EvalErrorData], and only add the missing context to make it
    /// an [crate:error::EvalError] once at the end.
    fn eval_closure_impl(&mut self, mut closure: Closure) -> Result<Closure, EvalErrorData> {
        #[cfg(feature = "metrics")]
        let start_time = std::time::Instant::now();

        // See https://github.com/rust-lang/rust-clippy/issues/15987.
        #[allow(clippy::let_and_return)]
        let result = loop {
            let Closure { value, mut env } = closure;
            let pos_idx = value.pos_idx();
            let has_cont_on_stack = self.stack.is_top_idx() || self.stack.is_top_cont();

            closure = match value.content_ref() {
                ValueContentRef::Thunk(thunk) => {
                    self.enter_cache_index(None, thunk.clone(), pos_idx, env)?
                }
                ValueContentRef::Term(Term::Value(value)) => Closure {
                    value: value.clone(),
                    env,
                },
                ValueContentRef::Term(Term::Sealed(data)) => {
                    let stack_item = self.stack.peek_op_cont();
                    let closure = Closure {
                        value: NickelValue::term(Term::Sealed(data.clone()), pos_idx),
                        env: env.clone(),
                    };

                    // Update at the original index (the index which holds the result of the op) in
                    // both cases, even if we continue with a seq.
                    //
                    // We do this because we are on a `Sealed` term which is in weak head normal
                    // form, and if we don't, we will be unwrapping a `Sealed` term and assigning
                    // the "unsealed" value to the result of the `Seq` operation. See also:
                    // https://github.com/tweag/nickel/issues/123
                    update_at_indices(&mut self.context.cache, &mut self.stack, &closure);

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
                            Closure {
                                value: data.inner.clone(),
                                env,
                            }
                        }
                        None | Some(..) => {
                            // This operation should not be allowed to evaluate a sealed term
                            break Err(EvalErrorData::BlameError {
                                evaluated_arg: data.label.get_evaluated_arg(&self.context.cache),
                                label: data.label.clone(),
                            });
                        }
                    }
                }
                ValueContentRef::Term(Term::Var(id)) => {
                    let idx = self.get_var(*id, &env, pos_idx)?;
                    self.enter_cache_index(Some(*id), idx, pos_idx, env)?
                }
                ValueContentRef::Term(Term::App(head, arg)) => {
                    self.call_stack.enter_app(&self.context.pos_table, pos_idx);

                    self.stack.push_arg(
                        Closure {
                            value: arg.clone(),
                            env: env.clone(),
                        },
                        pos_idx,
                    );
                    Closure {
                        value: head.clone(),
                        env,
                    }
                }
                ValueContentRef::Term(Term::Let(data)) => {
                    let mut indices = Vec::new();
                    let init_env = env.clone();

                    for (x, bound) in &data.bindings {
                        let bound_closure = Closure {
                            value: bound.clone(),
                            env: init_env.clone(),
                        };

                        let idx = self
                            .context
                            .cache
                            .add(bound_closure, data.attrs.binding_type.clone());

                        // Patch the environment with the (x <- closure) binding
                        if data.attrs.rec {
                            indices.push(idx.clone());
                        }

                        env.insert(x.ident(), idx);
                    }

                    for idx in indices {
                        self.context.cache.patch(idx, |cl| cl.env = env.clone());
                    }

                    Closure {
                        value: data.body.clone(),
                        env,
                    }
                }
                ValueContentRef::Term(Term::Op1(data)) => {
                    self.stack.push_op_cont(
                        OperationCont::Op1(data.op.clone(), data.arg.pos_idx()),
                        self.call_stack.len(),
                        pos_idx,
                    );

                    Closure {
                        value: data.arg.clone(),
                        env,
                    }
                }
                ValueContentRef::Term(Term::Op2(data)) => {
                    self.stack.push_op_cont(
                        OperationCont::Op2First(
                            data.op.clone(),
                            Closure {
                                value: data.arg2.clone(),
                                env: env.clone(),
                            },
                            data.arg1.pos_idx(),
                        ),
                        self.call_stack.len(),
                        pos_idx,
                    );

                    Closure {
                        value: data.arg1.clone(),
                        env,
                    }
                }
                ValueContentRef::Term(Term::OpN(data)) => {
                    // Arguments are passed as a stack to the operation continuation, so we reverse
                    // the original list.
                    let mut args_iter = data.args.iter();
                    let fst_arg = args_iter.next().ok_or_else(|| {
                        EvalErrorData::NotEnoughArgs(data.op.arity(), data.op.to_string(), pos_idx)
                    })?;

                    let pending: Vec<Closure> = args_iter
                        .rev()
                        .map(|value| Closure {
                            value: value.clone(),
                            env: env.clone(),
                        })
                        .collect();

                    self.stack.push_op_cont(
                        OperationCont::OpN {
                            op: data.op.clone(),
                            evaluated: Vec::with_capacity(pending.len() + 1),
                            pending,
                            current_pos_idx: fst_arg.pos_idx(),
                        },
                        self.call_stack.len(),
                        pos_idx,
                    );

                    Closure {
                        value: fst_arg.clone(),
                        env,
                    }
                }
                ValueContentRef::Term(Term::StrChunks(chunks)) => {
                    let mut chunks_iter = chunks.iter().cloned();
                    match chunks_iter.next_back() {
                        None => NickelValue::string(NickelString::new(), pos_idx).into(),
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
                                curr_pos: arg.pos_idx(),
                            });

                            // TODO: we should set up the stack properly, instead of allocating an
                            // `op1` term here.
                            Closure {
                                value: NickelValue::term(
                                    Term::op1(UnaryOp::ChunksConcat, arg),
                                    pos_idx,
                                ),
                                env,
                            }
                        }
                    }
                }
                ValueContentRef::Term(Term::Closurize(value)) => {
                    // Closurization is done the first time we see a value, so under normal
                    // conditions, this value should not be shared and we should be able to
                    // mutate it directly.
                    // TODO[RFC007]: we clone the value, so taking the content is meaningless. We
                    // should probably do a `content()` call at the top of the eval function.
                    let result = match value.clone().content() {
                        ValueContent::Array(lens) if lens.peek().is_inline_empty_array() => {
                            lens.restore()
                        }
                        ValueContent::Array(lens) => {
                            // unwrap(): we treated the empty array case above
                            let array_data = lens.take().into_opt().unwrap();

                            // This *should* make it unnecessary to call closurize in [operation].
                            // See the comment on the `BinaryOp::ArrayConcat` match arm.
                            let array = array_data
                                .array
                                .into_iter()
                                .map(|t| t.closurize(&mut self.context.cache, env.clone()))
                                .collect();

                            let pending_contracts = array_data
                                .pending_contracts
                                .into_iter()
                                .map(|ctr| {
                                    RuntimeContract::new(
                                        ctr.contract
                                            .closurize(&mut self.context.cache, env.clone()),
                                        ctr.label,
                                    )
                                })
                                .collect();

                            NickelValue::array(array, pending_contracts, pos_idx)
                        }
                        ValueContent::Record(lens) if lens.peek().is_inline_empty_record() => {
                            lens.restore()
                        }
                        ValueContent::Record(lens) => NickelValue::record(
                            // unwrap(): we treated the empty record case already
                            lens.take()
                                .into_opt()
                                .unwrap()
                                .closurize(&mut self.context.cache, env),
                            pos_idx,
                        ),
                        ValueContent::EnumVariant(lens) => {
                            let EnumVariantData { tag, arg } = lens.take();
                            let arg = arg.map(|arg| arg.closurize(&mut self.context.cache, env));
                            NickelValue::enum_variant(tag, arg, pos_idx)
                        }
                        lens => {
                            // This case is a red flag (it should be unreachable), but
                            // isn't a blocker per se, so we only fail in debug mode.
                            debug_assert!(false, "trying to closurize a non-container value");
                            lens.restore()
                        }
                    };

                    // We can use an empty environment for a freshly closurized value
                    result.into()
                }
                ValueContentRef::Term(Term::RecRecord(data)) => {
                    // We start by closurizing the fields, which might not be if the record is
                    // coming out of the parser.

                    // We must avoid re-closurizing a recursive record that is already closurized
                    // (coming from `merge`, for example), as the current representation is broken
                    // if we add a new indirection. This should ideally be encoded in the Rust
                    // type, once we have a different representation for runtime evaluation,
                    // instead of relying on invariants. But for now, we have to live with it.
                    let (mut static_part, dyn_fields) = if !data.closurized {
                        let includes_as_terms: Result<Vec<_>, _> = data
                            .includes
                            .iter()
                            .map(|incl| -> Result<_, EvalErrorData> {
                                let field = Field {
                                    value: Some(NickelValue::thunk(
                                        self.get_var(incl.ident, &env, PosIdx::NONE)?,
                                        self.context.pos_table.push(incl.ident.pos),
                                    )),
                                    metadata: incl.metadata.clone(),
                                    pending_contracts: Vec::new(),
                                };

                                Ok((
                                    incl.ident,
                                    gen_pending_contracts::with_pending_contracts(
                                        &mut self.context.pos_table,
                                        field,
                                    )?,
                                ))
                            })
                            .collect();

                        // We assume that the parser doesn't allow conflicts between field
                        // definitions and includes (the same field is defined in both). This
                        // restriction might be lifted in the future (we would probably merge the
                        // included field and the other definition pieces), but for now it's
                        // simpler this way.
                        let mut record = data.record.clone();
                        record.fields.extend(includes_as_terms?);

                        closurize_rec_record(
                            &mut self.context.cache,
                            record,
                            data.dyn_fields.clone(),
                            data.deps.clone(),
                            env,
                        )
                    } else {
                        // In a record that has been already closurized, we expect include
                        // expressions to be evaluated away.
                        debug_assert!(data.includes.is_empty());
                        (data.record.clone(), data.dyn_fields.clone())
                    };

                    let rec_env = fixpoint::rec_env(
                        &mut self.context.cache,
                        static_part.fields.iter(),
                        pos_idx,
                    );

                    for rt in static_part.fields.values_mut() {
                        fixpoint::patch_field(&mut self.context.cache, rt, &rec_env);
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
                            let pos_dyn_field = field
                                .value
                                .as_ref()
                                .map(NickelValue::pos_idx)
                                .unwrap_or_else(|| name_as_term.pos_idx());

                            fixpoint::patch_field(&mut self.context.cache, &mut field, &rec_env);

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
                                    pos_dyn_field.to_inherited(&mut self.context.pos_table),
                                ),
                                None => extend,
                            }
                        },
                    );

                    extended.with_pos_idx(pos_idx).into()
                }
                ValueContentRef::Term(Term::ResolvedImport(id)) => {
                    increment!(format!("import:{id:?}"));

                    if let Some(val) = self.context.import_resolver.get(*id) {
                        val.into()
                    } else {
                        break Err(EvalErrorData::InternalError(
                            format!("Resolved import not found ({id:?})"),
                            pos_idx,
                        ));
                    }
                }
                ValueContentRef::Term(Term::Import(Import::Path { path, .. })) => {
                    break Err(EvalErrorData::InternalError(
                        format!("Unresolved import ({})", path.to_string_lossy()),
                        pos_idx,
                    ));
                }
                ValueContentRef::Term(Term::Import(Import::Package { id })) => {
                    return Err(EvalErrorData::InternalError(
                        format!("Unresolved package import ({id})"),
                        pos_idx,
                    ));
                }
                ValueContentRef::Term(Term::ParseError(parse_error)) => {
                    break Err(EvalErrorData::ParseError(parse_error.clone()));
                }
                ValueContentRef::Term(Term::RuntimeError(error)) => {
                    break Err(error.clone());
                }
                // For now, we simply erase annotations at runtime. They aren't accessible anyway
                // (as opposed to field metadata) and don't change the operational semantics, as
                // long as we generate the corresponding contract application when consuming it.
                //
                // The situation could change if we want to implement optimizations such as
                // avoiding repeated contract application. Annotations could then be a good way of
                // remembering which contracts have been applied to a value.
                ValueContentRef::Term(Term::Annotated(data)) => {
                    increment!("contract:free-standing(annotated)");

                    // We apply the contract coming from the static type annotation separately as
                    // it is optimized.
                    let static_contract = data.annot.static_contract(&mut self.context.pos_table);
                    let contracts = data.annot.pending_contracts(&mut self.context.pos_table)?;
                    let pos_inner = data.inner.pos_idx();
                    let inner = data.inner.clone();

                    let inner_with_static = if let Some(static_ctr) = static_contract {
                        static_ctr?.apply(inner, pos_inner)
                    } else {
                        inner
                    };

                    let inner_with_ctr =
                        RuntimeContract::apply_all(inner_with_static, contracts, pos_inner);

                    Closure {
                        value: inner_with_ctr,
                        env,
                    }
                }
                // Function call if there's no continuation on the stack (otherwise, the function
                // is just an argument to a primop or to put in the eval cache)
                ValueContentRef::Term(Term::Fun(FunData { arg, body })) if !has_cont_on_stack => {
                    if let Some((idx, pos_app)) = self.stack.pop_arg_as_idx(&mut self.context.cache)
                    {
                        self.call_stack.enter_fun(&self.context.pos_table, pos_app);
                        env.insert(arg.ident(), idx);
                        Closure {
                            value: body.clone(),
                            env,
                        }
                    } else {
                        break Ok(Closure { value, env });
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
                ValueContentRef::Term(Term::Match(data)) if !has_cont_on_stack => {
                    if let Some((arg, _)) = self.stack.pop_arg(&self.context.cache) {
                        Closure {
                            value: data.clone().compile(
                                &mut self.context.pos_table,
                                arg.value.closurize(&mut self.context.cache, arg.env),
                                pos_idx,
                            ),
                            env,
                        }
                    } else {
                        break Ok(Closure {
                            value: NickelValue::term(Term::Match(data.clone()), pos_idx),
                            env,
                        });
                    }
                }
                ValueContentRef::Term(Term::FunPattern(..) | Term::LetPattern(..)) => {
                    break Err(EvalErrorData::InternalError(
                        "unexpected let-pattern or fun-pattern during evaluation".to_owned(),
                        pos_idx,
                    ));
                }
                // At this point, we've evaluated the current term to a weak head normal form.
                _ => {
                    let evaluated = Closure { value, env };

                    // If there is a cache index update frame on the stack, we proceed with the
                    // update of the corresponding cached value.
                    if self.stack.is_top_idx() {
                        update_at_indices(&mut self.context.cache, &mut self.stack, &evaluated);
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
                    else if let Some((arg, pos_app)) = self.stack.pop_arg(&self.context.cache) {
                        break Err(EvalErrorData::NotAFunc(evaluated.value, arg.value, pos_app));
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
                Ok(val) => match val.content_ref() {
                    ValueContentRef::Array(Container::Alloc(data)) => {
                        for elt in data.array.iter() {
                            // After eval_closure, all the array elements  are
                            // closurized already, so we don't need to do any tracking
                            // of the env.
                            let value_with_ctr = RuntimeContract::apply_all(
                                elt.clone(),
                                data.pending_contracts.iter().cloned(),
                                elt.pos_idx(),
                            );
                            inner(this, acc, value_with_ctr, recursion_limit.saturating_sub(1));
                        }
                    }
                    ValueContentRef::Record(Container::Alloc(data)) => {
                        for (id, field) in &data.fields {
                            if let Some(v) = &field.value {
                                let value_with_ctr = RuntimeContract::apply_all(
                                    v.clone(),
                                    field.pending_contracts.iter().cloned(),
                                    v.pos_idx(),
                                );
                                inner(this, acc, value_with_ctr, recursion_limit.saturating_sub(1));
                            } else {
                                acc.push(this.err_with_ctxt(EvalErrorData::MissingFieldDef {
                                    id: *id,
                                    metadata: field.metadata.clone(),
                                    pos_record: pos_idx,
                                    pos_access: PosIdx::NONE,
                                }));
                            }
                        }
                    }
                    ValueContentRef::EnumVariant(EnumVariantData { arg: Some(arg), .. }) => {
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

    /// This is a temporary, ugly work-around for the fact that the VM owns both the position table
    /// and the cache, but [crate::program::Program] sometimes need to access both at the same
    /// time, mutably. Ideally, either the VM would borrow them, or the position table would be
    /// owned by something else and just passed for evaluation, or any other design - but it sounds
    /// abusive that the VM owns the two, which should be able to survive it or be initialized
    /// before it.
    pub(crate) fn _with_resolver_and_table<F, T>(&mut self, f: F) -> T
    where
        F: for<'a> FnOnce(&'a mut PosTable, &'a mut R) -> T,
    {
        f(
            &mut self.context.pos_table,
            &mut self.context.import_resolver,
        )
    }
}

impl<'ctxt, C: Cache> VirtualMachine<'ctxt, ImportCaches, C> {
    /// Creates a new VM, and automatically fills the initial environment with
    /// `context.import_resolver.mk_initial_env()`.
    pub fn new(context: &'ctxt mut VmContext<ImportCaches, C>) -> Self {
        let initial_env = context.import_resolver.mk_eval_env(&mut context.cache);

        VirtualMachine {
            context,
            call_stack: Default::default(),
            stack: Stack::new(),
            initial_env,
        }
    }
}

/// An wrapper around [VirtualMachine] which doesn't unwind the VM stack upon destruction.
///
/// Unwinding ensures all thunks are properly cleaned from their previous state if the VM abort,
/// which makes it possible to run subsequent evaluations (whether in the same or another VM
/// instance). Since VM are light instances that are built on the spot, the default behavior is to
/// unwind upon dropping, to avoid bad surprises (and unwinding is virtually free if the evaluation
/// succeeds, since the stack is then empty).
///
/// However, it could happen that in some workflows, one wishes to avoid the cost of unwinding. In
/// that case, use this wrapper instead of [VirtualMachine].
pub struct NoUnwindVirtualMachine<'ctxt, R: ImportResolver, C: Cache>(
    ManuallyDrop<VirtualMachine<'ctxt, R, C>>,
);

impl<'ctxt, R: ImportResolver, C: Cache> NoUnwindVirtualMachine<'ctxt, R, C> {
    pub fn new(vm: VirtualMachine<'ctxt, R, C>) -> Self {
        Self(ManuallyDrop::new(vm))
    }
}

impl<'ctxt, R: ImportResolver, C: Cache> Deref for NoUnwindVirtualMachine<'ctxt, R, C> {
    type Target = VirtualMachine<'ctxt, R, C>;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl<'ctxt, R: ImportResolver, C: Cache> DerefMut for NoUnwindVirtualMachine<'ctxt, R, C> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.deref_mut()
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
    if closure.value.is_inline_empty_record() {
        return Ok(());
    }

    // We unwrap potential `Closurize` sprinkled in unevaluated terms, which is typically the case
    // for the stdlib.
    let value = if let Some(Term::Closurize(inner)) = closure.value.as_term() {
        inner
    } else {
        &closure.value
    };

    let record = match value.content_ref() {
        ValueContentRef::Record(Container::Empty) => return Ok(()),
        ValueContentRef::Record(Container::Alloc(record)) => record,
        ValueContentRef::Term(Term::RecRecord(data)) => &data.record,
        _ => return Err(EnvBuildError::NotARecord(closure.value)),
    };

    let ext = record.fields.iter().filter_map(|(id, field)| {
        field.value.as_ref().map(|value| {
            (
                id.ident(),
                cache.add(
                    Closure {
                        value: value.clone(),
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

/// Free-standing implementation of [VirtualMachine::get_var].
fn get_var(
    pos_table: &PosTable,
    id: LocIdent,
    initial_env: &Environment,
    env: &Environment,
    pos_idx: PosIdx,
) -> Result<CacheIndex, EvalErrorData> {
    env.get(&id.ident())
        .or_else(|| initial_env.get(&id.ident()))
        .cloned()
        .ok_or(EvalErrorData::UnboundIdentifier(
            id,
            id.pos.or(pos_table.get(pos_idx)),
        ))
}

/// Recursively substitute each variable occurrence of a term for its value in the environment.
pub fn subst<C: Cache>(
    pos_table: &PosTable,
    cache: &C,
    value: NickelValue,
    initial_env: &Environment,
    env: &Environment,
) -> NickelValue {
    use value::lens::TermContent;

    let pos_idx = value.pos_idx();

    match value.content() {
        lens @ (ValueContent::Null(_)
        | ValueContent::Bool(_)
        | ValueContent::Number(_)
        | ValueContent::String(_)
        | ValueContent::CustomContract(_)
        | ValueContent::Label(_)
        // We could recurse in types, because types can contain terms which would then be subject
        // to substitution. Not recursing should be fine, though, because a type in term position
        // turns into a contract, and we don't substitute inside contracts either currently.
        | ValueContent::Type(_)
        | ValueContent::ForeignId(_)
        | ValueContent::SealingKey(_)) => lens.restore(),
        ValueContent::Thunk(lens) => {
            let closure = cache.get(lens.take());
            subst(pos_table, cache, closure.value, initial_env, &closure.env)
        }
        ValueContent::Record(lens) => {
            let Container::Alloc(record) = lens.take() else {
                return NickelValue::empty_record().with_pos_idx(pos_idx);
            };

            let record = record.map_defined_values(|_, value| subst(pos_table, cache, value, initial_env, env));

            //unwrap(): we didn't change the size of the record, so whether it was inline or not,
            //its position index should be of the according type
            NickelValue::record(record, pos_idx)
        }
        ValueContent::Array(lens) => {
            let Container::Alloc(array_data) = lens.take() else {
                return NickelValue::empty_array().with_pos_idx(pos_idx);
            };

            let array = array_data.array.into_iter()
                .map(|t| subst(pos_table, cache, t, initial_env, env))
                .collect();

            NickelValue::array(array, array_data.pending_contracts, pos_idx)
        }
        ValueContent::EnumVariant(lens) => {
            let EnumVariantData { tag, arg } = lens.take();
            let arg = arg.map(|arg| subst(pos_table, cache, arg, initial_env, env));

            NickelValue::enum_variant(tag, arg, pos_idx)
        }
        ValueContent::Term(content) => {
            match content {
                TermContent::Var(lens) => {
                    let id = lens.take();

                    get_var(pos_table, id, env, initial_env, PosIdx::NONE)
                    .map(|idx| {
                        let closure = cache.get(idx);
                        subst(pos_table, cache, closure.value, initial_env, &closure.env)
                    })
                    .unwrap_or_else(|_| NickelValue::term(Term::Var(id), pos_idx))
                }
                lens @ (TermContent::ParseError(_) | TermContent::RuntimeError(_)
                // Do not substitute under lambdas: mutually recursive function could cause an infinite
                // loop. Although avoidable, this requires some care and is not currently needed.
                | TermContent::Fun(..)
                | TermContent::Import(_)
                | TermContent::ResolvedImport(_)) => lens.restore(),
                TermContent::Let(lens) => {
                    let mut data = lens.take();

                    for (_key, val) in &mut data.bindings {
                        let prev = std::mem::take(val);
                        *val = subst(pos_table, cache, prev, initial_env, env);
                    }
                    data.body = subst(pos_table, cache, data.body, initial_env, env);

                    NickelValue::term(Term::Let(data), pos_idx)
                }
                lens @ (TermContent::LetPattern(..) | TermContent::FunPattern(..)) => panic!(
                    "Pattern {:?} has not been transformed before evaluation", lens.restore()
                ),
                TermContent::App(lens) => {
                    let (head, arg) = lens.take();
                    let head = subst(pos_table, cache, head, initial_env, env);
                    let arg = subst(pos_table, cache, arg, initial_env, env);

                    NickelValue::term(Term::App(head, arg), pos_idx)
                }
                TermContent::Match(lens) => {
                    let data = lens.take();
                    let branches = data.branches
                        .into_iter()
                        .map(|MatchBranch { pattern, guard, body} | {
                            MatchBranch {
                                pattern,
                                guard: guard.map(|cond| subst(pos_table, cache, cond, initial_env, env)),
                                body: subst(pos_table, cache, body, initial_env, env),
                            }
                        })
                        .collect();

                    NickelValue::term(Term::Match(MatchData { branches }), pos_idx)
                }
                TermContent::Op1(lens) => {
                    let mut data = lens.take();
                    data.arg = subst(pos_table, cache, data.arg, initial_env, env);

                    NickelValue::term(Term::Op1(data), pos_idx)
                }
                TermContent::Op2(lens) => {
                    let mut data = lens.take();
                    data.arg1 = subst(pos_table, cache, data.arg1, initial_env, env);
                    data.arg2 = subst(pos_table, cache, data.arg2, initial_env, env);

                    NickelValue::term(Term::Op2(data), pos_idx)
                }
                TermContent::OpN(lens) => {
                    let mut data = lens.take();
                    data.args = data.args
                        .into_iter()
                        .map(|t| subst(pos_table, cache, t, initial_env, env))
                        .collect();

                    NickelValue::term(Term::OpN(data), pos_idx)
                }
                TermContent::Sealed(lens) => {
                    let mut data = lens.take();
                    data.inner = subst(pos_table, cache, data.inner, initial_env, env);
                    NickelValue::term(Term::Sealed(data), pos_idx)
                }
                // Currently, we downright ignore `include` expressions. However, one could argue that
                // substituting `foo` for `bar` in `{include foo}` should result in `{foo = bar}`.
                TermContent::RecRecord(lens) => {
                    let mut data = lens.take();

                    data.record = data.record
                        .map_defined_values(|_, value| subst(pos_table, cache, value, initial_env, env));

                    data.dyn_fields = data.dyn_fields
                        .into_iter()
                        .map(|(id_t, field)| {
                            (
                                subst(pos_table, cache, id_t, initial_env, env),
                                field.map_value(|v| subst(pos_table, cache, v, initial_env, env)),
                            )
                        })
                        .collect();

                    NickelValue::term(Term::RecRecord(data), pos_idx)
                }
                TermContent::StrChunks(lens) => {
                    let chunks = lens.take()
                        .into_iter()
                        .map(|chunk| match chunk {
                            chunk @ StrChunk::Literal(_) => chunk,
                            StrChunk::Expr(t, indent) => StrChunk::Expr(
                                subst(pos_table, cache, t, initial_env, env),
                                indent,
                            ),
                        })
                        .collect();

                    NickelValue::term(Term::StrChunks(chunks), pos_idx)
                }
                TermContent::Annotated(lens) => {
                    let mut data = lens.take();
                    data.inner = subst(pos_table, cache, data.inner, initial_env, env);
                    // Currently, there is no interest in replacing variables inside contracts, thus we
                    // limit the work of `subst`.
                    NickelValue::term(Term::Annotated(data), pos_idx)
                }
                // We erase `Value` nodes during substitution. When performing substitution, the
                // goal is to remove indirections.
                TermContent::Value(lens) => {
                    subst(pos_table, cache, lens.take(), initial_env, env)
                }
                TermContent::Closurize(lens) => {
                    NickelValue::term(Term::Closurize(subst(pos_table, cache, lens.take(), initial_env, env)), pos_idx)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;
