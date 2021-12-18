//! Evaluation of a Nickel term.
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
use crate::{
    cache::ImportResolver,
    environment::Environment as GenericEnvironment,
    error::EvalError,
    identifier::Ident,
    mk_app,
    operation::{continuate_operation, OperationCont},
    position::RawSpan,
    position::TermPos,
    stack::Stack,
    term::{make as mk_term, BinaryOp, MetaValue, RichTerm, StrChunk, Term, UnaryOp},
};
use codespan::FileId;
use std::cell::{Ref, RefCell, RefMut};
use std::rc::{Rc, Weak};

/// The state of a thunk.
///
/// When created, a thunk is flagged as suspended. When accessed for the first time, a corresponding
/// [`ThunkUpdateFrame`](./struct.ThunkUpdateFrame.html) is pushed on the stack and the thunk is
/// flagged as black-hole. This prevents direct infinite recursions, since if a thunk is
/// re-accessed while still in a black-hole state, we are sure that the evaluation will loop, and
/// we can thus error out before overflowing the stack or looping forever. Finally, once the
/// content of a thunk has been evaluated, the thunk is updated with the new value and flagged as
/// evaluated, so that future accesses won't even push an update frame on the stack.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ThunkState {
    Blackholed,
    Suspended,
    Evaluated,
}

/// The mutable data stored inside a thunk.
#[derive(Clone, Debug, PartialEq)]
pub struct ThunkData {
    inner: InnerThunkData,
    state: ThunkState,
}

/// The part of [ThunkData] responsible for storing the closure itself. It can either be:
/// - A standard thunk, that is destructively updated once and for all
/// - A reversible thunk, that can be restored to its original expression. Used to implement
/// recursive merging of records and overriding (see the [RFC
/// overriding](https://github.com/tweag/nickel/pull/330))
#[derive(Clone, Debug, PartialEq)]
pub enum InnerThunkData {
    Standard(Closure),
    Reversible {
        orig: Rc<Closure>,
        cached: Rc<Closure>,
    },
}

impl ThunkData {
    /// Create new standard thunk data.
    pub fn new(closure: Closure) -> Self {
        ThunkData {
            inner: InnerThunkData::Standard(closure),
            state: ThunkState::Suspended,
        }
    }

    /// Create new reversible thunk data.
    pub fn new_rev(orig: Closure) -> Self {
        let rc = Rc::new(orig);

        ThunkData {
            inner: InnerThunkData::Reversible {
                orig: rc.clone(),
                cached: rc,
            },
            state: ThunkState::Suspended,
        }
    }

    /// Return a reference to the closure currently cached.
    pub fn closure(&self) -> &Closure {
        match self.inner {
            InnerThunkData::Standard(ref closure) => closure,
            InnerThunkData::Reversible { ref cached, .. } => cached,
        }
    }

    /// Return a mutable reference to the closure currently cached.
    pub fn closure_mut(&mut self) -> &mut Closure {
        match self.inner {
            InnerThunkData::Standard(ref mut closure) => closure,
            InnerThunkData::Reversible { ref mut cached, .. } => Rc::make_mut(cached),
        }
    }

    /// Consume the data and return the cached closure.
    pub fn into_closure(self) -> Closure {
        match self.inner {
            InnerThunkData::Standard(closure) => closure,
            InnerThunkData::Reversible { orig, cached } => {
                std::mem::drop(orig);
                Rc::try_unwrap(cached).unwrap_or_else(|rc| (*rc).clone())
            }
        }
    }

    /// Update the cached closure.
    pub fn update(&mut self, new: Closure) {
        match self.inner {
            InnerThunkData::Standard(ref mut closure) => *closure = new,
            InnerThunkData::Reversible { ref mut cached, .. } => *cached = Rc::new(new),
        }

        self.state = ThunkState::Evaluated;
    }

    /// Create fresh unevaluated thunk data from `self`, restored to its original state before the
    /// first update. For standard thunk data, the content is unchanged and the state is conserved: in
    /// this case, `restore()` is the same as `clone()`.
    pub fn restore(&self) -> Self {
        match self.inner {
            InnerThunkData::Standard(ref closure) => ThunkData {
                inner: InnerThunkData::Standard(closure.clone()),
                state: self.state,
            },
            InnerThunkData::Reversible { ref orig, .. } => ThunkData {
                inner: InnerThunkData::Reversible {
                    orig: Rc::clone(orig),
                    cached: Rc::clone(orig),
                },
                state: self.state,
            },
        }
    }
}

/// A thunk.
///
/// A thunk is a shared suspended computation. It is the primary device for the implementation of
/// lazy evaluation.
///
/// For the implementation of recursive merging, some thunks need to be reversible, in the sense
/// that we must be able to restore the original expression before update. Those are called
/// reversible thunks. Most expressions don't need reversible thunks as their evaluation will
/// always give the same result, but some others, such as the ones containing recursive references
/// inside a record may be invalidated by merging, and thus need to store the unaltered original
/// expression. Those aspects are mainly handled in [InnerThunkData].
#[derive(Clone, Debug, PartialEq)]
pub struct Thunk {
    data: Rc<RefCell<ThunkData>>,
    ident_kind: IdentKind,
}

/// A black-holed thunk was accessed, which would lead to infinite recursion.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct BlackholedError;

impl Thunk {
    /// Create a new standard thunk.
    pub fn new(closure: Closure, ident_kind: IdentKind) -> Self {
        Thunk {
            data: Rc::new(RefCell::new(ThunkData::new(closure))),
            ident_kind,
        }
    }

    /// Create a new reversible thunk.
    pub fn new_rev(closure: Closure, ident_kind: IdentKind) -> Self {
        Thunk {
            data: Rc::new(RefCell::new(ThunkData::new_rev(closure))),
            ident_kind,
        }
    }

    pub fn state(&self) -> ThunkState {
        self.data.borrow().state
    }

    /// Set the state to evaluated.
    pub fn set_evaluated(&mut self) {
        self.data.borrow_mut().state = ThunkState::Evaluated;
    }

    /// Generate an update frame from this thunk and set the state to `Blackholed`. Return an
    /// error if the thunk was already black-holed.
    pub fn mk_update_frame(&mut self) -> Result<ThunkUpdateFrame, BlackholedError> {
        if self.data.borrow().state == ThunkState::Blackholed {
            return Err(BlackholedError);
        }

        self.data.borrow_mut().state = ThunkState::Blackholed;

        Ok(ThunkUpdateFrame {
            data: Rc::downgrade(&self.data),
            ident_kind: self.ident_kind,
        })
    }

    /// Immutably borrow the inner closure. Panic if there is another active mutable borrow.
    pub fn borrow(&self) -> Ref<'_, Closure> {
        Ref::map(self.data.borrow(), |data| data.closure())
    }

    /// Mutably borrow the inner closure. Panic if there is any other active borrow.
    pub fn borrow_mut(&mut self) -> RefMut<'_, Closure> {
        RefMut::map(self.data.borrow_mut(), |data| data.closure_mut())
    }

    /// Get an owned clone of the inner closure.
    pub fn get_owned(&self) -> Closure {
        self.data.borrow().closure().clone()
    }

    pub fn ident_kind(&self) -> IdentKind {
        self.ident_kind
    }

    /// Consume the thunk and return an owned closure. Avoid cloning if this thunk is the only
    /// reference to the inner closure.
    pub fn into_closure(self) -> Closure {
        match Rc::try_unwrap(self.data) {
            Ok(inner) => inner.into_inner().into_closure(),
            Err(rc) => rc.borrow().closure().clone(),
        }
    }

    /// Create a fresh unevaluated thunk from `self`, restored to its original state before the
    /// first update. For a standard thunk, the content is unchanged and the state is conserved: in
    /// this case, `restore()` is the same as `clone()`.
    pub fn restore(&self) -> Self {
        Thunk {
            data: Rc::new(RefCell::new(self.data.borrow().restore())),
            ident_kind: self.ident_kind,
        }
    }
}

/// A thunk update frame.
///
/// A thunk update frame is put on the stack whenever a variable is entered, such that once this
/// variable is evaluated, the corresponding thunk can be updated. It is similar to a thunk but it
/// holds a weak reference to the inner closure, to avoid unnecessarily keeping the underlying
/// closure alive.
#[derive(Clone, Debug)]
pub struct ThunkUpdateFrame {
    data: Weak<RefCell<ThunkData>>,
    ident_kind: IdentKind,
}

impl ThunkUpdateFrame {
    /// Update the corresponding thunk with a closure. Set the state to `Evaluated`
    ///
    /// # Return
    ///
    /// - `true` if the thunk was successfully updated
    /// - `false` if the corresponding closure has been dropped since
    pub fn update(self, closure: Closure) -> bool {
        if let Some(data) = Weak::upgrade(&self.data) {
            data.borrow_mut().update(closure);
            true
        } else {
            false
        }
    }
}

/// A call stack, saving the history of function calls.
///
/// In a lazy language like Nickel, there are no well delimited stack frames due to how function
/// application is evaluated. Additional information about the history of function calls is thus
/// stored in the call stack solely for better error reporting.
#[derive(PartialEq, Clone, Default, Debug)]
pub struct CallStack(pub Vec<StackElem>);

/// Basic description of a function call. Used for error reporting.
pub struct CallDescr {
    /// The name of the called function, if any.
    pub head: Option<Ident>,
    /// The position of the application.
    pub span: RawSpan,
}

impl CallStack {
    pub fn new() -> Self {
        CallStack(Vec::new())
    }

    /// Push a marker to indicate that a var was entered.
    pub fn enter_var(&mut self, kind: IdentKind, id: Ident, pos: TermPos) {
        // We ignore generated variables introduced by program transformations. They are not
        // relevant for error reporting.
        if !id.is_generated() {
            self.0.push(StackElem::Var(kind, id, pos));
        }
    }

    /// Push a marker to indicate that an application was entered.
    pub fn enter_app(&mut self, pos: TermPos) {
        // We ignore application without positions, which have been generated by the interpreter.
        if pos.is_def() {
            self.0.push(StackElem::App(pos));
        }
    }

    /// Push a marker to indicate that during the evaluation an application, the function part was
    /// finally evaluated to an expression of the form `fun x => body`, and that the body of this
    /// function was entered.
    pub fn enter_fun(&mut self, pos: TermPos) {
        // We ignore application without positions, which have been generated by the interpreter.
        if pos.is_def() {
            self.0.push(StackElem::Fun(pos));
        }
    }

    /// Push a marker to indicate that a record field was entered.
    pub fn enter_field(&mut self, record_pos: TermPos, field_id: Ident, pos: TermPos) {
        self.0.push(StackElem::Field(record_pos, field_id, pos));
    }

    /// Process a raw callstack by aggregating elements belonging to the same call. Return a list
    /// of call descriptions from the most nested/recent to the least nested/recent, together with
    /// the last pending call, if any.
    ///
    /// Recall that when a call `f arg` is evaluated, the following events happen:
    /// 1. `arg` is pushed on the evaluation stack.
    /// 2. `f` is evaluated.
    /// 3. Hopefully, the result of this evaluation is a function `Func(id, body)`. `arg` is popped
    ///    from the stack, bound to `id` in the environment, and `body is entered`.
    ///
    /// For error reporting purpose, we want to be able to determine the chain of nested calls leading
    /// to the current code path at any moment. To do so, the Nickel abstract machine maintains a
    /// callstack via this basic mechanism:
    /// 1. When an application is evaluated, push a marker with the position of the application on the callstack.
    /// 2. When a function body is entered, push a marker with the position of the original application on the
    ///    callstack.
    /// 3. When a variable is evaluated, push a marker with its name and position on the callstack.
    /// 4. When a record field is accessed, push a marker with its name and position on the
    ///    callstack too.
    ///
    /// Both field and variable are useful to determine the name of a called function, when there
    /// is one.  The resulting stack is not suited to be reported to the user for the following
    /// reasons:
    ///
    /// 1. One call spans several items on the callstack. First the application is entered (pushing
    ///    an `App`), then possibly variables or other application are evaluated until we
    ///    eventually reach a function for the left hand side. Then body of this function is
    ///    entered (pushing a `Fun`).
    /// 2. Because of currying, multi-ary applications span several objects on the callstack.
    ///    Typically, `(fun x y => x + y) arg1 arg2` spans two `App` and two `Fun` elements in the
    ///    form `App1 App2 Fun2 Fun1`, where the position span of `App1` includes the position span
    ///    of `App2`.  We want to group them as one call.
    /// 3. The callstack includes calls to builtin contracts. These calls are inserted implicitly
    ///    by the abstract machine and are not written explicitly by the user. Showing them is
    ///    confusing and clutters the call chain, so we get rid of them too.
    ///
    /// This is the role of `group_by_calls`, which filter out unwanted elements and groups
    /// callstack elements into atomic call elements represented by [`CallDescr`].
    ///
    /// The final call description list is reversed such that the most nested calls, which are
    /// usually the most relevant to understand the error, are printed first.
    ///
    /// # Arguments
    ///
    /// - `contract_id`: the `FileId` of the source containing standard contracts, to filter their
    ///   calls out.
    pub fn group_by_calls(
        self: &CallStack,
        contract_id: FileId,
    ) -> (Vec<CallDescr>, Option<CallDescr>) {
        // We filter out calls and accesses made from within the builtin contracts.
        let it = self.0.iter().filter(|elem| match elem {
            StackElem::Var(_, _, TermPos::Original(RawSpan { src_id, .. }))
            | StackElem::Var(_, _, TermPos::Inherited(RawSpan { src_id, .. }))
            | StackElem::Fun(TermPos::Original(RawSpan { src_id, .. }))
            | StackElem::Field(_, _, TermPos::Original(RawSpan { src_id, .. }))
            | StackElem::Field(_, _, TermPos::Inherited(RawSpan { src_id, .. }))
            | StackElem::App(TermPos::Original(RawSpan { src_id, .. }))
            // We avoid applications (Fun/App) with inherited positions. Such calls include
            // contracts applications which add confusing call items whose positions don't point to
            // an actual call in the source.
                if *src_id != contract_id =>
            {
                true
            }
            _ => false,
        });

        // We maintain a stack of active calls (whose head is being evaluated).  When encountering
        // an identifier (variable or record field), we see if it could serve as a function name
        // for the current active call. When a `Fun` is encountered, we check if this correspond to
        // the current active call, and if it does, the call description is moved to a stack of
        // processed calls.
        //
        // We also merge subcalls, in the sense that subcalls of larger calls are not considered
        // separately. `app1` is a subcall of `app2` if the position of `app1` is included in the
        // one of `app2` and the starting index is equal. We want `f a b c` to be reported as only
        // one big call to `f` rather than three nested calls `f a`, `f a b`, and `f a b c`.
        let mut pending: Vec<CallDescr> = Vec::new();
        let mut entered: Vec<CallDescr> = Vec::new();

        for elt in it {
            match elt {
                StackElem::Var(_, id, pos) | StackElem::Field(_, id, pos) => {
                    match pending.last_mut() {
                        Some(CallDescr {
                            head: ref mut head @ None,
                            span: span_call,
                        }) if pos.unwrap() <= *span_call => *head = Some(id.clone()),
                        _ => (),
                    };
                }
                StackElem::App(pos) => {
                    let span = pos.unwrap();
                    match pending.last() {
                        Some(CallDescr {
                            span: span_call, ..
                        }) if span <= *span_call && span.start == span_call.start => (),
                        _ => pending.push(CallDescr { head: None, span }),
                    }
                }
                StackElem::Fun(pos) => {
                    let span = pos.unwrap();
                    if pending
                        .last()
                        .map(|cdescr| cdescr.span == span)
                        .unwrap_or(false)
                    {
                        entered.push(pending.pop().unwrap());
                    }
                    // Otherwise, we are most probably entering a subcall () of the currently
                    // active call (e.g. in an multi-ary application `f g h`, a subcall would be `f
                    // g`). In any case, we do nothing.
                }
            }
        }

        entered.reverse();
        (entered, pending.pop())
    }

    /// Return the length of the callstack. Wrapper for `callstack.0.len()`.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Truncate the callstack at a certain size. Used e.g. to quickly drop the elements introduced
    /// during the strict evaluation of the operand of a primitive operator. Wrapper for
    /// `callstack.0.truncate(len)`.
    pub fn truncate(&mut self, len: usize) {
        self.0.truncate(len)
    }
}

impl From<CallStack> for Vec<StackElem> {
    fn from(cs: CallStack) -> Self {
        cs.0
    }
}

/// A call stack element.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StackElem {
    /// A function body was entered. The position is the position of the original application.
    Fun(TermPos),
    /// An application was evaluated.
    App(TermPos),
    /// A variable was entered.
    Var(IdentKind, Ident, TermPos),
    /// A record field was entered.
    Field(
        TermPos, /* the position of the record */
        Ident,   /* the field name */
        TermPos, /* the position of the field access */
    ),
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
    let RichTerm { term, pos } = rt;

    match *term {
        Term::Record(bindings, _) | Term::RecRecord(bindings, ..) => {
            let ext = bindings.into_iter().map(|(id, t)| {
                (
                    id,
                    Thunk::new(Closure::atomic_closure(t), IdentKind::Record),
                )
            });

            env.extend(ext);
            Ok(())
        }
        t => Err(EnvBuildError::NotARecord(RichTerm::new(t, pos))),
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

/// Determine if a thunk is worth being put on the stack for future update.
///
/// Typically, WHNFs and enriched values will not be evaluated to a simpler expression and are not
/// worth updating.
fn should_update(t: &Term) -> bool {
    !t.is_whnf() && !t.is_metavalue()
}

/// Evaluate a Nickel term. Wrapper around [eval_closure](fn.eval_closure.html) that starts from an
/// empty local environment and drops the final environment.
pub fn eval<R>(t0: RichTerm, global_env: &Environment, resolver: &mut R) -> Result<Term, EvalError>
where
    R: ImportResolver,
{
    eval_closure(Closure::atomic_closure(t0), global_env, resolver, true).map(|(term, _)| term)
}

/// Fully evaluate a Nickel term: the result is not a WHNF but to a value with all variables substituted.
pub fn eval_full<R>(
    t0: RichTerm,
    global_env: &Environment,
    resolver: &mut R,
) -> Result<Term, EvalError>
where
    R: ImportResolver,
{
    use crate::transformations::fresh_var;

    let var = fresh_var();
    // Desugar to let x = term in deepSeq x x
    let wrapper = mk_term::let_in(
        var.clone(),
        t0,
        mk_app!(
            mk_term::op1(UnaryOp::DeepSeq(), Term::Var(var.clone())),
            Term::Var(var)
        ),
    );
    eval_closure(Closure::atomic_closure(wrapper), global_env, resolver, true)
        .map(|(term, env)| subst(term.into(), global_env, &env).into())
}

/// Evaluate a Nickel Term, stopping when a meta value is encountered at the top-level without
/// unwrapping it. Then evaluate the underlying value, and substitute variables in order to obtain
/// a WHNF that is printable.
///
/// Used to query the metadata of a value.
pub fn eval_meta<R>(
    t: RichTerm,
    global_env: &Environment,
    resolver: &mut R,
) -> Result<Term, EvalError>
where
    R: ImportResolver,
{
    let (term, env) = eval_closure(Closure::atomic_closure(t), global_env, resolver, false)?;

    match term {
        Term::MetaValue(mut meta) => {
            if let Some(t) = meta.value.take() {
                let pos = t.pos;
                let (evaluated, env) =
                    eval_closure(Closure { body: t, env }, global_env, resolver, true)?;
                let substituted = subst(RichTerm::new(evaluated, pos), global_env, &env);

                meta.value.replace(substituted);
            }

            Ok(Term::MetaValue(meta))
        }
        term => Ok(term),
    }
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
/// - `enriched_strict`: if evaluation is strict with respect to enriched values (metavalues).
///   Standard evaluation should be strict, but set to false when extracting the metadata of value.
///
/// # Return
///
/// Either:
///  - an evaluation error
///  - the evaluated term with its final environment
pub fn eval_closure<R>(
    mut clos: Closure,
    global_env: &Environment,
    resolver: &mut R,
    mut enriched_strict: bool,
) -> Result<(Term, Environment), EvalError>
where
    R: ImportResolver,
{
    let mut call_stack = CallStack::new();
    let mut stack = Stack::new();

    loop {
        let Closure {
            body: RichTerm {
                term: boxed_term,
                pos,
            },
            mut env,
        } = clos;
        let term = *boxed_term;

        if let Some(strict) = stack.pop_strictness_marker() {
            enriched_strict = strict;
        }

        clos = match term {
            Term::Var(x) => {
                let mut thunk = env
                    .get(&x)
                    .or_else(|| global_env.get(&x))
                    .ok_or_else(|| EvalError::UnboundIdentifier(x.clone(), pos))?;
                std::mem::drop(env); // thunk may be a 1RC pointer

                if thunk.state() != ThunkState::Evaluated {
                    if should_update(&thunk.borrow().body.term) {
                        match thunk.mk_update_frame() {
                            Ok(thunk_upd) => stack.push_thunk(thunk_upd),
                            Err(BlackholedError) => {
                                return Err(EvalError::InfiniteRecursion(call_stack, pos))
                            }
                        }
                    }
                    // If the thunk isn't to be updated, directly set the evaluated flag.
                    else {
                        thunk.set_evaluated();
                    }
                }

                call_stack.enter_var(thunk.ident_kind(), x, pos);
                thunk.into_closure()
            }
            Term::App(t1, t2) => {
                call_stack.enter_app(pos);

                if !enriched_strict {
                    stack.push_strictness(enriched_strict);
                }
                enriched_strict = true;
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
                let closure = Closure {
                    body: s,
                    env: env.clone(),
                };
                env.insert(x, Thunk::new(closure, IdentKind::Let));
                Closure { body: t, env }
            }
            Term::Switch(exp, cases, default) => {
                let has_default = default.is_some();

                if let Some(t) = default {
                    stack.push_arg(
                        Closure {
                            body: t,
                            env: env.clone(),
                        },
                        pos,
                    );
                }

                stack.push_arg(
                    Closure {
                        body: RichTerm::new(Term::Record(cases, Default::default()), pos),
                        env: env.clone(),
                    },
                    pos,
                );

                Closure {
                    body: RichTerm::new(Term::Op1(UnaryOp::Switch(has_default), exp), pos),
                    env,
                }
            }
            Term::Op1(op, t) => {
                if !enriched_strict {
                    stack.push_strictness(enriched_strict);
                }
                enriched_strict = true;
                stack.push_op_cont(OperationCont::Op1(op, t.pos), call_stack.len(), pos);
                Closure { body: t, env }
            }
            Term::Op2(op, fst, snd) => {
                let strict_op = op.is_strict();
                if enriched_strict != strict_op {
                    stack.push_strictness(enriched_strict);
                }
                enriched_strict = strict_op;
                stack.push_op_cont(
                    OperationCont::Op2First(
                        op,
                        Closure {
                            body: snd,
                            env: env.clone(),
                        },
                        fst.pos,
                    ),
                    call_stack.len(),
                    pos,
                );
                Closure { body: fst, env }
            }
            Term::OpN(op, mut args) => {
                let strict_op = op.is_strict();
                if enriched_strict != strict_op {
                    stack.push_strictness(enriched_strict);
                }
                enriched_strict = strict_op;

                // Arguments are passed as a stack to the operation continuation, so we reverse the
                // original list.
                args.reverse();
                let fst = args
                    .pop()
                    .ok_or_else(|| EvalError::NotEnoughArgs(op.arity(), op.to_string(), pos))?;

                let pending: Vec<Closure> = args
                    .into_iter()
                    .map(|t| Closure {
                        body: t,
                        env: env.clone(),
                    })
                    .collect();

                stack.push_op_cont(
                    OperationCont::OpN {
                        op,
                        evaluated: Vec::with_capacity(pending.len() + 1),
                        pending,
                        current_pos: fst.pos,
                    },
                    call_stack.len(),
                    pos,
                );

                Closure { body: fst, env }
            }
            Term::StrChunks(mut chunks) => match chunks.pop() {
                None => Closure {
                    body: Term::Str(String::new()).into(),
                    env: Environment::new(),
                },
                Some(chunk) => {
                    let (arg, indent) = match chunk {
                        StrChunk::Literal(s) => (Term::Str(s).into(), 0),
                        StrChunk::Expr(e, indent) => (e, indent),
                    };

                    if !enriched_strict {
                        stack.push_strictness(enriched_strict);
                    }
                    enriched_strict = true;
                    stack.push_str_chunks(chunks.into_iter());
                    stack.push_str_acc(String::new(), indent, env.clone());

                    Closure {
                        body: RichTerm::new(Term::Op1(UnaryOp::ChunksConcat(), arg), pos),
                        env,
                    }
                }
            },
            Term::RecRecord(ts, dyn_fields, attrs) => {
                // Thanks to the share normal form transformation, the content is either a constant or a
                // variable.
                let rec_env = ts.iter().try_fold::<_, _, Result<Environment, EvalError>>(
                    Environment::new(),
                    |mut rec_env, (id, rt)| match rt.as_ref() {
                        Term::Var(ref var_id) => {
                            let thunk = env.get(var_id).ok_or_else(|| {
                                EvalError::UnboundIdentifier(var_id.clone(), rt.pos)
                            })?;
                            rec_env.insert(id.clone(), thunk);
                            Ok(rec_env)
                        }
                        _ => {
                            // If we are in this branch, the term must be a constant after the
                            // share normal form transformation, hence it should not need an
                            // environment, which is why it is dropped.
                            let closure = Closure {
                                body: rt.clone(),
                                env: Environment::new(),
                            };
                            rec_env.insert(id.clone(), Thunk::new(closure, IdentKind::Let));
                            Ok(rec_env)
                        }
                    },
                )?;

                let new_ts = ts.into_iter().map(|(id, rt)| {
                    let RichTerm { term, pos } = rt;
                    match *term {
                        Term::Var(var_id) => {
                            // We already checked for unbound identifier in the previous fold,
                            // so function should always succeed
                            let mut thunk = env.get(&var_id).unwrap();
                            thunk.borrow_mut().env.extend(
                                rec_env
                                    .iter_elems()
                                    .map(|(id, thunk)| (id.clone(), thunk.clone())),
                            );
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

                let static_part = RichTerm::new(Term::Record(new_ts.collect(), attrs), pos);

                // Transform the static part `{stat1 = val1, ..., statn = valn}` and the dynamic
                // part `{exp1 = dyn_val1, ..., expm = dyn_valm}` to a sequence of extensions
                // `{stat1 = val1, ..., statn = valn} $[ exp1 = dyn_val1] ... $[ expn = dyn_valn ]`
                // The `dyn_val` are given access to the recursive environment, but not the dynamic
                // field names.
                let extended = dyn_fields
                    .into_iter()
                    .try_fold::<_, _, Result<RichTerm, EvalError>>(
                        static_part,
                        |acc, (id_t, t)| {
                            let RichTerm { term, pos } = t;
                            match *term {
                                Term::Var(var_id) => {
                                    let mut thunk = env.get(&var_id).ok_or_else(|| {
                                        EvalError::UnboundIdentifier(var_id.clone(), pos)
                                    })?;

                                    thunk.borrow_mut().env.extend(
                                        rec_env
                                            .iter_elems()
                                            .map(|(id, thunk)| (id.clone(), thunk.clone())),
                                    );
                                    Ok(Term::App(
                                        mk_term::op2(BinaryOp::DynExtend(), id_t, acc),
                                        mk_term::var(var_id).with_pos(pos),
                                    )
                                    .into())
                                }
                                _ => Ok(Term::App(
                                    mk_term::op2(BinaryOp::DynExtend(), id_t, acc),
                                    RichTerm { term, pos },
                                )
                                .into()),
                            }
                        },
                    )?;

                Closure {
                    body: extended.with_pos(pos),
                    env,
                }
            }
            // Unwrapping of enriched terms
            Term::MetaValue(meta) if enriched_strict => {
                if meta.value.is_some() {
                    /* Since we are forcing a metavalue, we are morally evaluating `force t` rather
                     * than `t` iteself.  Updating a thunk after having performed this forcing may
                     * alter the semantics of the program in an unexpected way (see issue
                     * https://github.com/tweag/nickel/issues/123): we update potential thunks now
                     * so that their content remains a meta value.
                     */
                    let update_closure = Closure {
                        body: RichTerm {
                            term: Box::new(Term::MetaValue(meta)),
                            pos,
                        },
                        env,
                    };
                    update_thunks(&mut stack, &update_closure);

                    let Closure {
                        body: RichTerm { term, .. },
                        env,
                    } = update_closure;

                    match *term {
                        Term::MetaValue(MetaValue {
                            value: Some(inner), ..
                        }) => Closure { body: inner, env },
                        _ => unreachable!(),
                    }
                }
                // TODO: improve error message using some positions
                else {
                    return Err(EvalError::Other(String::from("empty metavalue"), pos));
                }
            }
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
                    format!("Unresolved import ({})", path.to_string_lossy()),
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
                    continuate_operation(clos, &mut stack, &mut call_stack)?
                }
            }
            // Function call
            Term::Fun(x, t) => {
                if let Some((thunk, pos_app)) = stack.pop_arg_as_thunk() {
                    call_stack.enter_fun(pos_app);
                    env.insert(x, thunk);
                    Closure { body: t, env }
                } else {
                    return Ok((Term::Fun(x, t), env));
                }
            }
            // Otherwise, this is either an ill-formed application, or we are done
            t => {
                if let Some((arg, pos_app)) = stack.pop_arg() {
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
        thunk.update(closure.clone());
    }
}

/// Recursively substitute each variable occurrence of a term for its value in the environment.
pub fn subst(rt: RichTerm, global_env: &Environment, env: &Environment) -> RichTerm {
    use std::borrow::Cow;
    use std::collections::HashSet;

    // Maintain an additional set of variables bound by abstractions (`fun x => ..`), that must not
    // be substituted.
    fn subst_(
        rt: RichTerm,
        global_env: &Environment,
        env: &Environment,
        bound: Cow<HashSet<Ident>>,
    ) -> RichTerm {
        let RichTerm { term, pos } = rt;
        match *term {
            Term::Var(id) if !bound.as_ref().contains(&id) => env
                .get(&id)
                .or_else(|| global_env.get(&id))
                .map(|thunk| {
                    let closure = thunk.get_owned();
                    subst_(closure.body, global_env, &closure.env, bound)
                })
                .unwrap_or_else(|| RichTerm::new(Term::Var(id), pos)),
            v @ Term::Null
            | v @ Term::ParseError
            | v @ Term::Bool(_)
            | v @ Term::Num(_)
            | v @ Term::Str(_)
            // Do not substitute under lambdas: mutually recursive function could cause an infinite
            // loop. Although avoidable, this requires some care and is not currently needed.
            | v @ Term::Fun(..)
            | v @ Term::Lbl(_)
            | v @ Term::Sym(_)
            | v @ Term::Var(_)
            | v @ Term::Enum(_)
            | v @ Term::Import(_)
            | v @ Term::ResolvedImport(_) => RichTerm::new(v, pos),
            Term::Let(id, t1, t2) => {
                let t1 = subst_(t1, global_env, env, Cow::Borrowed(bound.as_ref()));
                let t2 = subst_(t2, global_env, env, bound);

                RichTerm::new(Term::Let(id, t1, t2), pos)
            }
            p @ Term::LetPattern(..) => panic!("Pattern {:?} has not been transformed before evaluation", p),
            Term::App(t1, t2) => {
                let t1 = subst_(t1, global_env, env, Cow::Borrowed(bound.as_ref()));
                let t2 = subst_(t2, global_env, env, bound);

                RichTerm::new(Term::App(t1, t2), pos)
            }
            Term::Switch(t, cases, default) => {
                let default =
                    default.map(|d| subst_(d, global_env, env, Cow::Borrowed(bound.as_ref())));
                let cases = cases
                    .into_iter()
                    .map(|(id, t)| {
                        (
                            id,
                            subst_(t, global_env, env, Cow::Borrowed(bound.as_ref())),
                        )
                    })
                    .collect();
                let t = subst_(t, global_env, env, bound);

                RichTerm::new(Term::Switch(t, cases, default), pos)
            }
            Term::Op1(op, t) => {
                let t = subst_(t, global_env, env, bound);

                RichTerm::new(Term::Op1(op, t), pos)
            }
            Term::Op2(op, t1, t2) => {
                let t1 = subst_(t1, global_env, env, Cow::Borrowed(bound.as_ref()));
                let t2 = subst_(t2, global_env, env, bound);

                RichTerm::new(Term::Op2(op, t1, t2), pos)
            }
            Term::OpN(op, ts) => {
                let ts = ts
                    .into_iter()
                    .map(|t| subst_(t, global_env, env, Cow::Borrowed(bound.as_ref())))
                    .collect();

                RichTerm::new(Term::OpN(op, ts), pos)
            }
            Term::Wrapped(i, t) => {
                let t = subst_(t, global_env, env, bound);

                RichTerm::new(Term::Wrapped(i, t), pos)
            }
            Term::Record(map, attrs) => {
                let map = map
                    .into_iter()
                    .map(|(id, t)| {
                        (
                            id,
                            subst_(t, global_env, env, Cow::Borrowed(bound.as_ref())),
                        )
                    })
                    .collect();

                RichTerm::new(Term::Record(map, attrs), pos)
            }
            Term::RecRecord(map, dyn_fields, attrs) => {
                let map = map
                    .into_iter()
                    .map(|(id, t)| {
                        (
                            id,
                            subst_(t, global_env, env, Cow::Borrowed(bound.as_ref())),
                        )
                    })
                    .collect();

                let dyn_fields = dyn_fields
                    .into_iter()
                    .map(|(id_t, t)| {
                        (
                            subst_(id_t, global_env, env, Cow::Borrowed(bound.as_ref())),
                            subst_(t, global_env, env, Cow::Borrowed(bound.as_ref())),
                        )
                    })
                    .collect();

                RichTerm::new(Term::RecRecord(map, dyn_fields, attrs), pos)
            }
            Term::List(ts) => {
                let ts = ts
                    .into_iter()
                    .map(|t| subst_(t, global_env, env, Cow::Borrowed(bound.as_ref())))
                    .collect();

                RichTerm::new(Term::List(ts), pos)
            }
            Term::StrChunks(chunks) => {
                let chunks = chunks
                    .into_iter()
                    .map(|chunk| match chunk {
                        chunk @ StrChunk::Literal(_) => chunk,
                        StrChunk::Expr(t, indent) => StrChunk::Expr(
                            subst_(t, global_env, env, Cow::Borrowed(bound.as_ref())),
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
                //             Types(AbsType::Flat(t)) => Types(AbsType::Flat(subst_(
                //                 t,
                //                 global_env,
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
                //         Types(AbsType::Flat(t)) => Types(AbsType::Flat(subst_(
                //             t,
                //             global_env,
                //             env,
                //             Cow::Borrowed(bound.as_ref()),
                //         ))),
                //         ty => ty,
                //     };
                //
                //     Contract { types, ..ctr }
                // });

                let value = meta.value.map(|t| subst_(t, global_env, env, bound));

                let meta = MetaValue {
                    doc: meta.doc,
                    value,
                    ..meta
                };

                RichTerm::new(Term::MetaValue(meta), pos)
            }
        }
    }

    subst_(rt, global_env, env, Cow::Owned(HashSet::new()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cache::resolvers::{DummyResolver, SimpleResolver};
    use crate::error::ImportError;
    use crate::label::Label;
    use crate::parser::{grammar, lexer};
    use crate::term::make as mk_term;
    use crate::term::{BinaryOp, StrChunk, UnaryOp};
    use crate::transformations::resolve_imports;
    use crate::{mk_app, mk_fun};
    use codespan::Files;

    /// Evaluate a term without import support.
    fn eval_no_import(t: RichTerm) -> Result<Term, EvalError> {
        eval(t, &Environment::new(), &mut DummyResolver {})
    }

    fn parse(s: &str) -> Option<RichTerm> {
        let id = Files::new().add("<test>", String::from(s));

        grammar::TermParser::new()
            .parse_term(id, lexer::Lexer::new(&s))
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
        if let Err(EvalError::BlameError(l, ..)) =
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

    fn mk_default(t: RichTerm) -> Term {
        use crate::term::MergePriority;

        let mut meta = MetaValue::from(t);
        meta.priority = MergePriority::Default;
        Term::MetaValue(meta)
    }

    fn mk_docstring<S>(t: RichTerm, s: S) -> Term
    where
        S: Into<String>,
    {
        let mut meta = MetaValue::from(t);
        meta.doc.replace(s.into());
        Term::MetaValue(meta)
    }

    #[test]
    fn enriched_terms_unwrapping() {
        let t = mk_default(mk_default(mk_docstring(Term::Bool(false).into(), "a").into()).into())
            .into();
        assert_eq!(Ok(Term::Bool(false)), eval_no_import(t));
    }

    #[test]
    fn merge_enriched_default() {
        let t = mk_term::op2(
            BinaryOp::Merge(),
            Term::Num(1.0),
            mk_default(Term::Num(2.0).into()),
        );
        assert_eq!(Ok(Term::Num(1.0)), eval_no_import(t));
    }

    #[test]
    fn merge_incompatible_defaults() {
        let t = mk_term::op2(
            BinaryOp::Merge(),
            mk_default(Term::Num(1.0).into()),
            mk_default(Term::Num(2.0).into()),
        );

        eval_no_import(t).unwrap_err();
    }

    #[test]
    fn imports() {
        let mut resolver = SimpleResolver::new();
        resolver.add_source(String::from("two"), String::from("1 + 1"));
        resolver.add_source(String::from("lib"), String::from("{f = true}"));
        resolver.add_source(String::from("bad"), String::from("^$*/.23ab 0°@"));
        resolver.add_source(
            String::from("nested"),
            String::from("let x = import \"two\" in x + 1"),
        );
        resolver.add_source(
            String::from("cycle"),
            String::from("let x = import \"cycle_b\" in {a = 1, b = x.a}"),
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
            resolve_imports(
                mk_term::let_in(var, mk_term::import(import), body),
                resolver,
            )
            .map(|(t, _)| t)
        }

        // let x = import "does_not_exist" in x
        match mk_import("x", "does_not_exist", mk_term::var("x"), &mut resolver).unwrap_err() {
            ImportError::IOError(_, _, _) => (),
            _ => assert!(false),
        };

        // let x = import "bad" in x
        match mk_import("x", "bad", mk_term::var("x"), &mut resolver).unwrap_err() {
            ImportError::ParseErrors(_, _) => (),
            _ => assert!(false),
        };

        // let x = import "two" in x
        assert_eq!(
            eval(
                mk_import("x", "two", mk_term::var("x"), &mut resolver).unwrap(),
                &Environment::new(),
                &mut resolver
            )
            .unwrap(),
            Term::Num(2.0)
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
                &Environment::new(),
                &mut resolver
            )
            .unwrap(),
            Term::Bool(true)
        );
    }

    #[test]
    fn interpolation_simple() {
        let mut chunks = vec![
            StrChunk::Literal(String::from("Hello")),
            StrChunk::expr(
                mk_term::op2(
                    BinaryOp::StrConcat(),
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
                    BinaryOp::StrConcat(),
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
        let mut global_env = Environment::new();
        let mut resolver = DummyResolver {};
        global_env.insert(
            Ident::from("g"),
            Thunk::new(
                Closure::atomic_closure(Term::Num(1.0).into()),
                IdentKind::Let,
            ),
        );

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
                    Thunk::new(Closure::atomic_closure(t), IdentKind::Let),
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
            subst(t, &global_env, &env),
            parse("let x = 1 in if true then 1 + (if false then 1 else \"Glob2\") else false")
                .unwrap()
        );

        let t =
            parse("switch {`x => [1, glob1], `y => loc2, `z => {id = true, other = glob3}} loc1")
                .unwrap();
        assert_eq!(
            subst(t, &global_env, &env),
            parse("switch {`x => [1, 1], `y => (if false then 1 else \"Glob2\"), `z => {id = true, other = false}} true").unwrap()
        );
    }
}
