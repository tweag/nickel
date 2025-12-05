//! Thunks and associated devices used to implement lazy evaluation.
use super::{BlackholedError, Cache, CacheIndex, Closure};
use crate::{
    eval::value::{self, NickelValue},
    identifier::Ident,
    metrics::increment,
    position::PosIdx,
    term::{BindingType, Term, record::FieldDeps},
};
use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;

/// The state of a thunk.
///
/// When created, a thunk is flagged as suspended. When accessed for the first time, a
/// corresponding [ThunkUpdateFrame] is pushed on the stack and the thunk is flagged as
/// black-hole.
///
/// This prevents direct infinite recursions, since if a thunk is re-accessed while still in a
/// black-hole state, we are sure that the evaluation will loop, and we can thus error out before
/// overflowing the stack or looping forever. Finally, once the content of a thunk has been
/// evaluated, the thunk is updated with the new value and flagged as evaluated, so that future
/// accesses won't even push an update frame on the stack.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub enum ThunkState {
    Blackholed,
    #[default]
    Suspended,
    Evaluated,
}

/// The mutable data stored inside a thunk.
#[derive(Clone, Debug, PartialEq)]
pub struct ThunkData {
    inner: InnerThunkData,
    state: ThunkState,
    /// A flag indicating whether the thunk is locked. See [Thunk::lock].
    locked: bool,
}

/// The part of [ThunkData] responsible for storing the closure itself. It can either be:
///
/// - A standard thunk, that is destructively updated once and for all
/// - A revertible thunk, that can be restored to its original expression. Used to implement
///   recursive merging of records and overriding (see the
///   [RFC overriding](https://github.com/tweag/nickel/pull/330)). A revertible thunk optionally
///   stores the set of recursive fields it depends on (see [`crate::transform::free_vars`]).
///
/// # Revertible thunks
///
/// Recursive records are better understood as being functions. For example, the recursive record:
///
/// ```nickel
/// {
///   foo = bar + baz + 1,
///   bar = 2,
///   baz = 1,
/// }
/// ```
///
/// is really a builtin representation of `fun self => {foo = self.bar + self.baz + 1, bar = 2, baz
/// = 1}`. The interpreter computes a fixpoint when evaluating a `RecRecord` away to a `Record`,
/// equivalent to `let rec fixpoint = repr fixpoint in fixpoint`. We use a different vocabulary and
/// representation, but this is what we essentially do. We're just hiding the details from the
/// user and pretend recursive records are data that can be used as a normal record because it's
/// both simple and natural for the user.
///
/// A naive representation of recursive records could be to store them as actual functions, and
/// re-compute the fixpoint at each record operation, such as a field access. While correct, this
/// would be wasteful, because the result of each fixpoint computation is actually the same
/// throughout most record operations (put differently, `self` doesn't change often). But we can't
/// just compute the fixpoint once, store it in a normal thunk and forget about the original
/// function, because upon merge, and more specifically upon recursive overriding, we do need to
/// compute a new fixpoint from the original function.
///
/// Revertible thunks are a **memoization device** for the functional representation `fun self =>
/// ...` of a recursive record. First, note that there are several different but equivalent ways of
/// encoding the example above as a function:
///
/// ```nickel
/// fun self => { foo = self.bar + self.baz + 1, bar = 2, baz = 1 }
///
/// # pushing the function down the fields
/// {foo = fun self => self.bar + self.baz + 1, bar = fun self => 2, baz = fun self => 1}
///
/// # removing unused arguments
/// {foo = fun self => self.bar + self.baz + 1, bar = 2, baz = 1}
///
/// # splitting the self argument
/// {foo = fun bar baz => bar + baz + 1, bar = 2, bar 1}
/// ```
///
/// All those representations are isomorphic, in that, modulo basic free variable analysis, we can
/// get from one to another mechanically. Pushing the functions down the fields is better because
/// it reveals more static information about the record (such as the list of fields) without having
/// to provide any argument first. Then, splitting `self` and removing the function altogether when
/// the field doesn't depend on other fields is more precise with respect to dependency tracking
/// (this has an important positive memory impact, see [crate::term::record::FieldDeps]).
///
/// In practice, Nickel uses the last representation. Fields that don't have any free variable
/// intersecting with the name of the other fields are allocated as normal thunks that will only be
/// evaluated at most once. Fields with actual dependencies are stored in revertible thunks, where
/// the `orig` field stores the body of the function (in our example the expression `bar + baz +
/// 1`), and the cached version is the application to `self` computed when we evaluated the
/// recursive record to a normal record. The function arguments (that is, the intersection of the
/// free variables of the expression with the set of fields) are stored inside `deps`. `deps`
/// represent the other fields a specific field is depending on, and those names are guaranteed to
/// appear free inside `orig`.
///
/// In fact, unless there is an unbound identifier, `deps` should be exactly the set of free
/// variables of `orig`.
#[derive(Clone, Debug, PartialEq)]
pub enum InnerThunkData {
    Standard(Closure),
    Revertible {
        orig: Rc<Closure>,
        cached: Option<Closure>,
        deps: FieldDeps,
    },
}

const REVTHUNK_NO_CACHED_VALUE_MSG: &str =
    "tried to get data from a revertible thunk without a cached value";

impl ThunkData {
    /// Create new standard thunk data.
    pub fn new(closure: Closure) -> Self {
        ThunkData {
            inner: InnerThunkData::Standard(closure),
            state: ThunkState::Suspended,
            locked: false,
        }
    }

    /// Create new revertible thunk data.
    pub fn new_rev(orig: Closure, deps: FieldDeps) -> Self {
        increment!("Thunk::new_rev");
        ThunkData {
            inner: InnerThunkData::Revertible {
                orig: Rc::new(orig),
                cached: None,
                deps,
            },
            state: ThunkState::Suspended,
            locked: false,
        }
    }

    /// Initialize the cached value of a revertible thunk, given the recursive environment of the
    /// corresponding record. This function is a no-op on a standard thunk.
    ///
    /// # Invariant
    ///
    /// **This function must be called exactly once** on a revertible thunk, after the initial
    /// construction. It's part of its initialization. Calling it on a revertible thunks a second
    /// time, with a `cached` value which is not set to `None`, will panic.
    ///
    /// Non-revertible thunks are not concerned: this function has no effect on them, even if
    /// called repeatedly.
    ///
    /// This function is similar in spirit to setting the cached value to be the explicit function
    /// application given as built by `saturate`, but applied to arguments taken from `rec_env`.
    /// The major difference is that `init_cached` avoids the creation of the intermediate redex
    /// `(fun id1 .. id n => orig) <closure@1> .. <closure@n>` as well as the intermediate thunks
    /// and terms, because we can compute the application right away, in-place.
    pub fn init_cached(&mut self, rec_env: &[(Ident, Thunk)]) {
        match self.inner {
            InnerThunkData::Standard(_) => (),
            InnerThunkData::Revertible {
                ref mut cached,
                ref orig,
                ref deps,
            } => {
                // `build_cached_value` must be called exactly once on a revertible thunk. This is
                // an invariant that MUST be maintained by the interpreter.
                //
                // `cached` set to `None` solely exists because we need to first allocate all the
                // revertible thunks corresponding to a recursive record, and only then can we
                // patch them (build the cached value) in a second step, but they should be
                // logically seen as one construction operation.
                assert!(
                    cached.is_none(),
                    "tried to build the cached value of a revertible thunk, but was already set"
                );

                let mut new_cached = Closure::clone(orig);

                match deps {
                    FieldDeps::Known(deps) if deps.is_empty() => (),
                    FieldDeps::Known(deps) => new_cached
                        .env
                        .extend(rec_env.iter().filter(|(id, _)| deps.contains(id)).cloned()),
                    FieldDeps::Unknown => new_cached.env.extend(rec_env.iter().cloned()),
                };

                *cached = Some(new_cached);
            }
        }
    }

    /// Revert a thunk and abstract over the provided arguments to get back a function. The result
    /// is returned in a new, non-revertible, thunk.
    ///
    /// Used by [Thunk::saturate]. See the corresponding documentation for more details.
    ///
    /// # Example
    ///
    /// If `orig` is `foo + bar + a` and `args` correspond to `bar, foo`, this functions returns a
    /// standard thunk containing `fun bar foo => foo + bar + a`.
    fn revthunk_as_explicit_fun<I>(self, args: I) -> Self
    where
        I: DoubleEndedIterator<Item = Ident>,
    {
        match self.inner {
            InnerThunkData::Standard(_) => self,
            InnerThunkData::Revertible { orig, .. } => {
                let Closure { value: body, env } =
                    Rc::try_unwrap(orig).unwrap_or_else(|rc| Closure::clone(&rc));

                // Build a list of the arguments that the function will need in the same order as
                // the original iterator. If the identifiers inside `args` are `a`, `b` and `c`, in
                // that order, we want to build `fun a => (fun b => (fun c => body))`. We thus need
                // a reverse fold.
                let as_function = args.rfold(body, |built, id| {
                    NickelValue::from(Term::fun(id.into(), built))
                });

                ThunkData::new(Closure {
                    value: as_function,
                    env,
                })
            }
        }
    }

    /// Return a reference to the closure currently cached.
    pub fn closure(&self) -> &Closure {
        match self.inner {
            InnerThunkData::Standard(ref closure) => closure,
            // Nothing should peek into a revertible thunk before the cached value has been
            // constructed by [`build_cached_value`]. This is an invariant that MUST be maintained
            // by the interpreter.
            //
            // `cached` set to `None` solely exists because we need to first allocate all the
            // revertible thunks corresponding to a recursive record, and only then can we patch
            // them (build the cached value) in a second step. But calling to [`ThunkData::new_rev`]
            // followed by [`ThunkData::build_cached_value`] should be logically seen as just one
            // construction operation.
            InnerThunkData::Revertible { ref cached, .. } => {
                cached.as_ref().expect(REVTHUNK_NO_CACHED_VALUE_MSG)
            }
        }
    }

    /// Return a reference to the closure currently cached. Return the original expression if this
    /// thunk is a revertible thunk that hasn't been built yet.
    ///
    /// This function is only used for contract equality checking on revertible thunks. In every
    /// other cases, you should use [Self::closure] instead.
    pub(crate) fn closure_or_orig(&self) -> &Closure {
        match self.inner {
            InnerThunkData::Standard(ref closure) => closure,
            InnerThunkData::Revertible {
                ref cached,
                ref orig,
                ..
            } => cached.as_ref().unwrap_or(orig),
        }
    }

    /// Return a mutable reference to the closure currently cached.
    pub fn closure_mut(&mut self) -> &mut Closure {
        match self.inner {
            InnerThunkData::Standard(ref mut closure) => closure,
            InnerThunkData::Revertible {
                ref mut cached,
                ref mut orig,
                ..
            } => cached.as_mut().unwrap_or_else(|| Rc::make_mut(orig)),
        }
    }

    /// Consume the data and return the cached closure.
    pub fn into_closure(self) -> Closure {
        match self.inner {
            InnerThunkData::Standard(closure) => closure,
            // Nothing should access the cached value of a revertible thunk before the cached value
            // has been constructed. This is an invariant that MUST be maintained by the interpreter
            //
            // `cached` set to `None` solely exists because we need to first allocate all the
            // revertible thunks corresponding to a recursive record, and only then can we patch
            // them (build the cached value) in a second step. But calling to [`ThunkData::new_rev`]
            // followed by [`ThunkData::build_cached_value`] should be logically seen as just one
            // construction operation.
            InnerThunkData::Revertible { cached, .. } => {
                cached.expect(REVTHUNK_NO_CACHED_VALUE_MSG)
            }
        }
    }

    /// Update the cached closure.
    pub fn update(&mut self, new: Closure) {
        match self.inner {
            InnerThunkData::Standard(ref mut closure) => *closure = new,
            InnerThunkData::Revertible { ref mut cached, .. } => *cached = Some(new),
        }

        self.state = ThunkState::Evaluated;
    }

    /// Create a freshly unevaluated thunk (minus `ident_type`) from a thunk, reverted to its
    /// original state before the first update. We need to operate at the [Thunk] level here
    /// (instead of [Self]) to avoid allocation in the non revertible case. Indeed, for standard
    /// thunk data, the content is unchanged and shared with the original thunk: in that case,
    /// `ThunkData::revert(thunk)` is the same as `thunk.clone()`.
    ///
    /// For revertible thunk data, the result is independent from the original one. Any update to
    /// one of the thunks doesn't affect the other.
    ///
    /// The new thunk is unlocked, regardless of the locking status of the original thunk.
    pub fn revert(thunk: &Thunk) -> Thunk {
        match thunk.data().borrow().inner {
            InnerThunkData::Standard(_) => thunk.clone(),
            InnerThunkData::Revertible {
                ref orig, ref deps, ..
            } => Thunk(NickelValue::thunk(
                ThunkData {
                    inner: InnerThunkData::Revertible {
                        orig: Rc::clone(orig),
                        cached: None,
                        deps: deps.clone(),
                    },
                    state: ThunkState::Suspended,
                    locked: false,
                },
                thunk.0.pos_idx(),
            )),
        }
    }

    /// Map a function over the content of the thunk to create a new independent thunk. If the
    /// thunk is revertible, the mapping function is applied on both the original expression and
    /// the cached expression.
    ///
    /// The new thunk is unlocked, whatever the locking status of the original thunk.
    pub fn map<F>(&self, mut f: F) -> Self
    where
        F: FnMut(&Closure) -> Closure,
    {
        match self.inner {
            InnerThunkData::Standard(ref c) => ThunkData {
                inner: InnerThunkData::Standard(f(c)),
                state: self.state,
                locked: false,
            },
            InnerThunkData::Revertible {
                ref orig,
                ref deps,
                ref cached,
            } => ThunkData {
                inner: InnerThunkData::Revertible {
                    orig: Rc::new(f(orig)),
                    cached: cached.as_ref().map(f),
                    deps: deps.clone(),
                },
                state: self.state,
                locked: false,
            },
        }
    }

    /// Return the potential field dependencies stored in a revertible thunk. See
    /// [`crate::transform::free_vars`]
    pub fn deps(&self) -> FieldDeps {
        match self.inner {
            InnerThunkData::Standard(_) => FieldDeps::empty(),
            InnerThunkData::Revertible { ref deps, .. } => deps.clone(),
        }
    }
}

/// A thunk.
///
/// A thunk is a shared suspended computation. It is the primary device for the implementation of
/// lazy evaluation.
///
/// For the implementation of recursive merging, some thunks need to be revertible, in the sense
/// that we must be able to revert to the original expression before update. Those are called
/// revertible thunks. Most expressions don't need revertible thunks as their evaluation will
/// always give the same result, but some others, such as the ones containing recursive references
/// inside a record may be invalidated by merging, and thus need to store the unaltered original
/// expression. Those aspects are handled and discussed in more detail in
/// [InnerThunkData].
///
/// # Representation
///
/// Since the introduction of [crate::eval::value::NickelValue], thunk data are stored inside a
/// value block, like most other values. A pointer to those data is thus precisely a `NickelValue`.
/// [Thunk] is a smart constructor for a value for which we know for sure it contains a thunk
/// (which makes it possible to perform most operations bypassing checks).
#[derive(Clone, Debug, PartialEq)]
// CAUTION: we rely on the fact that `Thunk` has the same layout as `NickelValue` (we transmute
// them freely). It's useful to allow zero-cost conversion between `&NickelValue` and `&Thunk`. Do
// not change the representation of `Thunk` lightly. Doing so without properly adapting the rest of
// the codebase will incur Undefined Behavior.
#[repr(transparent)]
pub struct Thunk(pub(in crate::eval) NickelValue);

impl Thunk {
    /// Create a new standard thunk.
    pub fn new(closure: Closure, pos_idx: PosIdx) -> Self {
        increment!("Thunk::new");
        Thunk(NickelValue::thunk(ThunkData::new(closure), pos_idx))
    }

    /// Create a new revertible thunk. If the dependencies are empty, this function acts as
    /// [Thunk::new] and create a standard (non-revertible) thunk.
    pub fn new_rev(closure: Closure, deps: FieldDeps, pos_idx: PosIdx) -> Self {
        match deps {
            FieldDeps::Known(deps) if deps.is_empty() => Self::new(closure, pos_idx),
            deps => Thunk(NickelValue::thunk(
                ThunkData::new_rev(closure, deps),
                pos_idx,
            )),
        }
    }

    /// Returns a reference to the inner `RefCell<ThunkData>`.
    fn data(&self) -> &RefCell<ThunkData> {
        // Safety: it's an invariant of `Thunk` that the inner `NickelValue` is a block of type
        // thunk.
        unsafe { self.0.as_thunk_data_unchecked() }
    }

    pub fn state(&self) -> ThunkState {
        self.data().borrow().state
    }

    /// Set the state to evaluated.
    pub fn set_evaluated(&self) {
        self.data().borrow_mut().state = ThunkState::Evaluated;
    }

    /// Lock a thunk. This flips a dedicated bit in the thunk's state. Returns `true` if the
    /// locking succeeded and the thunk wasn't previous locked, or `false` if the thunk was already
    /// in a locked state.
    ///
    /// Locking is used to prevent infinite loops for some step-by-step variants of deep evaluation
    /// such as `%deep_seq%`, `%force%` or [crate::program::Program::eval_record_spine], where the
    /// normal thunk update workflow isn't adapted.
    pub fn lock(&self) -> bool {
        let mut data_ref = self.data().borrow_mut();

        if data_ref.locked {
            return false;
        }

        data_ref.locked = true;
        true
    }

    /// Unlock a thunk previously locked (see [Self::lock]). If the thunk wasn't locked, this is a
    /// no-op. Returns `true` if the thunk was previously locked, `false` otherwise.
    pub fn unlock(&self) -> bool {
        let mut data_ref = self.data().borrow_mut();

        let was_locked = data_ref.locked;
        data_ref.locked = false;

        was_locked
    }

    /// Generate an update frame from this thunk and set the state to `Blackholed`. Return an
    /// error if the thunk was already black-holed.
    pub fn mk_update_frame(&mut self) -> Result<ThunkUpdateFrame, BlackholedError> {
        let mut data_ref = self.data().borrow_mut();

        if data_ref.state == ThunkState::Blackholed {
            return Err(BlackholedError);
        }

        data_ref.state = ThunkState::Blackholed;

        Ok(self.clone())
    }

    /// Immutably borrow the inner closure. Panic if there is another active mutable borrow.
    pub fn borrow(&self) -> Ref<'_, Closure> {
        Ref::map(self.data().borrow(), ThunkData::closure)
    }

    /// Mutably borrow the inner closure. Panic if there is any other active borrow.
    pub fn borrow_mut(&mut self) -> RefMut<'_, Closure> {
        RefMut::map(self.data().borrow_mut(), |data| data.closure_mut())
    }

    /// Immutably borrow the original expression stored in a thunk, even if it's a revertible
    /// thunks that hasn't been built yet (in which case the original expression is borrowed).
    /// **Beware**: the resulting closure might thus have unbound variables (recursive references of a
    /// freshly reverted thunks).
    ///
    /// Panic if there is another active mutable borrow.
    ///
    /// In general, no part of the interpreter should observe the intermediate state where a
    /// revertible thunk has been created but hasn't been built yet. Please use [Self::borrow]
    /// instead.
    ///
    /// There is, however, one exception: contract equality checks (needed for contract
    /// deduplication), which happen as part of merging and need to work on freshly reverted thunks
    /// which don't have their new recursive environment yet.
    ///
    /// This method is a leaking abstraction, which isn't very satisfying - in the future, we hope
    /// to have a proper incremental evaluation cache which performs (incremental) memoization, in
    /// which case revertible thunks wouldn't be needed anymore and could be replaced by a plain
    /// function.
    pub(crate) fn borrow_orig(&self) -> Ref<'_, Closure> {
        Ref::map(self.data().borrow(), ThunkData::closure_or_orig)
    }

    /// Get an owned clone of the inner closure.
    pub fn get_owned(&self) -> Closure {
        self.data().borrow().closure().clone()
    }

    /// Consume the thunk and return an owned closure. Avoid cloning if this thunk is the only
    /// reference to the inner closure.
    pub fn into_closure(self) -> Closure {
        // We reuse the code of lenses here, which is precisely what we need
        value::lens::ValueLens::<value::ThunkData>::with_content(
            self.0,
            |inner| inner.into_inner().into_closure(),
            |cell| cell.borrow().closure().clone(),
        )
    }

    /// Create a fresh unevaluated thunk from `self`, reverted to its original state before the
    /// first update. For a standard thunk, the content is unchanged and the state is conserved: in
    /// this case, `revert()` is the same as `clone()`.
    pub fn revert(&self) -> Self {
        ThunkData::revert(self)
    }

    pub fn build_cached(&self, rec_env: &[(Ident, Thunk)]) {
        self.data().borrow_mut().init_cached(rec_env)
    }

    /// Revert a thunk, abstract over its dependencies to get back a function, and apply the
    /// resulting function to the given variables. The function part is allocated in a fresh thunk
    /// with the same environment as the original expression.
    ///
    /// Recall that revertible thunks are just a memoization mechanism for function application.
    /// The original expression (`orig`) and the dependencies (`deps`) are a representation of a
    /// function. Most of the time, we don't have to manipulate the explicit function, but just
    /// manipulate the body of the function directly (which is what is stored inside the `orig`
    /// field).
    ///
    /// However, in the general case of merging two record fields which may be both recursive (i.e.
    /// which may contain a revertible thunk), we have to use the explicit function representation
    /// and apply it to variables, which correspond to the fields of the recursive record being
    /// built by merging.
    ///
    /// `saturate`:
    ///
    /// - abstracts the original expression of the underlying revertible thunk, forming a function.
    /// - stores this function in a fresh standard thunk
    /// - returns the application of this function to the provided record field names (as variables)
    ///
    /// Field names are taken as an iterator over identifiers.
    ///
    /// # Parameters
    ///
    /// `fields` is the fields of the resulting recursive record being built by merging. `fields`
    /// is used for two purposes:
    ///
    /// - to impose a fixed order on the arguments of the function. The particular order is not
    ///   important but it must be the same for forming the function and forming the
    ///   application, to avoid a mismatch like `(fun foo bar => ...) bar foo`
    /// - to know what parameters to use for reverting a thunk whose dependencies are unknown.
    ///   In that case, we must be conservative and abstract over all the fields of the
    ///   recursive record, but we can't get this information from `self` alone
    ///
    /// # Standard thunks (non-revertible)
    ///
    /// Non revertible thunks can be seen as a special case of revertible thunks with no
    /// dependencies. Thus the abstraction and application are nullary, and the result is just the
    /// current thunk.
    ///
    /// # Example
    ///
    /// If `orig` is `foo + bar + a` where `foo` and `bar` are thunk dependencies (hence are free
    /// variables) and `a` is bound in the environment. Say the iterator represents the fields `bar,
    /// b, foo` in that order. Then `saturate`:
    ///
    /// - stores `fun bar foo => foo + bar + a` in a fresh (say `thunk1`) thunk with the same environment as `self`
    ///   (in particular, `a` is bound)
    /// - returns the term `<closure@thunk1> bar foo`
    pub fn saturate<I: DoubleEndedIterator<Item = Ident> + Clone>(self, fields: I) -> NickelValue {
        let deps = self.deps();
        let pos_idx = self.0.pos_idx();
        let inner =
            value::lens::ValueLens::<value::ThunkData>::extract_or_clone(self.0).into_inner();

        let mut deps_filter: Box<dyn FnMut(&Ident) -> bool> = match deps {
            FieldDeps::Known(deps) => Box::new(move |id: &Ident| deps.contains(id)),
            FieldDeps::Unknown => Box::new(|_: &Ident| true),
        };

        let thunk_as_function = NickelValue::thunk(
            inner.revthunk_as_explicit_fun(fields.clone().filter(&mut deps_filter)),
            pos_idx,
        );

        let args = fields
            .filter(deps_filter)
            .map(|id| NickelValue::from(Term::Var(id.into())));

        args.fold(thunk_as_function, |partial_app, arg| {
            NickelValue::from(Term::app(partial_app, arg))
        })
    }

    /// Map a function over the content of the thunk to create a new, fresh independent thunk. If
    /// the thunk is revertible, the function is applied to both the original expression and the
    /// cached expression.
    pub fn map<F>(&self, f: F) -> Self
    where
        F: FnMut(&Closure) -> Closure,
    {
        Thunk(NickelValue::thunk(
            self.data().borrow().map(f),
            self.0.pos_idx(),
        ))
    }

    /// Determine if a thunk is worth being put on the stack for future update.
    ///
    /// Typically, expressions in weak head normal form won't evaluate further and their update can
    /// be skipped.
    pub fn should_update(&self) -> bool {
        !self.borrow().value.is_whnf()
    }

    /// Return a clone of the potential field dependencies stored in a revertible thunk. See
    /// [`crate::transform::free_vars`].
    pub fn deps(&self) -> FieldDeps {
        self.data().borrow().deps()
    }

    /// Check for physical equality between two thunks. This method is used for fast equality
    /// checking, as if two thunks are physically equal, they must be equal as Nickel values.
    pub fn ptr_eq(this: &Thunk, that: &Thunk) -> bool {
        NickelValue::phys_eq(&this.0, &that.0)
    }
}

impl From<Thunk> for NickelValue {
    fn from(thunk: Thunk) -> Self {
        thunk.0
    }
}

impl std::fmt::Pointer for Thunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Pointer::fmt(&self.0, f)
    }
}

/// A thunk update frame.
///
/// A thunk update frame is put on the stack whenever a variable is entered, such that once this
/// variable is evaluated, the corresponding thunk can be updated. It is currently exactly the same
/// a thunk.
pub type ThunkUpdateFrame = Thunk;

impl ThunkUpdateFrame {
    /// Update the corresponding thunk with a closure. Set the state to `Evaluated`
    pub fn update(self, closure: Closure) {
        self.data().borrow_mut().update(closure);
    }

    /// Reset the state of the thunk to Suspended
    /// Mainly used to reset the state of the VM between REPL runs
    pub fn reset_state(&self) {
        self.data().borrow_mut().state = ThunkState::Suspended;
    }
}

/// Placeholder [Cache] for the call-by-need evaluation strategy.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CBNCache {}

impl Cache for CBNCache {
    type UpdateIndex = ThunkUpdateFrame;

    fn get(&self, idx: CacheIndex) -> Closure {
        idx.get_owned()
    }

    fn get_update_index(
        &mut self,
        idx: &mut CacheIndex,
    ) -> Result<Option<Self::UpdateIndex>, BlackholedError> {
        // If the thunk is already evaluated, we don't return an update frame.
        if idx.state() == ThunkState::Evaluated {
            return Ok(None);
        }

        // If the thunk isn't worth updating, we set its state to evaluated and we don't create an
        // update frame either. There's one exception: if the thunk is blackholed, we still want to
        // raise an infinite recursion error. This will be handled by `mk_update_frame()` below.
        if !idx.should_update() && idx.state() != ThunkState::Blackholed {
            idx.set_evaluated();
            return Ok(None);
        }

        // Otherwise, we return an update frame.
        Some(idx.mk_update_frame()).transpose()
    }

    fn add(&mut self, clos: Closure, bty: BindingType) -> CacheIndex {
        let pos_idx = clos.value.pos_idx();

        match bty {
            BindingType::Normal => Thunk::new(clos, pos_idx),
            BindingType::Revertible(deps) => Thunk::new_rev(clos, deps, pos_idx),
        }
    }

    fn patch<F: FnOnce(&mut Closure)>(&mut self, mut idx: CacheIndex, f: F) {
        f(&mut idx.borrow_mut());
    }

    fn get_then<T, F: FnOnce(&Closure) -> T>(&self, idx: CacheIndex, f: F) -> T {
        f(&idx.borrow())
    }

    fn update(&mut self, clos: Closure, uidx: Self::UpdateIndex) {
        uidx.update(clos);
    }

    fn new() -> Self {
        CBNCache {}
    }

    fn reset_index_state(&mut self, idx: &mut Self::UpdateIndex) {
        idx.reset_state();
    }

    fn map_at_index<F: FnMut(&mut Self, &Closure) -> Closure>(
        &mut self,
        idx: &CacheIndex,
        mut f: F,
    ) -> CacheIndex {
        idx.map(|v| f(self, v))
    }

    fn build_cached(&mut self, idx: &mut CacheIndex, rec_env: &[(Ident, CacheIndex)]) {
        idx.build_cached(rec_env)
    }

    fn saturate<'a, I: DoubleEndedIterator<Item = Ident> + Clone>(
        &mut self,
        idx: CacheIndex,
        fields: I,
    ) -> NickelValue {
        idx.saturate(fields)
    }

    fn deps(&self, idx: &CacheIndex) -> Option<FieldDeps> {
        Some(idx.deps())
    }

    fn revert(&mut self, idx: &CacheIndex) -> CacheIndex {
        idx.revert()
    }

    fn make_update_index(
        &mut self,
        idx: &mut CacheIndex,
    ) -> Result<Self::UpdateIndex, BlackholedError> {
        idx.mk_update_frame()
    }
}
