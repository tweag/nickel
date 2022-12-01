//! Thunks and associated devices used to implement lazy evaluation.
use super::{Closure, Environment, IdentKind};
use crate::{
    identifier::Ident,
    term::{FieldDeps, RichTerm, Term},
};
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashSet;
use std::rc::{Rc, Weak};

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
/// user and pretend recursive records are data that can be used as a normal record, because it's
/// both simple and natural (for the user).
///
/// A naive representation of recursive records could be to always keep recursive records in their
/// functional representation, and re-compute the fixpoint at each record operation, such as field
/// access. While correct, this would be wasteful, because the result of the fixpoint computations
/// is actually the same throughout most record operations (put differently, `self` doesn't
/// change). But we can't just store the computed `fixpoint` in a normal thunk and forget
/// about the original function, because upon merge, and more specifically upon recursive
/// overriding, we do need to compute a new fixpoint.
///
/// Revertible thunks are a **memoization device** for the `fun self => ...` functional
/// representation of a recursive record. First, note that there are several different but
/// equivalent ways of encoding the example above as a function:
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
/// (this has an important positive memory impact, see [ThunkDeps]).
///
/// In practice, Nickel uses the last representation. Fields that don't have any free variable
/// intersecting with the name of the other fields are allocated as normal thunks that will only be
/// evaluated at most once. Fields with actual dependencies are stored in revertible thunks, where
/// the `orig` field stores the body of the function (in our example the expression `bar + baz +
/// 1`), and the cached version is the application to `self` computed when we evaluated the
/// recursive record to a normal record. The function arguments (that is, the intersection of the
/// free variables of the expression with the set of fields) are stored inside `deps`. `deps`
/// represent the other fields a specific field is depending on, and are guaranteed to appear free
/// inside `orig`.
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
        }
    }

    /// Create new revertible thunk data.
    pub fn new_rev(orig: Closure, deps: FieldDeps) -> Self {
        ThunkData {
            inner: InnerThunkData::Revertible {
                orig: Rc::new(orig),
                cached: None,
                deps,
            },
            state: ThunkState::Suspended,
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
    /// application given as built by `saturate`, but applied to arguments taken
    /// from `rec_env`. The major difference is that `init_cached` avoids the creation of the
    /// intermediate redex `(fun id1 .. id n => orig) %1 .. %n` as well as the intermediate thunks
    /// and terms, because we can compute the result application right away, in-place.
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
                    Some(deps) if deps.is_empty() => (),
                    Some(deps) => new_cached
                        .env
                        .extend(rec_env.iter().filter(|(id, _)| deps.contains(id)).cloned()),
                    None => new_cached.env.extend(rec_env.iter().cloned()),
                };

                *cached = Some(new_cached);
            }
        }
    }

    /// Revert a thunk and abstract over the provided arguments to get back a function. The result
    /// is returned in a new, non-revertible, thunk.
    ///
    /// Used by [Thunk::saturate]. See the corresponding documentation for more
    /// details.
    ///
    /// # Example
    ///
    /// If `orig` is `foo + bar + a` and `args` correspond to `bar, foo`, this functions returns a
    /// standard thunk containing `fun bar foo => foo + bar + a`.
    fn revthunk_as_explicit_fun<'a, I>(self, args: I) -> Self
    where
        I: DoubleEndedIterator<Item = &'a Ident>,
    {
        match self.inner {
            InnerThunkData::Standard(_) => self,
            InnerThunkData::Revertible { orig, .. } => {
                let Closure { body, env } =
                    Rc::try_unwrap(orig).unwrap_or_else(|rc| Closure::clone(&rc));

                // Build a list of the arguments that the function will need in the same order as
                // the original iterator. If the identifiers inside `args` are `a`, `b` and `c`, in
                // that order, we want to build `fun a => (fun b => (fun c => body))`. We thus need a
                // reverse fold.
                let as_function =
                    args.rfold(body, |built, id| RichTerm::from(Term::Fun(*id, built)));

                ThunkData::new(Closure {
                    body: as_function,
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
            // them (build the cached value) in a second step. But calling to
            // [`ThunkData::new_rev`] followed by [`ThunkData::build_cached_value`] should be logically
            // seen as just one construction operation.
            InnerThunkData::Revertible { ref cached, .. } => {
                cached.as_ref().expect(REVTHUNK_NO_CACHED_VALUE_MSG)
            }
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
            // Nothing should access the cached value of a revertible thunk before the cached
            // value has been constructed. This is an invariant that MUST be maintained by the
            // interpreter
            //
            // `cached` set to `None` solely exists because we need to first allocate all the
            // revertible thunks corresponding to a recursive record, and only then can we patch
            // them (build the cached value) in a second step. But calling to
            // [`ThunkData::new_rev`] followed by [`ThunkData::build_cached_value`] should be logically
            // seen as just one construction operation.
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
    /// original state before the first update.
    ///
    /// For standard thunk data, the content is unchanged and shared with the original thunk: in
    /// this case, `revert()` is the same as cloning the original thunk.
    ///
    /// For revertible thunk data, the result is independent from the original one: any update to
    /// one of the thunks doesn't affect the other.
    pub fn revert(thunk: &Rc<RefCell<ThunkData>>) -> Rc<RefCell<ThunkData>> {
        match thunk.borrow().inner {
            InnerThunkData::Standard(_) => Rc::clone(thunk),
            InnerThunkData::Revertible {
                ref orig, ref deps, ..
            } => Rc::new(RefCell::new(ThunkData {
                inner: InnerThunkData::Revertible {
                    orig: Rc::clone(orig),
                    cached: None,
                    deps: deps.clone(),
                },
                state: ThunkState::Suspended,
            })),
        }
    }

    /// Map a function over the content of the thunk to create a new independent thunk. If the
    /// thunk is revertible, the mapping function is applied on both the original expression and
    /// the cached expression.
    pub fn map<F>(&self, mut f: F) -> Self
    where
        F: FnMut(&Closure) -> Closure,
    {
        match self.inner {
            InnerThunkData::Standard(ref c) => ThunkData {
                inner: InnerThunkData::Standard(f(c)),
                state: self.state,
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
            },
        }
    }

    /// Return the potential field dependencies stored in a revertible thunk. See [`crate::transform::free_vars`]
    pub fn deps(&self) -> ThunkDeps {
        match self.inner {
            InnerThunkData::Standard(_) => ThunkDeps::Empty,
            InnerThunkData::Revertible { ref deps, .. } => deps
                .as_ref()
                .map(|deps| ThunkDeps::Known(Rc::clone(deps)))
                .unwrap_or(ThunkDeps::Unknown),
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

    /// Create a new revertible thunk.
    pub fn new_rev(closure: Closure, ident_kind: IdentKind, deps: FieldDeps) -> Self {
        Thunk {
            data: Rc::new(RefCell::new(ThunkData::new_rev(closure, deps))),
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
            _ident_kind: self.ident_kind,
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

    /// Create a fresh unevaluated thunk from `self`, reverted to its original state before the
    /// first update. For a standard thunk, the content is unchanged and the state is conserved: in
    /// this case, `revert()` is the same as `clone()`.
    pub fn revert(&self) -> Self {
        Thunk {
            data: ThunkData::revert(&self.data),
            ident_kind: self.ident_kind,
        }
    }

    pub fn build_cached(&mut self, rec_env: &[(Ident, Thunk)]) {
        self.data.borrow_mut().init_cached(rec_env)
    }

    /// Revert a thunk, abstract over its dependencies to get back a function, and apply the
    /// function to the given variables. The function part is allocated in a new fresh thunk,
    /// stored as a generated variable, with the same environment as the original expression.
    ///
    /// Recall that revertible thunks are just a memoization mechanism for function application.
    /// The original expression (`orig`) and the dependencies (`deps`) are a representation of a
    /// function. Most of the time, we don't have to go through an explicit function, and just
    /// manipulate the body of the function directly (which is what is stored inside the `orig`
    /// field).
    ///
    /// However, in the general case of merging two record fields which may be both recursive (i.e.
    /// which may contain a revertible thunk), we have to use the explicit function representation
    /// and apply it to variables, which correspond to the fields of the recursive record being
    /// built by merging.
    ///
    /// `saturate`:
    /// - abstracts the original expression of the underlying revertible thunk, forming a function.
    /// - stores this function in a fresh standard thunk
    /// - returns the application of this function to the provided record field names (as variables)
    ///
    /// Field names are taken as an iterator over identifiers.
    ///
    /// # Parameters
    ///
    /// - `env`: the environment in which the explicit function expression is closurized. When
    ///   performing recursive overriding, this is the local environment of the final merged field.
    /// - `fields`: the fields of the resulting recursive record being built by merging. `fields` is used for two
    ///   purposes:
    ///     - to impose a fixed order on the arguments of the function. The particular order is not
    ///       important but it must be the same used for forming the function and forming the
    ///       application, to avoid a mismatch like `(fun foo bar => ...) bar foo`
    ///     - to know what parameters to use for reverting a thunk whose dependencies are unknown.
    ///       In that case, we must be conservative and abstract over all the fields of the
    ///       recursive record, but we can't get this information from `self` alone
    ///
    /// # Standard thunks (non-revertible)
    ///
    /// Non revertible thunks can be seen as a special case of revertible thunks with no
    /// dependencies. Thus the abstraction and application are nullary, and the result is just the
    /// current thunk closurized in `env` as a fresh variable.
    ///
    /// # Example
    ///
    /// If `orig` is `foo + bar + a` where `foo` and `bar` are thunk dependencies (hence are free
    /// variables) and `a` is bound in the environment. Say the iterator represents the fields
    /// `bar, b, foo` in that order. Then `saturate`:
    ///
    /// - stores `fun bar foo => foo + bar + a` in a fresh thunk with the same environment as
    ///   `self` (in particular, `a` is bound)
    /// - allocates a fresh variable, say `%1`, and binds it to the previous thunk in `env`
    /// - returns the term `%1 foo bar`
    pub fn saturate<'a, I: DoubleEndedIterator<Item = &'a Ident> + Clone>(
        self,
        env: &mut Environment,
        fields: I,
    ) -> RichTerm {
        let deps = self.deps();
        let inner = Rc::try_unwrap(self.data)
            .map(RefCell::into_inner)
            .unwrap_or_else(|rc| rc.borrow().clone());

        let mut deps_filter: Box<dyn FnMut(&&Ident) -> bool> = match deps {
            ThunkDeps::Empty => Box::new(|_: &&Ident| false),
            ThunkDeps::Known(deps) => Box::new(move |id: &&Ident| deps.contains(id)),
            ThunkDeps::Unknown => Box::new(|_: &&Ident| true),
        };

        let thunk_as_function = Thunk {
            data: Rc::new(RefCell::new(
                inner.revthunk_as_explicit_fun(fields.clone().filter(&mut deps_filter)),
            )),
            ident_kind: self.ident_kind,
        };

        let fresh_var = Ident::fresh();
        env.insert(fresh_var, thunk_as_function);

        let as_function_closurized = RichTerm::from(Term::Var(fresh_var));
        let args = fields.filter_map(|id| deps_filter(&id).then(|| RichTerm::from(Term::Var(*id))));

        args.fold(as_function_closurized, |partial_app, arg| {
            RichTerm::from(Term::App(partial_app, arg))
        })
    }

    /// Map a function over the content of the thunk to create a new, fresh independent thunk. If
    /// the thunk is revertible, the function is applied to both the original expression and the
    /// cached expression.
    pub fn map<F>(&self, f: F) -> Self
    where
        F: FnMut(&Closure) -> Closure,
    {
        Thunk {
            data: Rc::new(RefCell::new(self.data.borrow().map(f))),
            ident_kind: self.ident_kind,
        }
    }

    /// Determine if a thunk is worth being put on the stack for future update.
    ///
    /// Typically, WHNFs and enriched values will not be evaluated to a simpler expression and are not
    /// worth updating.
    pub fn should_update(&self) -> bool {
        let term = &self.borrow().body.term;
        !term.is_whnf() && !term.is_metavalue()
    }

    /// Return a clone of the potential field dependencies stored in a revertible thunk. See
    /// [`crate::transform::free_vars`].
    pub fn deps(&self) -> ThunkDeps {
        self.data.borrow().deps()
    }
}

/// Possible alternatives for the field dependencies of a thunk.
#[derive(Clone, Debug)]
pub enum ThunkDeps {
    /// The thunk is revertible, containing potential recursive references to other fields, and the
    /// set of dependencies has been computed
    Known(Rc<HashSet<Ident>>),
    /// The thunk is revertible, but the set of dependencies hasn't been computed. In that case,
    /// the interpreter should be conservative and assume that any recursive references can appear
    /// in the content of the corresponding thunk.
    Unknown,
    /// The thunk is not revertible and can't contain recursive references. The interpreter can
    /// safely eschews the environment patching process entirely.
    Empty,
}

impl ThunkDeps {
    /// Compute the union of two thunk dependencies. [`ThunkDeps::Unknown`] can be see as the top
    /// element, meaning that if one of the two set of dependencies is [`ThunkDeps::Unknown`], so
    /// is the result.
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (ThunkDeps::Empty, ThunkDeps::Empty) => ThunkDeps::Empty,
            // If one of the field has unknown dependencies (understand: may depend on all the other
            // fields), then the resulting fields has unknown dependencies as well
            (ThunkDeps::Unknown, _) | (_, ThunkDeps::Unknown) => ThunkDeps::Unknown,
            (ThunkDeps::Empty, ThunkDeps::Known(deps))
            | (ThunkDeps::Known(deps), ThunkDeps::Empty) => ThunkDeps::Known(deps),
            (ThunkDeps::Known(deps1), ThunkDeps::Known(deps2)) => {
                let union: HashSet<Ident> = deps1.union(&*deps2).cloned().collect();
                ThunkDeps::Known(Rc::new(union))
            }
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
    _ident_kind: IdentKind,
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

    /// Reset the state of the thunk to Suspended
    /// Mainly used to reset the state of the vm between REPL runs
    pub fn reset_state(self) {
        if let Some(data) = Weak::upgrade(&self.data) {
            data.borrow_mut().state = ThunkState::Suspended;
        }
    }
}
