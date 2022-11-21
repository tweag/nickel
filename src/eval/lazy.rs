//! Thunks and associated devices used to implement lazy evaluation.
use super::{Closure, IdentKind};
use crate::{identifier::Ident, term::FieldDeps};
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
        cached: Rc<Closure>,
        deps: FieldDeps,
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

    /// Create new revertible thunk data.
    pub fn new_rev(orig: Closure, deps: FieldDeps) -> Self {
        let rc = Rc::new(orig);

        ThunkData {
            inner: InnerThunkData::Revertible {
                orig: rc.clone(),
                cached: rc,
                deps,
            },
            state: ThunkState::Suspended,
        }
    }

    /// Return a reference to the closure currently cached.
    pub fn closure(&self) -> &Closure {
        match self.inner {
            InnerThunkData::Standard(ref closure) => closure,
            InnerThunkData::Revertible { ref cached, .. } => cached,
        }
    }

    /// Return a mutable reference to the closure currently cached.
    pub fn closure_mut(&mut self) -> &mut Closure {
        match self.inner {
            InnerThunkData::Standard(ref mut closure) => closure,
            InnerThunkData::Revertible { ref mut cached, .. } => Rc::make_mut(cached),
        }
    }

    /// Consume the data and return the cached closure.
    pub fn into_closure(self) -> Closure {
        match self.inner {
            InnerThunkData::Standard(closure) => closure,
            InnerThunkData::Revertible { orig, cached, .. } => {
                std::mem::drop(orig);
                Rc::try_unwrap(cached).unwrap_or_else(|rc| (*rc).clone())
            }
        }
    }

    /// Update the cached closure.
    pub fn update(&mut self, new: Closure) {
        match self.inner {
            InnerThunkData::Standard(ref mut closure) => *closure = new,
            InnerThunkData::Revertible { ref mut cached, .. } => *cached = Rc::new(new),
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
                    cached: Rc::clone(orig),
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
                    cached: Rc::new(f(cached)),
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
