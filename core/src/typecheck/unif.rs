//! Types unification.

use super::*;

/// Unification variable or type constants unique identifier.
pub type VarId = usize;

/// Variable levels. Levels are used in order to implement polymorphism in a sound way: we need to
/// associate to each unification variable and rigid type variable a level, which depends on when
/// those variables were introduced, and to forbid some unifications if a condition on levels is
/// not met.
#[derive(Clone, Copy, Ord, Eq, PartialEq, PartialOrd, Debug)]
pub struct VarLevel(NonZeroU16);

impl VarLevel {
    /// Special constant used for level upper bound to indicate that a type doesn't contain any
    /// unification variable. It's equal to `1` and strictly smaller than [VarLevel::MIN_LEVEL], so
    /// it's strictly smaller than any concrete variable level.
    pub const NO_VAR: Self = VarLevel(NonZeroU16::MIN);
    /// The first available variable level, `2`.
    // unsafe is required because `unwrap()` is not usable in `const fn` code as of today in stable
    // Rust.
    // unsafe(): we must enforce the invariant that the argument `n` of `new_unchecked(n)` verifies
    // `0 < n`. Indeed `0 < 2`.
    pub const MIN_LEVEL: Self = unsafe { VarLevel(NonZeroU16::new_unchecked(2)) };
    /// The maximum level. Used as an upper bound to indicate that nothing can be said about the
    /// levels of the unification variables contained in a type.
    pub const MAX_LEVEL: Self = VarLevel(NonZeroU16::MAX);

    /// Increment the variable level by one. Panic if the maximum capacity of the underlying
    /// numeric type is reached (currently, `u16::MAX`).
    pub fn incr(&mut self) {
        let new_value = self
            .0
            .checked_add(1)
            .expect("reached the maxium unification variable level");
        self.0 = new_value;
    }
}

/// An element of the unification table. Contains the potential type this variable points to (or
/// `None` if the variable hasn't been unified with something yet), and the variable's level.
pub struct UnifSlot<Ty> {
    value: Option<Ty>,
    level: VarLevel,
}

impl<Ty> UnifSlot<Ty> {
    pub fn new(level: VarLevel) -> Self {
        UnifSlot { value: None, level }
    }
}

/// The unification table.
///
/// Map each unification variable to either another type variable or a concrete type it has been
/// unified with. Each binding `(ty, var)` in this map should be thought of an edge in a
/// unification graph.
///
/// The unification table is really three separate tables, corresponding to the different kinds of
/// types: standard types, record rows, and enum rows.
///
/// The unification table is a relatively low-level data structure, whose consumer has to ensure
/// specific invariants. It is used by the `unify` function and its variants, but you should avoid
/// using it directly, unless you know what you're doing.
#[derive(Default)]
pub struct UnifTable {
    types: Vec<UnifSlot<UnifType>>,
    rrows: Vec<UnifSlot<UnifRecordRows>>,
    erows: Vec<UnifSlot<UnifEnumRows>>,
    pending_type_updates: Vec<VarId>,
    pending_rrows_updates: Vec<VarId>,
    pending_erows_updates: Vec<VarId>,
}

impl UnifTable {
    pub fn new() -> Self {
        UnifTable::default()
    }

    /// Assign a type to a type unification variable.
    ///
    /// This method updates variables level, at least lazily, by pushing them to a stack of pending
    /// traversals.
    ///
    /// # Preconditions
    ///
    /// - This method doesn't check for the variable level conditions. This is the responsibility
    /// of the caller.
    /// - If the target type is a unification variable as well, it must not be assigned to another
    /// unification type. That is, `assign` should always be passed a root type. Otherwise, the
    /// handling of variable levels will be messed up.
    /// - This method doesn't force pending level updates when needed (calling to
    /// `force_type_updates`), i.e.
    /// when `uty` is a rigid type variable. Having pending variable level updates and using
    /// `assign_type` might make typechecking incorrect in some situation by unduely allowing
    /// unsound generalization. This is the responsibility of the caller.
    pub fn assign_type(&mut self, var: VarId, uty: UnifType) {
        // Unifying a free variable with itself is a no-op.
        if matches!(uty, UnifType::UnifVar { id, ..} if id == var) {
            return;
        }

        debug_assert!({
            if let UnifType::UnifVar { id, init_level: _ } = &uty {
                self.types[*id].value.is_none()
            } else {
                true
            }
        });
        debug_assert!(self.types[var].value.is_none());

        let uty_lvl_updated = self.update_type_level(var, uty, self.types[var].level);
        self.types[var].value = Some(uty_lvl_updated);
    }

    // Lazily propagate a variable level to the unification variables contained in `uty`. Either do
    // a direct update in constant time when possible, or push a stack of delayed updates for
    // composite types.
    fn update_type_level(&mut self, var: VarId, uty: UnifType, new_level: VarLevel) -> UnifType {
        match uty {
            // We can do the update right away
            UnifType::UnifVar { id, init_level } => {
                if new_level < self.types[id].level {
                    self.types[id].level = new_level;
                }

                UnifType::UnifVar { id, init_level }
            }
            // If a concrete type is a candidate for update, we push the pending update on the
            // stack
            UnifType::Concrete {
                typ,
                var_levels_data,
            } if var_levels_data.upper_bound >= new_level => {
                self.pending_type_updates.push(var);

                UnifType::Concrete {
                    typ,
                    var_levels_data: VarLevelsData {
                        pending: Some(new_level),
                        ..var_levels_data
                    },
                }
            }
            // The remaining types either don't contain unification variables or have all their
            // level greater than the updated level
            _ => uty,
        }
    }

    /// Assign record rows to a record rows unification variable.
    ///
    /// This method updates variables level, at least lazily, by pushing them to a stack of pending
    /// traversals.
    ///
    /// # Preconditions
    ///
    /// - This method doesn't check for the variable level conditions. This is the responsibility
    /// of the caller.
    /// - If the target type is a unification variable as well, it must not be assigned to another
    /// unification type. That is, `assign` should always be passed a root type. Otherwise, the
    /// handling of variable levels will be messed up.
    /// - This method doesn't force pending level updates when needed (calling to
    /// `force_rrows_updates`), i.e.
    /// when `uty` is a rigid type variable. Having pending variable level updates and using
    /// `assign_type` might make typechecking incorrect in some situation by unduly allowing
    /// unsound generalization. This is the responsibility of the caller.
    pub fn assign_rrows(&mut self, var: VarId, rrows: UnifRecordRows) {
        // Unifying a free variable with itself is a no-op.
        if matches!(rrows, UnifRecordRows::UnifVar { id, ..} if id == var) {
            return;
        }

        self.update_rrows_level(var, &rrows, self.rrows[var].level);
        debug_assert!(self.rrows[var].value.is_none());
        self.rrows[var].value = Some(rrows);
    }

    // cf `update_type_level()`
    fn update_rrows_level(&mut self, var: VarId, uty: &UnifRecordRows, new_level: VarLevel) {
        match uty {
            // We can do the update right away
            UnifRecordRows::UnifVar {
                id: var_id,
                init_level: _,
            } => {
                if new_level < self.rrows[*var_id].level {
                    self.rrows[*var_id].level = new_level;
                }
            }
            // If concrete rows are a candidate for update, we push the pending update on the stack
            UnifRecordRows::Concrete {
                var_levels_data, ..
            } if var_levels_data.upper_bound >= new_level => self.pending_rrows_updates.push(var),
            // The remaining rows either don't contain unification variables or have all their
            // level greater than the updated level
            _ => (),
        }
    }

    /// Assign enum rows to an enum rows unification variable.
    ///
    /// This method updates variables level, at least lazily, by pushing them to a stack of pending
    /// traversals.
    ///
    /// # Preconditions
    ///
    /// - This method doesn't check for the variable level conditions. This is the responsibility
    /// of the caller.
    /// - If the target type is a unification variable as well, it must not be assigned to another
    /// unification type. That is, `assign` should always be passed a root type. Otherwise, the
    /// handling of variable levels will be messed up.
    /// - This method doesn't force pending level updates when needed (calling to
    /// `force_erows_updates`), i.e.
    /// when `uty` is a rigid type variable. Having pending variable level updates and using
    /// `assign_type` might make typechecking incorrect in some situation by unduly allowing
    /// unsound generalization. This is the responsibility of the caller.
    pub fn assign_erows(&mut self, var: VarId, erows: UnifEnumRows) {
        // Unifying a free variable with itself is a no-op.
        if matches!(erows, UnifEnumRows::UnifVar { id, .. } if id == var) {
            return;
        }

        self.update_erows_level(var, &erows, self.erows[var].level);
        debug_assert!(self.erows[var].value.is_none());
        self.erows[var].value = Some(erows);
    }

    // cf `update_type_level()`
    fn update_erows_level(&mut self, var: VarId, uty: &UnifEnumRows, new_level: VarLevel) {
        match uty {
            // We can do the update right away
            UnifEnumRows::UnifVar {
                id: var_id,
                init_level: _,
            } => {
                if new_level < self.erows[*var_id].level {
                    self.erows[*var_id].level = new_level;
                }
            }
            // If concrete rows are a candidate for update, we push the pending update on the stack
            UnifEnumRows::Concrete {
                var_levels_data, ..
            } if var_levels_data.upper_bound >= new_level => self.pending_erows_updates.push(var),
            // The remaining rows either don't contain unification variables or have all their
            // level greater than the updated level
            _ => (),
        }
    }

    /// Retrieve the current assignment of a type unification variable.
    pub fn get_type(&self, var: VarId) -> Option<&UnifType> {
        self.types[var].value.as_ref()
    }

    /// Retrieve the current level of a unification variable or a rigid type variable.
    pub fn get_level(&self, var: VarId) -> VarLevel {
        self.types[var].level
    }

    /// Retrieve the current assignment of a record rows unification variable.
    pub fn get_rrows(&self, var: VarId) -> Option<&UnifRecordRows> {
        self.rrows[var].value.as_ref()
    }

    /// Retrieve the current level of a record rows unification variable or a record rows rigid
    /// type variable.
    pub fn get_rrows_level(&self, var: VarId) -> VarLevel {
        self.rrows[var].level
    }

    /// Retrieve the current assignment of an enum rows unification variable.
    pub fn get_erows(&self, var: VarId) -> Option<&UnifEnumRows> {
        self.erows[var].value.as_ref()
    }

    /// Retrieve the current level of an enu rows unification variable or a record rows rigid type
    /// variable.
    pub fn get_erows_level(&self, var: VarId) -> VarLevel {
        self.erows[var].level
    }

    /// Create a fresh type unification variable (or constant) identifier and allocate a
    /// corresponding slot in the table.
    pub fn fresh_type_var_id(&mut self, current_level: VarLevel) -> VarId {
        let next = self.types.len();
        self.types.push(UnifSlot::new(current_level));
        next
    }

    /// Create a fresh record rows variable (or constant) identifier and allocate a corresponding
    /// slot in the table.
    pub fn fresh_rrows_var_id(&mut self, current_level: VarLevel) -> VarId {
        let next = self.rrows.len();
        self.rrows.push(UnifSlot::new(current_level));
        next
    }

    /// Create a fresh enum rows variable (or constant) identifier and allocate a corresponding
    /// slot in the table.
    pub fn fresh_erows_var_id(&mut self, current_level: VarLevel) -> VarId {
        let next = self.erows.len();
        self.erows.push(UnifSlot::new(current_level));
        next
    }

    /// Create a fresh type unification variable and allocate a corresponding slot in the table.
    pub fn fresh_type_uvar(&mut self, current_level: VarLevel) -> UnifType {
        UnifType::UnifVar {
            id: self.fresh_type_var_id(current_level),
            init_level: current_level,
        }
    }

    /// Create a fresh record rows unification variable and allocate a corresponding slot in the
    /// table.
    pub fn fresh_rrows_uvar(&mut self, current_level: VarLevel) -> UnifRecordRows {
        UnifRecordRows::UnifVar {
            id: self.fresh_rrows_var_id(current_level),
            init_level: current_level,
        }
    }

    /// Create a fresh enum rows unification variable and allocate a corresponding slot in the
    /// table.
    pub fn fresh_erows_uvar(&mut self, current_level: VarLevel) -> UnifEnumRows {
        UnifEnumRows::UnifVar {
            id: self.fresh_erows_var_id(current_level),
            init_level: current_level,
        }
    }

    /// Create a fresh type constant and allocate a corresponding slot in the table.
    pub fn fresh_type_const(&mut self, current_level: VarLevel) -> UnifType {
        UnifType::Constant(self.fresh_type_var_id(current_level))
    }

    /// Create a fresh record rows constant and allocate a corresponding slot in the table.
    pub fn fresh_rrows_const(&mut self, current_level: VarLevel) -> UnifRecordRows {
        UnifRecordRows::Constant(self.fresh_rrows_var_id(current_level))
    }

    /// Create a fresh enum rows constant and allocate a corresponding slot in the table.
    pub fn fresh_erows_const(&mut self, current_level: VarLevel) -> UnifEnumRows {
        UnifEnumRows::Constant(self.fresh_erows_var_id(current_level))
    }

    /// Follow the links in the unification table to find the representative of the equivalence
    /// class of the type unification variable `x`.
    ///
    /// This corresponds to the find in union-find.
    // TODO This should be a union find like algorithm
    pub fn root_type(&self, var_id: VarId, init_level: VarLevel) -> UnifType {
        // All queried variable must have been introduced by `new_var` and thus a corresponding
        // entry must always exist in `state`. If not, the typechecking algorithm is not correct,
        // and we panic.
        match self.types[var_id].value.as_ref() {
            None => UnifType::UnifVar {
                id: var_id,
                init_level,
            },
            Some(UnifType::UnifVar { id, init_level }) => self.root_type(*id, *init_level),
            Some(ty) => ty.clone(),
        }
    }

    /// Follow the links in the unification table to find the representative of the equivalence
    /// class of the record rows unification variable `x`.
    ///
    /// This corresponds to the find in union-find.
    // TODO This should be a union find like algorithm
    pub fn root_rrows(&self, var_id: VarId, init_level: VarLevel) -> UnifRecordRows {
        // All queried variable must have been introduced by `new_var` and thus a corresponding
        // entry must always exist in `state`. If not, the typechecking algorithm is not correct,
        // and we panic.
        match self.rrows[var_id].value.as_ref() {
            None => UnifRecordRows::UnifVar {
                id: var_id,
                init_level,
            },
            Some(UnifRecordRows::UnifVar { id, init_level }) => self.root_rrows(*id, *init_level),
            Some(ty) => ty.clone(),
        }
    }

    /// Follow the links in the unification table to find the representative of the equivalence
    /// class of the enum rows unification variable `x`.
    ///
    /// This corresponds to the find in union-find.
    // TODO This should be a union find like algorithm
    pub fn root_erows(&self, var_id: VarId, init_level: VarLevel) -> UnifEnumRows {
        // All queried variable must have been introduced by `new_var` and thus a corresponding
        // entry must always exist in `state`. If not, the typechecking algorithm is not correct,
        // and we panic.
        match self.erows[var_id].value.as_ref() {
            None => UnifEnumRows::UnifVar {
                id: var_id,
                init_level,
            },
            Some(UnifEnumRows::UnifVar { id, init_level }) => self.root_erows(*id, *init_level),
            Some(ty) => ty.clone(),
        }
    }

    /// Return a `VarId` greater than all of the variables currently allocated (unification and
    /// rigid type variables, of all kinds, rows or types). The returned UID is guaranteed to be
    /// different from all the currently live variables. This is currently simply the max of the
    /// length of the various unification tables.
    ///
    /// Used inside [self::eq] to generate temporary rigid type variables that are guaranteed to
    /// not conflict with existing variables.
    pub fn max_uvars_count(&self) -> VarId {
        max(self.types.len(), max(self.rrows.len(), self.erows.len()))
    }

    /// This function forces pending type updates prior to unifying a variable with a rigid type
    /// variable of level `constant_level`. Updates that wouldn't change the outcome of such a
    /// unification are delayed further.
    ///
    /// The whole point of variable levels is to forbid some unsound unifications of a unification
    /// variable with a rigid type variable. For performance reasons, those levels aren't
    /// propagated immediatly when unifying a variable with a concrete type, but lazily stored at
    /// the level of types (see [VarLevel]).
    ///
    /// However, unifying with a rigid type variable is an instance that requires levels to be up
    /// to date. In this case, this function must be called before checking variable levels.
    ///
    /// # Parameters
    ///
    /// - `constant_level`: the level of the rigid type variable we're unifying with. While not
    ///   strictly required to propagate levels, it is used to eschew variable level updates that
    ///   wouldn't change the outcome of the unfication, which we can keep for later forced
    ///   updates.
    fn force_type_updates(&mut self, constant_level: VarLevel) {
        fn update_unr_with_lvl(
            table: &mut UnifTable,
            uty: UnifTypeUnrolling,
            level: VarLevel,
        ) -> UnifTypeUnrolling {
            uty.map_state(
                |uty, table| Box::new(update_utype_with_lvl(table, *uty, level)),
                |rrows, table| update_rrows_with_lvl(table, rrows, level),
                |erows, table| update_erows_with_lvl(table, erows, level),
                table,
            )
        }

        fn update_rrows_with_lvl(
            table: &mut UnifTable,
            rrows: UnifRecordRows,
            level: VarLevel,
        ) -> UnifRecordRows {
            let rrows = rrows.into_root(table);

            match rrows {
                UnifRecordRows::Concrete {
                    rrows,
                    var_levels_data,
                } => {
                    let rrows = rrows.map_state(
                        |uty, table| Box::new(update_utype_with_lvl(table, *uty, level)),
                        |rrows, table| Box::new(update_rrows_with_lvl(table, *rrows, level)),
                        table,
                    );

                    // [^var-level-kinds]: Note that for `UnifRecordRows` (and for enum rows as
                    // well), the variable levels data are concerned with record rows unification
                    // variables, not type unification variable. We thus let them untouched, as
                    // updating record rows variable levels is an orthogonal concern.
                    UnifRecordRows::Concrete {
                        rrows,
                        var_levels_data,
                    }
                }
                UnifRecordRows::UnifVar { .. } | UnifRecordRows::Constant(_) => rrows,
            }
        }

        fn update_erows_with_lvl(
            table: &mut UnifTable,
            erows: UnifEnumRows,
            level: VarLevel,
        ) -> UnifEnumRows {
            let erows = erows.into_root(table);

            match erows {
                UnifEnumRows::Concrete {
                    erows,
                    var_levels_data,
                } => {
                    let erows = erows.map_state(
                        |uty, table| Box::new(update_utype_with_lvl(table, *uty, level)),
                        |erows, table| Box::new(update_erows_with_lvl(table, *erows, level)),
                        table,
                    );

                    // see [^var-level-kinds]
                    UnifEnumRows::Concrete {
                        erows,
                        var_levels_data,
                    }
                }
                UnifEnumRows::UnifVar { .. } | UnifEnumRows::Constant(_) => erows,
            }
        }

        fn update_utype_with_lvl(
            table: &mut UnifTable,
            uty: UnifType,
            level: VarLevel,
        ) -> UnifType {
            let uty = uty.into_root(table);

            match uty {
                UnifType::UnifVar { id, init_level } => {
                    if table.types[id].level > level {
                        table.types[id].level = level;
                    }

                    UnifType::UnifVar { id, init_level }
                }
                UnifType::Concrete {
                    typ,
                    var_levels_data,
                } if var_levels_data.upper_bound > level => {
                    let level = var_levels_data
                        .pending
                        .map(|pending_level| max(pending_level, level))
                        .unwrap_or(level);
                    let typ = update_unr_with_lvl(table, typ, level);

                    UnifType::Concrete {
                        typ,
                        var_levels_data: VarLevelsData {
                            upper_bound: level,
                            pending: None,
                        },
                    }
                }
                UnifType::Constant(_) | UnifType::Contract(..) | UnifType::Concrete { .. } => uty,
            }
        }

        fn update_utype(
            table: &mut UnifTable,
            uty: UnifType,
            constant_level: VarLevel,
        ) -> (UnifType, bool) {
            match uty {
                UnifType::UnifVar { .. } => {
                    // We should never end up updating the level of a type variable, as this update
                    // is done on the spot.
                    debug_assert!(false);

                    (uty, false)
                }
                UnifType::Concrete {
                    typ,
                    var_levels_data:
                        VarLevelsData {
                            pending: Some(pending_level),
                            upper_bound,
                        },
                } => {
                    // [^irrelevant-level-update]: A level update where the if-condition below is
                    // true wouldn't change the outcome of unifying a variable with a constant of
                    // level `constant_level`.
                    //
                    // Impactful updates are updates that might change the level of a variable from
                    // a value greater than or equals to `constant_level` to a new level strictly
                    // smaller, but:
                    //
                    // 1. If `upper_bound` < `constant_level`, then all unification variable levels
                    //    are already strictly smaller than `constant_level`. An update won't change
                    //    this inequality (level update can only decrease levels)
                    // 2. If `pending_level` >= `constant_level`, then the update might only
                    //    decrease a level that was greater than `constant_level` to a
                    //    `pending_level` which is still greater than `constant_level`. Once again,
                    //    the update doesn't change the inequality with respect to constant_level.
                    //
                    // Thus, such updates might be delayed even more.
                    if upper_bound < constant_level || pending_level >= constant_level {
                        return (
                            UnifType::Concrete {
                                typ,
                                var_levels_data: VarLevelsData {
                                    upper_bound: pending_level,
                                    pending: Some(pending_level),
                                },
                            },
                            true,
                        );
                    }

                    let typ = if upper_bound > pending_level {
                        update_unr_with_lvl(table, typ, pending_level)
                    } else {
                        typ
                    };

                    (
                        UnifType::Concrete {
                            typ,
                            var_levels_data: VarLevelsData {
                                upper_bound: pending_level,
                                pending: None,
                            },
                        },
                        false,
                    )
                }
                // [^ignore-no-pending-level] If there is no pending level, then this update has
                // already been handled (possibly by a forced update on an enclosing type), and
                // there's nothing to do.
                //
                // Note that this type might still contain other pending updates deeper inside, but
                // those are registered as pending updates and will be processed in any case.
                UnifType::Constant(_) | UnifType::Contract(..) | UnifType::Concrete { .. } => {
                    (uty, false)
                }
            }
        }

        let rest = std::mem::take(&mut self.pending_type_updates)
            .into_iter()
            .filter(|id| {
                // unwrap(): if a unification variable has been push on the update stack, it
                // has been been by `assign_type`, and thus MUST have been assigned to
                // something.
                let typ = self.types[*id].value.take().unwrap();
                let (new_type, delayed) = update_utype(self, typ, constant_level);
                self.types[*id].value = Some(new_type);

                delayed
            })
            .collect();

        self.pending_type_updates = rest;
    }

    /// See `force_type_updates`. Same as `force_type_updates`, but when unifying a record row
    /// unification variable.
    pub fn force_rrows_updates(&mut self, constant_level: VarLevel) {
        fn update_rrows_unr_with_lvl(
            table: &mut UnifTable,
            rrows: UnifRecordRowsUnrolling,
            level: VarLevel,
        ) -> UnifRecordRowsUnrolling {
            rrows.map_state(
                |uty, table| Box::new(update_utype_with_lvl(table, *uty, level)),
                |rrows, table| Box::new(update_rrows_with_lvl(table, *rrows, level)),
                table,
            )
        }

        fn update_erows_unr_with_lvl(
            table: &mut UnifTable,
            erows: UnifEnumRowsUnrolling,
            level: VarLevel,
        ) -> UnifEnumRowsUnrolling {
            erows.map_state(
                |uty, table| Box::new(update_utype_with_lvl(table, *uty, level)),
                |erows, table| Box::new(update_erows_with_lvl(table, *erows, level)),
                table,
            )
        }

        fn update_utype_with_lvl(
            table: &mut UnifTable,
            utype: UnifType,
            level: VarLevel,
        ) -> UnifType {
            let utype = utype.into_root(table);

            match utype {
                UnifType::Concrete {
                    typ,
                    var_levels_data,
                } => {
                    let typ = typ.map_state(
                        |uty, table| Box::new(update_utype_with_lvl(table, *uty, level)),
                        |rrows, table| update_rrows_with_lvl(table, rrows, level),
                        |erows, table| update_erows_with_lvl(table, erows, level),
                        table,
                    );

                    // See [^var-level-kinds]
                    UnifType::Concrete {
                        typ,
                        var_levels_data,
                    }
                }
                UnifType::UnifVar { .. } | UnifType::Constant(_) | UnifType::Contract(..) => utype,
            }
        }

        fn update_rrows_with_lvl(
            table: &mut UnifTable,
            rrows: UnifRecordRows,
            level: VarLevel,
        ) -> UnifRecordRows {
            let rrows = rrows.into_root(table);

            match rrows {
                UnifRecordRows::UnifVar { id, init_level } => {
                    if table.rrows[id].level > level {
                        table.rrows[id].level = level;
                    }

                    UnifRecordRows::UnifVar { id, init_level }
                }
                UnifRecordRows::Concrete {
                    rrows,
                    var_levels_data,
                } if var_levels_data.upper_bound > level => {
                    let level = var_levels_data
                        .pending
                        .map(|pending_level| max(pending_level, level))
                        .unwrap_or(level);
                    let rrows = update_rrows_unr_with_lvl(table, rrows, level);

                    UnifRecordRows::Concrete {
                        rrows,
                        var_levels_data: VarLevelsData {
                            upper_bound: level,
                            pending: None,
                        },
                    }
                }
                UnifRecordRows::Constant(_) | UnifRecordRows::Concrete { .. } => rrows,
            }
        }

        fn update_erows_with_lvl(
            table: &mut UnifTable,
            erows: UnifEnumRows,
            level: VarLevel,
        ) -> UnifEnumRows {
            let erows = erows.into_root(table);

            match erows {
                UnifEnumRows::Concrete {
                    erows,
                    var_levels_data,
                } => {
                    let erows = update_erows_unr_with_lvl(table, erows, level);

                    // See [^var-level-kinds]
                    UnifEnumRows::Concrete {
                        erows,
                        var_levels_data,
                    }
                }
                UnifEnumRows::UnifVar { .. } | UnifEnumRows::Constant(_) => erows,
            }
        }

        fn update_rrows(
            table: &mut UnifTable,
            rrows: UnifRecordRows,
            constant_level: VarLevel,
        ) -> (UnifRecordRows, bool) {
            match rrows {
                UnifRecordRows::UnifVar { .. } => {
                    // We should never end up updating the level of a unification variable, as this
                    // update is done on the spot.
                    debug_assert!(false);

                    (rrows, false)
                }
                UnifRecordRows::Concrete {
                    rrows,
                    var_levels_data:
                        VarLevelsData {
                            pending: Some(pending_level),
                            upper_bound,
                        },
                } => {
                    // See [^irrelevant-level-update]
                    if upper_bound < constant_level || pending_level >= constant_level {
                        return (
                            UnifRecordRows::Concrete {
                                rrows,
                                var_levels_data: VarLevelsData {
                                    upper_bound: pending_level,
                                    pending: Some(pending_level),
                                },
                            },
                            true,
                        );
                    }

                    let rrows = if upper_bound > pending_level {
                        update_rrows_unr_with_lvl(table, rrows, pending_level)
                    } else {
                        rrows
                    };

                    (
                        UnifRecordRows::Concrete {
                            rrows,
                            var_levels_data: VarLevelsData {
                                upper_bound: pending_level,
                                pending: None,
                            },
                        },
                        false,
                    )
                }
                // See [^ignore-no-pending-level]
                UnifRecordRows::Constant(_) | UnifRecordRows::Concrete { .. } => (rrows, false),
            }
        }

        let rest = std::mem::take(&mut self.pending_rrows_updates)
            .into_iter()
            .filter(|id| {
                // unwrap(): if a unification variable has been push on the update stack, it
                // has been been by `assign_rrows`, and thus MUST have been assigned to
                // something.
                let rrows = self.rrows[*id].value.take().unwrap();
                let (new_rrows, delay) = update_rrows(self, rrows, constant_level);
                self.rrows[*id].value = Some(new_rrows);

                delay
            })
            .collect();

        self.pending_rrows_updates = rest;
    }

    /// See `force_type_updates`. Same as `force_type_updates`, but when unifying an enum row
    /// unification variable.
    pub fn force_erows_updates(&mut self, constant_level: VarLevel) {
        fn update_rrows_unr_with_lvl(
            table: &mut UnifTable,
            rrows: UnifRecordRowsUnrolling,
            level: VarLevel,
        ) -> UnifRecordRowsUnrolling {
            rrows.map_state(
                |uty, table| Box::new(update_utype_with_lvl(table, *uty, level)),
                |rrows, table| Box::new(update_rrows_with_lvl(table, *rrows, level)),
                table,
            )
        }

        fn update_erows_unr_with_lvl(
            table: &mut UnifTable,
            erows: UnifEnumRowsUnrolling,
            level: VarLevel,
        ) -> UnifEnumRowsUnrolling {
            erows.map_state(
                |uty, table| Box::new(update_utype_with_lvl(table, *uty, level)),
                |erows, table| Box::new(update_erows_with_lvl(table, *erows, level)),
                table,
            )
        }

        fn update_utype_with_lvl(
            table: &mut UnifTable,
            utype: UnifType,
            level: VarLevel,
        ) -> UnifType {
            let utype = utype.into_root(table);

            match utype {
                UnifType::Concrete {
                    typ,
                    var_levels_data,
                } => {
                    let typ = typ.map_state(
                        |uty, table| Box::new(update_utype_with_lvl(table, *uty, level)),
                        |rrows, table| update_rrows_with_lvl(table, rrows, level),
                        |erows, table| update_erows_with_lvl(table, erows, level),
                        table,
                    );

                    // See [^var-level-kinds]
                    UnifType::Concrete {
                        typ,
                        var_levels_data,
                    }
                }
                UnifType::UnifVar { .. } | UnifType::Constant(_) | UnifType::Contract(..) => utype,
            }
        }

        fn update_rrows_with_lvl(
            table: &mut UnifTable,
            rrows: UnifRecordRows,
            level: VarLevel,
        ) -> UnifRecordRows {
            let rrows = rrows.into_root(table);

            match rrows {
                UnifRecordRows::Concrete {
                    rrows,
                    var_levels_data,
                } => {
                    let rrows = update_rrows_unr_with_lvl(table, rrows, level);

                    // See [^var-level-kinds]
                    UnifRecordRows::Concrete {
                        rrows,
                        var_levels_data,
                    }
                }
                UnifRecordRows::UnifVar { .. } | UnifRecordRows::Constant(_) => rrows,
            }
        }

        fn update_erows_with_lvl(
            table: &mut UnifTable,
            erows: UnifEnumRows,
            level: VarLevel,
        ) -> UnifEnumRows {
            let erows = erows.into_root(table);

            match erows {
                UnifEnumRows::UnifVar { id, init_level } => {
                    if table.erows[id].level > level {
                        table.erows[id].level = level;
                    }

                    UnifEnumRows::UnifVar { id, init_level }
                }
                UnifEnumRows::Concrete {
                    erows,
                    var_levels_data,
                } if var_levels_data.upper_bound > level => {
                    let level = var_levels_data
                        .pending
                        .map(|pending_level| max(pending_level, level))
                        .unwrap_or(level);
                    let erows = update_erows_unr_with_lvl(table, erows, level);

                    UnifEnumRows::Concrete {
                        erows,
                        var_levels_data: VarLevelsData {
                            upper_bound: level,
                            pending: None,
                        },
                    }
                }
                UnifEnumRows::Constant(_) | UnifEnumRows::Concrete { .. } => erows,
            }
        }

        fn update_erows(
            table: &mut UnifTable,
            erows: UnifEnumRows,
            constant_level: VarLevel,
        ) -> (UnifEnumRows, bool) {
            match erows {
                UnifEnumRows::UnifVar { .. } => {
                    // We should never end up updating the level of a unification variable, as this
                    // update is done on the spot.
                    debug_assert!(false);

                    (erows, false)
                }
                UnifEnumRows::Concrete {
                    erows,
                    var_levels_data:
                        VarLevelsData {
                            pending: Some(pending_level),
                            upper_bound,
                        },
                } => {
                    // See [^irrelevant-level-update]
                    if upper_bound < constant_level || pending_level >= constant_level {
                        return (
                            UnifEnumRows::Concrete {
                                erows,
                                var_levels_data: VarLevelsData {
                                    upper_bound: pending_level,
                                    pending: Some(pending_level),
                                },
                            },
                            true,
                        );
                    }

                    let erows = if upper_bound > pending_level {
                        update_erows_unr_with_lvl(table, erows, pending_level)
                    } else {
                        erows
                    };

                    (
                        UnifEnumRows::Concrete {
                            erows,
                            var_levels_data: VarLevelsData {
                                upper_bound: pending_level,
                                pending: None,
                            },
                        },
                        false,
                    )
                }
                // See [^ignore-no-pending-level]
                UnifEnumRows::Constant(_) | UnifEnumRows::Concrete { .. } => (erows, false),
            }
        }

        let rest = std::mem::take(&mut self.pending_erows_updates)
            .into_iter()
            .filter(|id| {
                // unwrap(): if a unification variable has been pushed on the update stack, it must
                // have been done by `assign_erows`, and thus MUST have been assigned to something.
                let erows = self.erows[*id].value.take().unwrap();
                let (new_erows, delay) = update_erows(self, erows, constant_level);
                self.erows[*id].value = Some(new_erows);

                delay
            })
            .collect();

        self.pending_erows_updates = rest;
    }
}

/// Row constraints.
///
/// A row constraint applies to a unification variable appearing inside a row type (such as `r` in
/// `{ someId: SomeType ; r }` or `[| 'Foo Number, 'Baz; r |]`). A row constraint is a set of
/// identifiers that said row must NOT contain, to forbid ill-formed types with multiple
/// declaration of the same id, for example `{ a: Number, a: String}` or `[| 'Foo String, 'Foo
/// Number |]`.
///
/// Note that because the syntax (and pattern matching likewise) distinguishes between `'Foo` and
/// `'Foo some_arg`, the type `[| 'Foo, 'Foo SomeType |]` is unproblematic for typechecking. In
/// some sense, enum tags and enum variants live in a different dimension. It looks like we should
/// use separate sets of constraints for enum tag constraints and enum variants constraints. But a
/// set just for enum tag constraints is useless, because enum tags can never conflict, as they
/// don't have any argument: `'Foo` always "agrees with" another `'Foo` definition. In consequence,
/// we simply record enum variants constraints and ignore enum tags.
///
/// Note that a `VarId` always refer to either a type unification variable, a record row
/// unification variable or an enum row unification variable. Thus, we can use a single constraint
/// set per variable id (which isn't used at all for type unification variables). Because we expect
/// the map to be rather sparse, we use a `HashMap` instead of a `Vec`.
pub type RowConstrs = HashMap<VarId, HashSet<Ident>>;

trait PropagateConstrs {
    /// Check that unifying a variable with a type doesn't violate rows constraints, and update the
    /// row constraints of the unified type accordingly if needed.
    ///
    /// When a unification variable `UnifVar(p)` is unified with a type `uty` which is either a row type
    /// or another unification variable which could be later unified with a row type itself, the
    /// following operations are required:
    ///
    /// 1. If `uty` is a concrete row, check that it doesn't contain an identifier which is forbidden by
    ///    a row constraint on `p`.
    /// 2. If `uty` is either a unification variable `u` or a row type ending with a unification
    ///    variable `u`, we must add the constraints of `p` to the constraints of `u`. Indeed, take the
    ///    following situation: `p` appears in a row type `{a: Number ; p}`, hence has a constraint that
    ///    it must not contain a field `a`. Then `p` is unified with a fresh type variable `u`. If we
    ///    don't constrain `u`, `u` could be unified later with a row type `{a : String}` which violates
    ///    the original constraint on `p`. Thus, when unifying `p` with `u` or a row ending with `u`,
    ///    `u` must inherit all the constraints of `p`.
    fn propagate_constrs(&self, constr: &mut RowConstrs, var_id: VarId)
        -> Result<(), RowUnifError>;
}

impl PropagateConstrs for UnifRecordRows {
    fn propagate_constrs(
        &self,
        constr: &mut RowConstrs,
        var_id: VarId,
    ) -> Result<(), RowUnifError> {
        fn propagate(
            constr: &mut RowConstrs,
            var_id: VarId,
            var_constr: HashSet<Ident>,
            rrows: &UnifRecordRows,
        ) -> Result<(), RowUnifError> {
            match rrows {
                UnifRecordRows::Concrete {
                    rrows: RecordRowsF::Extend { row, .. },
                    ..
                } if var_constr.contains(&row.id.ident()) => {
                    Err(RowUnifError::RecordRowConflict(row.clone()))
                }
                UnifRecordRows::Concrete {
                    rrows: RecordRowsF::Extend { tail, .. },
                    ..
                } => propagate(constr, var_id, var_constr, tail),
                UnifRecordRows::UnifVar { id, .. } if *id != var_id => {
                    if let Some(tail_constr) = constr.get_mut(id) {
                        tail_constr.extend(var_constr);
                    } else {
                        constr.insert(*id, var_constr);
                    }

                    Ok(())
                }
                _ => Ok(()),
            }
        }

        if let Some(var_constr) = constr.remove(&var_id) {
            propagate(constr, var_id, var_constr, self)
        } else {
            Ok(())
        }
    }
}

impl PropagateConstrs for UnifEnumRows {
    fn propagate_constrs(
        &self,
        constr: &mut RowConstrs,
        var_id: VarId,
    ) -> Result<(), RowUnifError> {
        fn propagate(
            constr: &mut RowConstrs,
            var_id: VarId,
            var_constr: HashSet<Ident>,
            erows: &UnifEnumRows,
        ) -> Result<(), RowUnifError> {
            match erows {
                UnifEnumRows::Concrete {
                    // If the row is an enum tag (ie `typ` is `None`), it can't cause any conflict.
                    // See [RowConstrs] for more details.
                    erows:
                        EnumRowsF::Extend {
                            row:
                                row @ UnifEnumRow {
                                    id: _,
                                    typ: Some(_),
                                },
                            ..
                        },
                    ..
                } if var_constr.contains(&row.id.ident()) => {
                    Err(RowUnifError::EnumRowConflict(row.clone()))
                }
                UnifEnumRows::Concrete {
                    erows: EnumRowsF::Extend { tail, .. },
                    ..
                } => propagate(constr, var_id, var_constr, tail),
                UnifEnumRows::UnifVar { id, .. } if *id != var_id => {
                    if let Some(tail_constr) = constr.get_mut(id) {
                        tail_constr.extend(var_constr);
                    } else {
                        constr.insert(*id, var_constr);
                    }

                    Ok(())
                }
                _ => Ok(()),
            }
        }

        if let Some(var_constr) = constr.remove(&var_id) {
            propagate(constr, var_id, var_constr, self)
        } else {
            Ok(())
        }
    }
}

/// Types which can be unified.
pub(super) trait Unify {
    type Error;

    /// Try to unify two types. Unification corresponds to imposing an equality constraints on
    /// those types. This can fail if the types can't be matched.
    fn unify(self, t2: Self, state: &mut State, ctxt: &Context) -> Result<(), Self::Error>;
}

impl Unify for UnifType {
    type Error = UnifError;

    fn unify(self, t2: UnifType, state: &mut State, ctxt: &Context) -> Result<(), UnifError> {
        let t1 = self.into_root(state.table);
        let t2 = t2.into_root(state.table);

        // t1 and t2 are roots of the type
        match (t1, t2) {
            // If either type is a wildcard, unify with the associated type var
            (
                UnifType::Concrete {
                    typ: TypeF::Wildcard(id),
                    ..
                },
                ty2,
            )
            | (
                ty2,
                UnifType::Concrete {
                    typ: TypeF::Wildcard(id),
                    ..
                },
            ) => {
                let ty1 = get_wildcard_var(state.table, ctxt.var_level, state.wildcard_vars, id);
                ty1.unify(ty2, state, ctxt)
            }
            (
                UnifType::Concrete {
                    typ: s1,
                    var_levels_data: _,
                },
                UnifType::Concrete {
                    typ: s2,
                    var_levels_data: _,
                },
            ) => match (s1, s2) {
                (TypeF::Dyn, TypeF::Dyn)
                | (TypeF::Number, TypeF::Number)
                | (TypeF::Bool, TypeF::Bool)
                | (TypeF::String, TypeF::String)
                | (TypeF::Symbol, TypeF::Symbol) => Ok(()),
                (TypeF::Array(uty1), TypeF::Array(uty2)) => uty1.unify(*uty2, state, ctxt),
                (TypeF::Arrow(s1s, s1t), TypeF::Arrow(s2s, s2t)) => {
                    s1s.clone()
                        .unify((*s2s).clone(), state, ctxt)
                        .map_err(|err| UnifError::DomainMismatch {
                            expected: UnifType::concrete(TypeF::Arrow(s1s.clone(), s1t.clone())),
                            inferred: UnifType::concrete(TypeF::Arrow(s2s.clone(), s2t.clone())),
                            cause: Box::new(err),
                        })?;
                    s1t.clone()
                        .unify((*s2t).clone(), state, ctxt)
                        .map_err(|err| UnifError::CodomainMismatch {
                            expected: UnifType::concrete(TypeF::Arrow(s1s, s1t)),
                            inferred: UnifType::concrete(TypeF::Arrow(s2s, s2t)),
                            cause: Box::new(err),
                        })
                }
                (TypeF::Flat(expected), TypeF::Flat(inferred)) => {
                    Err(UnifError::IncomparableFlatTypes { expected, inferred })
                }
                (TypeF::Enum(erows1), TypeF::Enum(erows2)) => erows1
                    .clone()
                    .unify(erows2.clone(), state, ctxt)
                    .map_err(|err| {
                        err.into_unif_err(mk_uty_enum!(; erows1), mk_uty_enum!(; erows2))
                    }),
                (TypeF::Record(rrows1), TypeF::Record(rrows2)) => rrows1
                    .clone()
                    .unify(rrows2.clone(), state, ctxt)
                    .map_err(|err| {
                        err.into_unif_err(mk_uty_record!(; rrows1), mk_uty_record!(; rrows2))
                    }),
                (
                    TypeF::Dict {
                        type_fields: uty1, ..
                    },
                    TypeF::Dict {
                        type_fields: uty2, ..
                    },
                ) => uty1.unify(*uty2, state, ctxt),
                (
                    TypeF::Forall {
                        var: var1,
                        var_kind: var_kind1,
                        body: body1,
                    },
                    TypeF::Forall {
                        var: var2,
                        var_kind: var_kind2,
                        body: body2,
                    },
                ) if var_kind1 == var_kind2 => {
                    // Very stupid (slow) implementation
                    let (substd1, substd2) = match var_kind1 {
                        VarKind::Type => {
                            let constant_type = state.table.fresh_type_const(ctxt.var_level);
                            (
                                body1.subst(&var1, &constant_type),
                                body2.subst(&var2, &constant_type),
                            )
                        }
                        VarKind::RecordRows { .. } => {
                            let constant_type = state.table.fresh_rrows_const(ctxt.var_level);
                            (
                                body1.subst(&var1, &constant_type),
                                body2.subst(&var2, &constant_type),
                            )
                        }
                        VarKind::EnumRows { .. } => {
                            let constant_type = state.table.fresh_erows_const(ctxt.var_level);
                            (
                                body1.subst(&var1, &constant_type),
                                body2.subst(&var2, &constant_type),
                            )
                        }
                    };

                    substd1.unify(substd2, state, ctxt)
                }
                (TypeF::Var(ident), _) | (_, TypeF::Var(ident)) => {
                    Err(UnifError::UnboundTypeVariable(ident.into()))
                }
                (ty1, ty2) => Err(UnifError::TypeMismatch {
                    expected: UnifType::concrete(ty1),
                    inferred: UnifType::concrete(ty2),
                }),
            },
            (UnifType::UnifVar { id, .. }, uty) | (uty, UnifType::UnifVar { id, .. }) => {
                // [^check-unif-var-level]: If we are unifying a variable with a rigid type
                // variable, force potential unification variable level updates and check that the
                // level of the unification variable is greater or equals to the constant: that is,
                // that the variable doesn't "escape its scope". This is required to handle
                // polymorphism soundly, and is the whole point of all the machinery around variable
                // levels.
                if let UnifType::Constant(cst_id) = uty {
                    let constant_level = state.table.get_level(cst_id);
                    state.table.force_type_updates(constant_level);

                    if state.table.get_level(id) < constant_level {
                        return Err(UnifError::VarLevelMismatch {
                            constant_id: cst_id,
                            var_kind: VarKindDiscriminant::Type,
                        });
                    }
                }

                state.table.assign_type(id, uty);
                Ok(())
            }
            (UnifType::Constant(i1), UnifType::Constant(i2)) if i1 == i2 => Ok(()),
            (UnifType::Constant(i1), UnifType::Constant(i2)) => Err(UnifError::ConstMismatch {
                var_kind: VarKindDiscriminant::Type,
                expected_const_id: i1,
                inferred_const_id: i2,
            }),
            (ty, UnifType::Constant(i)) | (UnifType::Constant(i), ty) => {
                Err(UnifError::WithConst {
                    var_kind: VarKindDiscriminant::Type,
                    expected_const_id: i,
                    inferred: ty,
                })
            }
            (UnifType::Contract(t1, env1), UnifType::Contract(t2, env2))
                if eq::contract_eq(state.table.max_uvars_count(), &t1, &env1, &t2, &env2) =>
            {
                Ok(())
            }
            (uty1 @ UnifType::Contract(..), uty2) | (uty1, uty2 @ UnifType::Contract(..)) => {
                Err(UnifError::TypeMismatch {
                    expected: uty1,
                    inferred: uty2,
                })
            }
        }
    }
}

impl Unify for UnifEnumRows {
    type Error = RowUnifError;

    fn unify(
        self,
        uerows2: UnifEnumRows,
        state: &mut State,
        ctxt: &Context,
    ) -> Result<(), RowUnifError> {
        let uerows1 = self.into_root(state.table);
        let uerows2 = uerows2.into_root(state.table);

        match (uerows1, uerows2) {
            (
                UnifEnumRows::Concrete {
                    erows: erows1,
                    var_levels_data: _,
                },
                UnifEnumRows::Concrete {
                    erows: erows2,
                    var_levels_data: var_levels2,
                },
            ) => match (erows1, erows2) {
                (EnumRowsF::TailVar(id), _) | (_, EnumRowsF::TailVar(id)) => {
                    Err(RowUnifError::UnboundTypeVariable(id))
                }
                (EnumRowsF::Empty, EnumRowsF::Empty) => Ok(()),
                (
                    EnumRowsF::Empty,
                    EnumRowsF::Extend {
                        row: UnifEnumRow { id, .. },
                        ..
                    },
                ) => Err(RowUnifError::ExtraRow(id)),
                (
                    EnumRowsF::Extend {
                        row: UnifEnumRow { id, .. },
                        ..
                    },
                    EnumRowsF::Empty,
                ) => Err(RowUnifError::MissingRow(id)),
                (EnumRowsF::Extend { row, tail }, erows2 @ EnumRowsF::Extend { .. }) => {
                    let uerows2 = UnifEnumRows::Concrete {
                        erows: erows2,
                        var_levels_data: var_levels2,
                    };

                    let (ty2_result, t2_without_row) =
                        //TODO[adts]: it's ugly to create a temporary Option just to please the
                        //Box/Nobox types, we should find a better signature for remove_row
                        uerows2.remove_row(&row.id, &row.typ.clone().map(|typ| *typ), state, ctxt.var_level).map_err(|err| match err {
                            RemoveRowError::Missing => RowUnifError::MissingRow(row.id),
                            RemoveRowError::Conflict => RowUnifError::EnumRowConflict(row.clone()),
                        })?;

                    // The alternative to this if-condition is `RemoveRowResult::Extended`, which
                    // means that `t2` could be successfully extended with the row `id typ`, in
                    // which case we don't have to perform additional unification for this specific
                    // row
                    if let RemoveRowResult::Extracted(ty2) = ty2_result {
                        match (row.typ, ty2) {
                            (Some(typ), Some(ty2)) => {
                                typ.unify(ty2, state, ctxt).map_err(|err| {
                                    RowUnifError::EnumRowMismatch {
                                        id: row.id,
                                        cause: Some(Box::new(err)),
                                    }
                                })?;
                            }
                            (Some(_), None) | (None, Some(_)) => {
                                return Err(RowUnifError::EnumRowMismatch {
                                    id: row.id,
                                    cause: None,
                                });
                            }
                            (None, None) => (),
                        }
                    }

                    tail.unify(t2_without_row, state, ctxt)
                }
            },
            (UnifEnumRows::UnifVar { id, init_level: _ }, uerows)
            | (uerows, UnifEnumRows::UnifVar { id, init_level: _ }) => {
                // see [^check-unif-var-level]
                if let UnifEnumRows::Constant(cst_id) = uerows {
                    let constant_level = state.table.get_erows_level(cst_id);
                    state.table.force_erows_updates(constant_level);

                    if state.table.get_erows_level(id) < constant_level {
                        return Err(RowUnifError::VarLevelMismatch {
                            constant_id: cst_id,
                            var_kind: VarKindDiscriminant::EnumRows,
                        });
                    }
                }

                uerows.propagate_constrs(state.constr, id)?;
                state.table.assign_erows(id, uerows);
                Ok(())
            }
            (UnifEnumRows::Constant(i1), UnifEnumRows::Constant(i2)) if i1 == i2 => Ok(()),
            (UnifEnumRows::Constant(i1), UnifEnumRows::Constant(i2)) => {
                Err(RowUnifError::ConstMismatch {
                    var_kind: VarKindDiscriminant::EnumRows,
                    expected_const_id: i1,
                    inferred_const_id: i2,
                })
            }
            (uerows, UnifEnumRows::Constant(i)) | (UnifEnumRows::Constant(i), uerows) => {
                //TODO ROWS: should we refactor RowUnifError as well?
                Err(RowUnifError::WithConst {
                    var_kind: VarKindDiscriminant::EnumRows,
                    expected_const_id: i,
                    inferred: UnifType::concrete(TypeF::Enum(uerows)),
                })
            }
        }
    }
}

impl Unify for UnifRecordRows {
    type Error = RowUnifError;

    fn unify(
        self,
        urrows2: UnifRecordRows,
        state: &mut State,
        ctxt: &Context,
    ) -> Result<(), RowUnifError> {
        let urrows1 = self.into_root(state.table);
        let urrows2 = urrows2.into_root(state.table);

        match (urrows1, urrows2) {
            (
                UnifRecordRows::Concrete {
                    rrows: rrows1,
                    var_levels_data: _,
                },
                UnifRecordRows::Concrete {
                    rrows: rrows2,
                    var_levels_data: var_levels2,
                },
            ) => match (rrows1, rrows2) {
                (RecordRowsF::TailVar(id), _) | (_, RecordRowsF::TailVar(id)) => {
                    Err(RowUnifError::UnboundTypeVariable(id))
                }
                (RecordRowsF::Empty, RecordRowsF::Empty)
                | (RecordRowsF::TailDyn, RecordRowsF::TailDyn) => Ok(()),
                (RecordRowsF::Empty, RecordRowsF::TailDyn) => Err(RowUnifError::ExtraDynTail),
                (RecordRowsF::TailDyn, RecordRowsF::Empty) => Err(RowUnifError::MissingDynTail),
                (
                    RecordRowsF::Empty,
                    RecordRowsF::Extend {
                        row: UnifRecordRow { id, .. },
                        ..
                    },
                )
                | (
                    RecordRowsF::TailDyn,
                    RecordRowsF::Extend {
                        row: UnifRecordRow { id, .. },
                        ..
                    },
                ) => Err(RowUnifError::ExtraRow(id)),
                (
                    RecordRowsF::Extend {
                        row: UnifRecordRow { id, .. },
                        ..
                    },
                    RecordRowsF::TailDyn,
                )
                | (
                    RecordRowsF::Extend {
                        row: UnifRecordRow { id, .. },
                        ..
                    },
                    RecordRowsF::Empty,
                ) => Err(RowUnifError::MissingRow(id)),
                (RecordRowsF::Extend { row, tail }, rrows2 @ RecordRowsF::Extend { .. }) => {
                    let urrows2 = UnifRecordRows::Concrete {
                        rrows: rrows2,
                        var_levels_data: var_levels2,
                    };

                    let (ty2_result, urrows2_without_ty2) = urrows2
                        .remove_row(&row.id, &row.typ, state, ctxt.var_level)
                        .map_err(|err| match err {
                            RemoveRowError::Missing => RowUnifError::MissingRow(row.id),
                            RemoveRowError::Conflict => {
                                RowUnifError::RecordRowConflict(row.clone())
                            }
                        })?;

                    // The alternative to this if-condition is `RemoveRowResult::Extended`, which
                    // means that `t2` could be successfully extended with the row `id typ`, in
                    // which case we don't have to perform additional unification for this specific
                    // row
                    if let RemoveRowResult::Extracted(ty2) = ty2_result {
                        row.typ.unify(ty2, state, ctxt).map_err(|err| {
                            RowUnifError::RecordRowMismatch {
                                id: row.id,
                                cause: Box::new(err),
                            }
                        })?;
                    }

                    tail.unify(urrows2_without_ty2, state, ctxt)
                }
            },
            (UnifRecordRows::UnifVar { id, init_level: _ }, urrows)
            | (urrows, UnifRecordRows::UnifVar { id, init_level: _ }) => {
                // see [^check-unif-var-level]
                if let UnifRecordRows::Constant(cst_id) = urrows {
                    let constant_level = state.table.get_rrows_level(cst_id);
                    state.table.force_rrows_updates(constant_level);

                    if state.table.get_rrows_level(id) < constant_level {
                        return Err(RowUnifError::VarLevelMismatch {
                            constant_id: cst_id,
                            var_kind: VarKindDiscriminant::RecordRows,
                        });
                    }
                }

                urrows.propagate_constrs(state.constr, id)?;
                state.table.assign_rrows(id, urrows);
                Ok(())
            }
            (UnifRecordRows::Constant(i1), UnifRecordRows::Constant(i2)) if i1 == i2 => Ok(()),
            (UnifRecordRows::Constant(i1), UnifRecordRows::Constant(i2)) => {
                Err(RowUnifError::ConstMismatch {
                    var_kind: VarKindDiscriminant::RecordRows,
                    expected_const_id: i1,
                    inferred_const_id: i2,
                })
            }
            (urrows, UnifRecordRows::Constant(i)) | (UnifRecordRows::Constant(i), urrows) => {
                Err(RowUnifError::WithConst {
                    var_kind: VarKindDiscriminant::RecordRows,
                    expected_const_id: i,
                    inferred: UnifType::concrete(TypeF::Record(urrows)),
                })
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum RemoveRowError {
    // The row to add was missing and the row type was closed (no free unification variable in tail
    // position).
    Missing,
    // The row to add was missing and the row type couldn't be extended because of row constraints.
    Conflict,
}

#[derive(Clone, Debug)]
pub enum RemoveRowResult<RowContent: Clone> {
    Extracted(RowContent),
    Extended,
}

trait RemoveRow: Sized {
    /// The row data minus the identifier.
    type RowContent: Clone;

    /// Fetch a specific `row_id` from a row type, and return the content of the row together with
    /// the original row type without the found row.
    ///
    /// If the searched row isn't found:
    /// - If the row type is extensible, i.e. it ends with a free unification variable in tail
    ///   position, this function adds the missing row (with `row.types` as a type for record rows,
    ///   if allowed by row constraints) and then acts as if `remove_row` was called again on
    ///   this extended row type. That is, `remove_row` returns the new row and the extended type
    ///   without the added row).
    /// - Otherwise, raise a missing row error.
    ///
    /// # Motivation
    ///
    /// This method is used as part of row unification: let's say we want to unify `{ r1, ..tail1
    /// }` with `{ ..tail2 }` where `r1` is a row (the head of the left hand side rows), and
    /// `tail1` and `tail2` are sequences of rows.
    ///
    /// For those to unify, we must have either:
    ///
    ///  - `r1` is somewhere in `tail2`, and `tail1` unifies with `{..tail2'}` where `tail2'` is
    ///  `tail2` without `r1`.
    ///  - `tail2` is extensible, in which case we can extend `tail2` with `r1`, assuming that
    ///  `tail1` unifies with `{..tail2'}`, where `tail2'` is `tail2` after extending with `r1` and
    ///  then removing it. Modulo fresh unification variable shuffling, `tail2'` is in fact
    ///  isomorphic to `tail2` before it was extended.
    ///
    /// When we unify two row types, we destructure the left hand side to extract the head `r1` and
    /// the tail `tail1`. Then, we try to find and extract `r1` from `tail2`. If `r1` was found, we
    /// additionally unify the extracted type found in `tail2` (returned as part of
    /// [RemoveRowResult::Extracted]) with `r1.typ` to make sure they agree. In case of extension,
    /// we were free to chose the type of the new added row, which we set to be `r1.typ` (the
    /// `row_content` parameter of `remove_row`), and there's no additional check to perform (and
    /// indeed [RemoveRowResult::Extended] doesn't carry any information).
    ///
    /// Finally, since `remove_row` returns the initial row type minus the extracted row, we can go
    /// on recursively and unify `tail1` with this rest.
    ///
    /// # Parameters
    ///
    /// - `row_id`: the identifier of the row to extract
    /// - `row_content`: as explained above, `remove_row` is used in the context of unifying two row
    ///   types. If `self` doesn't contain `row_id` but is extensible, we must add a corresponding
    ///   new row: we fill it with `row_content`. In the context of unification, the is the content of
    ///   the row coming from the other row type.
    /// - `state`: the unification state
    /// - `var_level`: the ambient variable level
    fn remove_row(
        self,
        row_id: &LocIdent,
        row_content: &Self::RowContent,
        state: &mut State,
        var_level: VarLevel,
    ) -> Result<(RemoveRowResult<Self::RowContent>, Self), RemoveRowError>;
}

impl RemoveRow for UnifRecordRows {
    type RowContent = UnifType;

    fn remove_row(
        self,
        target: &LocIdent,
        target_content: &Self::RowContent,
        state: &mut State,
        var_level: VarLevel,
    ) -> Result<(RemoveRowResult<Self::RowContent>, UnifRecordRows), RemoveRowError> {
        let rrows = self.into_root(state.table);

        match rrows {
            UnifRecordRows::Concrete { rrows, .. } => match rrows {
                RecordRowsF::Empty | RecordRowsF::TailDyn | RecordRowsF::TailVar(_) => {
                    Err(RemoveRowError::Missing)
                }
                RecordRowsF::Extend {
                    row: next_row,
                    tail,
                } => {
                    if target.ident() == next_row.id.ident() {
                        Ok((RemoveRowResult::Extracted(*next_row.typ), *tail))
                    } else {
                        let (extracted_row, rest) =
                            tail.remove_row(target, target_content, state, var_level)?;
                        Ok((
                            extracted_row,
                            UnifRecordRows::concrete(RecordRowsF::Extend {
                                row: next_row,
                                tail: Box::new(rest),
                            }),
                        ))
                    }
                }
            },
            UnifRecordRows::UnifVar { id: var_id, .. } => {
                let tail_var_id = state.table.fresh_rrows_var_id(var_level);
                // We have to manually insert the constraint that `tail_var_id` can't contain a row
                // `target`, to avoid producing ill-formed record rows later
                state
                    .constr
                    .insert(tail_var_id, HashSet::from([target.ident()]));

                let row_to_insert = UnifRecordRow {
                    id: *target,
                    typ: Box::new(target_content.clone()),
                };

                let tail_var = UnifRecordRows::UnifVar {
                    id: tail_var_id,
                    init_level: var_level,
                };

                let tail_extended = UnifRecordRows::concrete(RecordRowsF::Extend {
                    row: row_to_insert,
                    tail: Box::new(tail_var.clone()),
                });

                tail_extended
                    .propagate_constrs(state.constr, var_id)
                    .map_err(|_| RemoveRowError::Conflict)?;
                state.table.assign_rrows(var_id, tail_extended);

                Ok((RemoveRowResult::Extended, tail_var))
            }
            UnifRecordRows::Constant(_) => Err(RemoveRowError::Missing),
        }
    }
}

impl RemoveRow for UnifEnumRows {
    type RowContent = Option<UnifType>;

    fn remove_row(
        self,
        target: &LocIdent,
        target_content: &Self::RowContent,
        state: &mut State,
        var_level: VarLevel,
    ) -> Result<(RemoveRowResult<Self::RowContent>, UnifEnumRows), RemoveRowError> {
        let uerows = self.into_root(state.table);

        match uerows {
            UnifEnumRows::Concrete { erows, .. } => match erows {
                EnumRowsF::Empty | EnumRowsF::TailVar(_) => Err(RemoveRowError::Missing),
                EnumRowsF::Extend {
                    row: next_row,
                    tail,
                } => {
                    // Enum variants and enum tags don't conflict, and can thus coexist in the same
                    // row type (for example, [| 'Foo Number, 'Foo |]). In some sense, they live
                    // inside different dimensions. Thus, when matching rows, we don't only compare
                    // the tag but also the nature of the enum row (tag vs variant)
                    if target.ident() == next_row.id.ident()
                        && target_content.is_some() == next_row.typ.is_some()
                    {
                        Ok((
                            RemoveRowResult::Extracted(next_row.typ.map(|typ| *typ)),
                            *tail,
                        ))
                    } else {
                        let (extracted_row, rest) =
                            tail.remove_row(target, target_content, state, var_level)?;
                        Ok((
                            extracted_row,
                            UnifEnumRows::concrete(EnumRowsF::Extend {
                                row: next_row,
                                tail: Box::new(rest),
                            }),
                        ))
                    }
                }
            },
            UnifEnumRows::UnifVar { id: var_id, .. } => {
                let tail_var_id = state.table.fresh_erows_var_id(var_level);

                // Enum tag are ignored for row conflict. See [RowConstrs]
                if target_content.is_some() {
                    state
                        .constr
                        .insert(tail_var_id, HashSet::from([target.ident()]));
                }

                let row_to_insert = UnifEnumRow {
                    id: *target,
                    typ: target_content.clone().map(Box::new),
                };

                let tail_var = UnifEnumRows::UnifVar {
                    id: tail_var_id,
                    init_level: var_level,
                };

                let tail_extended = UnifEnumRows::concrete(EnumRowsF::Extend {
                    row: row_to_insert,
                    tail: Box::new(tail_var.clone()),
                });

                tail_extended
                    .propagate_constrs(state.constr, var_id)
                    .map_err(|_| RemoveRowError::Conflict)?;
                state.table.assign_erows(var_id, tail_extended);

                Ok((RemoveRowResult::Extended, tail_var))
            }
            UnifEnumRows::Constant(_) => Err(RemoveRowError::Missing),
        }
    }
}
