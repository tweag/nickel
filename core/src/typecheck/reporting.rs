//! Helpers to convert a `TypeWrapper` to a human-readable `Type` representation for error
//! reporting.
use super::*;

/// A name registry used to replace unification variables and type constants with human-readable
/// and distinct names.
pub struct NameReg {
    /// Currently allocated names, including both variables written by the user and generated
    /// names.
    names: NameTable,
    /// A reverse name table, always kept in sync with `names`, in order to efficiently check if a
    /// name is already taken.
    taken: HashSet<Ident>,
    /// Counter used to generate fresh letters for unification variables.
    var_count: usize,
    /// Counter used to generate fresh letters for type constants.
    cst_count: usize,
}

impl NameReg {
    /// Create a new registry from an initial table corresponding to user-written type constants.
    pub fn new(names: NameTable) -> Self {
        let taken = names.values().copied().collect();

        NameReg {
            names,
            taken,
            var_count: 0,
            cst_count: 0,
        }
    }

    pub fn taken(&self, name: &str) -> bool {
        self.taken.contains(&name.into())
    }

    fn insert(&mut self, var_id: VarId, discriminant: VarKindDiscriminant, name: Ident) {
        self.names.insert((var_id, discriminant), name);
        self.taken.insert(name);
    }

    /// Create a fresh name candidate for a type variable or a type constant.
    ///
    /// Used to convert a unification type to a human-readable representation.
    ///
    /// To select a candidate, first check in `names` if the variable or the constant corresponds
    /// to a type variable written by the user. If it is, return the name of the variable.
    /// Otherwise, use the given counter to generate a new single letter.
    ///
    /// A generated name is clearly not necessarily unique. [`select_uniq`] must then be applied.
    fn gen_candidate_name(
        names: &NameTable,
        counter: &mut usize,
        id: VarId,
        kind: VarKindDiscriminant,
    ) -> String {
        match names.get(&(id, kind)) {
            // First check if that constant or variable was introduced by a forall. If it was, try
            // to use the same name.
            Some(orig) => format!("{orig}"),
            None => {
                //Otherwise, generate a new character
                let next = *counter;
                *counter += 1;

                let prefix = match kind {
                    VarKindDiscriminant::Type => "",
                    VarKindDiscriminant::EnumRows => "erows_",
                    VarKindDiscriminant::RecordRows => "rrows_",
                };
                let character = std::char::from_u32(('a' as u32) + ((next % 26) as u32)).unwrap();
                format!("{prefix}{character}")
            }
        }
    }

    /// Select a name distinct from all the others, starting from a candidate name for a type
    /// variable or a type constant. Insert the corresponding name in the name table.
    ///
    /// If the name is already taken, it just iterates by adding a numeric suffix `1`, `2`, .., and
    /// so on until a free name is found. See `var_to_type` and `cst_to_type`.
    fn select_uniq(&mut self, mut name: String, id: VarId, kind: VarKindDiscriminant) -> Ident {
        // To avoid clashing with already picked names, we add a numeric suffix to the picked
        // letter.
        if self.taken(&name) {
            let mut suffix = 1;

            name = format!("{name}{suffix}");
            while self.taken(&name) {
                suffix += 1;
            }
        }

        let sym = Ident::from(name);
        self.insert(id, kind, sym);
        sym
    }

    /// Either retrieve or generate a new fresh name for a unification variable for error reporting,
    /// and wrap it as an identifier. Unification variables are named `_a`, `_b`, .., `_a1`, `_b1`,
    /// .. and so on.
    pub fn gen_var_name(&mut self, id: VarId, kind: VarKindDiscriminant) -> Ident {
        self.names.get(&(id, kind)).cloned().unwrap_or_else(|| {
            // Select a candidate name and add a "_" prefix
            let candidate = format!(
                "_{}",
                Self::gen_candidate_name(&self.names, &mut self.var_count, id, kind)
            );
            // Add a suffix to make it unique if it has already been picked
            self.select_uniq(candidate, id, kind)
        })
    }

    /// Either retrieve or generate a new fresh name for a constant for error reporting, and wrap it
    /// as type variable. Constant are named `a`, `b`, .., `a1`, `b1`, .. and so on.
    pub fn gen_cst_name(&mut self, id: VarId, kind: VarKindDiscriminant) -> Ident {
        self.names.get(&(id, kind)).cloned().unwrap_or_else(|| {
            // Select a candidate name
            let candidate = Self::gen_candidate_name(&self.names, &mut self.cst_count, id, kind);
            // Add a suffix to make it unique if it has already been picked
            self.select_uniq(candidate, id, kind)
        })
    }
}

pub trait ToType {
    /// The target type to convert to. If `Self` is `UnifXXX`, then `Target` is `XXX`.
    type Target;

    /// Extract a concrete type corresponding to a unification type for error reporting purpose,
    /// given a registry of currently allocated names.
    ///
    /// As opposed to [`crate::typ::Type::from`], free unification variables and type constants are
    /// replaced by type variables which names are determined by this name registry.
    ///
    /// When reporting error, we want to distinguish occurrences of unification variables and type
    /// constants in a human-readable way.
    fn to_type(self, reg: &mut NameReg, table: &UnifTable) -> Self::Target;
}

impl ToType for UnifType {
    type Target = Type;

    fn to_type(self, reg: &mut NameReg, table: &UnifTable) -> Self::Target {
        let ty = self.into_root(table);

        match ty {
            UnifType::UnifVar { id, .. } => {
                Type::from(TypeF::Var(reg.gen_var_name(id, VarKindDiscriminant::Type)))
            }
            UnifType::Constant(id) => {
                Type::from(TypeF::Var(reg.gen_cst_name(id, VarKindDiscriminant::Type)))
            }
            UnifType::Concrete { typ, .. } => {
                let mapped = typ.map_state(
                    |btyp, reg| Box::new(btyp.to_type(reg, table)),
                    |rrows, reg| rrows.to_type(reg, table),
                    |erows, reg| erows.to_type(reg, table),
                    reg,
                );
                Type::from(mapped)
            }
            UnifType::Contract(t, _) => Type::from(TypeF::Flat(t)),
        }
    }
}

impl ToType for UnifRecordRows {
    type Target = RecordRows;

    fn to_type(self, reg: &mut NameReg, table: &UnifTable) -> Self::Target {
        let rrows = self.into_root(table);

        match rrows {
            UnifRecordRows::UnifVar { id, .. } => RecordRows(RecordRowsF::TailVar(
                reg.gen_var_name(id, VarKindDiscriminant::RecordRows).into(),
            )),
            UnifRecordRows::Constant(id) => RecordRows(RecordRowsF::TailVar(
                reg.gen_cst_name(id, VarKindDiscriminant::RecordRows).into(),
            )),
            UnifRecordRows::Concrete { rrows, .. } => {
                let mapped = rrows.map_state(
                    |btyp, reg| Box::new(btyp.to_type(reg, table)),
                    |rrows, reg| Box::new(rrows.to_type(reg, table)),
                    reg,
                );
                RecordRows(mapped)
            }
        }
    }
}

impl ToType for UnifEnumRows {
    type Target = EnumRows;

    fn to_type(self, reg: &mut NameReg, table: &UnifTable) -> Self::Target {
        let erows = self.into_root(table);

        match erows {
            UnifEnumRows::UnifVar { id, .. } => EnumRows(EnumRowsF::TailVar(
                reg.gen_var_name(id, VarKindDiscriminant::EnumRows).into(),
            )),
            UnifEnumRows::Constant(id) => EnumRows(EnumRowsF::TailVar(
                reg.gen_cst_name(id, VarKindDiscriminant::EnumRows).into(),
            )),
            UnifEnumRows::Concrete { erows, .. } => {
                let mapped = erows.map_state(
                    |btyp, reg| Box::new(btyp.to_type(reg, table)),
                    |erows, reg| Box::new(erows.to_type(reg, table)),
                    reg,
                );
                EnumRows(mapped)
            }
        }
    }
}

impl ToType for UnifEnumRow {
    type Target = EnumRow;

    fn to_type(self, reg: &mut NameReg, table: &UnifTable) -> Self::Target {
        EnumRow {
            id: self.id,
            typ: self.typ.map(|typ| Box::new(typ.to_type(reg, table))),
        }
    }
}

impl ToType for UnifRecordRow {
    type Target = RecordRow;

    fn to_type(self, reg: &mut NameReg, table: &UnifTable) -> Self::Target {
        RecordRow {
            id: self.id,
            typ: Box::new(self.typ.to_type(reg, table)),
        }
    }
}
