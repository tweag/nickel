//! Helpers to convert a `TypeWrapper` to a human-readable `Types` representation for error
//! reporting.
use super::*;
use std::collections::HashSet;

/// A name registry used to replace unification variables and type constants with human-readable
/// and distinct names.
pub struct NameReg {
    reg: HashMap<usize, Ident>,
    taken: HashSet<String>,
    var_count: usize,
    cst_count: usize,
}

impl NameReg {
    pub fn new() -> Self {
        NameReg {
            reg: HashMap::new(),
            taken: HashSet::new(),
            var_count: 0,
            cst_count: 0,
        }
    }
}

/// Create a fresh name candidate for a type variable or a type constant.
///
/// Used by [`to_type`] and subfunctions [`var_name`] and [`cst_name`] when converting a type
/// wrapper to a human-readable representation.
///
/// To select a candidate, first check in `names` if the variable or the constant corresponds to a
/// type variable written by the user. If it is, return the name of the variable. Otherwise, use
/// the given counter to generate a new single letter.
///
/// Generated name is clearly not necessarily unique. This is handled by [`select_uniq`].
fn mk_name(names: &HashMap<usize, Ident>, counter: &mut usize, id: usize) -> String {
    match names.get(&id) {
        // First check if that constant or variable was introduced by a forall. If it was, try
        // to use the same name.
        Some(orig) => format!("{}", orig),
        None => {
            //Otherwise, generate a new character
            let next = *counter;
            *counter += 1;
            std::char::from_u32(('a' as u32) + ((next % 26) as u32))
                .unwrap()
                .to_string()
        }
    }
}

/// Select a name distinct from all the others, starting from a candidate name for a type
/// variable or a type constant.
///
/// If the name is already taken, it just iterates by adding a numeric suffix `1`, `2`, .., and so
/// on until a free name is found. See `var_to_type` and `cst_to_type`.
fn select_uniq(name_reg: &mut NameReg, mut name: String, id: usize) -> Ident {
    // To avoid clashing with already picked names, we add a numeric suffix to the picked
    // letter.
    if name_reg.taken.contains(&name) {
        let mut suffix = 1;

        while name_reg.taken.contains(&format!("{}{}", name, suffix)) {
            suffix += 1;
        }

        name = format!("{}{}", name, suffix);
    }

    let ident = Ident::from(name);
    name_reg.reg.insert(id, ident);
    ident
}

/// Either retrieve or generate a new fresh name for a unification variable for error reporting,
/// and wrap it as an identifier. Unification variables are named `_a`, `_b`, .., `_a1`, `_b1`, ..
/// and so on.
fn var_name(names: &HashMap<usize, Ident>, name_reg: &mut NameReg, p: usize) -> Ident {
    name_reg.reg.get(&p).cloned().unwrap_or_else(|| {
        // Select a candidate name and add a "_" prefix
        let name = format!("_{}", mk_name(names, &mut name_reg.var_count, p));
        // Add a suffix to make it unique if it has already been picked
        select_uniq(name_reg, name, p)
    })
}

/// Either retrieve or generate a new fresh name for a constant for error reporting, and wrap it as
/// type variable. Constant are named `a`, `b`, .., `a1`, `b1`, .. and so on.
fn cst_name(names: &HashMap<usize, Ident>, name_reg: &mut NameReg, c: usize) -> Ident {
    name_reg.reg.get(&c).cloned().unwrap_or_else(|| {
        // Select a candidate name
        let name = mk_name(names, &mut name_reg.cst_count, c);
        // Add a suffix to make it unique if it has already been picked
        select_uniq(name_reg, name, c)
    })
}

/// Extract a concrete type corresponding to a unifiable type, for error reporting purpose.
///
/// Similar [`crate::types::Types::from`], excepted that free unification variables and type
/// constants are replaced by type variables which names are determined by the `var_name` and
/// `cst_name`.
///
/// Distinguishing occurrences of unification variables and type constants is more informative
/// than having `Dyn` everywhere.
pub fn to_type(
    table: &UnifTable,
    reported_names: &HashMap<usize, Ident>,
    names: &mut NameReg,
    ty: UnifType,
) -> Types {
    fn rrows_to_type(
        table: &UnifTable,
        reported_names: &HashMap<usize, Ident>,
        names: &mut NameReg,
        rrows: UnifRecordRows,
    ) -> RecordRows {
        let rrows = rrows.into_root(table);

        match rrows {
            UnifRecordRows::UnifVar(var_id) => RecordRows(RecordRowsF::TailVar(var_name(
                reported_names,
                names,
                var_id,
            ))),
            UnifRecordRows::Constant(c) => {
                RecordRows(RecordRowsF::TailVar(cst_name(reported_names, names, c)))
            }
            UnifRecordRows::Concrete(t) => {
                let mapped = t.map_state(
                    |btyp, names| Box::new(to_type(table, reported_names, names, *btyp)),
                    |rrows, names| Box::new(rrows_to_type(table, reported_names, names, *rrows)),
                    names,
                );
                RecordRows(mapped)
            }
        }
    }

    fn erows_to_type(
        table: &UnifTable,
        reported_names: &HashMap<usize, Ident>,
        names: &mut NameReg,
        erows: UnifEnumRows,
    ) -> EnumRows {
        let erows = erows.into_root(table);

        match erows {
            UnifEnumRows::UnifVar(var_id) => {
                EnumRows(EnumRowsF::TailVar(var_name(reported_names, names, var_id)))
            }
            UnifEnumRows::Constant(c) => {
                EnumRows(EnumRowsF::TailVar(cst_name(reported_names, names, c)))
            }
            UnifEnumRows::Concrete(t) => {
                let mapped =
                    t.map(|erows| Box::new(erows_to_type(table, reported_names, names, *erows)));
                EnumRows(mapped)
            }
        }
    }

    let ty = ty.into_root(table);

    match ty {
        UnifType::UnifVar(p) => Types(TypeF::Var(var_name(reported_names, names, p))),
        UnifType::Constant(c) => Types(TypeF::Var(cst_name(reported_names, names, c))),
        UnifType::Concrete(t) => {
            let mapped = t.map_state(
                |btyp, names| Box::new(to_type(table, reported_names, names, *btyp)),
                |rrows, names| rrows_to_type(table, reported_names, names, rrows),
                |erows, names| erows_to_type(table, reported_names, names, erows),
                names,
            );
            Types(mapped)
        }
        UnifType::Contract(t, _) => Types(TypeF::Flat(t)),
    }
}
