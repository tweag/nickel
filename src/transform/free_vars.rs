//! Annotate recursive record fields with the intersection of the set of their free variables and
//! the fields of the record. This way, we can track dependencies between recursive fields, which
//! make it possible in particular to avoid potential memory leaks by providing only references to
//! the recursive fields that actually appear in the definition of each field when computing the
//! fixpoint.
use crate::{
    destruct::{Destruct, Match},
    identifier::Ident,
    term::{RecordDeps, RichTerm, SharedTerm, StrChunk, Term},
    types::{AbsType, Types},
};

use std::collections::{HashMap, HashSet};

/// Apply the full free var transformation on a term.
pub fn transform(rt: &mut RichTerm) {
    collect_free_vars(rt, &mut HashSet::new())
}

/// Collect the free variables of a term inside the provided hashset. Doing so, fill the recursive
/// record dependencies data accordingly.
fn collect_free_vars(rt: &mut RichTerm, free_vars: &mut HashSet<Ident>) {
    match SharedTerm::make_mut(&mut rt.term) {
        Term::Var(id) => {
            free_vars.insert(id.clone());
        }
        Term::ParseError(_)
        | Term::Null
        | Term::Bool(_)
        | Term::Num(_)
        | Term::Str(_)
        | Term::Lbl(_)
        | Term::SealingKey(_)
        | Term::Enum(_)
        | Term::Import(_)
        | Term::ResolvedImport(_) => (),
        Term::Fun(id, t) => {
            let mut fresh = HashSet::new();

            collect_free_vars(t, &mut fresh);
            fresh.remove(id);

            free_vars.extend(fresh);
        }
        Term::FunPattern(id, dest_pat, body) => {
            let mut fresh = HashSet::new();

            collect_free_vars(body, &mut fresh);
            bind_pattern(dest_pat, &mut fresh);
            if let Some(id) = id {
                fresh.remove(id);
            }

            free_vars.extend(fresh);
        }
        Term::Let(id, t1, t2, attrs) => {
            let mut fresh = HashSet::new();

            if attrs.rec {
                collect_free_vars(t1, &mut fresh);
            } else {
                collect_free_vars(t1, free_vars);
            }

            collect_free_vars(t2, &mut fresh);
            fresh.remove(id);

            free_vars.extend(fresh);
        }
        Term::LetPattern(id, dest_pat, t1, t2) => {
            let mut fresh = HashSet::new();

            collect_free_vars(t1, free_vars);
            collect_free_vars(t2, &mut fresh);
            bind_pattern(dest_pat, &mut fresh);
            if let Some(id) = id {
                fresh.remove(id);
            }

            free_vars.extend(fresh);
        }
        Term::App(t1, t2) => {
            collect_free_vars(t1, free_vars);
            collect_free_vars(t2, free_vars);
        }
        Term::Switch(t, cases, default) => {
            collect_free_vars(t, free_vars);
            for t in cases.values_mut().chain(default.iter_mut()) {
                collect_free_vars(t, free_vars);
            }
        }
        Term::Op1(_, t) => collect_free_vars(t, free_vars),
        Term::Op2(_, t1, t2) => {
            collect_free_vars(t1, free_vars);
            collect_free_vars(t2, free_vars);
        }
        Term::OpN(_, ts) => {
            for t in ts {
                collect_free_vars(t, free_vars);
            }
        }
        Term::Sealed(_, t, _) => collect_free_vars(t, free_vars),
        Term::Record(map, _) => {
            for t in map.values_mut() {
                collect_free_vars(t, free_vars);
            }
        }
        Term::RecRecord(map, dyn_fields, _, deps) => {
            let rec_fields: HashSet<Ident> = map.keys().cloned().collect();
            let mut fresh = HashSet::new();
            let mut new_deps = RecordDeps {
                stat_fields: HashMap::with_capacity(map.len()),
                dyn_fields: Vec::with_capacity(dyn_fields.len()),
            };

            for (id, t) in map.iter_mut() {
                fresh.clear();

                collect_free_vars(t, &mut fresh);
                new_deps
                    .stat_fields
                    .insert(id.clone(), &fresh & &rec_fields);

                free_vars.extend(&fresh - &rec_fields);
            }
            for (t1, t2) in dyn_fields.iter_mut() {
                fresh.clear();

                // Currently, the identifier part of a dynamic definition is not recursive, i.e.
                // one can't write `{foo = "hey", "%{foo}" = 5}`. Hence, we add their free
                // variables directly in the final set without taking them into account for
                // recursive dependencies.
                collect_free_vars(t1, free_vars);
                collect_free_vars(t2, &mut fresh);
                new_deps.dyn_fields.push(&fresh & &rec_fields);

                free_vars.extend(&fresh - &rec_fields);
            }

            // Even if deps were previously filled (it shouldn't), we had to recompute the free
            // variables anyway for the nodes higher up, because deps alone is not sufficient to
            // reconstruct the full set of free variables. At this point, we override it in any
            // case.
            *deps = Some(new_deps);
        }
        Term::Array(ts, _) => {
            for t in ts.make_mut().iter_mut() {
                collect_free_vars(t, free_vars);
            }
        }
        Term::StrChunks(chunks) => {
            for chunk in chunks {
                if let StrChunk::Expr(t, _) = chunk {
                    collect_free_vars(t, free_vars)
                }
            }
        }
        Term::MetaValue(meta) => {
            for ctr in meta.contracts.iter_mut().chain(meta.types.iter_mut()) {
                collect_type_free_vars(&mut ctr.types, free_vars)
            }

            if let Some(ref mut t) = meta.value {
                collect_free_vars(t, free_vars);
            }
        }
    }
}

/// Collect the free variables of the potential terms inside a type (custom contracts) and insert
/// them the provided hashset. Doing so, fill the recursive records dependencies data accordingly.
fn collect_type_free_vars(ty: &mut Types, set: &mut HashSet<Ident>) {
    match &mut ty.0 {
        AbsType::Dyn()
        | AbsType::Num()
        | AbsType::Bool()
        | AbsType::Str()
        | AbsType::Sym()
        | AbsType::Var(_)
        | AbsType::RowEmpty()
        | AbsType::Wildcard(_) => (),
        AbsType::Forall(_, ty)
        | AbsType::Enum(ty)
        | AbsType::StaticRecord(ty)
        | AbsType::DynRecord(ty)
        | AbsType::Array(ty) => collect_type_free_vars(ty.as_mut(), set),
        AbsType::Arrow(ty1, ty2) => {
            collect_type_free_vars(ty1.as_mut(), set);
            collect_type_free_vars(ty2.as_mut(), set);
        }
        AbsType::RowExtend(_, ty_opt, tail) => {
            if let Some(ref mut ty) = ty_opt {
                collect_type_free_vars(ty, set);
            }
            collect_type_free_vars(tail.as_mut(), set);
        }
        AbsType::Flat(ref mut rt) => collect_free_vars(rt, set),
    }
}

/// Remove the variables bound by a destructuring pattern from a set of free variables.
fn bind_pattern(dest_pat: &Destruct, free_vars: &mut HashSet<Ident>) {
    match dest_pat {
        Destruct::Record { matches, rest, .. } => {
            for m in matches {
                bind_match(m, free_vars);
            }

            if let Some(rest) = rest {
                free_vars.remove(rest);
            }
        }
        Destruct::Array { matches, .. } => {
            for m in matches {
                bind_match(m, free_vars);
            }
        }
        Destruct::Empty => {}
    }
}

/// Remove the variables bound by a match expression (constituents of a destructuring pattern) from
/// a set of free variables.
fn bind_match(m: &Match, free_vars: &mut HashSet<Ident>) {
    match m {
        Match::Assign(_, _, (id, sub_pat)) => {
            if let Some(id) = id {
                free_vars.remove(id);
            }
            bind_pattern(sub_pat, free_vars);
        }
        Match::Simple(id, _) => {
            free_vars.remove(id);
        }
    }
}
