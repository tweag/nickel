//! Annotate recursive record fields with the intersection of the set of their free variables and
//! the fields of the record. This way, we can track dependencies between recursive fields, which
//! make it possible in particular to avoid potential memory leaks by providing only references to
//! the recursive fields that actually appear in the definition of each field when computing the
//! fixpoint.
use crate::{
    destruct::{Destruct, Match},
    identifier::Ident,
    term::{
        record::{Field, FieldDeps, RecordDeps},
        RichTerm, SharedTerm, StrChunk, Term,
    },
    types::{RecordRowF, RecordRows, RecordRowsF, TypeF, Types},
};

use std::collections::{HashMap, HashSet};

/// Apply the full free var transformation on a term.
pub fn transform(rt: &mut RichTerm) {
    rt.collect_free_vars(&mut HashSet::new())
}

pub trait CollectFreeVars {
    /// Collect the free variables of a term or type inside the provided hashset. Doing so, fill
    /// the recursive record dependencies data accordingly.
    fn collect_free_vars(&mut self, working_set: &mut HashSet<Ident>);
}

impl CollectFreeVars for RichTerm {
    fn collect_free_vars(&mut self, free_vars: &mut HashSet<Ident>) {
        match SharedTerm::make_mut(&mut self.term) {
            Term::Var(id) => {
                free_vars.insert(*id);
            }
            Term::ParseError(_)
            | Term::RuntimeError(_)
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

                t.collect_free_vars(&mut fresh);
                fresh.remove(id);

                free_vars.extend(fresh);
            }
            Term::FunPattern(id, dest_pat, body) => {
                let mut fresh = HashSet::new();

                body.collect_free_vars(&mut fresh);
                bind_pattern(dest_pat, &mut fresh);
                if let Some(id) = id {
                    fresh.remove(id);
                }

                free_vars.extend(fresh);
            }
            Term::Let(id, t1, t2, attrs) => {
                let mut fresh = HashSet::new();

                if attrs.rec {
                    t1.collect_free_vars(&mut fresh);
                } else {
                    t1.collect_free_vars(free_vars);
                }

                t2.collect_free_vars(&mut fresh);
                fresh.remove(id);

                free_vars.extend(fresh);
            }
            Term::LetPattern(id, dest_pat, t1, t2) => {
                let mut fresh = HashSet::new();

                t1.collect_free_vars(free_vars);
                t2.collect_free_vars(&mut fresh);
                bind_pattern(dest_pat, &mut fresh);
                if let Some(id) = id {
                    fresh.remove(id);
                }

                free_vars.extend(fresh);
            }
            Term::App(t1, t2) => {
                t1.collect_free_vars(free_vars);
                t2.collect_free_vars(free_vars);
            }
            Term::Match { cases, default } => {
                for t in cases.values_mut().chain(default.iter_mut()) {
                    t.collect_free_vars(free_vars);
                }
            }
            Term::Op1(_, t) => t.collect_free_vars(free_vars),
            Term::Op2(_, t1, t2) => {
                t1.collect_free_vars(free_vars);
                t2.collect_free_vars(free_vars);
            }
            Term::OpN(_, ts) => {
                for t in ts {
                    t.collect_free_vars(free_vars);
                }
            }
            Term::Sealed(_, t, _) => t.collect_free_vars(free_vars),
            Term::Record(record) => {
                for t in record.fields.values_mut() {
                    t.collect_free_vars(free_vars);
                }
            }
            Term::RecRecord(record, dyn_fields, deps) => {
                let rec_fields: HashSet<Ident> = record.fields.keys().cloned().collect();
                let mut fresh = HashSet::new();
                let mut new_deps = RecordDeps {
                    stat_fields: HashMap::with_capacity(record.fields.len()),
                    dyn_fields: Vec::with_capacity(dyn_fields.len()),
                };

                for (id, t) in record.fields.iter_mut() {
                    fresh.clear();

                    t.collect_free_vars(&mut fresh);
                    new_deps
                        .stat_fields
                        .insert(*id, FieldDeps::from(&fresh & &rec_fields));

                    free_vars.extend(&fresh - &rec_fields);
                }
                for (t1, t2) in dyn_fields.iter_mut() {
                    fresh.clear();

                    // Currently, the identifier part of a dynamic definition is not recursive, i.e.
                    // one can't write `{foo = "hey", "%{foo}" = 5}`. Hence, we add their free
                    // variables directly in the final set without taking them into account for
                    // recursive dependencies.
                    t1.collect_free_vars(free_vars);
                    t2.collect_free_vars(&mut fresh);
                    new_deps
                        .dyn_fields
                        .push(FieldDeps::from(&fresh & &rec_fields));

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
                    t.collect_free_vars(free_vars);
                }
            }
            Term::StrChunks(chunks) => {
                for chunk in chunks {
                    if let StrChunk::Expr(t, _) = chunk {
                        t.collect_free_vars(free_vars)
                    }
                }
            }
            Term::Annotated(annot, t) => {
                for ctr in annot.iter_mut() {
                    ctr.types.collect_free_vars(free_vars)
                }

                t.collect_free_vars(free_vars);
            }
        }
    }
}

impl CollectFreeVars for Types {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        match &mut self.0 {
            TypeF::Dyn
            | TypeF::Num
            | TypeF::Bool
            | TypeF::Str
            | TypeF::Sym
            | TypeF::Var(_)
            | TypeF::Wildcard(_) => (),
            TypeF::Forall { body: ty, .. } | TypeF::Dict(ty) | TypeF::Array(ty) => {
                ty.as_mut().collect_free_vars(set)
            }
            // No term can appear anywhere in a enum row type, hence we can stop here.
            TypeF::Enum(_) => (),
            TypeF::Record(rrows) => rrows.collect_free_vars(set),
            TypeF::Arrow(ty1, ty2) => {
                ty1.as_mut().collect_free_vars(set);
                ty2.as_mut().collect_free_vars(set);
            }
            TypeF::Flat(ref mut rt) => rt.collect_free_vars(set),
        }
    }
}

impl CollectFreeVars for RecordRows {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        match &mut self.0 {
            RecordRowsF::Empty | RecordRowsF::TailDyn | RecordRowsF::TailVar(_) => (),
            RecordRowsF::Extend {
                row: RecordRowF { types, .. },
                tail,
            } => {
                types.collect_free_vars(set);
                tail.collect_free_vars(set);
            }
        }
    }
}

impl CollectFreeVars for Field {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        for labeled_ty in self.metadata.annotation.iter_mut() {
            labeled_ty.types.collect_free_vars(set)
        }

        if let Some(ref mut value) = self.value {
            value.collect_free_vars(set);
        }
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
            if let Some(sub_pat) = sub_pat {
                bind_pattern(sub_pat, free_vars);
            }
        }
        Match::Simple(id, _) => {
            free_vars.remove(id);
        }
    }
}
