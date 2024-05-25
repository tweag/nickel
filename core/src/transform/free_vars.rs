//! Annotate recursive record fields with the intersection of the set of their free variables and
//! the fields of the record. This way, we can track dependencies between recursive fields, which
//! make it possible in particular to avoid potential memory leaks by providing only references to
//! the recursive fields that actually appear in the definition of each field when computing the
//! fixpoint.
use crate::{
    identifier::Ident,
    term::pattern::*,
    term::{
        record::{Field, FieldDeps, RecordDeps},
        IndexMap, MatchBranch, RichTerm, SharedTerm, StrChunk, Term,
    },
    typ::{RecordRowF, RecordRows, RecordRowsF, Type, TypeF},
};

use std::collections::HashSet;

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
                free_vars.insert(id.ident());
            }
            Term::ParseError(_)
            | Term::RuntimeError(_)
            | Term::Null
            | Term::Bool(_)
            | Term::Num(_)
            | Term::Str(_)
            | Term::Lbl(_)
            | Term::ForeignId(_)
            | Term::SealingKey(_)
            | Term::Enum(_)
            | Term::Import(_)
            | Term::ResolvedImport(_) => (),
            Term::Fun(id, t) => {
                let mut fresh = HashSet::new();

                t.collect_free_vars(&mut fresh);
                fresh.remove(&id.ident());

                free_vars.extend(fresh);
            }
            Term::FunPattern(pat, body) => {
                let mut fresh = HashSet::new();

                body.collect_free_vars(&mut fresh);
                pat.remove_bindings(&mut fresh);

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
                fresh.remove(&id.ident());

                free_vars.extend(fresh);
            }
            Term::LetPattern(pat, t1, t2) => {
                let mut fresh = HashSet::new();

                t1.collect_free_vars(free_vars);
                t2.collect_free_vars(&mut fresh);
                pat.remove_bindings(&mut fresh);

                free_vars.extend(fresh);
            }
            Term::App(t1, t2) | Term::Op2(_, t1, t2) => {
                t1.collect_free_vars(free_vars);
                t2.collect_free_vars(free_vars);
            }
            Term::Match(data) => {
                for MatchBranch {
                    pattern,
                    guard,
                    body,
                } in data.branches.iter_mut()
                {
                    let mut fresh = HashSet::new();

                    if let Some(guard) = guard {
                        guard.collect_free_vars(&mut fresh);
                    }
                    body.collect_free_vars(&mut fresh);

                    pattern.remove_bindings(&mut fresh);

                    free_vars.extend(fresh);
                }
            }
            Term::Op1(_, t) | Term::Sealed(_, t, _) | Term::EnumVariant { arg: t, .. } => {
                t.collect_free_vars(free_vars)
            }
            Term::OpN(_, ts) => {
                for t in ts {
                    t.collect_free_vars(free_vars);
                }
            }
            Term::Record(record) => {
                for t in record.fields.values_mut() {
                    t.collect_free_vars(free_vars);
                }
            }
            Term::RecRecord(record, dyn_fields, deps) => {
                let rec_fields: HashSet<Ident> =
                    record.fields.keys().map(|id| id.ident()).collect();
                let mut fresh = HashSet::new();
                let mut new_deps = RecordDeps {
                    stat_fields: IndexMap::with_capacity(record.fields.len()),
                    dyn_fields: Vec::with_capacity(dyn_fields.len()),
                };

                for (id, t) in record.fields.iter_mut() {
                    fresh.clear();

                    t.collect_free_vars(&mut fresh);
                    new_deps
                        .stat_fields
                        .insert(id.ident(), FieldDeps::from(&fresh & &rec_fields));

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
                    ctr.typ.collect_free_vars(free_vars)
                }

                t.collect_free_vars(free_vars);
            }
            Term::Type(ty) => {
                ty.collect_free_vars(free_vars);
            }
            Term::Closure(_) => {
                unreachable!("should never see closures at the transformation stage");
            }
        }
    }
}

impl CollectFreeVars for Type {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        match &mut self.typ {
            TypeF::Dyn
            | TypeF::Number
            | TypeF::Bool
            | TypeF::String
            | TypeF::ForeignId
            | TypeF::Symbol
            | TypeF::Var(_)
            | TypeF::Wildcard(_) => (),
            TypeF::Forall { body: ty, .. }
            | TypeF::Dict {
                type_fields: ty, ..
            }
            | TypeF::Array(ty) => ty.as_mut().collect_free_vars(set),
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
                row: RecordRowF { typ, .. },
                tail,
            } => {
                typ.collect_free_vars(set);
                tail.collect_free_vars(set);
            }
        }
    }
}

impl CollectFreeVars for Field {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        for labeled_ty in self.metadata.annotation.iter_mut() {
            labeled_ty.typ.collect_free_vars(set)
        }

        if let Some(ref mut value) = self.value {
            value.collect_free_vars(set);
        }
    }
}

trait RemoveBindings {
    /// For a binding form that introduces new variables in scope, typically patterns, remove the
    /// variable introduced by this binding form from the provided set of free variables.
    fn remove_bindings(&self, working_set: &mut HashSet<Ident>);
}

impl RemoveBindings for PatternData {
    fn remove_bindings(&self, working_set: &mut HashSet<Ident>) {
        match self {
            PatternData::Any(id) => {
                working_set.remove(&id.ident());
            }
            PatternData::Record(record_pat) => record_pat.remove_bindings(working_set),
            PatternData::Array(array_pat) => array_pat.remove_bindings(working_set),
            PatternData::Enum(enum_variant_pat) => enum_variant_pat.remove_bindings(working_set),
            PatternData::Or(or_pat) => or_pat.remove_bindings(working_set),
            // A wildcard pattern or a constant pattern doesn't bind any variable.
            PatternData::Wildcard | PatternData::Constant(_) => (),
        }
    }
}

impl RemoveBindings for Pattern {
    fn remove_bindings(&self, working_set: &mut HashSet<Ident>) {
        self.data.remove_bindings(working_set);

        if let Some(alias) = self.alias {
            working_set.remove(&alias.ident());
        }
    }
}

impl RemoveBindings for FieldPattern {
    fn remove_bindings(&self, working_set: &mut HashSet<Ident>) {
        self.pattern.remove_bindings(working_set);
    }
}

impl RemoveBindings for RecordPattern {
    fn remove_bindings(&self, working_set: &mut HashSet<Ident>) {
        for m in &self.patterns {
            m.remove_bindings(working_set);
        }

        if let TailPattern::Capture(rest) = self.tail {
            working_set.remove(&rest.ident());
        }
    }
}

impl RemoveBindings for ArrayPattern {
    fn remove_bindings(&self, working_set: &mut HashSet<Ident>) {
        for m in &self.patterns {
            m.remove_bindings(working_set);
        }

        if let TailPattern::Capture(rest) = self.tail {
            working_set.remove(&rest.ident());
        }
    }
}

impl RemoveBindings for EnumPattern {
    fn remove_bindings(&self, working_set: &mut HashSet<Ident>) {
        if let Some(ref arg_pat) = self.pattern {
            arg_pat.remove_bindings(working_set);
        }
    }
}

impl RemoveBindings for OrPattern {
    fn remove_bindings(&self, working_set: &mut HashSet<Ident>) {
        // Theoretically, we could just remove the bindings of the first pattern, as all
        // branches in an or patterns should bind exactly the same variables. However, at the
        // time of writing, this condition isn't enforced at parsing time (it's enforced
        // during typechecking). It doesn't cost much to be conservative and to remove all
        // the bindings (removing something non-existent from a hashet is a no-op), so that
        // we don't miss free variables in the case of ill-formed or-patterns, although we
        // should ideally rule those out before reaching the free var transformation.
        for pat in &self.patterns {
            pat.remove_bindings(working_set);
        }
    }
}
