//! Annotate recursive record fields with the intersection of the set of their free variables and
//! the fields of the record. This way, we can track dependencies between recursive fields, which
//! make it possible in particular to avoid potential memory leaks by providing only references to
//! the recursive fields that actually appear in the definition of each field when computing the
//! fixpoint.
use crate::{
    eval::value::{Container, NickelValue, ValueContentRefMut},
    identifier::Ident,
    term::pattern::*,
    term::{
        AnnotatedData, AppData, FunData, FunPatternData, IndexMap, LetData, LetPatternData,
        MatchBranch, MatchData, Op1Data, Op2Data, OpNData, RecRecordData, StrChunk, Term,
        TypeAnnotation,
        record::{Field, FieldDeps, Include, RecordDeps},
    },
    typ::{RecordRowF, RecordRows, RecordRowsF, Type, TypeF},
};

use std::{collections::HashSet, rc::Rc};

/// Apply the full free var transformation on a term.
pub fn transform(value: &mut NickelValue) {
    value.collect_free_vars(&mut HashSet::new())
}

pub trait CollectFreeVars {
    /// Collect the free variables of a term or type inside the provided hashset. Doing so, fill
    /// the recursive record dependencies data accordingly.
    fn collect_free_vars(&mut self, working_set: &mut HashSet<Ident>);
}

impl CollectFreeVars for NickelValue {
    fn collect_free_vars(&mut self, free_vars: &mut HashSet<Ident>) {
        match self.content_make_mut() {
            ValueContentRefMut::Null(_)
            | ValueContentRefMut::Bool(_)
            | ValueContentRefMut::Array(Container::Empty)
            | ValueContentRefMut::Record(Container::Empty)
            | ValueContentRefMut::Number(_)
            | ValueContentRefMut::String(_)
            | ValueContentRefMut::ForeignId(_)
            | ValueContentRefMut::SealingKey(_)
            | ValueContentRefMut::Label(_) => (),
            ValueContentRefMut::Array(Container::Alloc(array_data)) => {
                for t in array_data.array.iter_mut() {
                    t.collect_free_vars(free_vars);
                }
            }
            ValueContentRefMut::Record(Container::Alloc(record)) => {
                for t in record.fields.values_mut() {
                    t.collect_free_vars(free_vars);
                }
            }
            ValueContentRefMut::Term(term) => term.collect_free_vars(free_vars),
            ValueContentRefMut::EnumVariant(enum_variant) => {
                if let Some(arg) = &mut enum_variant.arg {
                    arg.collect_free_vars(free_vars);
                }
            }
            ValueContentRefMut::CustomContract(ctr) => {
                ctr.collect_free_vars(free_vars);
            }
            ValueContentRefMut::Type(type_data) => {
                type_data.typ.collect_free_vars(free_vars);
                type_data.contract.collect_free_vars(free_vars);
            }
            ValueContentRefMut::Thunk(_) => {
                unreachable!("should never see closures at the transformation stage")
            }
        }
    }
}

impl CollectFreeVars for Term {
    fn collect_free_vars(&mut self, free_vars: &mut HashSet<Ident>) {
        match self {
            Term::Var(id) => {
                free_vars.insert(id.ident());
            }
            Term::ParseError(_)
            | Term::RuntimeError(_)
            | Term::Import { .. }
            | Term::ResolvedImport(_) => (),
            Term::Fun(data) => data.collect_free_vars(free_vars),
            Term::FunPattern(data) => data.collect_free_vars(free_vars),
            Term::Let(data) => data.collect_free_vars(free_vars),
            Term::LetPattern(data) => data.collect_free_vars(free_vars),
            Term::App(data) => data.collect_free_vars(free_vars),
            Term::Op1(data) => data.collect_free_vars(free_vars),
            Term::Op2(data) => data.collect_free_vars(free_vars),
            Term::OpN(data) => data.collect_free_vars(free_vars),
            Term::Sealed(data) => data.inner.collect_free_vars(free_vars),
            Term::RecRecord(data) => data.collect_free_vars(free_vars),
            Term::StrChunks(chunks) => {
                for chunk in chunks {
                    if let StrChunk::Expr(t, _) = chunk {
                        t.collect_free_vars(free_vars)
                    }
                }
            }
            Term::Annotated(data) => data.collect_free_vars(free_vars),
            Term::Closurize(v) => v.collect_free_vars(free_vars),
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
            TypeF::Contract(rt) => rt.collect_free_vars(set),
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

impl CollectFreeVars for TypeAnnotation {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        for labeled_ty in self.iter_mut() {
            labeled_ty.typ.collect_free_vars(set);
        }
    }
}

impl CollectFreeVars for Field {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        if let Some(metadata) = &mut self.metadata.0 {
            for labeled_ty in Rc::make_mut(metadata).annotation.iter_mut() {
                labeled_ty.typ.collect_free_vars(set)
            }
        }

        if let Some(ref mut value) = self.value {
            value.collect_free_vars(set);
        }
    }
}

impl CollectFreeVars for Include {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        self.metadata.annotation.collect_free_vars(set);
    }
}

impl CollectFreeVars for FunData {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        let mut fresh = HashSet::new();

        self.body.collect_free_vars(&mut fresh);
        fresh.remove(&self.arg.ident());

        set.extend(fresh);
    }
}

impl CollectFreeVars for FunPatternData {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        let mut fresh = HashSet::new();

        self.body.collect_free_vars(&mut fresh);
        self.pattern.remove_bindings(&mut fresh);

        set.extend(fresh);
    }
}

impl CollectFreeVars for LetData {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        let mut fresh = HashSet::new();

        for (_id, value) in self.bindings.iter_mut() {
            if self.attrs.rec {
                value.collect_free_vars(&mut fresh);
            } else {
                value.collect_free_vars(set);
            }
        }

        self.body.collect_free_vars(&mut fresh);
        for (id, _value) in &self.bindings {
            fresh.remove(&id.ident());
        }

        set.extend(fresh);
    }
}

impl CollectFreeVars for LetPatternData {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        let mut fresh = HashSet::new();

        for (_pat, value) in self.bindings.iter_mut() {
            if self.attrs.rec {
                value.collect_free_vars(&mut fresh);
            } else {
                value.collect_free_vars(set);
            }
        }

        self.body.collect_free_vars(&mut fresh);
        for (pat, _value) in &self.bindings {
            pat.remove_bindings(&mut fresh);
        }

        set.extend(fresh);
    }
}

impl CollectFreeVars for RecRecordData {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        let mut fresh = HashSet::new();

        let mut rec_fields: HashSet<Ident> =
            self.record.fields.keys().map(|id| id.ident()).collect();
        // `{include foo, [..]}` is defined to have the semantics of `let foo_ = foo in
        // {foo = foo_, [..]}`, hence an included field also counts as a recursive field.
        rec_fields.extend(self.includes.iter().map(|incl| incl.ident.ident()));

        let mut new_deps = RecordDeps {
            stat_fields: IndexMap::with_capacity(self.record.fields.len() + self.includes.len()),
            dyn_fields: Vec::with_capacity(self.dyn_fields.len()),
        };

        for incl in self.includes.iter_mut() {
            fresh.clear();

            incl.collect_free_vars(&mut fresh);

            new_deps
                .stat_fields
                .insert(incl.ident.ident(), FieldDeps::from(&fresh & &rec_fields));

            set.extend(&fresh - &rec_fields);
            set.insert(incl.ident.ident());
        }

        for (id, t) in self.record.fields.iter_mut() {
            fresh.clear();

            t.collect_free_vars(&mut fresh);
            new_deps
                .stat_fields
                .insert(id.ident(), FieldDeps::from(&fresh & &rec_fields));

            set.extend(&fresh - &rec_fields);
        }

        for (t1, t2) in self.dyn_fields.iter_mut() {
            fresh.clear();

            // Currently, the identifier part of a dynamic definition is not recursive,
            // i.e. one can't write `{foo = "hey", "%{foo}" = 5}`. Hence, we add their free
            // variables directly to the final set without taking them into account for
            // recursive dependencies.
            t1.collect_free_vars(set);
            t2.collect_free_vars(&mut fresh);
            new_deps
                .dyn_fields
                .push(FieldDeps::from(&fresh & &rec_fields));

            set.extend(&fresh - &rec_fields);
        }

        // Even if deps were previously filled (it shouldn't), we had to recompute the free
        // variables anyway for the nodes higher up, because `deps` alone is not sufficient
        // to reconstruct the full set of free variables. At this point, we override it in
        // any case.
        self.deps = Some(new_deps);
    }
}

impl CollectFreeVars for Op1Data {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        self.arg.collect_free_vars(set);
    }
}

impl CollectFreeVars for Op2Data {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        self.arg1.collect_free_vars(set);
        self.arg2.collect_free_vars(set);
    }
}

impl CollectFreeVars for OpNData {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        for t in &mut self.args {
            t.collect_free_vars(set);
        }
    }
}

impl CollectFreeVars for AnnotatedData {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        use std::rc::Rc;

        for ctr in Rc::make_mut(&mut self.annot).iter_mut() {
            ctr.typ.collect_free_vars(set)
        }

        self.inner.collect_free_vars(set);
    }
}

impl CollectFreeVars for AppData {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        self.head.collect_free_vars(set);
        self.arg.collect_free_vars(set);
    }
}

impl CollectFreeVars for MatchData {
    fn collect_free_vars(&mut self, set: &mut HashSet<Ident>) {
        for MatchBranch {
            pattern,
            guard,
            body,
        } in self.branches.iter_mut()
        {
            let mut fresh = HashSet::new();

            if let Some(guard) = guard {
                guard.collect_free_vars(&mut fresh);
            }

            body.collect_free_vars(&mut fresh);
            pattern.remove_bindings(&mut fresh);
            set.extend(fresh);
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
