use std::collections::HashMap;

use codespan::FileId;
use nickel_lang_core::{
    identifier::Ident,
    position::RawSpan,
    term::{BinaryOp, RichTerm, Term, Traverse, TraverseControl, UnaryOp},
    typ::{Type, TypeF},
    typecheck::{
        reporting::{NameReg, ToType},
        TypeTables, TypecheckVisitor, UnifType,
    },
};

use crate::{
    field_walker::{Def, EltId},
    identifier::LocIdent,
    position::PositionLookup,
    term::RichTermPtr,
    usage::{Environment, UsageLookup},
};

#[derive(Clone, Debug)]
pub struct Parent {
    term: RichTerm,
    child_name: Option<EltId>,
}

impl From<RichTerm> for Parent {
    fn from(term: RichTerm) -> Self {
        Parent {
            term,
            child_name: None,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ParentLookup {
    table: HashMap<RichTermPtr, Parent>,
}

impl ParentLookup {
    pub fn new(rt: &RichTerm) -> Self {
        let mut table = HashMap::new();

        fn traversal(
            rt: &RichTerm,
            parent: &Option<Parent>,
            acc: &mut HashMap<RichTermPtr, Parent>,
        ) -> TraverseControl<Option<Parent>, ()> {
            if let Some(parent) = parent {
                acc.insert(RichTermPtr(rt.clone()), parent.clone());
            }
            match rt.as_ref() {
                Term::Record(data) | Term::RecRecord(data, _, _) => {
                    for (name, field) in &data.fields {
                        if let Some(child) = &field.value {
                            let parent = Parent {
                                term: rt.clone(),
                                child_name: Some(name.ident().into()),
                            };
                            child.traverse_ref(
                                &mut |rt, parent| traversal(rt, parent, acc),
                                &Some(parent),
                            );
                        }
                    }
                    TraverseControl::SkipBranch
                }
                Term::Array(arr, _) => {
                    for elt in arr.iter() {
                        let parent = Parent {
                            term: rt.clone(),
                            child_name: Some(EltId::ArrayElt),
                        };
                        elt.traverse_ref(
                            &mut |rt, parent| traversal(rt, parent, acc),
                            &Some(parent),
                        );
                    }
                    TraverseControl::SkipBranch
                }
                _ => TraverseControl::ContinueWithScope(Some(rt.clone().into())),
            }
        }

        rt.traverse_ref(&mut |rt, parent| traversal(rt, parent, &mut table), &None);

        ParentLookup { table }
    }

    pub fn parent(&self, rt: &RichTerm) -> Option<&Parent> {
        self.table.get(&RichTermPtr(rt.clone()))
    }

    pub fn parent_chain(&self, rt: &RichTerm) -> ParentChainIter<'_> {
        let next = self.parent(rt).cloned();
        ParentChainIter {
            table: self,
            path: Some(Vec::new()),
            next,
        }
    }
}

fn find_static_accesses(rt: &RichTerm) -> HashMap<Ident, Vec<RichTerm>> {
    let mut map: HashMap<Ident, Vec<RichTerm>> = HashMap::new();
    rt.traverse_ref(
        &mut |rt: &RichTerm, _scope: &()| {
            if let Term::Op1(UnaryOp::StaticAccess(id), _) = rt.as_ref() {
                map.entry(id.ident()).or_default().push(rt.clone());
            }
            TraverseControl::Continue::<_, ()>
        },
        &(),
    );
    map
}

/// Essentially an iterator over pairs of `(ancestor, reversed_path_to_the_original)`.
///
/// For example, if we are iterating over the AST of `foo.bar.baz`, the iterator
/// should return
/// - ancestor `foo.bar`, path \[`baz`\]; and then
/// - ancestor `foo`, path [`baz`, `bar`].
///
/// If, during our iteration, we encounter an ancestor that isn't a record then the
/// path will be none from then on. For example, if we traverse `(some_fn foo.bar.baz).quux`
/// starting from the `baz` AST node then the first couple of terms will have paths like
/// the previous example, but after that we'll get
/// - ancestor `(some_fn foo.bar.baz)`, path `None`; and then
/// - ancestor `(some_fn foo.bar.baz).quux`, path `None`.
///
/// This is a "streaming iterator" in the sense that the returned data borrows from
/// our state. Since streaming iterators are not (yet) in rust's stdlib, we don't
/// implement any traits here, but just do it by hand.
///
/// For borrowck reasons, the iteration is done in two parts: `next` advances the iterator
/// and returns just the term part. `path` retrieves the path corresponding to the previous
/// `next` call.
pub struct ParentChainIter<'a> {
    table: &'a ParentLookup,
    path: Option<Vec<EltId>>,
    next: Option<Parent>,
}

impl<'a> ParentChainIter<'a> {
    pub fn next(&mut self) -> Option<RichTerm> {
        if let Some(next) = self.next.take() {
            if let Some((ident, path)) = next.child_name.zip(self.path.as_mut()) {
                path.push(ident);
            }
            if !matches!(
                next.term.as_ref(),
                Term::Record(_)
                    | Term::RecRecord(..)
                    | Term::Annotated(..)
                    | Term::Op2(..)
                    | Term::Array(..)
            ) {
                self.path = None;
            }
            self.next = self.table.parent(&next.term).cloned();

            Some(next.term)
        } else {
            None
        }
    }

    /// Like `next`, but skips over everything except for merges, annotations, and records.
    pub fn next_merge(&mut self) -> Option<RichTerm> {
        let is_fieldy_term = |rt: &RichTerm| {
            matches!(
                rt.as_ref(),
                // There is also NAryOp::MergeContract, but only at eval time so we don't
                // expect it.
                Term::Op2(BinaryOp::Merge(_), _, _)
                    | Term::Annotated(_, _)
                    | Term::RecRecord(..)
                    | Term::Record(..)
            )
        };

        let is_merge_term = |rt: &RichTerm| {
            matches!(
                rt.as_ref(),
                Term::Op2(BinaryOp::Merge(_), _, _) | Term::Annotated(_, _)
            )
        };

        while let Some(p) = self.next() {
            // If a parent and a grandparent were both merges, we can skip the parent
            // because the grandparent will have a superset of its fields. This prevents
            // quadratic behavior on long chains of merges.
            if let Some(gp) = self.peek_gp() {
                if is_merge_term(gp) {
                    continue;
                }
            }

            if is_fieldy_term(&p) {
                return Some(p);
            }
        }
        None
    }

    pub fn path(&self) -> Option<&[EltId]> {
        self.path.as_deref()
    }

    /// Peek at the grandparent.
    pub fn peek_gp(&self) -> Option<&RichTerm> {
        if let Some(Parent { term, .. }) = &self.next {
            self.table.parent(term).map(|gp| &gp.term)
        } else {
            None
        }
    }
}

/// The initial analysis that we collect for a file.
///
/// This analysis is re-collected from scratch each time the file is updated.
#[derive(Default, Debug)]
pub struct Analysis {
    pub position_lookup: PositionLookup,
    pub usage_lookup: UsageLookup,
    pub parent_lookup: ParentLookup,
    pub type_lookup: CollectedTypes<Type>,

    /// A lookup table for static accesses, for looking up all occurrences of,
    /// say, `.foo` in a file.
    pub static_accesses: HashMap<Ident, Vec<RichTerm>>,
}

impl Analysis {
    pub fn new(
        term: &RichTerm,
        type_lookup: CollectedTypes<Type>,
        initial_env: &Environment,
    ) -> Self {
        Self {
            position_lookup: PositionLookup::new(term),
            usage_lookup: UsageLookup::new(term, initial_env),
            parent_lookup: ParentLookup::new(term),
            static_accesses: find_static_accesses(term),
            type_lookup,
        }
    }
}

/// The collection of analyses for every file that we know about.
#[derive(Default, Debug)]
pub struct AnalysisRegistry {
    // Most of the fields of `Analysis` are themselves hash tables. Having
    // a table of tables requires more lookups than necessary, but it makes
    // it easy to invalidate a whole file.
    pub analysis: HashMap<FileId, Analysis>,
}

impl AnalysisRegistry {
    pub fn insert(
        &mut self,
        file_id: FileId,
        type_lookups: CollectedTypes<Type>,
        term: &RichTerm,
        initial_env: &crate::usage::Environment,
    ) {
        self.analysis
            .insert(file_id, Analysis::new(term, type_lookups, initial_env));
    }

    /// Inserts a new file into the analysis, but only generates usage analysis for it.
    ///
    /// This is useful for temporary little pieces of input (like parts extracted from incomplete input)
    /// that need variable resolution but not the full analysis.
    pub fn insert_usage(&mut self, file_id: FileId, term: &RichTerm, initial_env: &Environment) {
        self.analysis.insert(
            file_id,
            Analysis {
                usage_lookup: UsageLookup::new(term, initial_env),
                ..Default::default()
            },
        );
    }

    pub fn remove(&mut self, file_id: FileId) {
        self.analysis.remove(&file_id);
    }

    pub fn get_def(&self, ident: &LocIdent) -> Option<&Def> {
        let file = ident.pos.as_opt_ref()?.src_id;
        self.analysis.get(&file)?.usage_lookup.def(ident)
    }

    pub fn get_usages(&self, span: &RawSpan) -> impl Iterator<Item = &LocIdent> {
        fn inner<'a>(
            slf: &'a AnalysisRegistry,
            span: &RawSpan,
        ) -> Option<impl Iterator<Item = &'a LocIdent>> {
            let file = span.src_id;
            Some(slf.analysis.get(&file)?.usage_lookup.usages(span))
        }
        inner(self, span).into_iter().flatten()
    }

    pub fn get_env(&self, rt: &RichTerm) -> Option<&crate::usage::Environment> {
        let file = rt.pos.as_opt_ref()?.src_id;
        self.analysis.get(&file)?.usage_lookup.env(rt)
    }

    pub fn get_type(&self, rt: &RichTerm) -> Option<&Type> {
        let file = rt.pos.as_opt_ref()?.src_id;
        self.analysis
            .get(&file)?
            .type_lookup
            .terms
            .get(&RichTermPtr(rt.clone()))
    }

    pub fn get_type_for_ident(&self, id: &LocIdent) -> Option<&Type> {
        let file = id.pos.as_opt_ref()?.src_id;
        self.analysis.get(&file)?.type_lookup.idents.get(id)
    }

    pub fn get_parent_chain<'a>(&'a self, rt: &'a RichTerm) -> Option<ParentChainIter<'a>> {
        let file = rt.pos.as_opt_ref()?.src_id;
        Some(self.analysis.get(&file)?.parent_lookup.parent_chain(rt))
    }

    pub fn get_static_accesses(&self, id: Ident) -> Vec<RichTerm> {
        self.analysis
            .values()
            .filter_map(|a| a.static_accesses.get(&id))
            .flatten()
            .cloned()
            .collect()
    }
}

#[derive(Debug, Default)]
pub struct TypeCollector {
    tables: CollectedTypes<UnifType>,
}

#[derive(Clone, Debug)]
pub struct CollectedTypes<Ty> {
    pub terms: HashMap<RichTermPtr, Ty>,
    pub idents: HashMap<LocIdent, Ty>,
}

impl<Ty> Default for CollectedTypes<Ty> {
    fn default() -> Self {
        Self {
            terms: Default::default(),
            idents: Default::default(),
        }
    }
}

impl TypecheckVisitor for TypeCollector {
    fn visit_term(&mut self, rt: &RichTerm, ty: UnifType) {
        self.tables.terms.insert(RichTermPtr(rt.clone()), ty);
    }

    fn visit_ident(&mut self, ident: &nickel_lang_core::identifier::LocIdent, new_type: UnifType) {
        self.tables.idents.insert((*ident).into(), new_type);
    }
}

impl TypeCollector {
    pub fn complete(self, type_tables: TypeTables) -> CollectedTypes<Type> {
        let mut name_reg = NameReg::new(type_tables.names.clone());

        let mut transform_type = |uty: UnifType| -> Type {
            let ty = uty.to_type(&mut name_reg, &type_tables.table);
            match ty.typ {
                TypeF::Wildcard(i) => type_tables.wildcards.get(i).unwrap_or(&ty).clone(),
                _ => ty,
            }
        };

        let terms = self
            .tables
            .terms
            .into_iter()
            .map(|(rt, uty)| (rt, transform_type(uty)))
            .collect();
        let idents = self
            .tables
            .idents
            .into_iter()
            .map(|(id, uty)| (id, transform_type(uty)))
            .collect();
        CollectedTypes { terms, idents }
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use nickel_lang_core::{identifier::Ident, term::Term};

    use crate::{
        field_walker::EltId,
        position::tests::parse,
        usage::{tests::locced, Environment, UsageLookup},
    };

    use super::ParentLookup;

    #[test]
    fn parent_chain() {
        let (file, rt) = parse("{ foo = [{ bar = 1 }] }");
        let bar_id = Ident::new("bar");
        let bar = locced(bar_id, file, 11..14);

        let parent = ParentLookup::new(&rt);
        let usages = UsageLookup::new(&rt, &Environment::new());
        let bar_rt = usages.def(&bar).unwrap().value().unwrap();

        let p = parent.parent(bar_rt).unwrap();
        assert_eq!(p.child_name, Some(EltId::Ident(bar_id)));
        assert_matches!(p.term.as_ref(), Term::RecRecord(..));

        let gp = parent.parent(&p.term).unwrap();
        assert_eq!(gp.child_name, Some(EltId::ArrayElt));
        assert_matches!(gp.term.as_ref(), Term::Array(..));

        let ggp = parent.parent(&gp.term).unwrap();
        assert_matches!(ggp.child_name, Some(EltId::Ident(_)));
        assert_matches!(ggp.term.as_ref(), Term::RecRecord(..));
    }
}
