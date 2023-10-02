use std::collections::HashMap;

use codespan::FileId;
use nickel_lang_core::{
    identifier::Ident,
    term::{RichTerm, Term, Traverse, TraverseControl},
    typ::{Type, TypeF},
    typecheck::{linearization::Linearizer, reporting::NameReg, Extra, UnifType},
};

use crate::{
    field_walker::DefWithPath,
    identifier::LocIdent,
    position::PositionLookup,
    term::RichTermPtr,
    usage::{Environment, UsageLookup},
};

#[derive(Clone, Debug)]
pub struct Parent {
    term: RichTerm,
    child_name: Option<Ident>,
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
                                child_name: Some(name.ident()),
                            };
                            child.traverse_ref(
                                &mut |rt, parent| traversal(rt, parent, acc),
                                &Some(parent),
                            );
                        }
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

/// An iterator over parents in the AST that also keeps track of a record path
/// from the ancestor to the original element.
///
/// For example, suppose we make a parent chain starting at `1` in
/// `{ foo = { bar = { baz = 1 } } }`. Then the first parent is `{baz = 1}`, and
/// the original element is accessible at the path `baz`. The grandparent is
/// `{ bar = { baz = 1 } }` and the original element is accessible at the path
/// `bar.baz`, and so on.
///
/// The path part of this analysis is only approximate: it can see through
/// records, merges, and annotations, but not arbitrary expressions.
pub struct ParentChainIter<'a> {
    table: &'a ParentLookup,
    path: Option<Vec<Ident>>,
    next: Option<Parent>,
}

impl<'a> Iterator for ParentChainIter<'a> {
    // TODO: we're cloning the path on each iteration.
    // did rust ever settle on a "streaming iterator" trait?
    /// A pair of `(ancestor, path_to_the_original)`.
    ///
    /// Once we stop being able to track the path to the original element,
    /// we return `None` for the path.
    type Item = (RichTerm, Option<Vec<Ident>>);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.next.take() {
            if let Some((ident, path)) = next.child_name.zip(self.path.as_mut()) {
                path.push(ident);
            }
            if !matches!(
                next.term.as_ref(),
                Term::Record(_) | Term::RecRecord(..) | Term::Annotated(..) | Term::Op2(..)
            ) {
                self.path = None;
            }
            self.next = self.table.parent(&next.term).cloned();

            let path = self
                .path
                .as_ref()
                .map(|p| p.iter().copied().rev().collect());
            Some((next.term, path))
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

    pub fn get_def(&self, ident: &LocIdent) -> Option<&DefWithPath> {
        let file = ident.pos.as_opt_ref()?.src_id;
        self.analysis.get(&file)?.usage_lookup.def(ident)
    }

    pub fn get_usages(&self, ident: &LocIdent) -> impl Iterator<Item = &LocIdent> {
        fn inner<'a>(
            slf: &'a AnalysisRegistry,
            ident: &LocIdent,
        ) -> Option<impl Iterator<Item = &'a LocIdent>> {
            let file = ident.pos.as_opt_ref()?.src_id;
            Some(slf.analysis.get(&file)?.usage_lookup.usages(ident))
        }
        inner(self, ident).into_iter().flatten()
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
}

#[derive(Default)]
pub struct TypeCollector {
    // Store a copy of the terms we've added so far. The index in this array is their ItemId.
    term_ids: Vec<RichTermPtr>,
}

#[derive(Clone, Debug)]
pub struct CollectedTypes<Ty> {
    pub terms: HashMap<RichTermPtr, Ty>,
    pub idents: HashMap<LocIdent, Ty>,
}

impl<Ty> Default for CollectedTypes<Ty> {
    fn default() -> Self {
        Self {
            terms: HashMap::new(),
            idents: HashMap::new(),
        }
    }
}

impl Linearizer for TypeCollector {
    type Building = CollectedTypes<UnifType>;
    type Completed = CollectedTypes<Type>;
    type CompletionExtra = Extra;
    type ItemId = usize;

    fn scope(&mut self) -> Self {
        TypeCollector::default()
    }

    fn scope_meta(&mut self) -> Self {
        TypeCollector::default()
    }

    fn add_term(&mut self, lin: &mut Self::Building, rt: &RichTerm, ty: UnifType) -> Option<usize> {
        self.term_ids.push(RichTermPtr(rt.clone()));
        lin.terms.insert(RichTermPtr(rt.clone()), ty);
        Some(self.term_ids.len() - 1)
    }

    fn complete(
        self,
        lin: Self::Building,
        Extra {
            table,
            names,
            wildcards,
        }: &Extra,
    ) -> Self::Completed {
        let mut name_reg = NameReg::new(names.clone());

        let mut transform_type = |uty: UnifType| -> Type {
            let ty = name_reg.to_type(table, uty);
            match ty.typ {
                TypeF::Wildcard(i) => wildcards.get(i).unwrap_or(&ty).clone(),
                _ => ty,
            }
        };

        let terms = lin
            .terms
            .into_iter()
            .map(|(rt, uty)| (rt, transform_type(uty)))
            .collect();
        let idents = lin
            .idents
            .into_iter()
            .map(|(id, uty)| (id, transform_type(uty)))
            .collect();
        CollectedTypes { terms, idents }
    }

    fn retype(&mut self, lin: &mut Self::Building, item_id: Option<usize>, new_type: UnifType) {
        if let Some(id) = item_id {
            lin.terms.insert(self.term_ids[id].clone(), new_type);
        }
    }

    fn retype_ident(
        &mut self,
        lin: &mut Self::Building,
        ident: &nickel_lang_core::identifier::LocIdent,
        new_type: UnifType,
    ) {
        lin.idents.insert((*ident).into(), new_type);
    }
}
