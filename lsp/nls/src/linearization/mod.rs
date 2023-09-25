use std::collections::HashMap;

use codespan::FileId;
use nickel_lang_core::{
    term::{RichTerm, Traverse, TraverseControl},
    typ::{Type, TypeF},
    typecheck::{linearization::Linearizer, reporting::NameReg, Extra, UnifType},
};

use crate::{
    field_walker::DefWithPath, identifier::LocIdent, position::PositionLookup, term::RichTermPtr,
    usage::UsageLookup,
};

#[derive(Clone, Debug)]
pub struct ParentLookup {
    table: HashMap<RichTermPtr, RichTerm>,
}

impl ParentLookup {
    pub fn new(rt: &RichTerm) -> Self {
        let mut table = HashMap::new();
        let mut traverse_merge =
            |rt: &RichTerm, parent: &Option<RichTerm>| -> TraverseControl<Option<RichTerm>, ()> {
                if let Some(parent) = parent {
                    table.insert(RichTermPtr(rt.clone()), parent.clone());
                }
                TraverseControl::ContinueWithScope(Some(rt.clone()))
            };

        rt.traverse_ref(&mut traverse_merge, &None);

        ParentLookup { table }
    }

    pub fn parent(&self, rt: &RichTerm) -> Option<&RichTerm> {
        self.table.get(&RichTermPtr(rt.clone()))
    }

    pub fn parent_chain<'a>(&'a self, rt: &'a RichTerm) -> ParentChainIter<'_> {
        ParentChainIter {
            table: self,
            next: Some(rt),
        }
    }
}

pub struct ParentChainIter<'a> {
    table: &'a ParentLookup,
    next: Option<&'a RichTerm>,
}

impl<'a> Iterator for ParentChainIter<'a> {
    type Item = &'a RichTerm;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.next {
            self.next = self.table.parent(next);
            Some(next)
        } else {
            None
        }
    }
}

/// TODO: rename and re-doc
#[derive(Clone, Default, Debug)]
pub struct LinRegistry {
    // Most of these tables do one more lookup than necessary: they look up a
    // file id and then they look up a term in an inner table. This is a little
    // inefficient for lookups, but it makes it easy to invalidate a whole file
    // in one go.
    pub position_lookups: HashMap<FileId, PositionLookup>,
    pub usage_lookups: HashMap<FileId, UsageLookup>,
    pub parent_lookups: HashMap<FileId, ParentLookup>,
    pub type_lookups: HashMap<FileId, CollectedTypes<Type>>,
}

impl LinRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(
        &mut self,
        file_id: FileId,
        type_lookups: CollectedTypes<Type>,
        term: &RichTerm,
        initial_env: &crate::usage::Environment,
    ) {
        self.position_lookups
            .insert(file_id, PositionLookup::new(term));
        self.usage_lookups
            .insert(file_id, UsageLookup::new(term, initial_env));
        self.parent_lookups.insert(file_id, ParentLookup::new(term));
        self.type_lookups.insert(file_id, type_lookups);
    }

    pub fn get_def(&self, ident: &LocIdent) -> Option<&DefWithPath> {
        let file = ident.pos.as_opt_ref()?.src_id;
        self.usage_lookups.get(&file)?.def(ident)
    }

    pub fn get_usages(&self, ident: &LocIdent) -> impl Iterator<Item = &LocIdent> {
        fn inner<'a>(
            slf: &'a LinRegistry,
            ident: &LocIdent,
        ) -> Option<impl Iterator<Item = &'a LocIdent>> {
            let file = ident.pos.as_opt_ref()?.src_id;
            Some(slf.usage_lookups.get(&file)?.usages(ident))
        }
        inner(self, ident).into_iter().flatten()
    }

    pub fn get_env(&self, rt: &RichTerm) -> Option<&crate::usage::Environment> {
        let file = rt.pos.as_opt_ref()?.src_id;
        self.usage_lookups.get(&file)?.env(rt)
    }

    pub fn get_type(&self, rt: &RichTerm) -> Option<&Type> {
        let file = rt.pos.as_opt_ref()?.src_id;
        self.type_lookups
            .get(&file)?
            .terms
            .get(&RichTermPtr(rt.clone()))
    }

    pub fn get_type_for_ident(&self, id: &LocIdent) -> Option<&Type> {
        let file = id.pos.as_opt_ref()?.src_id;
        self.type_lookups.get(&file)?.idents.get(id)
    }

    pub fn get_parent_chain<'a>(&'a self, rt: &'a RichTerm) -> Option<ParentChainIter<'a>> {
        let file = rt.pos.as_opt_ref()?.src_id;
        Some(self.parent_lookups.get(&file)?.parent_chain(rt))
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
