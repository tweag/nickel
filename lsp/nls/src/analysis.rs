use std::collections::HashMap;

use nickel_lang_core::{
    bytecode::ast::{primop::PrimOp, typ::Type, Ast, AstAlloc, Node},
    files::FileId,
    identifier::Ident,
    position::RawSpan,
    term::{BinaryOp, RichTerm, Term, UnaryOp},
    traverse::{Traverse, TraverseAlloc, TraverseControl},
    typ::TypeF,
    typecheck::{
        reporting::{NameReg, ToType},
        TypeTables, TypecheckVisitor, UnifType,
    },
};

use crate::{
    field_walker::{Def, EltId},
    identifier::LocIdent,
    position::PositionLookup,
    term::AstPtr,
    usage::{Environment, UsageLookup},
};

#[derive(Clone, Debug)]
pub struct Parent<'ast> {
    pub ast: &'ast Ast<'ast>,
    pub child_name: Option<EltId>,
}

impl<'ast> From<&'ast Ast<'ast>> for Parent<'ast> {
    fn from(ast: &'ast Ast<'ast>) -> Self {
        Parent {
            ast,
            child_name: None,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ParentLookup<'ast> {
    table: HashMap<AstPtr<'ast>, Parent<'ast>>,
}

impl<'ast> ParentLookup<'ast> {
    pub fn new(ast: &'ast Ast<'ast>) -> Self {
        let mut table = HashMap::new();

        fn traversal<'ast>(
            ast: &'ast Ast<'ast>,
            parent: &Option<Parent<'ast>>,
            acc: &mut HashMap<AstPtr<'ast>, Parent<'ast>>,
        ) -> TraverseControl<Option<Parent<'ast>>, ()> {
            if let Some(parent) = parent {
                acc.insert(AstPtr(ast), parent.clone());
            }

            match ast.node {
                Node::Record(data) => {
                    todo!();
                    // for (name, field) in &data.fields {
                    //     if let Some(child) = &field.value {
                    //         let parent = Parent {
                    //             ast: ast.clone(),
                    //             child_name: Some(name.ident().into()),
                    //         };
                    //         child.traverse_ref(
                    //             &mut |rt, parent| traversal(rt, parent, acc),
                    //             &Some(parent),
                    //         );
                    //     }
                    // }

                    // if let Term::RecRecord(_, dynamic, _) = ast.as_ref() {
                    //     let parent = Parent {
                    //         ast: ast.clone(),
                    //         child_name: None,
                    //     };
                    //     for (name_term, field) in dynamic {
                    //         name_term.traverse_ref(
                    //             &mut |rt, parent| traversal(rt, parent, acc),
                    //             &Some(parent.clone()),
                    //         );
                    //         if let Some(child) = &field.value {
                    //             child.traverse_ref(
                    //                 &mut |rt, parent| traversal(rt, parent, acc),
                    //                 &Some(parent.clone()),
                    //             );
                    //         }
                    //     }
                    // }
                    TraverseControl::SkipBranch
                }
                Node::Array(elts) => {
                    for elt in elts.iter() {
                        let parent = Parent {
                            ast,
                            child_name: Some(EltId::ArrayElt),
                        };
                        elt.traverse_ref(
                            &mut |rt, parent| traversal(rt, parent, acc),
                            &Some(parent),
                        );
                    }
                    TraverseControl::SkipBranch
                }
                _ => TraverseControl::ContinueWithScope(Some(ast.into())),
            }
        }

        ast.traverse_ref(&mut |ast, parent| traversal(ast, parent, &mut table), &None);

        ParentLookup { table }
    }

    pub fn parent(&self, ast: &'ast Ast<'ast>) -> Option<&Parent<'ast>> {
        self.table.get(&AstPtr(ast))
    }

    pub fn parent_chain(&self, ast: &'ast Ast<'ast>) -> ParentChainIter<'ast, '_> {
        let next = self.parent(ast).cloned();
        ParentChainIter {
            table: self,
            path: Some(Vec::new()),
            next,
        }
    }
}

fn find_static_accesses<'ast>(ast: &'ast Ast<'ast>) -> HashMap<Ident, Vec<&'ast Ast<'ast>>> {
    let mut map: HashMap<Ident, Vec<&'ast Ast<'ast>>> = HashMap::new();

    ast.traverse_ref(
        &mut |ast: &'ast Ast<'ast>, _scope: &()| {
            if let Node::PrimOpApp {
                op: PrimOp::RecordStatAccess(id),
                ..
            } = &ast.node
            {
                map.entry(id.ident()).or_default().push(ast);
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
pub struct ParentChainIter<'ast, 'a> {
    table: &'a ParentLookup<'ast>,
    path: Option<Vec<EltId>>,
    next: Option<Parent<'ast>>,
}

impl<'ast> ParentChainIter<'ast, '_> {
    pub fn next(&mut self) -> Option<&'ast Ast<'ast>> {
        if let Some(next) = self.next.take() {
            if let Some((ident, path)) = next.child_name.zip(self.path.as_mut()) {
                path.push(ident);
            }

            if !matches!(
                &next.ast.node,
                Node::Record(_) | Node::Annotated { .. } | Node::Array(..)
            ) && !matches!(&next.ast.node, Node::PrimOpApp { op, ..} if op.arity() == 2)
            {
                self.path = None;
            }
            self.next = self.table.parent(next.ast).cloned();

            Some(next.ast)
        } else {
            None
        }
    }

    /// Like `next`, but skips over everything except for merges, annotations, and records.
    pub fn next_merge(&mut self) -> Option<&'ast Ast<'ast>> {
        let is_fieldy_term = |ast: &Ast<'ast>| {
            matches!(
                &ast.node,
                Node::PrimOpApp {
                    op: PrimOp::Merge(_),
                    ..
                } | Node::Annotated { .. }
                    | Node::Record(_)
            )
        };

        let is_merge_term = |ast: &Ast<'ast>| {
            matches!(
                &ast.node,
                Node::PrimOpApp {
                    op: PrimOp::Merge(_),
                    ..
                } | Node::Annotated { .. }
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
    pub fn peek_gp(&self) -> Option<&'ast Ast<'ast>> {
        if let Some(Parent { ast, .. }) = &self.next {
            self.table.parent(ast).map(|gp| gp.ast)
        } else {
            None
        }
    }
}

/// The initial analysis that we collect for a file.
///
/// This analysis is re-collected from scratch each time the file is updated.
#[derive(Default, Debug)]
pub struct Analysis<'ast> {
    pub position_lookup: PositionLookup<'ast>,
    pub usage_lookup: UsageLookup<'ast>,
    pub parent_lookup: ParentLookup<'ast>,
    pub type_lookup: CollectedTypes<'ast, Type<'ast>>,

    /// A lookup table for static accesses, for looking up all occurrences of,
    /// say, `.foo` in a file.
    pub static_accesses: HashMap<Ident, Vec<&'ast Ast<'ast>>>,
}

impl<'ast> Analysis<'ast> {
    pub fn new(
        ast: &'ast Ast<'ast>,
        type_lookup: CollectedTypes<'ast, Type<'ast>>,
        initial_env: &Environment<'ast>,
    ) -> Self {
        Self {
            position_lookup: PositionLookup::new(ast),
            usage_lookup: UsageLookup::new(ast, initial_env),
            parent_lookup: ParentLookup::new(ast),
            static_accesses: find_static_accesses(ast),
            type_lookup,
        }
    }
}

/// The collection of analyses for every file that we know about.
#[derive(Default, Debug)]
pub struct AnalysisRegistry<'ast> {
    // Most of the fields of `Analysis` are themselves hash tables. Having
    // a table of tables requires more lookups than necessary, but it makes
    // it easy to invalidate a whole file.
    pub analysis: HashMap<FileId, Analysis<'ast>>,
}

impl<'ast> AnalysisRegistry<'ast> {
    pub fn insert(
        &mut self,
        file_id: FileId,
        type_lookups: CollectedTypes<'ast, Type<'ast>>,
        ast: &'ast Ast<'ast>,
        initial_env: &crate::usage::Environment<'ast>,
    ) {
        self.analysis
            .insert(file_id, Analysis::new(ast, type_lookups, initial_env));
    }

    /// Inserts a new file into the analysis, but only generates usage analysis for it.
    ///
    /// This is useful for temporary little pieces of input (like parts extracted from incomplete input)
    /// that need variable resolution but not the full analysis.
    pub fn insert_usage(
        &mut self,
        file_id: FileId,
        term: &'ast Ast<'ast>,
        initial_env: &Environment<'ast>,
    ) {
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

    pub fn get_env(&self, ast: &'ast Ast<'ast>) -> Option<&Environment<'ast>> {
        let file = ast.pos.as_opt_ref()?.src_id;
        self.analysis.get(&file)?.usage_lookup.env(ast)
    }

    pub fn get_type(&self, ast: &'ast Ast<'ast>) -> Option<&Type<'ast>> {
        let file = ast.pos.as_opt_ref()?.src_id;
        self.analysis
            .get(&file)?
            .type_lookup
            .terms
            .get(&AstPtr(ast))
    }

    pub fn get_type_for_ident(&self, id: &LocIdent) -> Option<&Type> {
        let file = id.pos.as_opt_ref()?.src_id;
        self.analysis.get(&file)?.type_lookup.idents.get(id)
    }

    pub fn get_parent_chain<'a>(
        &'a self,
        ast: &'ast Ast<'ast>,
    ) -> Option<ParentChainIter<'ast, 'a>> {
        let file = ast.pos.as_opt_ref()?.src_id;
        Some(self.analysis.get(&file)?.parent_lookup.parent_chain(ast))
    }

    pub fn get_parent<'a>(&'a self, ast: &'ast Ast<'ast>) -> Option<&'a Parent<'ast>> {
        let file = ast.pos.as_opt_ref()?.src_id;
        self.analysis.get(&file)?.parent_lookup.parent(ast)
    }

    pub fn get_static_accesses(&self, id: Ident) -> Vec<&'ast Ast<'ast>> {
        self.analysis
            .values()
            .filter_map(|a| a.static_accesses.get(&id))
            .flatten()
            .cloned()
            .collect()
    }
}

#[derive(Debug, Default)]
pub struct TypeCollector<'ast> {
    tables: CollectedTypes<'ast, UnifType<'ast>>,
}

#[derive(Clone, Debug)]
pub struct CollectedTypes<'ast, Ty> {
    pub terms: HashMap<AstPtr<'ast>, Ty>,
    pub idents: HashMap<LocIdent, Ty>,
}

impl<'ast, Ty> Default for CollectedTypes<'ast, Ty> {
    fn default() -> Self {
        Self {
            terms: Default::default(),
            idents: Default::default(),
        }
    }
}

impl<'ast> TypecheckVisitor<'ast> for TypeCollector<'ast> {
    fn visit_term(&mut self, ast: &'ast Ast<'ast>, ty: UnifType<'ast>) {
        self.tables.terms.insert(AstPtr(ast), ty);
    }

    fn visit_ident(
        &mut self,
        ident: &nickel_lang_core::identifier::LocIdent,
        new_type: UnifType<'ast>,
    ) {
        self.tables.idents.insert((*ident).into(), new_type);
    }
}

impl<'ast> TypeCollector<'ast> {
    pub fn complete(
        self,
        alloc: &'ast AstAlloc,
        type_tables: TypeTables<'ast>,
    ) -> CollectedTypes<'ast, Type<'ast>> {
        let mut name_reg = NameReg::new(type_tables.names.clone());

        let mut transform_type = |uty: UnifType<'ast>| -> Type<'ast> {
            let ty = uty.to_type(alloc, &mut name_reg, &type_tables.table);
            match ty.typ {
                TypeF::Wildcard(i) => type_tables.wildcards.get(i).unwrap_or(&ty).clone(),
                _ => ty,
            }
        };

        // See [^disable-clippy-mutable-key-type]
        #[allow(clippy::mutable_key_type)]
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
    use codespan::ByteIndex;
    use nickel_lang_core::{
        bytecode::ast::{AstAlloc, Node},
        files::Files,
        identifier::Ident,
        parser::{grammar, lexer, ErrorTolerantParser as _},
    };

    use crate::{
        field_walker::EltId,
        position::{tests::parse, PositionLookup},
        usage::{tests::locced, Environment, UsageLookup},
    };

    use super::ParentLookup;

    #[test]
    fn parent_chain() {
        let alloc = AstAlloc::new();

        let (file, rt) = parse(&alloc, "{ foo = [{ bar = 1 }] }");
        let bar_id = Ident::new("bar");
        let bar = locced(bar_id, file, 11..14);

        let parent = ParentLookup::new(&rt);
        let usages = UsageLookup::new(&rt, &Environment::new());
        let values = usages.def(&bar).unwrap().values();

        assert_eq!(values.len(), 1);
        let bar_val = values.into_iter().next().unwrap();

        let p = parent.parent(bar_val).unwrap();
        assert_eq!(p.child_name, Some(EltId::Ident(bar_id)));
        assert_matches!(&p.ast.node, Node::Record(_));

        let gp = parent.parent(&p.ast).unwrap();
        assert_eq!(gp.child_name, Some(EltId::ArrayElt));
        assert_matches!(&gp.ast.node, Node::Array { .. });

        let ggp = parent.parent(&gp.ast).unwrap();
        assert_matches!(ggp.child_name, Some(EltId::Ident(_)));
        assert_matches!(&ggp.ast.node, Node::Record(_));
    }

    #[test]
    fn parse_error_parent() {
        let alloc = AstAlloc::new();

        // The field that fails to parse should have a record as its parent.
        let s = "{ field. }";
        let file = Files::new().add("<test>", s.to_owned());

        let (ast, _errors) = grammar::TermParser::new()
            .parse_tolerant(&alloc, file, lexer::Lexer::new(s))
            .unwrap();

        let parent = ParentLookup::new(&ast);
        let positions = PositionLookup::new(&ast);
        let err = positions.get(ByteIndex(5)).unwrap();

        dbg!(&ast, err);

        let p = parent.parent(err).unwrap();
        assert!(p.child_name.is_none());
        assert_matches!(&p.ast.node, Node::Record(_));
    }
}
