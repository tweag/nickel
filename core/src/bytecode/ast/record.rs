use super::{Annotation, Ast, AstAlloc, TraverseAlloc, TraverseControl, TraverseOrder};

use crate::{
    identifier::{Ident, LocIdent},
    position::TermPos,
    term::IndexMap,
};

pub use crate::term::MergePriority;

use std::rc::Rc;

/// Element of a record field path in a record field definition. For example, in  `{ a."%{"hello-"
/// ++ "world"}".c = true }`, the path `a."%{b}".c` is composed of three elements: an identifier
/// `a`, an expression `"hello" ++ "world"`, and another identifier `c`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FieldPathElem<'ast> {
    /// A statically known identifier.
    Ident(LocIdent),
    /// A dynamic field name written as a quoted expression, e.g. `"%{protocol}" = .. `. Normally,
    /// the expression must be a [crate::bytecode::ast::Node::StringChunks], so we could store the
    /// chunks directly which would be more precise. However, it's useful to keep a general
    /// [crate::bytecode::ast::Ast] to store errors when part of the field path failed to parse
    /// correctly.
    Expr(Ast<'ast>),
}

impl<'ast> FieldPathElem<'ast> {
    /// Returns the position of the field path element.
    pub fn pos(&self) -> TermPos {
        match self {
            FieldPathElem::Ident(ident) => ident.pos,
            FieldPathElem::Expr(expr) => expr.pos,
        }
    }

    /// Crate a path composed of a single static identifier.
    pub fn single_ident_path(
        alloc: &'ast AstAlloc,
        ident: LocIdent,
    ) -> &'ast [FieldPathElem<'ast>] {
        alloc.alloc_singleton(FieldPathElem::Ident(ident))
    }

    /// Create a path composed of a single dynamic expression.
    pub fn single_expr_path(alloc: &'ast AstAlloc, expr: Ast<'ast>) -> &'ast [FieldPathElem<'ast>] {
        alloc.alloc_singleton(FieldPathElem::Expr(expr))
    }

    /// Try to interpret this element element as a static identifier. Returns `None` if the element
    /// is an expression with interpolation inside. Dual of [Self::try_as_dyn_expr].
    pub fn try_as_ident(&self) -> Option<LocIdent> {
        match self {
            FieldPathElem::Ident(ident) => Some(*ident),
            FieldPathElem::Expr(expr) => expr
                .node
                .try_str_chunk_as_static_str()
                .map(|s| LocIdent::from(s).with_pos(expr.pos)),
        }
    }

    /// Tries to interpret this element as a dynamic identifier. Returns `None` if the element is a
    /// static identifier (that is, if [Self::try_as_indent] returns `Some(_)`).
    pub fn try_as_dyn_expr(&self) -> Option<&Ast<'ast>> {
        match self {
            FieldPathElem::Expr(expr) if expr.node.try_str_chunk_as_static_str().is_none() => {
                Some(expr)
            }
            _ => None,
        }
    }
}

/// A field definition. A field is defined by a dot-separated path of identifier or interpolated
/// strings, a potential value, and associated metadata.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FieldDef<'ast> {
    /// A sequence of field path elements, composing the left hand side (with respect to the `=`)
    /// of the field definition.
    ///
    /// # Invariants
    ///
    /// **Important**: The path must be non-empty, or some of `FieldDef` methods will panic.
    pub path: &'ast [FieldPathElem<'ast>],
    /// The metadata and the optional value bundled as a field.
    pub metadata: FieldMetadata<'ast>,
    pub value: Option<Ast<'ast>>,
    /// The position of the whole field definition.
    pub pos: TermPos,
}

impl FieldDef<'_> {
    /// Returns the identifier corresponding to this definition if the path is composed of exactly
    /// one element which is a static identifier. Returns `None` otherwise.
    pub fn path_as_ident(&self) -> Option<LocIdent> {
        if let [elem] = self.path {
            elem.try_as_ident()
        } else {
            None
        }
    }

    /// Returns the declared field name, that is the last element of the path, as a static
    /// identifier. Returns `None` if the last element is an expression.
    pub fn name_as_ident(&self) -> Option<LocIdent> {
        self.path.last().expect("empty field path").try_as_ident()
    }

    /// Returns the root identifier of the field path, that is the first element of the path, as a
    /// static identifier. Returns `None` if the first element is an expression.
    pub fn root_as_ident(&self) -> Option<LocIdent> {
        self.path.first().expect("empty field path").try_as_ident()
    }
}

/// The metadata attached to record fields.
#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct FieldMetadata<'ast> {
    /// The documentation of the field.
    pub doc: Option<&'ast str>,
    /// Type and contract annotations.
    pub annotation: Annotation<'ast>,
    /// If the field is optional.
    pub opt: bool,
    /// If the field should be skipped during serialization.
    pub not_exported: bool,
    /// The merge priority.
    pub priority: MergePriority,
}

impl FieldMetadata<'_> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn is_empty(&self) -> bool {
        self.doc.is_none()
            && self.annotation.is_empty()
            && !self.opt
            && !self.not_exported
            && matches!(self.priority, MergePriority::Neutral)
    }
}

impl<'ast> From<Annotation<'ast>> for FieldMetadata<'ast> {
    fn from(annotation: Annotation<'ast>) -> Self {
        FieldMetadata {
            annotation,
            ..Default::default()
        }
    }
}

/// A nickel record literal.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Record<'ast> {
    /// Field definitions.
    pub field_defs: &'ast [FieldDef<'ast>],
    /// If the record is open, i.e. if it ended with `..`.
    pub open: bool,
}

impl<'ast> Record<'ast> {
    /// A record with no fields and the default set of attributes.
    pub fn empty() -> Self {
        Default::default()
    }

    /// Returns self with the open flag set to true.
    pub fn open(self) -> Self {
        Record { open: true, ..self }
    }

    /// Returns `false` if at least one field in the first layer of the record (that is the first
    /// element of each field path) is defined dynamically, and `true` otherwise.
    pub fn has_static_structure(&self) -> bool {
        self.field_defs
            .iter()
            .all(|field| field.path.iter().any(|elem| elem.try_as_ident().is_some()))
    }

    /// Returns the top-level static fields of this record.
    ///
    /// # Example
    ///
    /// The top-level static fields of this record are `foo` and `bar`:
    ///
    /// ```nickel
    /// {
    ///   foo.bar = 1,
    ///   foo.baz = 2,
    ///   bar.baz = 3,
    ///   "%{x}" = false,
    /// }
    /// ```
    pub fn toplvl_stat_fields(&self) -> Vec<LocIdent> {
        self.field_defs
            .iter()
            .filter_map(|field| field.path.first()?.try_as_ident())
            .collect()
    }

    /// Returns the top-level dynamically defined fields of this record.
    pub fn toplvl_dyn_fields(&self) -> Vec<&Ast<'ast>> {
        self.field_defs
            .iter()
            .filter_map(|field| field.path.first()?.try_as_dyn_expr())
            .collect()
    }

    /// Returns all the pieces that defines the field with the given identifier. This requires to
    /// make a linear search over this record.
    pub fn defs_of(&self, ident: Ident) -> Vec<&FieldDef<'ast>> {
        self.field_defs
            .iter()
            .filter(|field| {
                field
                    .path
                    .first()
                    .and_then(FieldPathElem::try_as_ident)
                    .is_some_and(|i| i.ident() == ident)
            })
            .collect()
    }

    /// Returns an iterator over all field definitions, grouped by the first identifier of their
    /// paths (that is, the field which they are defining). Field that aren't statically defined
    /// (i.e. whose path's first element isn't an ident) are ignored.
    pub fn group_by_field_id(&self) -> IndexMap<Ident, Vec<&FieldDef<'ast>>> {
        let mut map = IndexMap::new();

        for (id, field) in self.field_defs.iter().filter_map(|field| {
            field
                .path
                .first()
                .and_then(FieldPathElem::try_as_ident)
                .map(|i| (i, field))
        }) {
            map.entry(id.ident()).or_insert_with(Vec::new).push(field);
        }

        map
    }
}

impl<'ast> TraverseAlloc<'ast, Ast<'ast>> for FieldDef<'ast> {
    fn traverse<F, E>(
        self,
        alloc: &'ast AstAlloc,
        f: &mut F,
        order: TraverseOrder,
    ) -> Result<Self, E>
    where
        F: FnMut(Ast<'ast>) -> Result<Ast<'ast>, E>,
    {
        let path: Result<Vec<_>, E> = self
            .path
            .iter()
            .map(|elem| match elem {
                FieldPathElem::Ident(ident) => Ok(FieldPathElem::Ident(*ident)),
                FieldPathElem::Expr(expr) => expr
                    .clone()
                    .traverse(alloc, f, order)
                    .map(FieldPathElem::Expr),
            })
            .collect();

        let metadata = FieldMetadata {
            annotation: self.metadata.annotation.traverse(alloc, f, order)?,
            ..self.metadata
        };

        let value = self
            .value
            .map(|v| v.traverse(alloc, f, order))
            .transpose()?;

        Ok(FieldDef {
            path: alloc.alloc_many(path?),
            metadata,
            value,
            pos: self.pos,
        })
    }

    fn traverse_ref<S, U>(
        &'ast self,
        f: &mut dyn FnMut(&'ast Ast<'ast>, &S) -> TraverseControl<S, U>,
        scope: &S,
    ) -> Option<U> {
        self.path
            .iter()
            .find_map(|elem| match elem {
                FieldPathElem::Ident(_) => None,
                FieldPathElem::Expr(expr) => expr.traverse_ref(f, scope),
            })
            .or_else(|| self.metadata.annotation.traverse_ref(f, scope))
            .or_else(|| self.value.as_ref().and_then(|v| v.traverse_ref(f, scope)))
    }
}
