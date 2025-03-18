use std::{borrow::Cow, cell::RefCell, collections::HashSet};

use nickel_lang_core::{
    bytecode::ast::{
        primop::PrimOp,
        record::{FieldDef, FieldMetadata, Record as RecordData},
        typ::{iter::*, RecordRows, Type, TypeF},
        Annotation, Ast, LetMetadata, Node,
    },
    identifier::Ident,
    position::RawSpan,
    pretty::ident_quoted,
    typecheck::AnnotSeqRef,
};

use crate::{identifier::LocIdent, requests::completion::CompletionItem, world::World};

/// Either a record term or a record type.
#[derive(Clone, Debug, PartialEq)]
pub enum Record<'ast> {
    RecordTerm(&'ast RecordData<'ast>),
    RecordType(&'ast RecordRows<'ast>),
    /// **Invariant**: note that a final value definition isn't a proper container, it's akin to a
    /// stand-alone expression (which might be resolved to a container again). Thus, we impose the
    /// invariant that the piece IS NOT a final value, that is `field_def_piece.is_final_value() ==
    /// false`.
    FieldDefPiece(FieldDefPiece<'ast>),
}

impl<'ast> Record<'ast> {
    pub fn defs_of(&self, id: Ident) -> Vec<(LocIdent, Option<FieldDefPiece<'ast>>)> {
        match self {
            Record::RecordTerm(data) => piece_defs_of(data, id)
                .into_iter()
                // unwrap(): the fields returned by `defs_of` necessarily have their predecessor
                // and its ident defined.
                .map(|def_piece| {
                    (
                        def_piece.backtrack().unwrap().ident().unwrap(),
                        Some(def_piece),
                    )
                })
                .collect(),
            Record::RecordType(rows) => rows
                .find_path(&[id])
                .map(|r| (r.id.into(), None))
                .into_iter()
                .collect(),
            Record::FieldDefPiece(def_piece) => {
                let ident = def_piece.ident().unwrap();
                vec![(ident, Some(*def_piece))]
            }
        }
    }

    pub fn field_locs(&self, id: Ident) -> Vec<LocIdent> {
        self.defs_of(id).iter().map(|pair| pair.0).collect()
    }

    pub fn field_pieces(&self, id: Ident) -> Vec<FieldDefPiece<'ast>> {
        self.defs_of(id).iter().filter_map(|pair| pair.1).collect()
    }

    /// Returns a [`CompletionItem`] for every field in this record.
    pub fn completion_items(&self) -> Vec<CompletionItem<'ast>> {
        match self {
            Record::RecordTerm(data) => data
                .group_by_field_id()
                .iter()
                .map(|(_id, val)| {
                    // unwrap(): if an indentifier is in the group, it has at least one definition.
                    // unwrap(): if a definition ends up grouped here, it must have a static
                    // identifier as the root identifier.
                    //
                    // We arbitrarily take the first occurrence of the group to get a location for
                    // this ident.
                    let loc_id = val.first().unwrap().root_as_ident().unwrap();

                    CompletionItem {
                        label: ident_quoted(&loc_id),
                        metadata: val.iter().map(|def| Cow::Borrowed(&def.metadata)).collect(),
                        ident: Some(loc_id.into()),
                    }
                })
                .collect(),
            Record::RecordType(rows) => rows
                .iter()
                .filter_map(|r| match r {
                    RecordRowsItem::TailDyn => None,
                    RecordRowsItem::TailVar(_) => None,
                    RecordRowsItem::Row(r) => Some(CompletionItem {
                        label: ident_quoted(&r.id),
                        metadata: vec![Cow::Owned(FieldMetadata {
                            annotation: Annotation {
                                typ: Some(r.typ.clone()),
                                contracts: &[],
                            },
                            ..Default::default()
                        })],
                        //detail: vec![r.typ.to_string()],
                        ..Default::default()
                    }),
                })
                .collect(),
            Record::FieldDefPiece(def_piece) => def_piece
                .ident()
                .map(|id| {
                    vec![CompletionItem {
                        label: ident_quoted(&(id.into())),
                        metadata: def_piece
                            .metadata()
                            .map(|metadata| vec![Cow::Borrowed(metadata)])
                            .unwrap_or_default(),
                        ident: Some(id),
                    }]
                })
                .unwrap_or_default(),
        }
    }
}

impl<'ast> TryFrom<Container<'ast>> for Record<'ast> {
    type Error = ();

    fn try_from(c: Container<'ast>) -> Result<Self, Self::Error> {
        match c {
            Container::RecordTerm(r) => Ok(Record::RecordTerm(r)),
            Container::RecordType(r) => Ok(Record::RecordType(r)),
            Container::FieldDefPiece(fdp) => Ok(Record::FieldDefPiece(fdp)),
            Container::Dict(_) => Err(()),
            Container::Array(_) => Err(()),
        }
    }
}

impl<'ast> TryFrom<FieldDefPiece<'ast>> for Record<'ast> {
    type Error = ();

    fn try_from(fdp: FieldDefPiece<'ast>) -> Result<Self, Self::Error> {
        if fdp.is_final_value() {
            Err(())
        } else {
            Ok(Record::FieldDefPiece(fdp))
        }
    }
}

impl<'ast> From<Record<'ast>> for Container<'ast> {
    fn from(r: Record<'ast>) -> Self {
        match r {
            Record::RecordTerm(rt) => Container::RecordTerm(rt),
            Record::RecordType(rt) => Container::RecordType(rt),
            Record::FieldDefPiece(fdp) => Container::FieldDefPiece(fdp),
        }
    }
}

impl<'ast> TryFrom<FieldDefPiece<'ast>> for Container<'ast> {
    type Error = ();

    fn try_from(fdp: FieldDefPiece<'ast>) -> Result<Self, Self::Error> {
        if fdp.is_final_value() {
            Err(())
        } else {
            Ok(Container::FieldDefPiece(fdp))
        }
    }
}

/// A `Container` is something that has elements.
///
/// The elements could have names (e.g. in a record) or not (e.g. in an array).
/// The public interface of this module is only interested in the record
/// variants (see [`Record`]), but our internal resolution functions also need
/// to be transparent to other types of containers.
#[derive(Clone, Debug, PartialEq)]
pub enum Container<'ast> {
    RecordTerm(&'ast RecordData<'ast>),
    /// When resolving piecewise field defintion, such as `{ foo.bar.baz = 1}`, we need to
    /// represent the intermediate container `bar.baz = 1`.
    ///
    /// **Invariant**: note that a final value definition isn't a proper container, it's akin to a
    /// stand-alone expression (which might be resolved to a container again). Thus, we impose the
    /// invariant that the piece IS NOT a final value, that is `field_def_piece.is_final_value() ==
    /// false`.
    FieldDefPiece(FieldDefPiece<'ast>),
    RecordType(&'ast RecordRows<'ast>),
    Dict(&'ast Type<'ast>),
    Array(&'ast Type<'ast>),
}

/// A `ChildId` identifies an element of a container.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EltId {
    /// A named element, for example the field of a record.
    Ident(Ident),
    /// A dynamically named element, for example a dynamic field definition in a record.
    DynIdent,
    /// An array element.
    ArrayElt,
}

impl From<Ident> for EltId {
    fn from(id: Ident) -> Self {
        EltId::Ident(id)
    }
}

impl<'ast> Container<'ast> {
    /// If this `Container` has a field named `id`, returns its value. There might be several of
    /// them because of piecewise definitions.
    pub fn get(&self, id: EltId) -> Vec<FieldContent<'ast>> {
        match (self, id) {
            (Container::RecordTerm(data), EltId::Ident(id)) => piece_defs_of(data, id)
                .into_iter()
                .map(FieldContent::FieldDefPiece)
                .collect(),
            (Container::FieldDefPiece(field_def_piece), EltId::Ident(id)) => {
                match field_def_piece.ident() {
                    // unwrap(): if `def_id` is defined, `field_def_piece` can't be a final value,
                    // so `advance()` must be defined.
                    Some(def_id) if def_id.ident == id => vec![FieldContent::FieldDefPiece(
                        (*field_def_piece).advance().unwrap(),
                    )],
                    _ => Vec::new(),
                }
            }
            (Container::Dict(ty), EltId::Ident(_)) => vec![FieldContent::Type(ty)],
            (Container::RecordType(rows), EltId::Ident(id)) => rows
                .find_path(&[id])
                .map(|row| FieldContent::Type(row.typ))
                .into_iter()
                .collect(),
            (Container::Array(ty), EltId::ArrayElt) => vec![FieldContent::Type(ty)],
            _ => Vec::new(),
        }
    }

    /// If this `Container` is a record term, try to retrieve all the pieces that define the field
    /// `id`, if any.
    pub fn get_field_def_pieces(&self, id: Ident) -> Vec<FieldDefPiece<'ast>> {
        self.get(id.into())
            .into_iter()
            .filter_map(|fc| match fc {
                FieldContent::FieldDefPiece(fdp) => Some(fdp),
                _ => None,
            })
            .collect()
    }
}

/// [`Container`]s can have fields that are either record fields or types which is represented by
/// [FieldContent].
///
/// It is convenient, as often, to be able to talk about a piecewise field defintion _at some
/// index_, for example in `{ foo.bar.baz = 1}`, the definition of `foo` which is `bar.baz = 1`. We
/// thus use [FieldDefPiece]s.
#[derive(Clone, Debug, PartialEq)]
enum FieldContent<'ast> {
    FieldDefPiece(FieldDefPiece<'ast>),
    Type(&'ast Type<'ast>),
}

/// Store a definition that might be part of a larger piecewise definition (but it can also be a
/// standard, complete definition).
///
/// We mostly store a [nickel_lang_core::bytecode::ast::record::FieldDef] but with an additional
/// suffix of the original path.
///
/// Indeed, take the defintion `{ foo.bar.baz = 1}`. This will lead to 3 [Def]s: one for `foo`, one
/// for `bar` and one for `baz`. To differentiate them, we store the suffix of the path that
/// corresponds to the definition: it will be the whole `[foo, bar, baz]` for the first definition,
/// then `[bar, baz]` for the second and `[baz]` for the last. Note that we can recover the
/// identifier by looking at the first element of the path, so we don't store it.
///
/// As a useful generalization for containers, we also accept an empty path (that is, `index` is
/// equal to `field_def.path.len()`) to represent the value alone, that can't be indexed further.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FieldDefPiece<'ast> {
    /// The index into the path of [Self::field_def] that represents the definition.
    ///
    /// # Invariants
    ///
    /// The following invariants must be respected when constructing or modifying a
    /// `FieldDefPiece`:
    ///
    /// - `index <= field_def.path.len()`
    /// - `index == field_def.path.len() || field_def.path[index].try_as_ident().is_some()`
    pub(crate) index: usize,
    /// The corresponding field definition.
    pub(crate) field_def: &'ast FieldDef<'ast>,
}

impl<'ast> FieldDefPiece<'ast> {
    /// Advances the index in the path of the field definition, or return `None` if we are at the
    /// end.
    pub(crate) fn advance(self) -> Option<Self> {
        if self.index == self.field_def.path.len() {
            None
        } else {
            Some(FieldDefPiece {
                index: self.index + 1,
                field_def: self.field_def,
            })
        }
    }

    /// Backtracks the index in the path of the field definition, or return `None` if we are at the
    /// beginning.
    pub(crate) fn backtrack(self) -> Option<Self> {
        self.index.checked_sub(1).map(|index| FieldDefPiece {
            index,
            field_def: self.field_def,
        })
    }

    /// Returns the metadata associated to this definition. Metadata are returned only if this
    /// piece is a leaf; see [Self::is_leaf].
    pub(crate) fn metadata(&self) -> Option<&'ast FieldMetadata<'ast>> {
        if self.is_leaf() {
            Some(&self.field_def.metadata)
        } else {
            None
        }
    }

    /// Returns the value associated to this definition. Values are returned only if this piece is
    /// a leaf; See [Self::is_leaf].
    pub(crate) fn value(&self) -> Option<&'ast Ast<'ast>> {
        if self.is_leaf() {
            self.field_def.value.as_ref()
        } else {
            None
        }
    }

    /// Returns the identifier that this piece defines, if any.
    pub(crate) fn ident(&self) -> Option<LocIdent> {
        self.field_def
            .path
            .get(self.index)
            .and_then(|path_elem| path_elem.try_as_ident().map(LocIdent::from))
    }

    /// Checks if this definition piece is simple, that is `self.index` is equal to
    /// `self.field_def.path.len() - 1`.
    ///
    /// # Example
    ///
    /// In `{ foo.bar.baz | C = 2 }`, the definition pieces of index `0`, and `1` (defining
    /// respectively `foo` and `bar`) are not simple, while the one of index `2` (defining `baz`)
    /// is.
    pub(crate) fn is_simple(&self) -> bool {
        self.index == self.field_def.path.len() - 1
    }

    /// Checks if this definition piece is leaf. A leaf definition is a either a simple definition
    /// ([Self::is_simple]) or a final value definition ([Self::is_final_value]).
    pub(crate) fn is_leaf(&self) -> bool {
        self.is_simple() || self.is_final_value()
    }

    /// Checks if this definition piece is a final value, that is `self.index` is equal to
    /// `self.field_def.path.len()`.
    pub(crate) fn is_final_value(&self) -> bool {
        self.index == self.field_def.path.len()
    }

    /// Return the span of this definition piece. It starts at the field path element at
    /// `self.index` and ends at the end of the field definition (either the end of the value, or
    /// the end of the last annotation if there's no value).
    ///
    /// # Returns
    ///
    /// This method returns `None` if either:
    ///
    /// - there's no defined position in the field definition (neither in the field path from
    /// `self.index` to the end, nor in the annotations, nor in the value)
    /// - this piece is a final value definition and there's no annotation nor value.
    pub(crate) fn span(&self) -> Option<RawSpan> {
        // Most of the children of a field def piece are ordered, but not all (the type annotation
        // in particular can appear anywhere in the metadata). Instead of trying to be smart to
        // save a few iterations, we gather all the defined positions and compute their covering
        // span.

        let all_spans = self.field_def.path[self.index..]
            .iter()
            .filter_map(|path_elem| path_elem.pos().into_opt())
            .chain(
                self.field_def
                    .value
                    .as_ref()
                    .and_then(|val| val.pos.into_opt()),
            )
            .chain(
                self.field_def
                    .metadata
                    .annotation
                    .iter()
                    .filter_map(|typ| typ.pos.into_opt()),
            );

        // unwrap(): we expect all the components of a field definition piece to lie in the same
        // file.
        all_spans.reduce(|span1, span2| RawSpan::fuse(span1, span2).unwrap())
    }

    /// Returns the path of this definition piece in the parent record, that is
    /// `self.field_def.path[..self.index]`. Returns `None` if one of the element is a dynamic
    /// field def.
    pub(crate) fn path_in_parent(&self) -> Option<Vec<Ident>> {
        self.field_def.path[..self.index]
            .iter()
            // If there is a dynamic field definition in the middle, we can't say
            // anything useful
            .map(|pe| Some(pe.try_as_ident()?.ident()))
            .collect()
    }
}

impl<'ast> From<&'ast FieldDef<'ast>> for FieldDefPiece<'ast> {
    fn from(field_def: &'ast FieldDef<'ast>) -> Self {
        FieldDefPiece {
            index: 0,
            field_def,
        }
    }
}

/// The definition site of an identifier.
#[derive(Clone, Debug, PartialEq)]
pub enum Def<'ast> {
    /// A definition site that's a let binding (possibly a pattern binding).
    Let {
        ident: LocIdent,
        /// Potential metadata annotation on the left hand side of the binding.
        metadata: &'ast LetMetadata<'ast>,
        /// The right hand side of the let binding. Note that in the case of a
        /// pattern binding, this may not be the value that's actually bound to
        /// `ident`. (See `path`.)
        value: &'ast Ast<'ast>,
        /// The path that `ident` refers to in `value`.
        path: Vec<Ident>,
    },
    /// A definition introduced by a match expression, for example `x` in `y |> match { {x, z} => x
    /// }`. In this case we can get information about this definition indirectly from `y`.
    MatchBinding {
        ident: LocIdent,
        metadata: &'ast FieldMetadata<'ast>,
        /// The matched value, if it could be deduced from the context (this will typically be
        /// `None` in a stand-alone match expression that isn't applied).
        value: Option<&'ast Ast<'ast>>,
        /// The path that `ident` refers to in `value`.
        path: Vec<Ident>,
    },
    /// An identifier bound as the argument to a function.
    ///
    /// Note that this can also be a pattern binding (and therefore come with
    /// an associated path) but because we don't track any bound value here, we
    /// don't need to track the path either.
    Fn { ident: LocIdent },
    /// An identifier bound as a record field.
    Field {
        /// The name of the field.
        ///
        /// Note that we have potentially several occurrence of the identifier here. We store the
        /// common underlying identifier in [Self::ident]. If you need an identifier with a
        /// position, use [Self::loc_ident], although the choice of the position is arbitrary.
        ident: Ident,
        /// The pieces that compose this definition.
        pieces: Vec<FieldDefPiece<'ast>>,
        /// The record where this field definition lives.
        record: &'ast Ast<'ast>,
        /// The path at which this definition lives in the parent record [Self::record]. For simple
        /// field definition (non-piecewise), this is empty, as in `{def = value}`. However, for a
        /// piecewise definition such as `subdef` in `{def.subdef.field1 = value, def.subdef.field2
        /// = value}`, `subdef` lives in its parent record at path `["def"]`.
        path_in_record: Vec<Ident>,
    },
}

// let x = { foo.bar = { x, y = bar}, foo.bar = y, baz = foo }
// { foo = {bar = x & y}

impl<'ast> Def<'ast> {
    /// Returns the identifier of this definition. For piecewise definition, we can't pinpoint one
    /// particular location for this identifier, hence the return type is
    /// [nickel_lang_core::identifier::Ident] here.
    ///
    /// If you need a located ident, use [Self::loc_ident].
    pub fn ident(&self) -> Ident {
        match self {
            Def::Let { ident, .. } | Def::MatchBinding { ident, .. } | Def::Fn { ident, .. } => {
                ident.ident
            }
            Def::Field { ident, .. } => *ident,
        }
    }

    /// Returns the located identifier of this definition.
    ///
    /// For a piecewise field definition, there may be several different locations.
    ///
    /// However, NLS sometimes need one single position (typically when listing symbols of the
    /// current document), so we pick one with the following simple heuristics:
    ///
    /// - find the first definition with a defined value, and use the location of its identifier if
    ///   it's defined.
    /// - otherwise, take the first identifier of the definition list with a defined position.
    /// - otherwise, use the unlocated identifier with [nickel_lang_core::position::TermPos::None]
    pub fn loc_ident(&self) -> LocIdent {
        match self {
            Def::Let { ident, .. } | Def::Fn { ident, .. } | Def::MatchBinding { ident, .. } => {
                *ident
            }
            Def::Field { ident, pieces, .. } => pieces
                .iter()
                .find_map(|p| p.value().and_then(|_| p.ident()))
                .or(pieces.iter().find_map(|p| p.ident()))
                .unwrap_or_else(|| LocIdent {
                    ident: *ident,
                    pos: Default::default(),
                }),
        }
    }

    pub fn parent_record(&self) -> Option<&'ast Ast<'ast>> {
        match self {
            Def::Field { record, .. } => Some(*record),
            _ => None,
        }
    }

    pub fn values(&self) -> Vec<&'ast Ast<'ast>> {
        match self {
            Def::Let { value, .. } => vec![value],
            Def::MatchBinding { value, .. } => (*value).into_iter().collect(),
            Def::Field { pieces, .. } => pieces
                .iter()
                .filter_map(|p| p.field_def.value.as_ref())
                .collect(),
            Def::Fn { .. } => vec![],
        }
    }

    pub fn metadata(&self) -> Vec<Cow<'ast, FieldMetadata<'ast>>> {
        match self {
            Def::Let { metadata, .. } => vec![Cow::Owned((*metadata).clone().into())],
            Def::MatchBinding { metadata, .. } => vec![Cow::Borrowed(*metadata)],
            Def::Field { pieces, .. } => pieces
                .iter()
                .filter_map(|p| p.metadata())
                .map(Cow::Borrowed)
                .collect(),
            Def::Fn { .. } => vec![],
        }
    }

    pub fn annots(&self) -> Vec<&'ast Annotation<'ast>> {
        match self {
            Def::Fn { .. } => vec![],
            Def::Let { metadata, .. } => vec![&metadata.annotation],
            Def::MatchBinding { metadata, .. } => vec![&metadata.annotation],
            Def::Field { pieces, .. } => pieces
                .iter()
                .map(|p| &p.field_def.metadata.annotation)
                .collect(),
        }
    }

    pub fn path(&self) -> &[Ident] {
        match self {
            Def::Let { path, .. } | Def::MatchBinding { path, .. } => path.as_slice(),
            _ => &[],
        }
    }

    pub fn completion_item(&self) -> CompletionItem {
        CompletionItem {
            label: ident_quoted(&self.ident().into()),
            metadata: self.metadata(),
            ..Default::default()
        }
    }
}

fn filter_records(containers: Vec<Container>) -> Vec<Record> {
    containers
        .into_iter()
        .filter_map(|c| c.try_into().ok())
        .collect()
}

/// Contains the context needed to resolve records and fields.
///
/// "Resolution" here is a kind of baby evaluation, where we can resolve
/// - variables to their definitions,
/// - static accesses to the fields they refer to,
/// - imports to the imported term,
/// - ...and a few other things.
///
/// Because this resolution takes merges into account, a single term can resolve
/// to multiple results. For example, resolving the path `foo` in the term
/// `({foo = {...}} & {foo = {...}})` will return both values of `foo`.
#[derive(Clone)]
pub struct FieldResolver<'ast> {
    world: &'ast World,

    // Most of our analysis moves "down" the AST and so can't get stuck in a loop.
    // Variable resolution is an exception, however, and so we protect against
    // loops by recording the ids that we are currently resolving and refusing to
    // resolve them again.
    blackholed_ids: RefCell<HashSet<LocIdent>>,
}

impl<'ast> FieldResolver<'ast> {
    pub fn new(world: &'ast World) -> Self {
        Self {
            world,
            blackholed_ids: Default::default(),
        }
    }

    /// Finds all the records that are descended from `ast` at the given path.
    ///
    /// For example, if `ast` is { foo.bar = { ...1 } } & { foo.bar = { ...2 } }`
    /// and `path` is ['foo', 'bar'] then this will return `{ ...1 }` and `{ ...2 }`.
    pub fn resolve_path(
        &self,
        ast: &'ast Ast<'ast>,
        path: impl Iterator<Item = Ident>,
    ) -> Vec<Record<'ast>> {
        filter_records(self.containers_at_path(ast, path))
    }

    /// If this term resolves to one or more records, return them all.
    pub fn resolve_record(&self, ast: &'ast Ast<'ast>) -> Vec<Record<'ast>> {
        filter_records(self.resolve_container(ast))
    }

    /// Finds all the containers that are descended from `ast` at the given path.
    ///
    /// The path can mix field access and array "accesses". The array accesses are only used
    /// in array types -- we never actually index an array value -- but they can be used,
    /// for example, to see that `{ foo | Array { bar | { baz | Number } } }` evaluated
    /// at the path `["foo", EltId::ArrayElt, "bar"]` is the record `{ baz | Number }`.
    fn containers_at_path(
        &self,
        ast: &'ast Ast<'ast>,
        path: impl Iterator<Item = impl Into<EltId>>,
    ) -> Vec<Container<'ast>> {
        let fields = self.resolve_container(ast);
        self.resolve_containers_at_path(fields, path)
    }

    /// Finds all the containers that are descended from any of the provided containers at the given path.
    ///
    /// The path can mix field access and array "accesses". The array accesses are only used
    /// in array types -- we never actually index an array value -- but they can be used,
    /// for example, to see that `{ foo | Array { bar | { baz | Number } } }` evaluated
    /// at the path `["foo", EltId::ArrayElt, "bar"]` is the record `{ baz | Number }`.
    pub fn resolve_containers_at_path(
        &self,
        containers: impl IntoIterator<Item = impl Into<Container<'ast>>>,
        path: impl IntoIterator<Item = impl Into<EltId>>,
    ) -> Vec<Container<'ast>> {
        let containers = containers.into_iter();
        let path = path.into_iter();

        let mut containers: Vec<_> = containers.map(|c| c.into()).collect();

        for id in path.map(Into::into) {
            let def_pieces = containers
                .iter()
                .flat_map(|container| container.get(id))
                .collect::<Vec<_>>();

            containers.clear();

            for def_piece in def_pieces {
                match def_piece {
                    FieldContent::FieldDefPiece(piece) => {
                        let field_def = piece.field_def;
                        let as_container: Result<Container<'_>, _> = piece.try_into();
                        // If we're still in the middle of the path a field definition, we just add
                        // the advanced definition piece.
                        if let Ok(c) = as_container {
                            containers.push(c);
                        }
                        // Otherwise, we've reached the value, which might be a container itself.
                        else {
                            containers.extend(self.resolve_annot(&field_def.metadata.annotation));

                            if let Some(value) = &field_def.value {
                                containers.extend(self.resolve_container(value));
                            }
                        }
                    }
                    FieldContent::Type(ty) => {
                        containers.extend_from_slice(&self.resolve_type(ty));
                    }
                }
            }
        }

        containers
    }

    /// Find the "cousins" of this definition.
    ///
    /// When resolving references, we want to take merged records into account. For example, when
    /// looking at the first definition of `foo` in
    ///
    /// ```nickel
    /// { foo = 1 } | { foo | Number | doc "blah blah" }
    /// ```
    ///
    /// We often need to access the information in the second "foo". We call these two `foo`s
    /// "cousin" definitions because they look like cousins in the AST. Note that these can also
    /// happen at arbitrary depths, like the two `foo`s in
    ///
    /// ```nickel
    /// { bar = { foo = 1 } } | { bar | { foo | Number | doc "blah blah" } }
    /// ```
    pub fn cousin_defs(&self, def: &Def<'ast>) -> Vec<FieldDefPiece<'ast>> {
        if let Some(parent) = def.parent_record() {
            log::debug!("** Found parent {parent}");
            self.cousin_defs_at(parent, def.ident())
        } else {
            Vec::new()
        }
    }

    /// Variant of [Self::cousin_defs] that takes an AST node and an explicit ident instead of a
    /// definition.
    pub fn cousin_defs_at(
        &self,
        record: &'ast Ast<'ast>,
        ident: Ident,
    ) -> Vec<FieldDefPiece<'ast>> {
        log::debug!("cousin_defs_at({record}, {ident})");

        let uncles = self.cousin_containers(record);

        log::debug!("** Found {} uncles", uncles.len());

        uncles
            .iter()
            .flat_map(|uncle| uncle.get_field_def_pieces(ident))
            .collect()
    }

    /// Variant of [Self::cousin_defs_at] that takes an additional path to resolve first before
    /// looking for the idnet of interest. Typically used to find the cousin of e.g. `field` in a
    /// piecewise field definition `first.second.field.last = value`, where in this example `path`
    /// would be `first.second`.
    pub fn cousin_defs_at_path(
        &self,
        record: &'ast Ast<'ast>,
        path: impl IntoIterator<Item = impl Into<EltId>>,
        ident: Ident,
    ) -> Vec<FieldDefPiece<'ast>> {
        log::debug!("cousin_defs_at_path({record})",);

        let uncles = self.cousin_containers(record);

        log::debug!("** Found {} uncles", uncles.len());

        // Albeit close, note that the following isn't equivalent to first resolving at `path +
        // [ident]` and then filtering out field definition pieces, because `container_at_path`
        // converts definition pieces that are final values to containers, while we want to keep the
        // field definition piece here instead.
        self.resolve_containers_at_path(uncles, path)
            .into_iter()
            .flat_map(|uncle| uncle.get_field_def_pieces(ident))
            .collect()
    }

    /// Find all records that are "cousins" of this term.
    ///
    /// See [`FieldResolver::cousin_defs`] for more detail.
    pub fn cousin_records(&self, ast: &'ast Ast<'ast>) -> Vec<Record<'ast>> {
        filter_records(self.cousin_containers(ast))
    }

    fn cousin_containers(&self, ast: &'ast Ast<'ast>) -> Vec<Container<'ast>> {
        let mut ret = Vec::new();
        if let Some(mut ancestors) = self.world.analysis_reg.get_parent_chain(ast) {
            while let Some(ancestor) = ancestors.next_merge() {
                let path = ancestors.rev_path().unwrap_or_default();
                ret.extend(self.containers_at_path(ancestor, path.iter().rev().copied()));
            }
        }
        ret
    }

    fn resolve_def_with_path(&self, def: &Def<'ast>) -> Vec<Container<'ast>> {
        let mut fields = Vec::new();

        fields.extend(def.values().into_iter().flat_map(|val| {
            self.containers_at_path(val, def.path().iter().copied().map(EltId::Ident))
        }));

        fields.extend(
            def.annots()
                .into_iter()
                .flat_map(|annot| self.resolve_annot(annot)),
        );

        for def_piece in self.cousin_defs(def) {
            if let Some(meta) = def_piece.metadata() {
                fields.extend(self.resolve_annot(&meta.annotation));
            }
            if let Some(val) = def_piece.field_def.value.as_ref() {
                fields.extend(self.resolve_container(val));
            }
        }

        fields
    }

    fn resolve_annot<'a>(
        &'a self,
        annot: &'ast Annotation<'ast>,
    ) -> impl Iterator<Item = Container<'ast>> + 'a {
        annot
            .iter()
            .flat_map(|lty| self.resolve_type(lty).into_iter())
    }

    /// Find all the containers that a term resolves to.
    fn resolve_container(&self, ast: &'ast Ast<'ast>) -> Vec<Container<'ast>> {
        let term_fields = match &ast.node {
            Node::Record(data) => vec![Container::RecordTerm(data)],
            Node::Var(id) => {
                let id = LocIdent::from(*id);
                if self.blackholed_ids.borrow_mut().insert(id) {
                    let ret = self
                        .world
                        .analysis_reg
                        .get_def(&id)
                        .map(|def| {
                            log::info!("found def for {}", id.ident);
                            self.resolve_def_with_path(def)
                        })
                        .unwrap_or_else(|| {
                            log::info!("no def for {id:?}");
                            Default::default()
                        });
                    self.blackholed_ids.borrow_mut().remove(&id);
                    ret
                } else {
                    log::warn!("detected recursion when resolving {id:?}");
                    Vec::new()
                }
            }
            Node::Import(_) => {
                let target_id = self.world.get_import_target(ast.pos);
                target_id
                    .and_then(|id| self.world.analysis_reg.get(id))
                    .map(|anl| self.resolve_container(anl.ast()))
                    .unwrap_or_default()
            }
            Node::PrimOpApp {
                op: PrimOp::Merge(_),
                args,
            } => args
                .iter()
                .flat_map(|t| self.resolve_container(t))
                .collect(),
            Node::Let { body, .. } => self.resolve_container(body),
            Node::PrimOpApp {
                op: PrimOp::RecordStatAccess(id),
                args: [arg],
            } => self.containers_at_path(arg, std::iter::once(id.ident())),
            Node::Annotated { annot, inner } => {
                let defs = self.resolve_annot(annot);
                defs.chain(self.resolve_container(inner)).collect()
            }
            Node::Type(typ) => self.resolve_type(typ),
            _ => Default::default(),
        };

        let typ_fields = if let Some(typ) = self.world.analysis_reg.get_type(ast) {
            log::info!("got inferred type {typ}");
            self.resolve_type(typ)
        } else {
            Vec::new()
        };

        combine(term_fields, typ_fields)
    }

    fn resolve_type(&self, typ: &'ast Type<'ast>) -> Vec<Container<'ast>> {
        match &typ.typ {
            TypeF::Record(rows) => vec![Container::RecordType(rows)],
            TypeF::Dict { type_fields, .. } => vec![Container::Dict(type_fields)],
            TypeF::Array(elt_ty) => vec![Container::Array(elt_ty)],
            TypeF::Contract(rt) => self.resolve_container(rt),
            _ => Default::default(),
        }
    }
}

fn combine<T>(mut left: Vec<T>, mut right: Vec<T>) -> Vec<T> {
    left.append(&mut right);
    left
}

/// Wraper around [nickel_lang_core::bytecode::ast::record::Record::defs_of] that returns field
/// definition pieces with the index properly set to `1` (since we accessed the field `id`).
fn piece_defs_of<'ast>(record: &RecordData<'ast>, id: Ident) -> Vec<FieldDefPiece<'ast>> {
    record
        .defs_of(id)
        .map(|field_def| FieldDefPiece {
            index: 1,
            field_def,
        })
        .collect()
}
