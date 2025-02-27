//! Everything related to memory allocation of AST components.

use std::{ffi::OsString, iter};

use bumpalo::Bump;

use super::*;

/// Marker trait for AST nodes that don't need to be dropped (in practice, it's often equivalent to
/// not owning any heap allocated data) and can be used with [allocator][AstAlloc::alloc]. The
/// current exceptions are [Number] and [crate::error::ParseError], which must be allocated through
/// specialized method in [AstAlloc].
pub trait Allocable {}

impl Allocable for Ast<'_> {}
impl Allocable for Node<'_> {}
impl<T: Allocable> Allocable for StringChunk<T> {}
impl Allocable for LetBinding<'_> {}
impl Allocable for LetMetadata<'_> {}
impl Allocable for PrimOp {}
impl Allocable for Annotation<'_> {}
impl Allocable for MatchBranch<'_> {}

impl Allocable for Record<'_> {}
impl Allocable for record::FieldPathElem<'_> {}
impl Allocable for record::FieldDef<'_> {}
impl Allocable for record::FieldMetadata<'_> {}

impl Allocable for Pattern<'_> {}
impl Allocable for EnumPattern<'_> {}
impl Allocable for FieldPattern<'_> {}
impl Allocable for RecordPattern<'_> {}
impl Allocable for ArrayPattern<'_> {}
impl Allocable for OrPattern<'_> {}
impl Allocable for ConstantPattern<'_> {}
impl Allocable for ConstantPatternData<'_> {}

impl Allocable for Type<'_> {}
impl Allocable for typ::RecordRows<'_> {}
impl Allocable for typ::EnumRows<'_> {}
impl Allocable for typ::EnumRow<'_> {}
impl Allocable for typ::RecordRow<'_> {}

/// Owns the arenas required to allocate new AST nodes and provide builder methods to create them.
///
/// # Drop and arena allocation
///
/// The most popular choice for arena is the `bumpalo` crate, which is a fast bump allocator that
/// can handle heterogeneous data. However, it doesn't support destructors, which is a problem
/// because some of the nodes in the AST owns heap allocated data and needs to be de-allocated
/// (numbers and parse errors currently).
///
/// Another choice is `typed-arena` and derivatives, which do run destructors, but can only store
/// one type of values. As the number of types that need to be dropped is relatively small, we use
/// a general `bumpalo` arena by default, and specialized typed arenas for stuff that need to be
/// dropped.
///
/// # Guarantees
///
/// [AstAlloc] guarantees that the memory that has been allocated won't be moved until [Self] is
/// deallocated.
pub struct AstAlloc {
    generic_arena: Bump,
    number_arena: typed_arena::Arena<Number>,
    error_arena: typed_arena::Arena<ParseError>,
}

impl AstAlloc {
    /// Creates a new ast allocator.
    pub fn new() -> Self {
        Self {
            generic_arena: Bump::new(),
            number_arena: typed_arena::Arena::new(),
            error_arena: typed_arena::Arena::new(),
        }
    }

    /// Return the current number of allocated bytes.
    pub fn allocated_bytes(&self) -> usize {
        self.generic_arena.allocated_bytes() + self.number_arena.len() + self.error_arena.len()
    }

    /// Allocates an AST component in the arena.
    ///
    /// [Self] never guarantees that all destructors are going to be run when using such a generic
    /// allocation function. We don't want to allocate values that need to be dropped through this
    /// method, typically because they own heap-allocated data, such as numbers or parse errors.
    /// That's why we use a marker trait to specify which types can be allocated freely. Types that
    /// need to be dropped don't implement [Allocable] and have a dedicated method for allocation.
    pub fn alloc<T: Allocable>(&self, value: T) -> &T {
        self.generic_arena.alloc(value)
    }

    /// Allocates a sequence of AST components in the arena.
    ///
    /// See [Self::alloc].
    pub fn alloc_many<T: Allocable, I>(&self, iter: I) -> &[T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        self.generic_arena.alloc_slice_fill_iter(iter)
    }

    /// Allocates an array with exactly one element in the arena.
    pub fn alloc_singleton<T: Allocable>(&self, value: T) -> &[T] {
        self.generic_arena.alloc_slice_fill_iter(iter::once(value))
    }

    /// Allocates a string in the arena.
    pub fn alloc_str<'ast>(&'ast self, s: &str) -> &'ast str {
        self.generic_arena.alloc_str(s)
    }

    /// Deep clone an already allocated AST component from another arena to the current one.
    pub fn clone_from<'from, 'to, T: CloneTo>(&'to self, data: T::Data<'from>) -> T::Data<'to> {
        T::clone_to(data, self)
    }

    /// Same as [Self::clone_in] but take an arena-allocated reference instead.
    pub fn clone_ref_from<'from, 'to, T: CloneTo>(
        &'to self,
        data: &'from T::Data<'from>,
    ) -> &'to T::Data<'to>
    where
        T: for<'a> CloneTo<Data<'a>: Clone + Allocable>,
    {
        self.alloc(T::clone_to(data.clone(), self))
    }

    pub fn number(&self, number: Number) -> Node<'_> {
        Node::Number(self.number_arena.alloc(number))
    }

    pub fn alloc_number(&self, number: Number) -> &'_ Number {
        self.number_arena.alloc(number)
    }

    pub fn string<'ast>(&'ast self, s: &str) -> Node<'ast> {
        Node::String(self.generic_arena.alloc_str(s))
    }

    pub fn string_chunks<'ast, I>(&'ast self, chunks: I) -> Node<'ast>
    where
        I: IntoIterator<Item = StringChunk<Ast<'ast>>>,
        I::IntoIter: ExactSizeIterator,
    {
        Node::StringChunks(self.generic_arena.alloc_slice_fill_iter(chunks))
    }

    pub fn fun<'ast, I>(&'ast self, args: I, body: Ast<'ast>) -> Node<'ast>
    where
        I: IntoIterator<Item = Pattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        Node::Fun {
            args: self.generic_arena.alloc_slice_fill_iter(args),
            body: self.generic_arena.alloc(body),
        }
    }

    pub fn unary_fun<'ast>(&'ast self, arg: Pattern<'ast>, body: Ast<'ast>) -> Node<'ast> {
        Node::Fun {
            args: self.alloc_singleton(arg),
            body: self.generic_arena.alloc(body),
        }
    }

    pub fn let_block<'ast, I>(&'ast self, bindings: I, body: Ast<'ast>, rec: bool) -> Node<'ast>
    where
        I: IntoIterator<Item = LetBinding<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let bindings = self.generic_arena.alloc_slice_fill_iter(bindings);
        let body = self.generic_arena.alloc(body);

        Node::Let {
            bindings,
            body,
            rec,
        }
    }

    pub fn app<'ast, I>(&'ast self, head: Ast<'ast>, args: I) -> Node<'ast>
    where
        I: IntoIterator<Item = Ast<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        Node::App {
            head: self.generic_arena.alloc(head),
            args: self.generic_arena.alloc_slice_fill_iter(args),
        }
    }

    pub fn enum_variant<'ast>(&'ast self, tag: LocIdent, arg: Option<Ast<'ast>>) -> Node<'ast> {
        Node::EnumVariant {
            tag,
            arg: arg.map(|arg| &*self.generic_arena.alloc(arg)),
        }
    }

    pub fn record<'ast>(&'ast self, record: Record<'ast>) -> Node<'ast> {
        let record = self.generic_arena.alloc(record);
        Node::Record(record)
    }

    pub fn record_data<'ast, Ss, Ds>(&'ast self, field_defs: Ds, open: bool) -> &'ast Record<'ast>
    where
        Ds: IntoIterator<Item = FieldDef<'ast>>,
        Ds::IntoIter: ExactSizeIterator,
    {
        self.generic_arena.alloc(Record {
            field_defs: self.generic_arena.alloc_slice_fill_iter(field_defs),
            open,
        })
    }

    pub fn if_then_else<'ast>(
        &'ast self,
        cond: Ast<'ast>,
        then_branch: Ast<'ast>,
        else_branch: Ast<'ast>,
    ) -> Node<'ast> {
        Node::IfThenElse {
            cond: self.generic_arena.alloc(cond),
            then_branch: self.generic_arena.alloc(then_branch),
            else_branch: self.generic_arena.alloc(else_branch),
        }
    }

    pub fn match_expr<'ast, I>(&'ast self, branches: I) -> Node<'ast>
    where
        I: IntoIterator<Item = MatchBranch<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        Node::Match(Match {
            branches: self.generic_arena.alloc_slice_fill_iter(branches),
        })
    }

    pub fn array<'ast, I>(&'ast self, elts: I) -> Node<'ast>
    where
        I: IntoIterator<Item = Ast<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        Node::Array(self.generic_arena.alloc_slice_fill_iter(elts))
    }

    pub fn prim_op<'ast, I>(&'ast self, op: PrimOp, args: I) -> Node<'ast>
    where
        I: IntoIterator<Item = Ast<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let op = self.generic_arena.alloc(op);
        let args = self.generic_arena.alloc_slice_fill_iter(args);
        Node::PrimOpApp { op, args }
    }

    pub fn annotated<'ast>(&'ast self, annot: Annotation<'ast>, inner: Ast<'ast>) -> Node<'ast> {
        Node::Annotated {
            annot: self.generic_arena.alloc(annot),
            inner: self.generic_arena.alloc(inner),
        }
    }

    pub fn annotation<'ast, I>(
        &'ast self,
        typ: Option<Type<'ast>>,
        contracts: I,
    ) -> Annotation<'ast>
    where
        I: IntoIterator<Item = Type<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        Annotation {
            typ,
            contracts: self.generic_arena.alloc_slice_fill_iter(contracts),
        }
    }

    pub fn import_path(&self, path: OsString, format: InputFormat) -> Node<'_> {
        Node::Import(Import::Path {
            path: self.generic_arena.alloc(path),
            format,
        })
    }

    pub fn import_package(&self, id: Ident) -> Node<'_> {
        Node::Import(Import::Package { id })
    }

    pub fn typ<'ast>(&'ast self, typ: Type<'ast>) -> Node<'ast> {
        Node::Type(self.generic_arena.alloc(typ))
    }

    pub fn type_data<'ast>(&'ast self, typ: TypeUnr<'ast>, pos: TermPos) -> &'ast Type<'ast> {
        self.generic_arena.alloc(Type { typ, pos })
    }

    pub fn enum_rows<'ast>(&'ast self, erows: EnumRowsUnr<'ast>) -> &'ast EnumRows<'ast> {
        self.generic_arena.alloc(EnumRows(erows))
    }

    pub fn record_rows<'ast>(&'ast self, rrows: RecordRowsUnr<'ast>) -> &'ast RecordRows<'ast> {
        self.generic_arena.alloc(RecordRows(rrows))
    }

    pub fn parse_error(&self, error: ParseError) -> Node<'_> {
        Node::ParseError(self.error_arena.alloc(error))
    }

    pub fn record_pattern<'ast, I>(
        &'ast self,
        patterns: I,
        tail: TailPattern,
        pos: TermPos,
    ) -> &'ast RecordPattern<'ast>
    where
        I: IntoIterator<Item = FieldPattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        self.generic_arena.alloc(RecordPattern {
            patterns: self.generic_arena.alloc_slice_fill_iter(patterns),
            tail,
            pos,
        })
    }

    pub fn array_pattern<'ast, I>(
        &'ast self,
        patterns: I,
        tail: TailPattern,
        pos: TermPos,
    ) -> &'ast ArrayPattern<'ast>
    where
        I: IntoIterator<Item = Pattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        self.generic_arena.alloc(ArrayPattern {
            patterns: self.generic_arena.alloc_slice_fill_iter(patterns),
            tail,
            pos,
        })
    }

    pub fn or_pattern<'ast, I>(&'ast self, patterns: I, pos: TermPos) -> &'ast OrPattern<'ast>
    where
        I: IntoIterator<Item = Pattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        self.generic_arena.alloc(OrPattern {
            patterns: self.generic_arena.alloc_slice_fill_iter(patterns),
            pos,
        })
    }
}

// Phony implementation of `Debug` so that we can still derive the trait for structure that holds
// onto an allocator.
impl fmt::Debug for AstAlloc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "AstAlloc")
    }
}

/// Deeply clones an AST component from one allocator to the other.
pub trait CloneTo {
    /// This is always `Self`, be we need associated types to make Rust understand that `Self` is
    /// always parametric over the `'ast` lifetime. We're using GATs to emulate higher-kinded
    /// types.
    type Data<'a>;

    /// Clones owned data from the current allocator to `dest`.
    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to>;
}

impl CloneTo for Ast<'_> {
    type Data<'a> = Ast<'a>;

    fn clone_to<'from, 'to>(data: Ast<'from>, dest: &'to AstAlloc) -> Ast<'to> {
        Ast {
            node: Node::clone_to(data.node.clone(), dest),
            pos: data.pos,
        }
    }
}

impl CloneTo for Node<'_> {
    type Data<'a> = Node<'a>;

    fn clone_to<'from, 'to>(data: Node<'from>, dest: &'to AstAlloc) -> Node<'to> {
        match data {
            Node::Null => Node::Null,
            Node::Bool(b) => Node::Bool(b),
            Node::Number(rational) => dest.number(rational.clone()),
            Node::String(s) => Node::String(dest.alloc_str(s)),
            Node::StringChunks(str_chunks) => Node::StringChunks(
                dest.alloc_many(
                    str_chunks
                        .iter()
                        .map(|chunk| StringChunk::clone_to(chunk.clone(), dest)),
                ),
            ),
            Node::Fun { args, body } => Node::Fun {
                args: dest.alloc_many(args.iter().map(|arg| Pattern::clone_to(arg.clone(), dest))),
                body: dest.clone_ref_from::<Ast>(body),
            },
            Node::Let {
                bindings,
                body,
                rec,
            } => Node::Let {
                bindings: dest.alloc_many(
                    bindings
                        .iter()
                        .map(|binding| LetBinding::clone_to(binding.clone(), dest)),
                ),
                body: dest.clone_ref_from::<Ast>(body),
                rec,
            },
            Node::App { head, args } => Node::App {
                head: dest.clone_ref_from::<Ast>(head),
                args: dest.alloc_many(args.iter().map(|arg| Ast::clone_to(arg.clone(), dest))),
            },
            Node::Var(loc_ident) => Node::Var(loc_ident),
            Node::EnumVariant { tag, arg } => Node::EnumVariant {
                tag,
                arg: arg.map(|arg| dest.clone_ref_from::<Ast>(arg)),
            },
            Node::Record(record) => Node::Record(dest.clone_ref_from::<Record>(record)),
            Node::IfThenElse {
                cond,
                then_branch,
                else_branch,
            } => Node::IfThenElse {
                cond: dest.clone_ref_from::<Ast>(cond),
                then_branch: dest.clone_ref_from::<Ast>(then_branch),
                else_branch: dest.clone_ref_from::<Ast>(else_branch),
            },
            Node::Match(data) => Node::Match(Match {
                branches: dest.alloc_many(
                    data.branches
                        .iter()
                        .map(|branch| MatchBranch::clone_to(branch.clone(), dest)),
                ),
            }),
            Node::Array(asts) => Node::Array(
                dest.alloc_many(asts.iter().map(|ast| Ast::clone_to(ast.clone(), dest))),
            ),
            Node::PrimOpApp { op, args } => Node::PrimOpApp {
                op: dest.alloc(op.clone()),
                args: dest.alloc_many(args.iter().map(|arg| Ast::clone_to(arg.clone(), dest))),
            },
            Node::Annotated { annot, inner } => Node::Annotated {
                annot: dest.clone_ref_from::<Annotation>(annot),
                inner: dest.clone_ref_from::<Ast>(inner),
            },
            Node::Import(import) => match import {
                Import::Path { path, format } => dest.import_path(path.to_owned(), format),
                Import::Package { id } => Node::Import(Import::Package { id }),
            },
            Node::Type(ty) => Node::Type(dest.clone_ref_from::<Type>(ty)),
            Node::ParseError(parse_error) => dest.parse_error(parse_error.clone()),
        }
    }
}

impl CloneTo for LetBinding<'_> {
    type Data<'ast> = LetBinding<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        LetBinding {
            pattern: Pattern::clone_to(data.pattern, dest),
            metadata: LetMetadata::clone_to(data.metadata, dest),
            value: Ast::clone_to(data.value, dest),
        }
    }
}

impl CloneTo for LetMetadata<'_> {
    type Data<'ast> = LetMetadata<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        LetMetadata {
            doc: data.doc.map(|s| dest.alloc_str(s)),
            annotation: Annotation::clone_to(data.annotation, dest),
        }
    }
}

impl CloneTo for Record<'_> {
    type Data<'ast> = Record<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        Record {
            field_defs: dest.alloc_many(
                data.field_defs
                    .iter()
                    .map(|field_def| FieldDef::clone_to(field_def.clone(), dest)),
            ),
            open: data.open,
        }
    }
}

impl CloneTo for FieldDef<'_> {
    type Data<'ast> = FieldDef<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        FieldDef {
            path: dest.alloc_many(
                data.path
                    .iter()
                    .map(|elem| record::FieldPathElem::clone_to(elem.clone(), dest)),
            ),
            metadata: record::FieldMetadata::clone_to(data.metadata, dest),
            value: data.value.map(|v| Ast::clone_to(v, dest)),
            pos: data.pos,
        }
    }
}

impl CloneTo for record::FieldPathElem<'_> {
    type Data<'ast> = record::FieldPathElem<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        match data {
            record::FieldPathElem::Ident(loc_ident) => record::FieldPathElem::Ident(loc_ident),
            record::FieldPathElem::Expr(ast) => {
                record::FieldPathElem::Expr(Ast::clone_to(ast, dest))
            }
        }
    }
}

impl CloneTo for record::FieldMetadata<'_> {
    type Data<'ast> = record::FieldMetadata<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        record::FieldMetadata {
            doc: data.doc.map(|doc| dest.alloc_str(doc)),
            annotation: Annotation::clone_to(data.annotation, dest),
            ..data
        }
    }
}

impl CloneTo for MatchBranch<'_> {
    type Data<'ast> = MatchBranch<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        MatchBranch {
            pattern: Pattern::clone_to(data.pattern, dest),
            guard: data.guard.map(|ast| Ast::clone_to(ast, dest)),
            body: Ast::clone_to(data.body, dest),
        }
    }
}

impl CloneTo for Annotation<'_> {
    type Data<'ast> = Annotation<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        Annotation {
            typ: data.typ.map(|typ| Type::clone_to(typ, dest)),
            contracts: dest.alloc_many(
                data.contracts
                    .iter()
                    .map(|typ| Type::clone_to(typ.clone(), dest)),
            ),
        }
    }
}

impl CloneTo for Type<'_> {
    type Data<'ast> = Type<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        let typ = match data.typ {
            TypeF::Dyn => TypeF::Dyn,
            TypeF::Number => TypeF::Number,
            TypeF::Bool => TypeF::Bool,
            TypeF::String => TypeF::String,
            TypeF::Symbol => TypeF::Symbol,
            TypeF::ForeignId => TypeF::ForeignId,
            TypeF::Contract(ast) => TypeF::Contract(dest.clone_ref_from::<Ast>(ast)),
            TypeF::Arrow(src, tgt) => TypeF::Arrow(
                dest.clone_ref_from::<Type>(src),
                dest.clone_ref_from::<Type>(tgt),
            ),
            TypeF::Var(id) => TypeF::Var(id),
            TypeF::Forall {
                var,
                var_kind,
                body,
            } => TypeF::Forall {
                var,
                var_kind,
                body: dest.clone_ref_from::<Type>(body),
            },
            TypeF::Enum(erows) => TypeF::Enum(typ::EnumRows::clone_to(erows, dest)),
            TypeF::Record(rrows) => TypeF::Record(typ::RecordRows::clone_to(rrows, dest)),
            TypeF::Dict {
                type_fields,
                flavour,
            } => TypeF::Dict {
                type_fields: dest.clone_ref_from::<Type>(type_fields),
                flavour,
            },
            TypeF::Array(ty) => TypeF::Array(dest.clone_ref_from::<Type>(ty)),
            TypeF::Wildcard(wildcard_id) => TypeF::Wildcard(wildcard_id),
        };

        Type { typ, pos: data.pos }
    }
}

impl CloneTo for typ::EnumRows<'_> {
    type Data<'ast> = typ::EnumRows<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        use typ::*;

        let inner = match data.0 {
            EnumRowsF::Empty => EnumRowsF::Empty,
            EnumRowsF::Extend { row, tail } => EnumRowsF::Extend {
                row: EnumRow::clone_to(row, dest),
                tail: dest.clone_ref_from::<EnumRows>(tail),
            },
            EnumRowsF::TailVar(loc_ident) => EnumRowsF::TailVar(loc_ident),
        };

        EnumRows(inner)
    }
}

impl CloneTo for typ::EnumRow<'_> {
    type Data<'ast> = typ::EnumRow<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        typ::EnumRow {
            id: data.id,
            typ: data.typ.map(|ty| dest.clone_ref_from::<Type>(ty)),
        }
    }
}

impl CloneTo for typ::RecordRows<'_> {
    type Data<'ast> = typ::RecordRows<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        use typ::*;

        let inner = match data.0 {
            RecordRowsF::Empty => RecordRowsF::Empty,
            RecordRowsF::Extend { row, tail } => RecordRowsF::Extend {
                row: RecordRow::clone_to(row, dest),
                tail: dest.clone_ref_from::<RecordRows>(tail),
            },
            RecordRowsF::TailVar(loc_ident) => RecordRowsF::TailVar(loc_ident),
            RecordRowsF::TailDyn => RecordRowsF::TailDyn,
        };

        RecordRows(inner)
    }
}

impl CloneTo for typ::RecordRow<'_> {
    type Data<'ast> = typ::RecordRow<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        typ::RecordRow {
            id: data.id,
            typ: dest.clone_ref_from::<Type>(data.typ),
        }
    }
}

impl CloneTo for StringChunk<Ast<'_>> {
    type Data<'ast> = StringChunk<Ast<'ast>>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        match data {
            StringChunk::Literal(s) => StringChunk::Literal(s),
            StringChunk::Expr(ast, indent) => StringChunk::Expr(Ast::clone_to(ast, dest), indent),
        }
    }
}

impl CloneTo for Pattern<'_> {
    type Data<'ast> = Pattern<'ast>;

    fn clone_to<'from, 'to>(pat: Pattern<'from>, dest: &'to AstAlloc) -> Pattern<'to> {
        let data = match pat.data {
            PatternData::Wildcard => PatternData::Wildcard,
            PatternData::Any(id) => PatternData::Any(id),
            PatternData::Record(record_pat) => {
                PatternData::Record(dest.clone_ref_from::<RecordPattern>(record_pat))
            }
            PatternData::Array(array_pat) => {
                PatternData::Array(dest.clone_ref_from::<ArrayPattern>(array_pat))
            }
            PatternData::Enum(enum_pat) => {
                PatternData::Enum(dest.clone_ref_from::<EnumPattern>(enum_pat))
            }
            PatternData::Constant(const_pat) => {
                PatternData::Constant(dest.clone_ref_from::<ConstantPattern>(const_pat))
            }
            PatternData::Or(or_pat) => PatternData::Or(dest.clone_ref_from::<OrPattern>(or_pat)),
        };

        Pattern { data, ..pat }
    }
}

impl CloneTo for EnumPattern<'_> {
    type Data<'ast> = EnumPattern<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        EnumPattern {
            pattern: data.pattern.map(|pat| Pattern::clone_to(pat, dest)),
            ..data
        }
    }
}

impl CloneTo for RecordPattern<'_> {
    type Data<'ast> = RecordPattern<'ast>;

    fn clone_to<'from, 'to>(pat: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        RecordPattern {
            patterns: dest.alloc_many(
                pat.patterns
                    .iter()
                    .map(|field_pat| FieldPattern::clone_to(field_pat.clone(), dest)),
            ),
            ..pat
        }
    }
}

impl CloneTo for FieldPattern<'_> {
    type Data<'ast> = FieldPattern<'ast>;

    fn clone_to<'from, 'to>(pat: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        FieldPattern {
            annotation: Annotation::clone_to(pat.annotation, dest),
            default: pat.default.map(|ast| Ast::clone_to(ast, dest)),
            pattern: Pattern::clone_to(pat.pattern, dest),
            ..pat
        }
    }
}

impl CloneTo for ArrayPattern<'_> {
    type Data<'ast> = ArrayPattern<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        ArrayPattern {
            patterns: dest.alloc_many(
                data.patterns
                    .iter()
                    .map(|pat| Pattern::clone_to(pat.clone(), dest)),
            ),
            ..data
        }
    }
}

impl CloneTo for ConstantPattern<'_> {
    type Data<'ast> = ConstantPattern<'ast>;

    fn clone_to<'from, 'to>(pat: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        ConstantPattern {
            data: ConstantPatternData::clone_to(pat.data, dest),
            ..pat
        }
    }
}

impl CloneTo for ConstantPatternData<'_> {
    type Data<'ast> = ConstantPatternData<'ast>;

    fn clone_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        match data {
            ConstantPatternData::Bool(b) => ConstantPatternData::Bool(b),
            ConstantPatternData::Number(n) => {
                ConstantPatternData::Number(dest.alloc_number(n.clone()))
            }
            ConstantPatternData::String(s) => ConstantPatternData::String(dest.alloc_str(s)),
            ConstantPatternData::Null => ConstantPatternData::Null,
        }
    }
}

impl CloneTo for OrPattern<'_> {
    type Data<'ast> = OrPattern<'ast>;

    fn clone_to<'from, 'to>(pat: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        OrPattern {
            patterns: dest.alloc_many(
                pat.patterns
                    .iter()
                    .map(|pat| Pattern::clone_to(pat.clone(), dest)),
            ),
            ..pat
        }
    }
}
