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

    /// Copies an already allocated AST component from another arena to the current one.
    pub fn move_in<'from, 'to, T: MoveTo>(&'to self, data: T::Data<'from>) -> T::Data<'to> {
        T::move_to(data, self)
    }

    /// Same as [Self::move_in] but take an arena-allocated reference inside.
    pub fn move_ref<'from, 'to, T: MoveTo>(
        &'to self,
        data: &'from T::Data<'from>,
    ) -> &'to T::Data<'to>
    where
        T: for<'a> MoveTo<Data<'a>: Clone + Allocable>,
    {
        self.alloc(T::move_to(data.clone(), self))
    }

    /// Specialized version of [Self::move_ref] for `T = Ast`. Given the design of the [MoveTo]
    /// trait, [Self::move_in] always require an type extra annotation, as in
    /// `move_ref::<Annotation>`. This we use `move_ref::<Ast>` quite a lot, we provide a specialized
    /// method for it.
    pub fn move_ast_ref<'from, 'to>(&'to self, data: &'from Ast<'from>) -> &'to Ast<'to> {
        self.move_ref::<Ast>(data)
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
            args: self.generic_arena.alloc_slice_fill_iter(iter::once(arg)),
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

/// Copy an AST component from one allocator to the other.
pub trait MoveTo {
    /// This is always `Self`, be we need associated types to make Rust understand that `Self` is
    /// always parametric over the `'ast` lifetime. We're using GATs to emulate higher-kinded
    /// types.
    type Data<'a>;

    /// Copies owned data from the current allocator to `dest`.
    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to>;
}

impl MoveTo for Ast<'_> {
    type Data<'a> = Ast<'a>;

    fn move_to<'from, 'to>(data: Ast<'from>, dest: &'to AstAlloc) -> Ast<'to> {
        Ast {
            node: Node::move_to(data.node.clone(), dest),
            pos: data.pos,
        }
    }
}

impl MoveTo for Node<'_> {
    type Data<'a> = Node<'a>;

    fn move_to<'from, 'to>(data: Node<'from>, dest: &'to AstAlloc) -> Node<'to> {
        match data {
            Node::Null => Node::Null,
            Node::Bool(b) => Node::Bool(b),
            Node::Number(rational) => dest.number(rational.clone()),
            Node::String(s) => Node::String(dest.alloc_str(s)),
            Node::StringChunks(str_chunks) => Node::StringChunks(
                dest.alloc_many(
                    str_chunks
                        .iter()
                        .map(|chunk| StringChunk::move_to(chunk.clone(), dest)),
                ),
            ),
            Node::Fun { args, body } => Node::Fun {
                args: dest.alloc_many(args.iter().map(|arg| Pattern::move_to(arg.clone(), dest))),
                body: dest.move_ast_ref(body),
            },
            Node::Let {
                bindings,
                body,
                rec,
            } => Node::Let {
                bindings: dest.alloc_many(
                    bindings
                        .iter()
                        .map(|binding| LetBinding::move_to(binding.clone(), dest)),
                ),
                body: dest.move_ast_ref(body),
                rec,
            },
            Node::App { head, args } => Node::App {
                head: dest.move_ast_ref(head),
                args: dest.alloc_many(args.iter().map(|arg| Ast::move_to(arg.clone(), dest))),
            },
            Node::Var(loc_ident) => Node::Var(loc_ident),
            Node::EnumVariant { tag, arg } => Node::EnumVariant {
                tag,
                arg: arg.map(|arg| dest.move_ast_ref(arg)),
            },
            Node::Record(record) => Node::Record(dest.move_ref::<Record>(record)),
            Node::IfThenElse {
                cond,
                then_branch,
                else_branch,
            } => Node::IfThenElse {
                cond: dest.move_ast_ref(cond),
                then_branch: dest.move_ast_ref(then_branch),
                else_branch: dest.move_ast_ref(else_branch),
            },
            Node::Match(data) => Node::Match(Match {
                branches: dest.alloc_many(
                    data.branches
                        .iter()
                        .map(|branch| MatchBranch::move_to(branch.clone(), dest)),
                ),
            }),
            Node::Array(asts) => {
                Node::Array(dest.alloc_many(asts.iter().map(|ast| Ast::move_to(ast.clone(), dest))))
            }
            Node::PrimOpApp { op, args } => Node::PrimOpApp {
                op: dest.alloc(op.clone()),
                args: dest.alloc_many(args.iter().map(|arg| Ast::move_to(arg.clone(), dest))),
            },
            Node::Annotated { annot, inner } => Node::Annotated {
                annot: dest.move_ref::<Annotation>(annot),
                inner: dest.move_ast_ref(inner),
            },
            Node::Import(import) => match import {
                Import::Path { path, format } => dest.import_path(path.to_owned(), format),
                Import::Package { id } => Node::Import(Import::Package { id }),
            },
            Node::Type(ty) => Node::Type(dest.move_ref::<Type>(ty)),
            Node::ParseError(parse_error) => dest.parse_error(parse_error.clone()),
        }
    }
}

impl MoveTo for LetBinding<'_> {
    type Data<'ast> = LetBinding<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        LetBinding {
            pattern: Pattern::move_to(data.pattern, dest),
            metadata: LetMetadata::move_to(data.metadata, dest),
            value: Ast::move_to(data.value, dest),
        }
    }
}

impl MoveTo for LetMetadata<'_> {
    type Data<'ast> = LetMetadata<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        LetMetadata {
            doc: data.doc.map(|s| dest.alloc_str(s)),
            annotation: Annotation::move_to(data.annotation, dest),
        }
    }
}

impl MoveTo for Record<'_> {
    type Data<'ast> = Record<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        Record {
            field_defs: dest.alloc_many(
                data.field_defs
                    .iter()
                    .map(|field_def| FieldDef::move_to(field_def.clone(), dest)),
            ),
            open: data.open,
        }
    }
}

impl MoveTo for FieldDef<'_> {
    type Data<'ast> = FieldDef<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        FieldDef {
            path: dest.alloc_many(
                data.path
                    .iter()
                    .map(|elem| record::FieldPathElem::move_to(elem.clone(), dest)),
            ),
            metadata: record::FieldMetadata::move_to(data.metadata, dest),
            value: data.value.map(|v| Ast::move_to(v, dest)),
            pos: data.pos,
        }
    }
}

impl MoveTo for record::FieldPathElem<'_> {
    type Data<'ast> = record::FieldPathElem<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        match data {
            record::FieldPathElem::Ident(loc_ident) => record::FieldPathElem::Ident(loc_ident),
            record::FieldPathElem::Expr(ast) => {
                record::FieldPathElem::Expr(Ast::move_to(ast, dest))
            }
        }
    }
}

impl MoveTo for record::FieldMetadata<'_> {
    type Data<'ast> = record::FieldMetadata<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        record::FieldMetadata {
            doc: data.doc.map(|doc| dest.alloc_str(doc)),
            annotation: Annotation::move_to(data.annotation, dest),
            ..data
        }
    }
}

impl MoveTo for MatchBranch<'_> {
    type Data<'ast> = MatchBranch<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        MatchBranch {
            pattern: Pattern::move_to(data.pattern, dest),
            guard: data.guard.map(|ast| Ast::move_to(ast, dest)),
            body: Ast::move_to(data.body, dest),
        }
    }
}

impl MoveTo for Annotation<'_> {
    type Data<'ast> = Annotation<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        Annotation {
            typ: data.typ.map(|typ| Type::move_to(typ, dest)),
            contracts: dest.alloc_many(
                data.contracts
                    .iter()
                    .map(|typ| Type::move_to(typ.clone(), dest)),
            ),
        }
    }
}

impl MoveTo for Type<'_> {
    type Data<'ast> = Type<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        let typ = match data.typ {
            TypeF::Dyn => TypeF::Dyn,
            TypeF::Number => TypeF::Number,
            TypeF::Bool => TypeF::Bool,
            TypeF::String => TypeF::String,
            TypeF::Symbol => TypeF::Symbol,
            TypeF::ForeignId => TypeF::ForeignId,
            TypeF::Contract(ast) => TypeF::Contract(dest.move_ast_ref(ast)),
            TypeF::Arrow(src, tgt) => {
                TypeF::Arrow(dest.move_ref::<Type>(src), dest.move_ref::<Type>(tgt))
            }
            TypeF::Var(id) => TypeF::Var(id),
            TypeF::Forall {
                var,
                var_kind,
                body,
            } => TypeF::Forall {
                var,
                var_kind,
                body: dest.move_ref::<Type>(body),
            },
            TypeF::Enum(erows) => TypeF::Enum(typ::EnumRows::move_to(erows, dest)),
            TypeF::Record(rrows) => TypeF::Record(typ::RecordRows::move_to(rrows, dest)),
            TypeF::Dict {
                type_fields,
                flavour,
            } => TypeF::Dict {
                type_fields: dest.move_ref::<Type>(type_fields),
                flavour,
            },
            TypeF::Array(ty) => TypeF::Array(dest.move_ref::<Type>(ty)),
            TypeF::Wildcard(wildcard_id) => TypeF::Wildcard(wildcard_id),
        };

        Type { typ, pos: data.pos }
    }
}

impl MoveTo for typ::EnumRows<'_> {
    type Data<'ast> = typ::EnumRows<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        use typ::*;

        let inner = match data.0 {
            EnumRowsF::Empty => EnumRowsF::Empty,
            EnumRowsF::Extend { row, tail } => EnumRowsF::Extend {
                row: EnumRow::move_to(row, dest),
                tail: dest.move_ref::<EnumRows>(tail),
            },
            EnumRowsF::TailVar(loc_ident) => EnumRowsF::TailVar(loc_ident),
        };

        EnumRows(inner)
    }
}

impl MoveTo for typ::EnumRow<'_> {
    type Data<'ast> = typ::EnumRow<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        typ::EnumRow {
            id: data.id,
            typ: data.typ.map(|ty| dest.move_ref::<Type>(ty)),
        }
    }
}

impl MoveTo for typ::RecordRows<'_> {
    type Data<'ast> = typ::RecordRows<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        use typ::*;

        let inner = match data.0 {
            RecordRowsF::Empty => RecordRowsF::Empty,
            RecordRowsF::Extend { row, tail } => RecordRowsF::Extend {
                row: RecordRow::move_to(row, dest),
                tail: dest.move_ref::<RecordRows>(tail),
            },
            RecordRowsF::TailVar(loc_ident) => RecordRowsF::TailVar(loc_ident),
            RecordRowsF::TailDyn => RecordRowsF::TailDyn,
        };

        RecordRows(inner)
    }
}

impl MoveTo for typ::RecordRow<'_> {
    type Data<'ast> = typ::RecordRow<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        typ::RecordRow {
            id: data.id,
            typ: dest.move_ref::<Type>(data.typ),
        }
    }
}

impl MoveTo for StringChunk<Ast<'_>> {
    type Data<'ast> = StringChunk<Ast<'ast>>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        match data {
            StringChunk::Literal(s) => StringChunk::Literal(s),
            StringChunk::Expr(ast, indent) => StringChunk::Expr(Ast::move_to(ast, dest), indent),
        }
    }
}

impl MoveTo for Pattern<'_> {
    type Data<'ast> = Pattern<'ast>;

    fn move_to<'from, 'to>(pat: Pattern<'from>, dest: &'to AstAlloc) -> Pattern<'to> {
        let data = match pat.data {
            PatternData::Wildcard => PatternData::Wildcard,
            PatternData::Any(id) => PatternData::Any(id),
            PatternData::Record(record_pat) => {
                PatternData::Record(dest.move_ref::<RecordPattern>(record_pat))
            }
            PatternData::Array(array_pat) => {
                PatternData::Array(dest.move_ref::<ArrayPattern>(array_pat))
            }
            PatternData::Enum(enum_pat) => {
                PatternData::Enum(dest.move_ref::<EnumPattern>(enum_pat))
            }
            PatternData::Constant(const_pat) => {
                PatternData::Constant(dest.move_ref::<ConstantPattern>(const_pat))
            }
            PatternData::Or(or_pat) => PatternData::Or(dest.move_ref::<OrPattern>(or_pat)),
        };

        Pattern { data, ..pat }
    }
}

impl MoveTo for EnumPattern<'_> {
    type Data<'ast> = EnumPattern<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        EnumPattern {
            pattern: data.pattern.map(|pat| Pattern::move_to(pat, dest)),
            ..data
        }
    }
}

impl MoveTo for RecordPattern<'_> {
    type Data<'ast> = RecordPattern<'ast>;

    fn move_to<'from, 'to>(pat: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        RecordPattern {
            patterns: dest.alloc_many(
                pat.patterns
                    .iter()
                    .map(|field_pat| FieldPattern::move_to(field_pat.clone(), dest)),
            ),
            ..pat
        }
    }
}

impl MoveTo for FieldPattern<'_> {
    type Data<'ast> = FieldPattern<'ast>;

    fn move_to<'from, 'to>(pat: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        FieldPattern {
            annotation: Annotation::move_to(pat.annotation, dest),
            default: pat.default.map(|ast| Ast::move_to(ast, dest)),
            pattern: Pattern::move_to(pat.pattern, dest),
            ..pat
        }
    }
}

impl MoveTo for ArrayPattern<'_> {
    type Data<'ast> = ArrayPattern<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        ArrayPattern {
            patterns: dest.alloc_many(
                data.patterns
                    .iter()
                    .map(|pat| Pattern::move_to(pat.clone(), dest)),
            ),
            ..data
        }
    }
}

impl MoveTo for ConstantPattern<'_> {
    type Data<'ast> = ConstantPattern<'ast>;

    fn move_to<'from, 'to>(pat: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        ConstantPattern {
            data: ConstantPatternData::move_to(pat.data, dest),
            ..pat
        }
    }
}

impl MoveTo for ConstantPatternData<'_> {
    type Data<'ast> = ConstantPatternData<'ast>;

    fn move_to<'from, 'to>(data: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
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

impl MoveTo for OrPattern<'_> {
    type Data<'ast> = OrPattern<'ast>;

    fn move_to<'from, 'to>(pat: Self::Data<'from>, dest: &'to AstAlloc) -> Self::Data<'to> {
        OrPattern {
            patterns: dest.alloc_many(
                pat.patterns
                    .iter()
                    .map(|pat| Pattern::move_to(pat.clone(), dest)),
            ),
            ..pat
        }
    }
}
