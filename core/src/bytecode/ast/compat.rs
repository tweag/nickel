//! Compatibility with the current stable Nickel version.
//!
//! This module defines a trait for converting to and from the representation used in stable Nickel
//! to the new AST representation of the bytecode compiler, and implements it for the types defined
//! in [crate::bytecode::ast].

use super::{primop::PrimOp, *};
use crate::{combine::Combine, label, position::RawSpan, term, typ as mline_type};
use indexmap::IndexMap;
use smallvec::SmallVec;

/// Convert from the mainline Nickel representation to the new AST representation. This trait is
/// mostly `From` with an additional argument for the allocator.
///
/// # Parameters
///
/// - `'ast`: the lifetime of the AST nodes, tied to the allocator
/// - `'a`: the lifetime of the reference to the mainline Nickel object, which doesn't need to be
///   related to `'ast` (we will copy any required data into the allocator)
/// - `T`: the type of the mainline Nickel object ([term::Term], [term::pattern::Pattern], etc.)
pub trait FromMainline<'ast, T> {
    fn from_mainline(alloc: &'ast AstAlloc, mainline: &T) -> Self;
}

impl<'ast> FromMainline<'ast, term::pattern::Pattern> for &'ast Pattern<'ast> {
    fn from_mainline(
        alloc: &'ast AstAlloc,
        pattern: &term::pattern::Pattern,
    ) -> &'ast Pattern<'ast> {
        alloc.alloc(pattern.to_ast(alloc))
    }
}

impl<'ast> FromMainline<'ast, term::pattern::Pattern> for Pattern<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, pattern: &term::pattern::Pattern) -> Self {
        Pattern {
            data: pattern.data.to_ast(alloc),
            alias: pattern.alias,
            pos: pattern.pos,
        }
    }
}

impl<'ast> FromMainline<'ast, term::pattern::PatternData> for PatternData<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, data: &term::pattern::PatternData) -> Self {
        match data {
            term::pattern::PatternData::Wildcard => PatternData::Wildcard,
            term::pattern::PatternData::Any(id) => PatternData::Any(*id),
            term::pattern::PatternData::Record(record_pattern) => record_pattern.to_ast(alloc),
            term::pattern::PatternData::Array(array_pattern) => array_pattern.to_ast(alloc),
            term::pattern::PatternData::Enum(enum_pattern) => enum_pattern.to_ast(alloc),
            term::pattern::PatternData::Constant(constant_pattern) => {
                constant_pattern.to_ast(alloc)
            }
            term::pattern::PatternData::Or(or_pattern) => or_pattern.to_ast(alloc),
        }
    }
}

impl<'ast> FromMainline<'ast, term::pattern::RecordPattern> for PatternData<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, record_pat: &term::pattern::RecordPattern) -> Self {
        let patterns = record_pat
            .patterns
            .iter()
            .map(|field_pattern| field_pattern.to_ast(alloc));

        let tail = match record_pat.tail {
            term::pattern::TailPattern::Empty => TailPattern::Empty,
            term::pattern::TailPattern::Open => TailPattern::Open,
            term::pattern::TailPattern::Capture(id) => TailPattern::Capture(id),
        };

        PatternData::Record(alloc.record_pattern(patterns, tail, record_pat.pos))
    }
}

impl<'ast> FromMainline<'ast, term::pattern::FieldPattern> for FieldPattern<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, field_pat: &term::pattern::FieldPattern) -> Self {
        let pattern = field_pat.pattern.to_ast(alloc);

        let default = field_pat.default.as_ref().map(|term| term.to_ast(alloc));

        let annotation = field_pat.annotation.to_ast(alloc);

        FieldPattern {
            matched_id: field_pat.matched_id,
            annotation,
            default,
            pattern,
            pos: field_pat.pos,
        }
    }
}

impl<'ast> FromMainline<'ast, term::pattern::ArrayPattern> for PatternData<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, array_pat: &term::pattern::ArrayPattern) -> Self {
        let patterns = array_pat.patterns.iter().map(|pat| pat.to_ast(alloc));

        let tail = match array_pat.tail {
            term::pattern::TailPattern::Empty => TailPattern::Empty,
            term::pattern::TailPattern::Open => TailPattern::Open,
            term::pattern::TailPattern::Capture(id) => TailPattern::Capture(id),
        };

        PatternData::Array(alloc.array_pattern(patterns, tail, array_pat.pos))
    }
}

impl<'ast> FromMainline<'ast, term::pattern::EnumPattern> for PatternData<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, enum_pat: &term::pattern::EnumPattern) -> Self {
        let pattern = enum_pat.pattern.as_ref().map(|pat| (**pat).to_ast(alloc));
        PatternData::Enum(alloc.alloc(EnumPattern {
            tag: enum_pat.tag,
            pattern,
            pos: enum_pat.pos,
        }))
    }
}

impl<'ast> FromMainline<'ast, term::pattern::ConstantPattern> for PatternData<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, pattern: &term::pattern::ConstantPattern) -> Self {
        let data = match &pattern.data {
            term::pattern::ConstantPatternData::Bool(b) => ConstantPatternData::Bool(*b),
            term::pattern::ConstantPatternData::Number(n) => {
                ConstantPatternData::Number(alloc.generic_arena.alloc(n.clone()))
            }
            term::pattern::ConstantPatternData::String(s) => {
                ConstantPatternData::String(alloc.generic_arena.alloc_str(s))
            }
            term::pattern::ConstantPatternData::Null => ConstantPatternData::Null,
        };

        PatternData::Constant(alloc.alloc(ConstantPattern {
            data,
            pos: pattern.pos,
        }))
    }
}

impl<'ast> FromMainline<'ast, term::pattern::OrPattern> for PatternData<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, pattern: &term::pattern::OrPattern) -> Self {
        let patterns = pattern
            .patterns
            .iter()
            .map(|pat| pat.to_ast(alloc))
            .collect::<Vec<_>>();

        PatternData::Or(alloc.or_pattern(patterns, pattern.pos))
    }
}

impl<'ast> FromMainline<'ast, term::TypeAnnotation> for Annotation<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, annot: &term::TypeAnnotation) -> Self {
        let typ = annot.typ.as_ref().map(|typ| typ.typ.to_ast(alloc));

        let contracts = alloc.alloc_many(
            annot
                .contracts
                .iter()
                .map(|contract| contract.typ.to_ast(alloc)),
        );

        Annotation { typ, contracts }
    }
}

impl<'ast> FromMainline<'ast, (LocIdent, term::record::Field)> for record::FieldDef<'ast> {
    fn from_mainline(
        alloc: &'ast AstAlloc,
        (id, content): &(LocIdent, term::record::Field),
    ) -> Self {
        let pos = content
            .value
            .as_ref()
            .map(|value| id.pos.fuse(value.pos))
            .unwrap_or(id.pos);

        record::FieldDef {
            path: alloc.alloc_singleton(record::FieldPathElem::Ident(*id)),
            value: content.value.as_ref().map(|term| term.to_ast(alloc)),
            metadata: content.metadata.to_ast(alloc),
            pos,
        }
    }
}

impl<'ast> FromMainline<'ast, term::record::FieldMetadata> for record::FieldMetadata<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, metadata: &term::record::FieldMetadata) -> Self {
        let doc = metadata.doc.as_ref().map(|doc| alloc.alloc_str(doc));

        record::FieldMetadata {
            doc,
            annotation: metadata.annotation.to_ast(alloc),
            opt: metadata.opt,
            not_exported: metadata.not_exported,
            priority: metadata.priority.clone(),
        }
    }
}

impl<'ast> FromMainline<'ast, mline_type::Type> for Type<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, mainline: &mline_type::Type) -> Self {
        Type {
            typ: mainline.typ.to_ast(alloc),
            pos: mainline.pos,
        }
    }
}

type MainlineTypeUnr = mline_type::TypeF<
    Box<mline_type::Type>,
    mline_type::RecordRows,
    mline_type::EnumRows,
    term::RichTerm,
>;

impl<'ast> FromMainline<'ast, MainlineTypeUnr> for TypeUnr<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, typ: &MainlineTypeUnr) -> Self {
        typ.clone().map(
            |typ| &*alloc.generic_arena.alloc((*typ).to_ast(alloc)),
            |rrows| rrows.to_ast(alloc),
            |erows| erows.to_ast(alloc),
            |ctr| ctr.to_ast(alloc),
        )
    }
}

impl<'ast> FromMainline<'ast, mline_type::RecordRows> for RecordRows<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, rrows: &mline_type::RecordRows) -> Self {
        RecordRows(rrows.0.to_ast(alloc))
    }
}

impl<'ast> FromMainline<'ast, mline_type::EnumRows> for EnumRows<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, erows: &mline_type::EnumRows) -> Self {
        EnumRows(erows.0.to_ast(alloc))
    }
}

type MainlineEnumRowsUnr = mline_type::EnumRowsF<Box<mline_type::Type>, Box<mline_type::EnumRows>>;

impl<'ast> FromMainline<'ast, MainlineEnumRowsUnr> for EnumRowsUnr<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, erows: &MainlineEnumRowsUnr) -> Self {
        erows.clone().map(
            |typ| &*alloc.generic_arena.alloc((*typ).to_ast(alloc)),
            |erows| &*alloc.generic_arena.alloc((*erows).to_ast(alloc)),
        )
    }
}

type MainlineRecordRowsUnr =
    mline_type::RecordRowsF<Box<mline_type::Type>, Box<mline_type::RecordRows>>;

impl<'ast> FromMainline<'ast, MainlineRecordRowsUnr> for RecordRowsUnr<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, rrows: &MainlineRecordRowsUnr) -> Self {
        rrows.clone().map(
            |typ| &*alloc.generic_arena.alloc((*typ).to_ast(alloc)),
            |rrows| &*alloc.generic_arena.alloc((*rrows).to_ast(alloc)),
        )
    }
}

impl<'ast> FromMainline<'ast, term::Term> for Node<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, term: &term::Term) -> Self {
        use term::Term;

        match term {
            Term::Null => Node::Null,
            Term::Bool(b) => Node::Bool(*b),
            Term::Num(n) => alloc.number(n.clone()),
            Term::Str(s) => alloc.string(s),
            Term::StrChunks(chunks) => {
                alloc.string_chunks(chunks.iter().rev().map(|chunk| match chunk {
                    term::StrChunk::Literal(s) => StringChunk::Literal(s.clone()),
                    term::StrChunk::Expr(expr, indent) => {
                        StringChunk::Expr(expr.to_ast(alloc), *indent)
                    }
                }))
            }
            t @ (Term::Fun(_, body) | Term::FunPattern(_, body)) => {
                let fst_arg = match t {
                    Term::Fun(id, _) => Pattern::any(*id),
                    Term::FunPattern(pat, _) => pat.to_ast(alloc),
                    // unreachable!(): we are in a match arm that matches either Fun or FunPattern
                    _ => unreachable!(),
                };

                let mut args = vec![fst_arg];
                let mut maybe_next_fun = body;

                let final_body = loop {
                    match maybe_next_fun.as_ref() {
                        Term::Fun(next_id, next_body) => {
                            args.push(Pattern::any(*next_id));
                            maybe_next_fun = next_body;
                        }
                        Term::FunPattern(next_pat, next_body) => {
                            args.push(next_pat.to_ast(alloc));
                            maybe_next_fun = next_body;
                        }
                        _ => break maybe_next_fun,
                    }
                };

                alloc.fun(args, final_body.to_ast(alloc))
            }
            Term::Let(bindings, body, attrs) => alloc.let_block(
                bindings.iter().map(|(id, value)| LetBinding {
                    pattern: Pattern::any(*id),
                    value: value.to_ast(alloc),
                    metadata: Default::default(),
                }),
                body.to_ast(alloc),
                attrs.rec,
            ),
            Term::LetPattern(bindings, body, attrs) => alloc.let_block(
                bindings.iter().map(|(pat, value)| LetBinding {
                    pattern: pat.to_ast(alloc),
                    value: value.to_ast(alloc),
                    metadata: Default::default(),
                }),
                body.to_ast(alloc),
                attrs.rec,
            ),
            Term::App(fun, arg) => {
                match fun.as_ref() {
                    // We have to special-case if-then-else, which is encoded as a primop application
                    // of the unary operator if-then-else to the condition, followed by two normal
                    // applications for the if and else branches, which is a bit painful to match.
                    Term::App(fun_inner, arg_inner)
                        if matches!(
                            fun_inner.as_ref(),
                            Term::Op1(term::UnaryOp::IfThenElse, _)
                        ) =>
                    {
                        if let Term::Op1(term::UnaryOp::IfThenElse, cond) = fun_inner.as_ref() {
                            return alloc.if_then_else(
                                cond.to_ast(alloc),
                                arg_inner.to_ast(alloc),
                                arg.to_ast(alloc),
                            );
                        }
                    }
                    _ => (),
                };

                let mut args = vec![arg.to_ast(alloc)];
                let mut maybe_next_app = fun.as_ref();

                while let Term::App(next_fun, next_arg) = maybe_next_app {
                    args.push(next_arg.to_ast(alloc));
                    maybe_next_app = next_fun.as_ref();
                }

                // Application is left-associative: `f x y` is parsed as `(f x) y`. So we see the
                // outer argument `y` first, and `args` will be `[y, x]`. We need to reverse it to
                // match the expected order of a flat application node.
                args.reverse();
                alloc.app(fun.to_ast(alloc), args)
            }
            Term::Var(id) => Node::Var(*id),
            Term::Enum(id) => alloc.enum_variant(*id, None),
            Term::EnumVariant { tag, arg, attrs: _ } => {
                alloc.enum_variant(*tag, Some(arg.to_ast(alloc)))
            }
            Term::RecRecord(data, dyn_fields, _deps) => {
                let mut field_defs = Vec::new();

                field_defs.extend(data.fields.iter().map(|(id, field)| {
                    let pos = field
                        .value
                        .as_ref()
                        .map(|value| id.pos.fuse(value.pos))
                        .unwrap_or(id.pos);

                    record::FieldDef {
                        path: record::FieldPathElem::single_ident_path(alloc, *id),
                        value: field.value.as_ref().map(|term| term.to_ast(alloc)),
                        metadata: field.metadata.to_ast(alloc),
                        pos,
                    }
                }));

                field_defs.extend(dyn_fields.iter().map(|(expr, field)| {
                    let pos_field_name = expr.pos;
                    let pos = field
                        .value
                        .as_ref()
                        .map(|v| pos_field_name.fuse(v.pos))
                        .unwrap_or(pos_field_name);

                    record::FieldDef {
                        path: record::FieldPathElem::single_expr_path(alloc, expr.to_ast(alloc)),
                        metadata: field.metadata.to_ast(alloc),
                        value: field.value.as_ref().map(|term| term.to_ast(alloc)),
                        pos,
                    }
                }));

                alloc.record(Record {
                    field_defs: alloc.alloc_many(field_defs),
                    open: data.attrs.open,
                })
            }
            Term::Record(data) => {
                let field_defs = alloc.alloc_many(data.fields.iter().map(|(id, field)| {
                    let pos = field
                        .value
                        .as_ref()
                        .map(|value| id.pos.fuse(value.pos))
                        .unwrap_or(id.pos);

                    record::FieldDef {
                        path: record::FieldPathElem::single_ident_path(alloc, *id),
                        value: field.value.as_ref().map(|term| term.to_ast(alloc)),
                        metadata: field.metadata.to_ast(alloc),
                        pos,
                    }
                }));

                alloc.record(Record {
                    field_defs,
                    open: data.attrs.open,
                })
            }
            Term::Match(data) => {
                let branches = data.branches.iter().map(|branch| MatchBranch {
                    pattern: branch.pattern.to_ast(alloc),
                    guard: branch.guard.as_ref().map(|term| term.to_ast(alloc)),
                    body: branch.body.to_ast(alloc),
                });

                alloc.match_expr(branches)
            }
            Term::Array(data, _attrs) => {
                // We should probably make array's iterator an ExactSizeIterator. But for now, we
                // don't care about the translation's performance so it's simpler to just collect
                // them in a vec locally.
                let elts = data
                    .iter()
                    .map(|term| term.to_ast(alloc))
                    .collect::<Vec<_>>();
                alloc.array(elts)
            }
            Term::Op1(op, arg) => {
                alloc.prim_op(PrimOp::from(op), std::iter::once(arg.to_ast(alloc)))
            }
            Term::Op2(op, arg1, arg2) => {
                // [^primop-argument-order]: Some primops have had exotic arguments order for
                // historical reasons. The new AST tries to follow the stdlib argument order
                // whenever possible, which means we have to swap the arguments for a few primops.

                let op = PrimOp::from(op);
                let mut args = [arg1.to_ast(alloc), arg2.to_ast(alloc)];

                if matches!(op, PrimOp::ArrayAt | PrimOp::StringContains) {
                    args.swap(0, 1);
                }

                alloc.prim_op(op, args)
            }
            Term::OpN(op, args) => {
                // See [^primop-argument-order].
                let op = PrimOp::from(op);
                let mut args: Vec<_> = args.iter().map(|arg| arg.to_ast(alloc)).collect();
                if let PrimOp::StringSubstr = op {
                    debug_assert_eq!(args.len(), 3);
                    // The original order is: the string, then start and end.
                    // The target order is: start, end and then the string.
                    args.swap(0, 1);
                    args.swap(1, 2);
                }

                alloc.prim_op(op, args)
            }
            Term::SealingKey(_) => panic!("didn't expect a sealing key at the first stage"),
            Term::Sealed(..) => panic!("didn't expect a sealed term at the first stage"),
            Term::Annotated(annot, term) => {
                alloc.annotated(annot.to_ast(alloc), term.to_ast(alloc))
            }
            Term::Import(term::Import::Path { path, format }) => {
                alloc.import_path(path.clone(), *format)
            }
            Term::Import(term::Import::Package { id }) => alloc.import_package(*id),
            Term::ResolvedImport(_) => panic!("didn't expect a resolved import at parsing stage"),
            Term::Type { typ, .. } => alloc.typ(typ.to_ast(alloc)),
            Term::CustomContract(_) => panic!("didn't expect a custom contract at parsing stage"),
            Term::ParseError(error) => alloc.parse_error(error.clone()),
            Term::RuntimeError(_) => panic!("didn't expect a runtime error at parsing stage"),
            Term::Closure(_) => panic!("didn't expect a closure at parsing stage"),
            Term::ForeignId(_) => panic!("didn't expect a foreign id at parsing stage"),
            _ => unimplemented!(),
        }
    }
}

impl<'ast> FromMainline<'ast, term::RichTerm> for Ast<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, rterm: &term::RichTerm) -> Self {
        Ast {
            node: rterm.as_ref().to_ast(alloc),
            pos: rterm.pos,
        }
    }
}

impl<'ast> FromMainline<'ast, term::RichTerm> for &'ast Ast<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, rterm: &term::RichTerm) -> Self {
        alloc.alloc(rterm.to_ast(alloc))
    }
}

/// Symmetric to `FromMainline`, as `Into` is to `From`.
pub trait ToAst<'ast, T> {
    fn to_ast(&self, alloc: &'ast AstAlloc) -> T;
}

impl<'ast, S, T> ToAst<'ast, T> for S
where
    T: FromMainline<'ast, S>,
{
    fn to_ast(&self, alloc: &'ast AstAlloc) -> T {
        T::from_mainline(alloc, self)
    }
}

// Primops don't need any heap allocation, so we can implement `From` directly.
impl From<&term::UnaryOp> for PrimOp {
    fn from(op: &term::UnaryOp) -> Self {
        match op {
            term::UnaryOp::IfThenElse => {
                panic!("if-then-else should have been handed separately by special casing")
            }
            term::UnaryOp::Typeof => PrimOp::Typeof,
            term::UnaryOp::BoolAnd => PrimOp::BoolAnd,
            term::UnaryOp::BoolOr => PrimOp::BoolOr,
            term::UnaryOp::BoolNot => PrimOp::BoolNot,
            term::UnaryOp::Blame => PrimOp::Blame,
            term::UnaryOp::EnumEmbed(loc_ident) => PrimOp::EnumEmbed(*loc_ident),
            term::UnaryOp::RecordAccess(loc_ident) => PrimOp::RecordStatAccess(*loc_ident),
            term::UnaryOp::ArrayMap => PrimOp::ArrayMap,
            term::UnaryOp::RecordMap => PrimOp::RecordMap,
            term::UnaryOp::LabelFlipPol => PrimOp::LabelFlipPol,
            term::UnaryOp::LabelPol => PrimOp::LabelPol,
            term::UnaryOp::LabelGoDom => PrimOp::LabelGoDom,
            term::UnaryOp::LabelGoCodom => PrimOp::LabelGoCodom,
            term::UnaryOp::LabelGoArray => PrimOp::LabelGoArray,
            term::UnaryOp::LabelGoDict => PrimOp::LabelGoDict,
            term::UnaryOp::Seq => PrimOp::Seq,
            term::UnaryOp::DeepSeq => PrimOp::DeepSeq,
            term::UnaryOp::ArrayLength => PrimOp::ArrayLength,
            term::UnaryOp::ArrayGen => PrimOp::ArrayGen,
            term::UnaryOp::RecordFields(record_op_kind) => PrimOp::RecordFields(*record_op_kind),
            term::UnaryOp::RecordValues => PrimOp::RecordValues,
            term::UnaryOp::StringTrim => PrimOp::StringTrim,
            term::UnaryOp::StringChars => PrimOp::StringChars,
            term::UnaryOp::StringUppercase => PrimOp::StringUppercase,
            term::UnaryOp::StringLowercase => PrimOp::StringLowercase,
            term::UnaryOp::StringLength => PrimOp::StringLength,
            term::UnaryOp::ToString => PrimOp::ToString,
            term::UnaryOp::NumberFromString => PrimOp::NumberFromString,
            term::UnaryOp::EnumFromString => PrimOp::EnumFromString,
            term::UnaryOp::StringIsMatch => PrimOp::StringIsMatch,
            term::UnaryOp::StringFind => PrimOp::StringFind,
            term::UnaryOp::StringFindAll => PrimOp::StringFindAll,
            term::UnaryOp::Force {
                ignore_not_exported,
            } => PrimOp::Force {
                ignore_not_exported: *ignore_not_exported,
            },
            term::UnaryOp::RecordEmptyWithTail => PrimOp::RecordEmptyWithTail,
            term::UnaryOp::RecordFreeze => PrimOp::RecordFreeze,
            term::UnaryOp::Trace => PrimOp::Trace,
            term::UnaryOp::LabelPushDiag => PrimOp::LabelPushDiag,
            #[cfg(feature = "nix-experimental")]
            term::UnaryOp::EvalNix => PrimOp::EvalNix,
            term::UnaryOp::EnumGetArg => PrimOp::EnumGetArg,
            term::UnaryOp::EnumMakeVariant => PrimOp::EnumMakeVariant,
            term::UnaryOp::EnumIsVariant => PrimOp::EnumIsVariant,
            term::UnaryOp::EnumGetTag => PrimOp::EnumGetTag,
            term::UnaryOp::ContractCustom => PrimOp::ContractCustom,
            term::UnaryOp::NumberArcCos => PrimOp::NumberArcCos,
            term::UnaryOp::NumberArcSin => PrimOp::NumberArcSin,
            term::UnaryOp::NumberArcTan => PrimOp::NumberArcTan,
            term::UnaryOp::NumberCos => PrimOp::NumberCos,
            term::UnaryOp::NumberSin => PrimOp::NumberSin,
            term::UnaryOp::NumberTan => PrimOp::NumberTan,

            op @ (term::UnaryOp::TagsOnlyMatch { .. }
            | term::UnaryOp::ChunksConcat
            | term::UnaryOp::StringIsMatchCompiled(_)
            | term::UnaryOp::StringFindCompiled(_)
            | term::UnaryOp::StringFindAllCompiled(_)
            | term::UnaryOp::RecDefault
            | term::UnaryOp::RecForce
            | term::UnaryOp::PatternBranch
            | term::UnaryOp::ContractPostprocessResult
            | term::UnaryOp::ContractAttachDefaultLabel) => {
                panic!("didn't expect {op} at the parsing stage")
            }
        }
    }
}

impl From<&term::BinaryOp> for PrimOp {
    fn from(op: &term::BinaryOp) -> Self {
        match op {
            term::BinaryOp::Plus => PrimOp::Plus,
            term::BinaryOp::Sub => PrimOp::Sub,
            term::BinaryOp::Mult => PrimOp::Mult,
            term::BinaryOp::Div => PrimOp::Div,
            term::BinaryOp::Modulo => PrimOp::Modulo,
            term::BinaryOp::NumberArcTan2 => PrimOp::NumberArcTan2,
            term::BinaryOp::NumberLog => PrimOp::NumberLog,
            term::BinaryOp::Pow => PrimOp::Pow,
            term::BinaryOp::StringConcat => PrimOp::StringConcat,
            term::BinaryOp::Eq => PrimOp::Eq,
            term::BinaryOp::LessThan => PrimOp::LessThan,
            term::BinaryOp::LessOrEq => PrimOp::LessOrEq,
            term::BinaryOp::GreaterThan => PrimOp::GreaterThan,
            term::BinaryOp::GreaterOrEq => PrimOp::GreaterOrEq,
            term::BinaryOp::ContractApply => PrimOp::ContractApply,
            term::BinaryOp::ContractCheck => PrimOp::ContractCheck,
            term::BinaryOp::LabelWithErrorData => PrimOp::LabelWithErrorData,
            term::BinaryOp::LabelGoField => PrimOp::LabelGoField,
            // This corresponds to a call to `%record/insert%` from the source language. Other
            // forms are introduced by the evaluator, e.g. when evaluating a recursive record to a
            // record.
            term::BinaryOp::RecordInsert {
                metadata,
                pending_contracts,
                ext_kind,
                op_kind,
            } if metadata.is_empty()
                && pending_contracts.is_empty()
                && *ext_kind == term::RecordExtKind::WithValue =>
            {
                PrimOp::RecordInsert(*op_kind)
            }
            term::BinaryOp::RecordRemove(record_op_kind) => PrimOp::RecordRemove(*record_op_kind),
            term::BinaryOp::RecordGet => PrimOp::RecordGet,
            term::BinaryOp::RecordHasField(record_op_kind) => {
                PrimOp::RecordHasField(*record_op_kind)
            }
            term::BinaryOp::RecordFieldIsDefined(record_op_kind) => {
                PrimOp::RecordFieldIsDefined(*record_op_kind)
            }
            term::BinaryOp::RecordSplitPair => PrimOp::RecordSplitPair,
            term::BinaryOp::RecordDisjointMerge => PrimOp::RecordDisjointMerge,
            term::BinaryOp::ArrayConcat => PrimOp::ArrayConcat,
            term::BinaryOp::ArrayAt => PrimOp::ArrayAt,
            term::BinaryOp::Merge(merge_label) => PrimOp::Merge(merge_label.kind),
            term::BinaryOp::Hash => PrimOp::Hash,
            term::BinaryOp::Serialize => PrimOp::Serialize,
            term::BinaryOp::Deserialize => PrimOp::Deserialize,
            term::BinaryOp::StringSplit => PrimOp::StringSplit,
            term::BinaryOp::StringContains => PrimOp::StringContains,
            term::BinaryOp::StringCompare => PrimOp::StringCompare,
            term::BinaryOp::ContractArrayLazyApp => PrimOp::ContractArrayLazyApp,
            term::BinaryOp::ContractRecordLazyApp => PrimOp::ContractRecordLazyApp,
            term::BinaryOp::LabelWithMessage => PrimOp::LabelWithMessage,
            term::BinaryOp::LabelWithNotes => PrimOp::LabelWithNotes,
            term::BinaryOp::LabelAppendNote => PrimOp::LabelAppendNote,
            term::BinaryOp::LabelLookupTypeVar => PrimOp::LabelLookupTypeVar,

            term::BinaryOp::Seal => PrimOp::Seal,
            term::BinaryOp::Unseal => PrimOp::Unseal,

            term::BinaryOp::RecordInsert { .. } => {
                panic!("didn't expect %record/insert% at the parsing stage")
            }
        }
    }
}

impl From<&term::NAryOp> for PrimOp {
    fn from(op: &term::NAryOp) -> Self {
        match op {
            term::NAryOp::StringReplace => PrimOp::StringReplace,
            term::NAryOp::StringReplaceRegex => PrimOp::StringReplaceRegex,
            term::NAryOp::StringSubstr => PrimOp::StringSubstr,
            term::NAryOp::MergeContract => PrimOp::MergeContract,
            term::NAryOp::RecordSealTail => PrimOp::RecordSealTail,
            term::NAryOp::RecordUnsealTail => PrimOp::RecordUnsealTail,
            term::NAryOp::LabelInsertTypeVar => PrimOp::LabelInsertTypeVar,
            term::NAryOp::ArraySlice => PrimOp::ArraySlice,
        }
    }
}

/// Trait from converting from the new AST representation to the mainline Nickel representation.
///
/// Note that in that direction, we don't need the allocator: those traits are thus isomorphic to
/// to `From<_>` and `Into<_>` respectively. However, we convert from a reference to an owned
/// value. We initially used `From` directly, but this causes annoying inference issue around auto
/// deref and blanket implementations of `From`/`Into`. It's just simpler and more explicit to have
/// a separate trait for this conversion as well.
pub trait FromAst<T> {
    fn from_ast(ast: &T) -> Self;
}

pub trait ToMainline<T> {
    fn to_mainline(&self) -> T;
}

impl<S, T> ToMainline<T> for S
where
    T: FromAst<S>,
{
    fn to_mainline(&self) -> T {
        T::from_ast(self)
    }
}

impl<'ast> FromAst<Pattern<'ast>> for term::pattern::Pattern {
    fn from_ast(pattern: &Pattern<'ast>) -> Self {
        term::pattern::Pattern {
            data: pattern.data.to_mainline(),
            alias: pattern.alias,
            pos: pattern.pos,
        }
    }
}

impl<'ast> FromAst<PatternData<'ast>> for term::pattern::PatternData {
    fn from_ast(ast: &PatternData<'ast>) -> Self {
        match ast {
            PatternData::Wildcard => term::pattern::PatternData::Wildcard,
            PatternData::Any(id) => term::pattern::PatternData::Any(*id),
            PatternData::Record(record_pattern) => (*record_pattern).to_mainline(),
            PatternData::Array(array_pattern) => (*array_pattern).to_mainline(),
            PatternData::Enum(enum_pattern) => (*enum_pattern).to_mainline(),
            PatternData::Constant(constant_pattern) => (*constant_pattern).to_mainline(),
            PatternData::Or(or_pattern) => (*or_pattern).to_mainline(),
        }
    }
}

impl<'ast> FromAst<RecordPattern<'ast>> for term::pattern::PatternData {
    fn from_ast(record_pat: &RecordPattern<'ast>) -> Self {
        let patterns = record_pat
            .patterns
            .iter()
            .map(|field_pattern| field_pattern.to_mainline())
            .collect();

        let tail = match record_pat.tail {
            TailPattern::Empty => term::pattern::TailPattern::Empty,
            TailPattern::Open => term::pattern::TailPattern::Open,
            TailPattern::Capture(id) => term::pattern::TailPattern::Capture(id),
        };

        term::pattern::PatternData::Record(term::pattern::RecordPattern {
            patterns,
            tail,
            pos: record_pat.pos,
        })
    }
}

impl<'ast> FromAst<FieldPattern<'ast>> for term::pattern::FieldPattern {
    fn from_ast(field_pat: &FieldPattern<'ast>) -> Self {
        let pattern = field_pat.pattern.to_mainline();

        let default = field_pat.default.as_ref().map(|term| term.to_mainline());

        let annotation = field_pat.annotation.to_mainline();

        term::pattern::FieldPattern {
            matched_id: field_pat.matched_id,
            annotation,
            default,
            pattern,
            pos: field_pat.pos,
        }
    }
}

impl<'ast> FromAst<ArrayPattern<'ast>> for term::pattern::PatternData {
    fn from_ast(array_pat: &ArrayPattern<'ast>) -> Self {
        let patterns = array_pat
            .patterns
            .iter()
            .map(|pat| pat.to_mainline())
            .collect();

        let tail = match array_pat.tail {
            TailPattern::Empty => term::pattern::TailPattern::Empty,
            TailPattern::Open => term::pattern::TailPattern::Open,
            TailPattern::Capture(id) => term::pattern::TailPattern::Capture(id),
        };

        term::pattern::PatternData::Array(term::pattern::ArrayPattern {
            patterns,
            tail,
            pos: array_pat.pos,
        })
    }
}

impl<'ast> FromAst<EnumPattern<'ast>> for term::pattern::PatternData {
    fn from_ast(enum_pat: &EnumPattern<'ast>) -> Self {
        let pattern = enum_pat
            .pattern
            .as_ref()
            .map(|pat| Box::new(pat.to_mainline()));

        term::pattern::PatternData::Enum(term::pattern::EnumPattern {
            tag: enum_pat.tag,
            pattern,
            pos: enum_pat.pos,
        })
    }
}

impl<'ast> FromAst<ConstantPattern<'ast>> for term::pattern::PatternData {
    fn from_ast(pattern: &ConstantPattern<'ast>) -> Self {
        let data = match pattern.data {
            ConstantPatternData::Bool(b) => term::pattern::ConstantPatternData::Bool(b),
            ConstantPatternData::Number(n) => term::pattern::ConstantPatternData::Number(n.clone()),
            ConstantPatternData::String(s) => term::pattern::ConstantPatternData::String(s.into()),
            ConstantPatternData::Null => term::pattern::ConstantPatternData::Null,
        };

        term::pattern::PatternData::Constant(term::pattern::ConstantPattern {
            data,
            pos: pattern.pos,
        })
    }
}

impl<'ast> FromAst<OrPattern<'ast>> for term::pattern::PatternData {
    fn from_ast(pattern: &OrPattern<'ast>) -> Self {
        let patterns = pattern
            .patterns
            .iter()
            .map(|pat| pat.to_mainline())
            .collect::<Vec<_>>();

        term::pattern::PatternData::Or(term::pattern::OrPattern {
            patterns,
            pos: pattern.pos,
        })
    }
}

impl<'ast> FromAst<Annotation<'ast>> for term::TypeAnnotation {
    fn from_ast(annot: &Annotation<'ast>) -> Self {
        let typ = annot.typ.as_ref().map(ToMainline::to_mainline);

        let contracts = annot
            .contracts
            .iter()
            .map(ToMainline::to_mainline)
            .collect();

        term::TypeAnnotation { typ, contracts }
    }
}

impl<'ast> FromAst<StringChunk<Ast<'ast>>> for term::StrChunk<term::RichTerm> {
    fn from_ast(chunk: &StringChunk<Ast<'ast>>) -> Self {
        match chunk {
            StringChunk::Literal(s) => term::StrChunk::Literal(s.clone()),
            StringChunk::Expr(expr, indent) => term::StrChunk::Expr(expr.to_mainline(), *indent),
        }
    }
}

/// Similar to [record::FieldPathElem], but for the mainline representation: either an identifier
/// or a quoted identifier.
pub enum FieldName {
    Ident(LocIdent),
    Expr(term::RichTerm),
}

impl FromAst<record::FieldPathElem<'_>> for FieldName {
    fn from_ast(elem: &record::FieldPathElem<'_>) -> Self {
        match elem {
            record::FieldPathElem::Ident(id) => FieldName::Ident(*id),
            record::FieldPathElem::Expr(node) => FieldName::Expr(node.to_mainline()),
        }
    }
}

impl<'ast> FromAst<record::FieldDef<'ast>> for (FieldName, term::record::Field) {
    fn from_ast(field: &record::FieldDef<'ast>) -> Self {
        /// Elaborate a record field definition specified as a path, like `a.b.c = foo`, into a regular
        /// flat definition `a = {b = {c = foo}}`.
        ///
        /// # Preconditions
        /// - /!\ path must be **non-empty**, otherwise this function panics
        use super::record::FieldPathElem;

        // unwrap(): field paths must be non-empty
        let name_innermost = field.path.last().unwrap().try_as_ident();

        let initial = term::record::Field {
            value: field.value.as_ref().map(ToMainline::to_mainline),
            metadata: term::record::FieldMetadata::from_ast(&field.metadata)
                .with_field_name(name_innermost),
            pending_contracts: Vec::new(),
        };

        let mut it = field.path.iter();
        let fst = it.next().unwrap();

        let content = it.rev().fold(initial, |acc, path_elem| {
            // We first compute a position for the intermediate generated records (it's useful
            // in particular for the LSP). The position starts at the subpath corresponding to
            // the intermediate record and ends at the final value.
            let acc_pos = acc.value.as_ref().map(|value| value.pos);

            let pos = acc_pos
                .map(|p| p.fuse(path_elem.pos()))
                .unwrap_or_else(|| path_elem.pos());

            match path_elem {
                FieldPathElem::Ident(id) => {
                    let mut fields = IndexMap::new();
                    fields.insert(*id, acc);
                    term::record::Field::from(term::RichTerm::new(
                        term::Term::Record(term::record::RecordData {
                            fields,
                            ..Default::default()
                        }),
                        pos,
                    ))
                }
                FieldPathElem::Expr(expr) => {
                    let pos = expr.pos;
                    let expr = term::RichTerm::from_ast(expr);
                    let static_access = expr.as_ref().try_str_chunk_as_static_str();

                    if let Some(static_access) = static_access {
                        let id = LocIdent::new_with_pos(static_access, pos);
                        let mut fields = IndexMap::new();
                        fields.insert(id, acc);

                        term::record::Field::from(term::RichTerm::new(
                            term::Term::Record(term::record::RecordData {
                                fields,
                                ..Default::default()
                            }),
                            pos,
                        ))
                    } else {
                        // The record we create isn't recursive, because it is only comprised of
                        // one dynamic field. It's just simpler to use the infrastructure of
                        // `RecRecord` to handle dynamic fields at evaluation time rather than
                        // right here
                        term::record::Field::from(term::RichTerm::new(
                            term::Term::RecRecord(
                                term::record::RecordData::empty(),
                                vec![(expr, acc)],
                                None,
                            ),
                            pos,
                        ))
                    }
                }
            }
        });

        (fst.to_mainline(), content)
    }
}

impl<'ast> FromAst<record::FieldMetadata<'ast>> for term::record::FieldMetadata {
    fn from_ast(metadata: &record::FieldMetadata<'ast>) -> Self {
        let doc = metadata.doc.as_ref().map(|doc| String::from(&**doc));

        term::record::FieldMetadata {
            doc,
            annotation: metadata.annotation.to_mainline(),
            opt: metadata.opt,
            not_exported: metadata.not_exported,
            priority: metadata.priority.clone(),
        }
    }
}

impl<'ast> FromAst<Type<'ast>> for mline_type::Type {
    fn from_ast(typ: &Type<'ast>) -> Self {
        mline_type::Type {
            typ: typ.typ.to_mainline(),
            pos: typ.pos,
        }
    }
}

impl<'ast> FromAst<TypeUnr<'ast>> for MainlineTypeUnr {
    fn from_ast(typ: &TypeUnr<'ast>) -> Self {
        typ.clone().map(
            |typ| Box::new(typ.to_mainline()),
            |rrows| rrows.to_mainline(),
            |erows| erows.to_mainline(),
            |ctr| ctr.to_mainline(),
        )
    }
}

impl<'ast> FromAst<RecordRows<'ast>> for mline_type::RecordRows {
    fn from_ast(rrows: &RecordRows<'ast>) -> Self {
        mline_type::RecordRows(rrows.0.to_mainline())
    }
}

impl<'ast> FromAst<EnumRows<'ast>> for mline_type::EnumRows {
    fn from_ast(erows: &EnumRows<'ast>) -> Self {
        mline_type::EnumRows(erows.0.to_mainline())
    }
}

impl<'ast> FromAst<EnumRowsUnr<'ast>> for MainlineEnumRowsUnr {
    fn from_ast(erows: &EnumRowsUnr<'ast>) -> Self {
        erows.clone().map(
            |typ| Box::new(typ.to_mainline()),
            |erows| Box::new(erows.to_mainline()),
        )
    }
}

impl<'ast> FromAst<EnumRow<'ast>> for mline_type::EnumRow {
    fn from_ast(erow: &EnumRow<'ast>) -> Self {
        mline_type::EnumRow {
            id: erow.id,
            typ: erow.typ.as_ref().map(|ty| Box::new((*ty).to_mainline())),
        }
    }
}

impl<'ast> FromAst<RecordRowsUnr<'ast>> for MainlineRecordRowsUnr {
    fn from_ast(rrows: &RecordRowsUnr<'ast>) -> Self {
        rrows.clone().map(
            |typ| Box::new(typ.to_mainline()),
            |rrows| Box::new(rrows.to_mainline()),
        )
    }
}

impl<'ast> FromAst<RecordRow<'ast>> for mline_type::RecordRow {
    fn from_ast(rrow: &RecordRow<'ast>) -> Self {
        mline_type::RecordRowF {
            id: rrow.id,
            typ: Box::new(rrow.typ.to_mainline()),
        }
    }
}

impl<'ast> FromAst<Type<'ast>> for term::LabeledType {
    fn from_ast(typ: &Type<'ast>) -> Self {
        let typ: mline_type::Type = typ.to_mainline();
        //TODO:remove
        if typ.pos.into_opt().is_none() {
            panic!("Expected a position to be set for the type {typ:?}");
        }
        // We expect the new AST node to always have a position set. In fact we should
        // probably switch to `RawSpan` instead of `TermPos` everywhere; but let's do that
        // later
        let span = typ.pos.unwrap();

        term::LabeledType {
            typ: typ.clone(),
            label: label::Label {
                typ: std::rc::Rc::new(typ),
                span,
                ..Default::default()
            },
        }
    }
}

impl<'ast> FromAst<MatchBranch<'ast>> for term::MatchBranch {
    fn from_ast(branch: &MatchBranch<'ast>) -> Self {
        term::MatchBranch {
            pattern: branch.pattern.to_mainline(),
            guard: branch.guard.as_ref().map(|ast| ast.to_mainline()),
            body: branch.body.to_mainline(),
        }
    }
}

/// One data type representing all possible primops from the mainline AST, whether unary, binary or
/// multi-ary.
enum TermPrimOp {
    Unary(term::UnaryOp),
    Binary(term::BinaryOp),
    NAry(term::NAryOp),
}

impl FromAst<PrimOp> for TermPrimOp {
    fn from_ast(op: &PrimOp) -> Self {
        match op {
            PrimOp::Typeof => TermPrimOp::Unary(term::UnaryOp::Typeof),
            PrimOp::BoolAnd => TermPrimOp::Unary(term::UnaryOp::BoolAnd),
            PrimOp::BoolOr => TermPrimOp::Unary(term::UnaryOp::BoolOr),
            PrimOp::BoolNot => TermPrimOp::Unary(term::UnaryOp::BoolNot),
            PrimOp::Blame => TermPrimOp::Unary(term::UnaryOp::Blame),
            PrimOp::EnumEmbed(loc_ident) => TermPrimOp::Unary(term::UnaryOp::EnumEmbed(*loc_ident)),
            PrimOp::RecordStatAccess(loc_ident) => {
                TermPrimOp::Unary(term::UnaryOp::RecordAccess(*loc_ident))
            }
            PrimOp::ArrayMap => TermPrimOp::Unary(term::UnaryOp::ArrayMap),
            PrimOp::RecordMap => TermPrimOp::Unary(term::UnaryOp::RecordMap),
            PrimOp::LabelFlipPol => TermPrimOp::Unary(term::UnaryOp::LabelFlipPol),
            PrimOp::LabelPol => TermPrimOp::Unary(term::UnaryOp::LabelPol),
            PrimOp::LabelGoDom => TermPrimOp::Unary(term::UnaryOp::LabelGoDom),
            PrimOp::LabelGoCodom => TermPrimOp::Unary(term::UnaryOp::LabelGoCodom),
            PrimOp::LabelGoArray => TermPrimOp::Unary(term::UnaryOp::LabelGoArray),
            PrimOp::LabelGoDict => TermPrimOp::Unary(term::UnaryOp::LabelGoDict),
            PrimOp::Seq => TermPrimOp::Unary(term::UnaryOp::Seq),
            PrimOp::DeepSeq => TermPrimOp::Unary(term::UnaryOp::DeepSeq),
            PrimOp::ArrayLength => TermPrimOp::Unary(term::UnaryOp::ArrayLength),
            PrimOp::ArrayGen => TermPrimOp::Unary(term::UnaryOp::ArrayGen),
            PrimOp::RecordFields(record_op_kind) => {
                TermPrimOp::Unary(term::UnaryOp::RecordFields(*record_op_kind))
            }
            PrimOp::RecordValues => TermPrimOp::Unary(term::UnaryOp::RecordValues),
            PrimOp::StringTrim => TermPrimOp::Unary(term::UnaryOp::StringTrim),
            PrimOp::StringChars => TermPrimOp::Unary(term::UnaryOp::StringChars),
            PrimOp::StringUppercase => TermPrimOp::Unary(term::UnaryOp::StringUppercase),
            PrimOp::StringLowercase => TermPrimOp::Unary(term::UnaryOp::StringLowercase),
            PrimOp::StringLength => TermPrimOp::Unary(term::UnaryOp::StringLength),
            PrimOp::ToString => TermPrimOp::Unary(term::UnaryOp::ToString),
            PrimOp::NumberFromString => TermPrimOp::Unary(term::UnaryOp::NumberFromString),
            PrimOp::EnumFromString => TermPrimOp::Unary(term::UnaryOp::EnumFromString),
            PrimOp::StringIsMatch => TermPrimOp::Unary(term::UnaryOp::StringIsMatch),
            PrimOp::StringFind => TermPrimOp::Unary(term::UnaryOp::StringFind),
            PrimOp::StringFindAll => TermPrimOp::Unary(term::UnaryOp::StringFindAll),
            PrimOp::Force {
                ignore_not_exported,
            } => TermPrimOp::Unary(term::UnaryOp::Force {
                ignore_not_exported: *ignore_not_exported,
            }),
            PrimOp::RecordEmptyWithTail => TermPrimOp::Unary(term::UnaryOp::RecordEmptyWithTail),
            PrimOp::RecordFreeze => TermPrimOp::Unary(term::UnaryOp::RecordFreeze),
            PrimOp::Trace => TermPrimOp::Unary(term::UnaryOp::Trace),
            PrimOp::LabelPushDiag => TermPrimOp::Unary(term::UnaryOp::LabelPushDiag),
            PrimOp::EnumGetArg => TermPrimOp::Unary(term::UnaryOp::EnumGetArg),
            PrimOp::EnumMakeVariant => TermPrimOp::Unary(term::UnaryOp::EnumMakeVariant),
            PrimOp::EnumIsVariant => TermPrimOp::Unary(term::UnaryOp::EnumIsVariant),
            PrimOp::EnumGetTag => TermPrimOp::Unary(term::UnaryOp::EnumGetTag),
            PrimOp::ContractCustom => TermPrimOp::Unary(term::UnaryOp::ContractCustom),
            PrimOp::NumberArcCos => TermPrimOp::Unary(term::UnaryOp::NumberArcCos),
            PrimOp::NumberArcSin => TermPrimOp::Unary(term::UnaryOp::NumberArcSin),
            PrimOp::NumberArcTan => TermPrimOp::Unary(term::UnaryOp::NumberArcTan),
            PrimOp::NumberCos => TermPrimOp::Unary(term::UnaryOp::NumberCos),
            PrimOp::NumberSin => TermPrimOp::Unary(term::UnaryOp::NumberSin),
            PrimOp::NumberTan => TermPrimOp::Unary(term::UnaryOp::NumberTan),
            #[cfg(feature = "nix-experimental")]
            PrimOp::EvalNix => TermPrimOp::Unary(term::UnaryOp::EvalNix),

            // Binary operations
            PrimOp::Plus => TermPrimOp::Binary(term::BinaryOp::Plus),
            PrimOp::Sub => TermPrimOp::Binary(term::BinaryOp::Sub),
            PrimOp::Mult => TermPrimOp::Binary(term::BinaryOp::Mult),
            PrimOp::Div => TermPrimOp::Binary(term::BinaryOp::Div),
            PrimOp::Modulo => TermPrimOp::Binary(term::BinaryOp::Modulo),
            PrimOp::NumberArcTan2 => TermPrimOp::Binary(term::BinaryOp::NumberArcTan2),
            PrimOp::NumberLog => TermPrimOp::Binary(term::BinaryOp::NumberLog),
            PrimOp::Pow => TermPrimOp::Binary(term::BinaryOp::Pow),
            PrimOp::StringConcat => TermPrimOp::Binary(term::BinaryOp::StringConcat),
            PrimOp::Eq => TermPrimOp::Binary(term::BinaryOp::Eq),
            PrimOp::LessThan => TermPrimOp::Binary(term::BinaryOp::LessThan),
            PrimOp::LessOrEq => TermPrimOp::Binary(term::BinaryOp::LessOrEq),
            PrimOp::GreaterThan => TermPrimOp::Binary(term::BinaryOp::GreaterThan),
            PrimOp::GreaterOrEq => TermPrimOp::Binary(term::BinaryOp::GreaterOrEq),
            PrimOp::ContractApply => TermPrimOp::Binary(term::BinaryOp::ContractApply),
            PrimOp::ContractCheck => TermPrimOp::Binary(term::BinaryOp::ContractCheck),
            PrimOp::LabelWithErrorData => TermPrimOp::Binary(term::BinaryOp::LabelWithErrorData),
            PrimOp::LabelGoField => TermPrimOp::Binary(term::BinaryOp::LabelGoField),
            PrimOp::RecordInsert(record_op_kind) => {
                TermPrimOp::Binary(term::BinaryOp::RecordInsert {
                    metadata: Default::default(),
                    pending_contracts: Vec::new(),
                    ext_kind: term::RecordExtKind::WithValue,
                    op_kind: *record_op_kind,
                })
            }
            PrimOp::RecordRemove(record_op_kind) => {
                TermPrimOp::Binary(term::BinaryOp::RecordRemove(*record_op_kind))
            }
            PrimOp::RecordGet => TermPrimOp::Binary(term::BinaryOp::RecordGet),
            PrimOp::RecordHasField(record_op_kind) => {
                TermPrimOp::Binary(term::BinaryOp::RecordHasField(*record_op_kind))
            }
            PrimOp::RecordFieldIsDefined(record_op_kind) => {
                TermPrimOp::Binary(term::BinaryOp::RecordFieldIsDefined(*record_op_kind))
            }
            PrimOp::RecordSplitPair => TermPrimOp::Binary(term::BinaryOp::RecordSplitPair),
            PrimOp::RecordDisjointMerge => TermPrimOp::Binary(term::BinaryOp::RecordDisjointMerge),
            PrimOp::ArrayConcat => TermPrimOp::Binary(term::BinaryOp::ArrayConcat),
            PrimOp::ArrayAt => TermPrimOp::Binary(term::BinaryOp::ArrayAt),
            PrimOp::Merge(merge_kind) => {
                // [^merge-label-span] The mainline AST requires a `MergeLabel` object, itself
                // demanding a `RawSpan` that we can't provide here - it's stored higher up in the
                // AST, at the `PrimOpApp` node. We generate a dummy span and rely on the caller
                // (in practice `FromAst<Node<'_>>`) to post-process a merge primop application,
                // setting the span of the dummy merge label correctly.
                let dummy_label: label::MergeLabel = label::Label::dummy().into();

                TermPrimOp::Binary(term::BinaryOp::Merge(label::MergeLabel {
                    kind: *merge_kind,
                    ..dummy_label
                }))
            }
            PrimOp::Hash => TermPrimOp::Binary(term::BinaryOp::Hash),
            PrimOp::Serialize => TermPrimOp::Binary(term::BinaryOp::Serialize),
            PrimOp::Deserialize => TermPrimOp::Binary(term::BinaryOp::Deserialize),
            PrimOp::StringSplit => TermPrimOp::Binary(term::BinaryOp::StringSplit),
            PrimOp::StringContains => TermPrimOp::Binary(term::BinaryOp::StringContains),
            PrimOp::StringCompare => TermPrimOp::Binary(term::BinaryOp::StringCompare),
            PrimOp::Seal => TermPrimOp::Binary(term::BinaryOp::Seal),
            PrimOp::Unseal => TermPrimOp::Binary(term::BinaryOp::Unseal),
            PrimOp::ContractArrayLazyApp => {
                TermPrimOp::Binary(term::BinaryOp::ContractArrayLazyApp)
            }
            PrimOp::ContractRecordLazyApp => {
                TermPrimOp::Binary(term::BinaryOp::ContractRecordLazyApp)
            }
            PrimOp::LabelWithMessage => TermPrimOp::Binary(term::BinaryOp::LabelWithMessage),
            PrimOp::LabelWithNotes => TermPrimOp::Binary(term::BinaryOp::LabelWithNotes),
            PrimOp::LabelAppendNote => TermPrimOp::Binary(term::BinaryOp::LabelAppendNote),
            PrimOp::LabelLookupTypeVar => TermPrimOp::Binary(term::BinaryOp::LabelLookupTypeVar),

            // N-ary operations
            PrimOp::StringReplace => TermPrimOp::NAry(term::NAryOp::StringReplace),
            PrimOp::StringReplaceRegex => TermPrimOp::NAry(term::NAryOp::StringReplaceRegex),
            PrimOp::StringSubstr => TermPrimOp::NAry(term::NAryOp::StringSubstr),
            PrimOp::MergeContract => TermPrimOp::NAry(term::NAryOp::MergeContract),
            PrimOp::RecordSealTail => TermPrimOp::NAry(term::NAryOp::RecordSealTail),
            PrimOp::RecordUnsealTail => TermPrimOp::NAry(term::NAryOp::RecordUnsealTail),
            PrimOp::LabelInsertTypeVar => TermPrimOp::NAry(term::NAryOp::LabelInsertTypeVar),
            PrimOp::ArraySlice => TermPrimOp::NAry(term::NAryOp::ArraySlice),
        }
    }
}

impl<'ast> FromAst<Node<'ast>> for term::Term {
    fn from_ast(node: &Node<'ast>) -> Self {
        use term::Term;

        match node {
            Node::Null => Term::Null,
            Node::Bool(b) => Term::Bool(*b),
            Node::Number(n) => Term::Num((**n).clone()),
            Node::String(s) => Term::Str((*s).into()),
            Node::StringChunks(chunks) => {
                let chunks = chunks
                    .iter()
                    .rev()
                    .map(|chunk| match chunk {
                        StringChunk::Literal(s) => term::StrChunk::Literal(s.clone()),
                        StringChunk::Expr(expr, indent) => {
                            term::StrChunk::Expr(expr.to_mainline(), *indent)
                        }
                    })
                    .collect();

                Term::StrChunks(chunks)
            }
            Node::Fun { args, body } => {
                let body_pos = body.pos;

                // We transform a n-ary function representation to a chain of nested unary
                // functions, the latter being the representation used in the mainline AST.
                args.iter()
                    .rev()
                    .fold(term::RichTerm::from_ast(body), |acc, arg| {
                        let term = match arg.data {
                            PatternData::Any(id) => Term::Fun(id, acc),
                            _ => term::Term::FunPattern((*arg).to_mainline(), acc),
                        };

                        // [^nary-constructors-unrolling]: this case is a bit annoying: we need to
                        // extract the position of the intermediate created functions to satisfy
                        // the old AST structure, but this information isn't available directly.
                        //
                        // What we do here is to fuse the span of the term being built and the one
                        // of the current argument, which should be a reasonable approximation (if
                        // not exactly the same thing).
                        term::RichTerm::new(term, arg.pos.fuse(body_pos))
                    })
                    .term
                    .into_owned()
            }
            Node::Let {
                bindings,
                body,
                rec,
            } => {
                // Mainline term bindings can't have any metadata associated with them. We need to
                // rewrite let metadata to be free-standing type and contract annotations instead,
                // which is achieved by this helper.
                fn with_metadata(metadata: &LetMetadata<'_>, value: &Ast<'_>) -> term::RichTerm {
                    let value: term::RichTerm = value.to_mainline();
                    let pos = value.pos;

                    if metadata.annotation.is_empty() {
                        return value;
                    }

                    term::RichTerm::new(
                        term::Term::Annotated(metadata.annotation.to_mainline(), value),
                        pos,
                    )
                }

                // We try to collect all patterns as single identifiers. If this works, we can emit
                // a simpler / more compact `Let`.
                let try_bindings = bindings
                    .iter()
                    .map(
                        |LetBinding {
                             pattern,
                             metadata,
                             value,
                         }| match pattern.data {
                            PatternData::Any(id) => Some((id, with_metadata(metadata, value))),
                            _ => None,
                        },
                    )
                    .collect::<Option<SmallVec<_>>>();

                let body = body.to_mainline();
                let attrs = term::LetAttrs {
                    rec: *rec,
                    ..Default::default()
                };

                if let Some(bindings) = try_bindings {
                    Term::Let(bindings, body, attrs)
                } else {
                    let bindings = bindings
                        .iter()
                        .map(
                            |LetBinding {
                                 pattern,
                                 value,
                                 metadata,
                             }| {
                                (pattern.to_mainline(), with_metadata(metadata, value))
                            },
                        )
                        .collect();

                    Term::LetPattern(bindings, body, attrs)
                }
            }
            Node::App { head, args } => {
                let head_pos = head.pos;

                // We transform a n-ary application representation to a chain of nested unary
                // applications, the latter being the representation used in the mainline AST.
                args.iter()
                    .fold(head.to_mainline(), |result, arg| {
                        // see [^nary-constructors-unrolling]
                        let arg_pos = arg.pos;
                        term::RichTerm::new(
                            Term::App(result, arg.to_mainline()),
                            head_pos.fuse(arg_pos),
                        )
                    })
                    .term
                    .into_owned()
            }
            Node::Var(loc_ident) => Term::Var(*loc_ident),
            Node::EnumVariant { tag, arg } => {
                if let Some(arg) = arg {
                    Term::EnumVariant {
                        tag: *tag,
                        arg: arg.to_mainline(),
                        attrs: term::EnumVariantAttrs::default(),
                    }
                } else {
                    Term::Enum(*tag)
                }
            }
            Node::Record(record) => {
                let (data, dyn_fields) = (*record).to_mainline();
                Term::RecRecord(data, dyn_fields, None)
            }
            Node::IfThenElse {
                cond,
                then_branch,
                else_branch,
            } => term::make::if_then_else(
                term::RichTerm::from_ast(cond),
                term::RichTerm::from_ast(then_branch),
                term::RichTerm::from_ast(else_branch),
            )
            .term
            .into_owned(),
            Node::Match(data) => {
                let branches = data.branches.iter().map(ToMainline::to_mainline).collect();

                Term::Match(term::MatchData { branches })
            }
            Node::Array(array) => {
                let array = array.iter().map(ToMainline::to_mainline).collect();
                Term::Array(array, term::array::ArrayAttrs::default())
            }
            Node::PrimOpApp { op, args } => match (*op).to_mainline() {
                TermPrimOp::Unary(op) => Term::Op1(op, args[0].to_mainline()),
                // If `op` is `Merge`, we need to patch the span of the merge label with the
                // correct value. Unfortunately, we still don't have access to the right span,
                // which is the position of this whole node. We delegate this to the caller, that
                // is `from_ast::<Ast<'ast>>`. See [^merge-label-span].
                TermPrimOp::Binary(op) => {
                    Term::Op2(op, args[0].to_mainline(), args[1].to_mainline())
                }
                TermPrimOp::NAry(op) => {
                    Term::OpN(op, args.iter().map(|arg| (*arg).to_mainline()).collect())
                }
            },
            Node::Annotated { annot, inner } => {
                Term::Annotated((*annot).to_mainline(), inner.to_mainline())
            }
            Node::Import(Import::Path { path, format }) => Term::Import(term::Import::Path {
                path: (*path).to_owned(),
                format: *format,
            }),
            Node::Import(Import::Package { id }) => Term::Import(term::Import::Package { id: *id }),
            Node::Type(typ) => {
                let typ: mline_type::Type = (*typ).to_mainline();

                let contract = typ
                    .contract()
                    // It would be painful to change the interface of `ToMainline` and make it
                    // fallible just for this one special case. Instead, if the contract
                    // conversion causes an unbound variable error (which it shouldn't anyway if
                    // the term has been correctly typechecked), we pack this error as parse
                    // error in the AST.
                    .unwrap_or_else(|err| {
                        Term::ParseError(ParseError::UnboundTypeVariables(vec![err.0])).into()
                    });

                Term::Type { typ, contract }
            }
            Node::ParseError(error) => Term::ParseError((*error).clone()),
        }
    }
}

impl<'ast> FromAst<Ast<'ast>> for term::RichTerm {
    fn from_ast(ast: &Ast<'ast>) -> Self {
        let mut result = term::RichTerm::new(ast.node.to_mainline(), ast.pos);
        // See [^merge-label-span]
        if let term::Term::Op2(term::BinaryOp::Merge(ref mut label), _, _) =
            term::SharedTerm::make_mut(&mut result.term)
        {
            // unwrap(): we expect all position to be set in the new AST (should be using span
            // directly in the future)
            label.span = ast.pos.unwrap();
        }

        result
    }
}

impl<'ast> FromAst<&'ast Ast<'ast>> for term::RichTerm {
    fn from_ast(ast: &&'ast Ast<'ast>) -> Self {
        FromAst::from_ast(*ast)
    }
}

/// Convert a record definition to a mainline recursive record definition (with static and dynamic
/// fields). If a field is defined several times, the different definitions are merged, statically
/// if possible.
impl<'ast> FromAst<Record<'ast>>
    for (
        term::record::RecordData,
        Vec<(term::RichTerm, term::record::Field)>,
    )
{
    fn from_ast(record: &Record<'ast>) -> Self {
        use indexmap::map::Entry;

        fn insert_static_field(
            static_fields: &mut IndexMap<LocIdent, term::record::Field>,
            id: LocIdent,
            field: term::record::Field,
        ) {
            match static_fields.entry(id) {
                Entry::Occupied(mut occpd) => {
                    // temporarily putting an empty field in the entry to take the previous value.
                    let prev = occpd.insert(term::record::Field::default());

                    // unwrap(): the field's identifier must have a position during parsing.
                    occpd.insert(merge_fields(id.pos.unwrap(), prev, field));
                }
                Entry::Vacant(vac) => {
                    vac.insert(field);
                }
            }
        }

        let mut static_fields = IndexMap::new();
        let mut dynamic_fields = Vec::new();

        for def in record.field_defs.iter().map(ToMainline::to_mainline) {
            match def {
                (FieldName::Ident(id), field) => insert_static_field(&mut static_fields, id, field),
                (FieldName::Expr(expr), field) => {
                    let pos = expr.pos;
                    // Dynamic fields (whose name is defined by an interpolated string) have a different
                    // semantics than fields whose name can be determined statically. However, static
                    // fields with special characters are also parsed as string chunks:
                    //
                    // ```
                    // let x = "dynamic" in {"I%am.static" = false, "%{x}" = true}
                    // ```
                    //
                    // Here, both fields are parsed as `StrChunks`, but the first field is actually a
                    // static one, just with special characters. The following code determines which fields
                    // are actually static or not, and inserts them in the right location.
                    let static_access = expr.term.as_ref().try_str_chunk_as_static_str();

                    if let Some(static_access) = static_access {
                        insert_static_field(
                            &mut static_fields,
                            LocIdent::new_with_pos(static_access, pos),
                            field,
                        )
                    } else {
                        dynamic_fields.push((expr, field));
                    }
                }
            }
        }

        (
            term::record::RecordData::new(
                static_fields,
                term::record::RecordAttrs {
                    open: record.open,
                    ..Default::default()
                },
                None,
            ),
            dynamic_fields,
        )
    }
}

/// Merge two fields by performing the merge of both their value (dynamically if
/// necessary, by introducing a merge operator) and their metadata (statically).
///
/// If the values of both fields are static records ([`term::Term::Record`]s), their merge is
/// computed statically. This prevents building terms whose depth is linear in the number of fields
/// if partial definitions are involved. This manifested in
/// https://github.com/tweag/nickel/issues/1427.
///
/// This is a helper for the conversion of a record definition to mainline.
fn merge_fields(
    id_span: RawSpan,
    field1: term::record::Field,
    field2: term::record::Field,
) -> term::record::Field {
    use crate::eval::merge::{merge_doc, split};
    use term::{make as mk_term, record::RecordData, BinaryOp, RichTerm, Term};

    // FIXME: We're duplicating a lot of the logic in
    // [`eval::merge::merge_fields`] but not quite enough to actually factor
    // it out
    fn merge_values(id_span: RawSpan, t1: term::RichTerm, t2: term::RichTerm) -> term::RichTerm {
        let RichTerm {
            term: t1,
            pos: pos1,
        } = t1;
        let RichTerm {
            term: t2,
            pos: pos2,
        } = t2;
        match (t1.into_owned(), t2.into_owned()) {
            (Term::Record(rd1), Term::Record(rd2)) => {
                let split::SplitResult {
                    left,
                    center,
                    right,
                } = split::split(rd1.fields, rd2.fields);
                let mut fields = IndexMap::with_capacity(left.len() + center.len() + right.len());
                fields.extend(left);
                fields.extend(right);
                for (id, (field1, field2)) in center.into_iter() {
                    fields.insert(id, merge_fields(id_span, field1, field2));
                }
                Term::Record(RecordData::new(
                    fields,
                    Combine::combine(rd1.attrs, rd2.attrs),
                    None,
                ))
                .into()
            }
            (t1, t2) => mk_term::op2(
                BinaryOp::Merge(label::MergeLabel {
                    span: id_span,
                    kind: label::MergeKind::PiecewiseDef,
                }),
                RichTerm::new(t1, pos1),
                RichTerm::new(t2, pos2),
            ),
        }
    }

    let (value, priority) = match (field1.value, field2.value) {
        (Some(t1), Some(t2)) if field1.metadata.priority == field2.metadata.priority => (
            Some(merge_values(id_span, t1, t2)),
            field1.metadata.priority,
        ),
        (Some(t), _) if field1.metadata.priority > field2.metadata.priority => {
            (Some(t), field1.metadata.priority)
        }
        (_, Some(t)) if field1.metadata.priority < field2.metadata.priority => {
            (Some(t), field2.metadata.priority)
        }
        (Some(t), None) => (Some(t), field1.metadata.priority),
        (None, Some(t)) => (Some(t), field2.metadata.priority),
        (None, None) => (None, Default::default()),
        _ => unreachable!(),
    };

    // At this stage, pending contracts aren't filled nor meaningful, and should all be empty.
    debug_assert!(field1.pending_contracts.is_empty() && field2.pending_contracts.is_empty());

    term::record::Field {
        value,
        // [`FieldMetadata::combine`] produces subtly different behaviour from the runtime merging
        // code: we don't use [`Combine::combine`] here and replicate the merging logic instead.
        metadata: term::record::FieldMetadata {
            doc: merge_doc(field1.metadata.doc, field2.metadata.doc),
            annotation: Combine::combine(field1.metadata.annotation, field2.metadata.annotation),
            opt: field1.metadata.opt && field2.metadata.opt,
            not_exported: field1.metadata.not_exported || field2.metadata.not_exported,
            priority,
        },
        pending_contracts: Vec::new(),
    }
}
