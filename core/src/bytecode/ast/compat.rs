//! Compatibility with the current stable Nickel version.
//!
//! This module defines a trait for converting from the representation used in stable Nickel to the
//! new AST representation of the bytecode compiler, and implements it for the types defined in
//! [crate::bytecode::ast].

use super::{primop::PrimOp, *};
use crate::{term, typ as mainline_type};

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
        alloc.move_pattern(pattern.to_ast(alloc))
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
        PatternData::Enum(alloc.enum_pattern(enum_pat.tag, pattern, enum_pat.pos))
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

        PatternData::Constant(alloc.constant_pattern(data, pattern.pos))
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

        let contracts = alloc.types(
            annot
                .contracts
                .iter()
                .map(|contract| contract.typ.to_ast(alloc)),
        );

        Annotation { typ, contracts }
    }
}

impl<'ast> FromMainline<'ast, term::record::Field> for record::Field<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, field: &term::record::Field) -> Self {
        record::Field {
            value: field.value.as_ref().map(|term| term.to_ast(alloc)),
            metadata: field.metadata.to_ast(alloc),
        }
    }
}

impl<'ast> FromMainline<'ast, term::record::FieldMetadata> for record::FieldMetadata<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, metadata: &term::record::FieldMetadata) -> Self {
        let doc = metadata.doc.as_ref().map(|doc| rc::Rc::from(doc.as_str()));

        record::FieldMetadata {
            doc,
            annotation: metadata.annotation.to_ast(alloc),
            opt: metadata.opt,
            not_exported: metadata.not_exported,
            priority: metadata.priority.clone(),
        }
    }
}

impl<'ast> FromMainline<'ast, mainline_type::Type> for Type<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, mainline: &mainline_type::Type) -> Self {
        Type {
            typ: mainline.typ.to_ast(alloc),
            pos: mainline.pos,
        }
    }
}

type MainlineTypeUnr = mainline_type::TypeF<
    Box<mainline_type::Type>,
    mainline_type::RecordRows,
    mainline_type::EnumRows,
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

impl<'ast> FromMainline<'ast, mainline_type::RecordRows> for RecordRows<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, rrows: &mainline_type::RecordRows) -> Self {
        RecordRows(rrows.0.to_ast(alloc))
    }
}

impl<'ast> FromMainline<'ast, mainline_type::EnumRows> for EnumRows<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, erows: &mainline_type::EnumRows) -> Self {
        EnumRows(erows.0.to_ast(alloc))
    }
}

type MainlineEnumRowsUnr =
    mainline_type::EnumRowsF<Box<mainline_type::Type>, Box<mainline_type::EnumRows>>;

impl<'ast> FromMainline<'ast, MainlineEnumRowsUnr> for EnumRowsUnr<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, erows: &MainlineEnumRowsUnr) -> Self {
        erows.clone().map(
            |typ| &*alloc.generic_arena.alloc((*typ).to_ast(alloc)),
            |erows| &*alloc.generic_arena.alloc((*erows).to_ast(alloc)),
        )
    }
}

type MainlineRecordRowsUnr =
    mainline_type::RecordRowsF<Box<mainline_type::Type>, Box<mainline_type::RecordRows>>;

impl<'ast> FromMainline<'ast, MainlineRecordRowsUnr> for RecordRowsUnr<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, rrows: &MainlineRecordRowsUnr) -> Self {
        rrows.clone().map(
            |typ| &*alloc.generic_arena.alloc((*typ).to_ast(alloc)),
            |rrows| &*alloc.generic_arena.alloc((*rrows).to_ast(alloc)),
        )
    }
}

impl<'ast> FromMainline<'ast, term::Term> for &'ast Node<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, term: &term::Term) -> Self {
        use term::Term;

        match term {
            Term::Null => alloc.null(),
            Term::Bool(b) => alloc.bool(*b),
            Term::Num(n) => alloc.number(n.clone()),
            Term::Str(s) => alloc.string(s),
            Term::StrChunks(chunks) => alloc.str_chunks(
                chunks
                    .iter()
                    .map(|chunk| match chunk {
                        term::StrChunk::Literal(s) => StrChunk::Literal(s.clone()),
                        term::StrChunk::Expr(expr, indent) => {
                            StrChunk::Expr(expr.to_ast(alloc), *indent)
                        }
                    })
                    .rev(),
            ),
            Term::Fun(id, body) => alloc.fun(
                alloc.generic_arena.alloc(Pattern::any(*id)),
                body.to_ast(alloc),
            ),
            Term::FunPattern(pat, body) => alloc.fun(pat.to_ast(alloc), body.to_ast(alloc)),
            Term::Let(bindings, body, attrs) => alloc.let_binding(
                bindings
                    .iter()
                    .map(|(id, term)| (Pattern::any(*id), term.to_ast(alloc))),
                body.to_ast(alloc),
                attrs.rec,
            ),
            Term::LetPattern(bindings, body, attrs) => alloc.let_binding(
                bindings
                    .iter()
                    .map(|(pat, term)| (pat.to_ast(alloc), term.to_ast(alloc))),
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

                alloc.app(fun.to_ast(alloc), args.into_iter().rev())
            }
            Term::Var(id) => alloc.var(*id),
            Term::Enum(id) => alloc.enum_variant(*id, None),
            Term::EnumVariant { tag, arg, attrs: _ } => {
                alloc.enum_variant(*tag, Some(arg.to_ast(alloc)))
            }
            Term::RecRecord(data, dyn_fields, _deps) => {
                let stat_fields = alloc.generic_arena.alloc_slice_fill_iter(
                    data.fields
                        .iter()
                        .map(|(id, field)| (*id, field.to_ast(alloc))),
                );

                let dyn_fields = alloc.generic_arena.alloc_slice_fill_iter(
                    dyn_fields
                        .iter()
                        .map(|(expr, field)| (expr.to_ast(alloc), field.to_ast(alloc))),
                );

                let open = data.attrs.open;

                alloc.record(Record {
                    stat_fields,
                    dyn_fields,
                    open,
                })
            }
            Term::Record(data) => {
                let stat_fields = alloc.generic_arena.alloc_slice_fill_iter(
                    data.fields
                        .iter()
                        .map(|(id, field)| (*id, field.to_ast(alloc))),
                );

                let open = data.attrs.open;

                alloc.record(Record {
                    stat_fields,
                    dyn_fields: alloc
                        .generic_arena
                        .alloc_slice_fill_iter(std::iter::empty()),
                    open,
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
            Term::Import { path, format } => alloc.import(path.clone(), *format),
            Term::ResolvedImport(_) => panic!("didn't expect a resolved import at parsing stage"),
            Term::Type { typ, .. } => alloc.typ_move(typ.to_ast(alloc)),
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
        alloc.ast(rterm.as_ref().to_ast(alloc), rterm.pos)
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
            term::UnaryOp::RecordFields(record_op_kind) => {
                PrimOp::RecordFields((*record_op_kind).into())
            }
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
            | term::UnaryOp::ContractPostprocessResult) => {
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
                PrimOp::RecordInsert((*op_kind).into())
            }
            term::BinaryOp::RecordRemove(record_op_kind) => {
                PrimOp::RecordRemove((*record_op_kind).into())
            }
            term::BinaryOp::RecordGet => PrimOp::RecordGet,
            term::BinaryOp::RecordHasField(record_op_kind) => {
                PrimOp::RecordHasField((*record_op_kind).into())
            }
            term::BinaryOp::RecordFieldIsDefined(record_op_kind) => {
                PrimOp::RecordFieldIsDefined((*record_op_kind).into())
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

            op @ (term::BinaryOp::RecordInsert { .. }
            | term::BinaryOp::Unseal
            | term::BinaryOp::Seal) => panic!("didn't expect {op} at the parsing stage"),
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
