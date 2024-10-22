//! Compatibility with the current stable Nickel version.
//!
//! This module defines a trait for converting from the representation used in stable Nickel to the
//! new AST representation of the bytecode compiler, and implements it for the types defined in
//! [crate::bytecode::ast].

use super::*;
use crate::term;

trait FromMainline<'ast, 'a, T> {
    /// Convert from the mainline Nickel representation to the new AST representation. This trait
    /// is mostly `From` with an additional parameter for the allocator.
    fn from_mainline(alloc: &'ast AstAlloc, mainline: &'a T) -> Self;
}

impl<'ast, 'a> FromMainline<'ast, 'a, term::pattern::Pattern> for &'ast Pattern<'ast> {
    fn from_mainline(
        alloc: &'ast AstAlloc,
        pattern: &'a term::pattern::Pattern,
    ) -> &'ast Pattern<'ast> {
        alloc.move_pattern(pattern.into_ast(alloc))
    }
}

impl<'ast, 'a> FromMainline<'ast, 'a, term::pattern::Pattern> for Pattern<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, pattern: &'a term::pattern::Pattern) -> Self {
        Pattern {
            data: pattern.data.into_ast(alloc),
            alias: pattern.alias,
            pos: pattern.pos,
        }
    }
}

impl<'ast, 'a> FromMainline<'ast, 'a, term::pattern::PatternData> for PatternData<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, data: &'a term::pattern::PatternData) -> Self {
        match data {
            term::pattern::PatternData::Wildcard => PatternData::Wildcard,
            term::pattern::PatternData::Any(id) => PatternData::Any(*id),
            term::pattern::PatternData::Record(record_pattern) => record_pattern.into_ast(alloc),
            term::pattern::PatternData::Array(array_pattern) => array_pattern.into_ast(alloc),
            term::pattern::PatternData::Enum(enum_pattern) => enum_pattern.into_ast(alloc),
            term::pattern::PatternData::Constant(constant_pattern) => {
                constant_pattern.into_ast(alloc)
            }
            term::pattern::PatternData::Or(or_pattern) => or_pattern.into_ast(alloc),
        }
    }
}

impl<'ast, 'a> FromMainline<'ast, 'a, term::pattern::RecordPattern> for PatternData<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, record_pat: &'a term::pattern::RecordPattern) -> Self {
        let patterns = record_pat
            .patterns
            .iter()
            .map(|field_pattern| field_pattern.into_ast(alloc));

        let tail = match record_pat.tail {
            term::pattern::TailPattern::Empty => TailPattern::Empty,
            term::pattern::TailPattern::Open => TailPattern::Open,
            term::pattern::TailPattern::Capture(id) => TailPattern::Capture(id),
        };

        PatternData::Record(alloc.record_pattern(patterns, tail, record_pat.pos))
    }
}

impl<'ast, 'a> FromMainline<'ast, 'a, term::pattern::FieldPattern> for FieldPattern<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, field_pat: &'a term::pattern::FieldPattern) -> Self {
        let pattern = field_pat.pattern.into_ast(alloc);

        let default = field_pat.default.as_ref().map(|term| term.into_ast(alloc));

        let annotation = field_pat.annotation.into_ast(alloc);

        FieldPattern {
            matched_id: field_pat.matched_id,
            annotation,
            default,
            pattern,
            pos: field_pat.pos,
        }
    }
}

impl<'ast, 'a> FromMainline<'ast, 'a, term::pattern::ArrayPattern> for PatternData<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, array_pat: &'a term::pattern::ArrayPattern) -> Self {
        let patterns = array_pat.patterns.iter().map(|pat| pat.into_ast(alloc));

        let tail = match array_pat.tail {
            term::pattern::TailPattern::Empty => TailPattern::Empty,
            term::pattern::TailPattern::Open => TailPattern::Open,
            term::pattern::TailPattern::Capture(id) => TailPattern::Capture(id),
        };

        PatternData::Array(alloc.array_pattern(patterns, tail, array_pat.pos))
    }
}

impl<'ast, 'a> FromMainline<'ast, 'a, term::pattern::EnumPattern> for PatternData<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, enum_pat: &'a term::pattern::EnumPattern) -> Self {
        let pattern = enum_pat
            .pattern
            .as_ref()
            .map(|pat| (&**pat).into_ast(alloc));
        PatternData::Enum(alloc.enum_pattern(enum_pat.tag, pattern, enum_pat.pos))
    }
}

impl<'ast, 'a> FromMainline<'ast, 'a, term::pattern::ConstantPattern> for PatternData<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, pattern: &'a term::pattern::ConstantPattern) -> Self {
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

impl<'ast, 'a> FromMainline<'ast, 'a, term::pattern::OrPattern> for PatternData<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, pattern: &'a term::pattern::OrPattern) -> Self {
        let patterns = pattern
            .patterns
            .iter()
            .map(|pat| pat.into_ast(alloc))
            .collect::<Vec<_>>();

        PatternData::Or(alloc.or_pattern(patterns, pattern.pos))
    }
}

impl<'ast, 'a> FromMainline<'ast, 'a, term::TypeAnnotation> for Annotation<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, annot: &'a term::TypeAnnotation) -> Self {
        let typ = annot.typ.as_ref().map(|typ| typ.typ.clone());

        let contracts = alloc
            .type_arena
            .alloc_extend(annot.contracts.iter().map(|contract| contract.typ.clone()));

        Annotation { typ, contracts }
    }
}

impl<'ast, 'a> FromMainline<'ast, 'a, term::record::Field> for record::Field<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, field: &'a term::record::Field) -> Self {
        record::Field {
            value: field.value.as_ref().map(|term| term.into_ast(alloc)),
            metadata: field.metadata.into_ast(alloc),
        }
    }
}

impl<'ast, 'a> FromMainline<'ast, 'a, term::record::FieldMetadata> for record::FieldMetadata<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, metadata: &'a term::record::FieldMetadata) -> Self {
        let doc = metadata.doc.as_ref().map(|doc| rc::Rc::from(doc.as_str()));

        record::FieldMetadata {
            doc,
            annotation: metadata.annotation.into_ast(alloc),
            opt: metadata.opt,
            not_exported: metadata.not_exported,
            priority: metadata.priority.clone(),
        }
    }
}

impl<'ast, 'a> FromMainline<'ast, 'a, term::Term> for &'ast Node<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, term: &'a term::Term) -> Self {
        use term::Term;

        match term {
            Term::Null => alloc.null(),
            Term::Bool(b) => alloc.bool(*b),
            Term::Num(n) => alloc.number(n.clone()),
            Term::Str(s) => alloc.string(s),
            Term::StrChunks(chunks) => alloc.str_chunks_iter(
                chunks
                    .iter()
                    .map(|chunk| match chunk {
                        term::StrChunk::Literal(s) => StrChunk::Literal(s.clone()),
                        term::StrChunk::Expr(expr, indent) => {
                            StrChunk::Expr(expr.into_ast(alloc), *indent)
                        }
                    })
                    .rev(),
            ),
            Term::Fun(id, body) => alloc.fun(
                alloc.generic_arena.alloc(Pattern::any(*id)),
                body.into_ast(alloc),
            ),
            Term::FunPattern(pat, body) => alloc.fun(pat.into_ast(alloc), body.into_ast(alloc)),
            Term::Let(bindings, body, attrs) => alloc.let_binding(
                bindings
                    .iter()
                    .map(|(id, term)| (Pattern::any(*id), term.into_ast(alloc))),
                body.into_ast(alloc),
                attrs.rec,
            ),
            Term::LetPattern(bindings, body, attrs) => alloc.let_binding(
                bindings
                    .iter()
                    .map(|(pat, term)| (pat.into_ast(alloc), term.into_ast(alloc))),
                body.into_ast(alloc),
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
                                cond.into_ast(alloc),
                                arg_inner.into_ast(alloc),
                                arg.into_ast(alloc),
                            );
                        }
                    }
                    _ => (),
                };

                let mut args = vec![arg.into_ast(alloc)];
                let mut maybe_next_app = fun.as_ref();

                while let Term::App(next_fun, next_arg) = maybe_next_app {
                    args.push(next_arg.into_ast(alloc));
                    maybe_next_app = next_fun.as_ref();
                }

                alloc.app(fun.into_ast(alloc), args.into_iter().rev())
            }
            Term::Var(id) => alloc.var(*id),
            Term::Enum(id) => alloc.enum_variant(*id, None),
            Term::EnumVariant { tag, arg, attrs: _ } => {
                alloc.enum_variant(*tag, Some(arg.into_ast(alloc)))
            }
            Term::RecRecord(data, dyn_fields, _deps) => {
                let stat_fields = alloc.generic_arena.alloc_slice_fill_iter(
                    data.fields
                        .iter()
                        .map(|(id, field)| (*id, field.into_ast(alloc))),
                );

                let dyn_fields = alloc.generic_arena.alloc_slice_fill_iter(
                    dyn_fields
                        .iter()
                        .map(|(expr, field)| (expr.into_ast(alloc), field.into_ast(alloc))),
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
                        .map(|(id, field)| (*id, field.into_ast(alloc))),
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
                    pattern: branch.pattern.into_ast(alloc),
                    guard: branch.guard.as_ref().map(|term| term.into_ast(alloc)),
                    body: branch.body.into_ast(alloc),
                });

                alloc.match_expr(branches)
            }
            Term::Array(data, _attrs) => {
                // We should probably make array's iterator an ExactSizeIterator. But for now, we
                // don't care about the translation's performance so it's simpler to just collect
                // them in a vec locally.
                let elts = data
                    .iter()
                    .map(|term| term.into_ast(alloc))
                    .collect::<Vec<_>>();
                alloc.array(elts)
            }
            Term::Op1(op, arg) => {
                alloc.prim_op(PrimOp::from(op), std::iter::once(arg.into_ast(alloc)))
            }
            Term::Op2(op, arg1, arg2) => alloc.prim_op(
                PrimOp::from(op),
                [arg1, arg2].iter().map(|arg| (*arg).into_ast(alloc)),
            ),
            Term::OpN(op, args) => {
                alloc.prim_op(PrimOp::from(op), args.iter().map(|arg| arg.into_ast(alloc)))
            }
            Term::SealingKey(_) => panic!("didn't expect a sealing key at the first stage"),
            Term::Sealed(..) => panic!("didn't expect a sealed term at the first stage"),
            Term::Annotated(annot, term) => {
                alloc.annotated(annot.into_ast(alloc), term.into_ast(alloc))
            }
            Term::Import { path, format } => alloc.import(path.clone(), *format),
            Term::ResolvedImport(_) => panic!("didn't expect a resolved import at parsing stage"),
            Term::Type { typ, .. } => alloc.typ(typ.clone()),
            Term::CustomContract(_) => panic!("didn't expect a custom contract at parsing stage"),
            Term::ParseError(error) => alloc.parse_error(error.clone()),
            Term::RuntimeError(_) => panic!("didn't expect a runtime error at parsing stage"),
            Term::Closure(_) => panic!("didn't expect a closure at parsing stage"),
            Term::ForeignId(_) => panic!("didn't expect a foreign id at parsing stage"),
            _ => unimplemented!(),
        }
    }
}

impl<'ast, 'a> FromMainline<'ast, 'a, term::RichTerm> for Ast<'ast> {
    fn from_mainline(alloc: &'ast AstAlloc, rterm: &'a term::RichTerm) -> Self {
        alloc.ast(rterm.as_ref().into_ast(alloc), rterm.pos)
    }
}

/// Symmetric to `FromMainline`, as `Into` is to `From`.
trait IntoAst<'ast, 'a, T> {
    fn into_ast(&'a self, alloc: &'ast AstAlloc) -> T;
}

impl<'ast, 'a, S, T> IntoAst<'ast, 'a, T> for S
where
    T: FromMainline<'ast, 'a, S>,
{
    fn into_ast(&'a self, alloc: &'ast AstAlloc) -> T {
        T::from_mainline(alloc, self)
    }
}
