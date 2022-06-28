use crate::cache::Cache;
use crate::parser::utils::mk_span;
use crate::term::{BinaryOp, UnaryOp};
use crate::term::{RichTerm, Term};
use codespan::FileId;
use rnix;
use rnix::types::{BinOp, EntryHolder, TokenWrapper, TypedNode, UnaryOp as UniOp};
use std::collections::HashMap;

impl From<(UniOp, FileId)> for RichTerm {
    fn from(op: (UniOp, FileId)) -> Self {
        use rnix::types::UnaryOpKind::*;
        let (op, file_id) = op;
        let value = op.value().unwrap();
        match op.operator() {
            Negate => Term::Op2(
                BinaryOp::Sub(),
                Term::Num(0.).into(),
                translate(value, file_id),
            )
            .into(),
            Invert => Term::Op1(UnaryOp::BoolNot(), translate(value, file_id)).into(),
        }
    }
}

impl From<(BinOp, FileId)> for RichTerm {
    fn from(op: (BinOp, FileId)) -> Self {
        use rnix::types::BinOpKind::*;
        let (op, file_id) = op;
        let lhs = op.lhs().unwrap();
        let rhs = op.rhs().unwrap();
        match op.operator().unwrap() {
            // TODO: how to manage diff between strconcat and arrayconcat?
            Concat => Term::Op2(
                BinaryOp::ArrayConcat(),
                translate(lhs, file_id),
                translate(rhs, file_id),
            )
            .into(),
            IsSet => unimplemented!(),
            Update => unimplemented!(),

            Add => Term::Op2(
                BinaryOp::Plus(),
                translate(lhs, file_id),
                translate(rhs, file_id),
            )
            .into(),
            Sub => Term::Op2(
                BinaryOp::Sub(),
                translate(lhs, file_id),
                translate(rhs, file_id),
            )
            .into(),
            Mul => Term::Op2(
                BinaryOp::Mult(),
                translate(lhs, file_id),
                translate(rhs, file_id),
            )
            .into(),
            Div => Term::Op2(
                BinaryOp::Div(),
                translate(lhs, file_id),
                translate(rhs, file_id),
            )
            .into(),

            Equal => Term::Op2(
                BinaryOp::Eq(),
                translate(lhs, file_id),
                translate(rhs, file_id),
            )
            .into(),
            Less => Term::Op2(
                BinaryOp::LessThan(),
                translate(lhs, file_id),
                translate(rhs, file_id),
            )
            .into(),
            More => Term::Op2(
                BinaryOp::GreaterThan(),
                translate(lhs, file_id),
                translate(rhs, file_id),
            )
            .into(),
            LessOrEq => Term::Op2(
                BinaryOp::LessOrEq(),
                translate(lhs, file_id),
                translate(rhs, file_id),
            )
            .into(),
            MoreOrEq => Term::Op2(
                BinaryOp::GreaterOrEq(),
                translate(lhs, file_id),
                translate(rhs, file_id),
            )
            .into(),
            NotEqual => Term::Op1(
                UnaryOp::BoolNot(),
                Term::Op2(
                    BinaryOp::Eq(),
                    translate(lhs, file_id),
                    translate(rhs, file_id),
                )
                .into(),
            )
            .into(),

            Implication => Term::App(
                Term::App(
                    Term::Op1(UnaryOp::Ite(), translate(lhs, file_id)).into(),
                    translate(rhs, file_id),
                )
                .into(),
                Term::Bool(true).into(),
            )
            .into(),

            And => Term::App(
                Term::Op1(UnaryOp::BoolAnd(), translate(lhs, file_id)).into(),
                translate(rhs, file_id),
            )
            .into(),
            Or => Term::App(
                Term::Op1(UnaryOp::BoolOr(), translate(lhs, file_id)).into(),
                translate(rhs, file_id),
            )
            .into(),
        }
    }
}

fn translate(node: rnix::SyntaxNode, file_id: FileId) -> RichTerm {
    use rnix::types::{self, ParsedType, Wrapper};
    use rnix::value;
    use rnix::SyntaxKind::*;
    use std::convert::TryInto;
    let pos = node.text_range();
    let span = mk_span(file_id, pos.start().into(), pos.end().into());
    println!("{:?}", node);
    let node: ParsedType = node.try_into().unwrap();
    match node {
        ParsedType::Error(_) => Term::ParseError.into(),
        ParsedType::Root(n) => translate(n.inner().unwrap(), file_id),
        ParsedType::Paren(n) => translate(n.inner().unwrap(), file_id),

        ParsedType::Assert(n) => unimplemented!(),

        ParsedType::Value(n) => match n.to_value().unwrap() {
            value::Value::Float(v) => Term::Num(v).into(),
            value::Value::Integer(v) => Term::Num(v as f64).into(),
            value::Value::String(v) => Term::Str(v).into(),
            // TODO: How to manage Paths in nickel?
            value::Value::Path(a, v) => Term::Str(v).into(),
        },
        ParsedType::Str(n) => {
            let parts = n.parts();
            //TODO: Do we actualy need the `Term::Str` variant in nickel AST?
            Term::StrChunks(
                parts
                    .into_iter()
                    .enumerate()
                    .map(|(i, c)| match c {
                        rnix::value::StrPart::Literal(s) => crate::term::StrChunk::Literal(s),
                        rnix::value::StrPart::Ast(a) => crate::term::StrChunk::Expr(
                            translate(a.first_child().unwrap(), file_id),
                            i,
                        ),
                    })
                    .collect(),
            )
            .into()
        }
        ParsedType::List(n) => Term::Array(
            n.items().map(|elm| translate(elm, file_id)).collect(),
            Default::default(),
        )
        .into(),
        ParsedType::AttrSet(n) => {
            use crate::parser::utils::{build_record, elaborate_field_path, FieldPathElem};
            let fields: Vec<(_, _)> = n
                .entries()
                .map(|kv| {
                    let val = translate(kv.value().unwrap(), file_id);
                    let p: Vec<_> = kv
                        .key()
                        .unwrap()
                        .path()
                        .map(|p| match p.kind() {
                            NODE_IDENT => FieldPathElem::Ident(
                                rnix::types::Ident::cast(p).unwrap().as_str().into(),
                            ),
                            _ => FieldPathElem::Expr(translate(p, file_id)),
                        })
                        .collect();
                    elaborate_field_path(p, val)
                })
                .collect();
            build_record(fields, Default::default()).into()
        }

        ParsedType::Ident(n) => Term::Var(n.as_str().into()).into(),
        ParsedType::LegacyLet(_) => unimplemented!(), // Probably useless to suport it in a short term.
        ParsedType::LetIn(n) => {
            use crate::destruct;
            use crate::identifier::Ident;
            let mut destruct_vec = Vec::new();
            let mut fields = HashMap::new();
            for kv in n.entries() {
                // In `let` blocks, the key is suposed to be a single ident so `Path` exactly one
                // element.
                let id: Ident = types::Ident::cast(kv.key().unwrap().path().next().unwrap())
                    .unwrap()
                    .as_str()
                    .into();
                let rt = translate(kv.value().unwrap(), file_id);
                destruct_vec.push(destruct::Match::Simple(id.clone(), Default::default()));
                fields.insert(id.into(), rt);
            }
            Term::LetPattern(
                None,
                destruct::Destruct::Record {
                    matches: destruct_vec,
                    open: false,
                    rest: None,
                    span,
                },
                Term::RecRecord(fields, vec![], Default::default(), None).into(),
                translate(n.body().unwrap(), file_id),
            )
            .into()
        }
        ParsedType::With(n) => unimplemented!(),

        ParsedType::Lambda(n) => {
            let arg = n.arg().unwrap();
            match arg.kind() {
                NODE_IDENT => Term::Fun(
                    arg.to_string().into(),
                    translate(n.body().unwrap(), file_id),
                ),
                NODE_PATTERN => {
                    use crate::destruct::*;
                    let pat = rnix::types::Pattern::cast(arg).unwrap();
                    let at = pat.at().map(|id| id.as_str().into());
                    // TODO: manage default values:
                    let matches = pat
                        .entries()
                        .map(|e| {
                            Match::Simple(e.name().unwrap().as_str().into(), Default::default())
                        })
                        .collect();
                    let dest = Destruct::Record {
                        matches,
                        open: pat.ellipsis(),
                        rest: None,
                        span,
                    };
                    Term::FunPattern(at, dest, translate(n.body().unwrap(), file_id)).into()
                }
                _ => unreachable!(),
            }
            .into()
        }

        ParsedType::Apply(n) => Term::App(
            translate(n.lambda().unwrap(), file_id),
            translate(n.value().unwrap(), file_id),
        )
        .into(),
        ParsedType::IfElse(n) => Term::App(
            Term::App(
                Term::Op1(UnaryOp::Ite(), translate(n.condition().unwrap(), file_id)).into(),
                translate(n.body().unwrap(), file_id),
            )
            .into(),
            translate(n.else_body().unwrap(), file_id),
        )
        .into(),
        ParsedType::BinOp(n) => (n, file_id).into(),
        ParsedType::UnaryOp(n) => (n, file_id).into(),
        ParsedType::OrDefault(n) => unimplemented!(),

        // TODO: what are these?
        ParsedType::Dynamic(_)
        | ParsedType::Select(_)
        | ParsedType::PatBind(_)
        | ParsedType::PathWithInterpol(_) => unimplemented!(),

        // Is not a `RichTerm` so is transformed as part of actual ones.
        ParsedType::Pattern(_)
        | ParsedType::PatEntry(_)
        | ParsedType::Inherit(..)
        | ParsedType::InheritFrom(..)
        | ParsedType::Key(..)
        | ParsedType::KeyValue(..) => unreachable!(),
    }
    .with_pos(crate::position::TermPos::Original(span))
}

pub fn parse(cache: &Cache, file_id: FileId) -> Result<RichTerm, rnix::parser::ParseError> {
    let source = cache.files().source(file_id);
    let nixast = rnix::parse(source).as_result()?;
    Ok(translate(nixast.node(), file_id))
}
