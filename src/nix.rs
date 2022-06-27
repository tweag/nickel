use crate::cache::Cache;
use crate::parser::utils::mk_span;
use crate::term::make;
use crate::term::{BinaryOp, UnaryOp};
use crate::term::{RichTerm, Term};
use codespan::FileId;
use rnix::types::{BinOp, EntryHolder, TokenWrapper, TypedNode, UnaryOp as UniOp};
use rnix::{self, SyntaxNode};
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
    use rnix::SyntaxKind::*;
    let pos = node.text_range();
    println!("{:?}", node);
    // TODO: is there a nix boolean type?
    match node.kind() {
        NODE_ERROR => Term::ParseError.into(),
        NODE_ROOT | NODE_PAREN => node
            .children()
            .map(|n| translate(n, file_id))
            .next()
            .unwrap(),

        NODE_LITERAL => node
            .children_with_tokens()
            .map(|n| {
                println!("  {:?}", n);
                match n.kind() {
                    TOKEN_INTEGER | TOKEN_FLOAT => {
                        Term::Num(n.as_token().unwrap().text().to_string().parse().unwrap()).into()
                    }
                    _ => panic!("{} token is not a literal", n),
                }
            })
            .next()
            .unwrap(),
        NODE_STRING => {
            let parts = rnix::types::Str::cast(node).unwrap().parts();
            //TODO: Do we actualy need the `Term::Str` variant in nickel AST?
            Term::StrChunks(
                parts
                    .iter()
                    .enumerate()
                    .map(|(i, c)| match c {
                        rnix::value::StrPart::Literal(s) => {
                            crate::term::StrChunk::Literal(s.clone())
                        }
                        rnix::value::StrPart::Ast(a) => crate::term::StrChunk::Expr(
                            translate(a.first_child().unwrap(), file_id),
                            i,
                        ),
                    })
                    .collect(),
            )
            .into()
        }
        NODE_LIST => Term::Array(
            rnix::types::List::cast(node)
                .unwrap()
                .items()
                .map(|n| translate(n, file_id))
                .collect(),
            Default::default(),
        )
        .into(),
        NODE_ATTR_SET => {
            use crate::parser::utils::{build_record, elaborate_field_path, FieldPathElem};
            let attrset = rnix::types::AttrSet::cast(node).unwrap();
            let fields: Vec<(_, _)> = attrset
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

        NODE_IDENT => Term::Var(rnix::types::Ident::cast(node).unwrap().as_str().into()).into(),
        NODE_LET_IN => {
            use crate::destruct;
            use crate::identifier::Ident;
            use rnix::types::LetIn;
            let letin = LetIn::cast(node).unwrap();
            let mut destruct_vec = Vec::new();
            let mut fields = HashMap::new();
            for kv in letin.entries() {
                // In `let` blocks, the key is suposed to be a single ident so `Path` exactly one
                // element.
                let id: Ident = rnix::types::Ident::cast(kv.key().unwrap().path().next().unwrap())
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
                    span: mk_span(file_id, pos.start().into(), pos.end().into()),
                },
                Term::RecRecord(fields, vec![], Default::default(), None).into(),
                translate(letin.body().unwrap(), file_id),
            )
            .into()
        }

        NODE_LAMBDA => {
            let fun = rnix::types::Lambda::cast(node).unwrap();
            let arg = fun.arg().unwrap();
            match arg.kind() {
                NODE_IDENT => Term::Fun(
                    arg.to_string().into(),
                    translate(fun.body().unwrap(), file_id),
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
                        span: mk_span(file_id, pos.start().into(), pos.end().into()),
                    };
                    Term::FunPattern(at, dest, translate(fun.body().unwrap(), file_id)).into()
                }
                _ => unimplemented!(),
            }
            .into()
        }

        NODE_APPLY => {
            let fun = rnix::types::Apply::cast(node).unwrap();
            Term::App(
                translate(fun.lambda().unwrap(), file_id),
                translate(fun.value().unwrap(), file_id),
            )
            .into()
        }
        NODE_IF_ELSE => {
            let ifelse = rnix::types::IfElse::cast(node).unwrap();
            Term::App(
                Term::App(
                    Term::Op1(
                        UnaryOp::Ite(),
                        translate(ifelse.condition().unwrap(), file_id),
                    )
                    .into(),
                    translate(ifelse.body().unwrap(), file_id),
                )
                .into(),
                translate(ifelse.else_body().unwrap(), file_id),
            )
            .into()
        }
        NODE_BIN_OP => (BinOp::cast(node).unwrap(), file_id).into(),
        NODE_UNARY_OP => (UniOp::cast(node).unwrap(), file_id).into(),
        _ => panic!("{}", node),
    }
    .with_pos(crate::position::TermPos::Original(mk_span(
        file_id,
        pos.start().into(),
        pos.end().into(),
    )))
}

pub fn parse(cache: &Cache, file_id: FileId) -> Result<RichTerm, rnix::parser::ParseError> {
    let source = cache.files().source(file_id);
    let nixast = rnix::parse(source).as_result()?;
    Ok(translate(nixast.node(), file_id))
}
