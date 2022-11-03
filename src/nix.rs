use crate::cache::Cache;
use crate::conversion::State;
pub use crate::conversion::ToNickel;
use crate::identifier::Ident;
use crate::mk_app;
use crate::parser::utils::{mk_span, FieldPathElem};
use crate::term::make::{self, if_then_else};
use crate::term::{record::RecordData, BinaryOp, UnaryOp};
use crate::term::{MergePriority, MetaValue, RichTerm, Term};
use codespan::FileId;
use rnix::ast::{
    Attr as NixAttr, BinOp as NixBinOp, Ident as NixIdent, Str as NixStr, UnaryOp as NixUniOp,
};
use rowan::ast::AstNode;
use std::collections::HashMap;

// kept for future use.
//fn path_elem_from_nix(attr: NixAttr, state: &State) -> FieldPathElem {
//    match attr {
//        NixAttr::Ident(id) => FieldPathElem::Ident(id.to_string().into()),
//        NixAttr::Str(s) => FieldPathElem::Expr(s.translate(state)),
//        NixAttr::Dynamic(d) => FieldPathElem::Expr(d.expr().unwrap().translate(state)),
//    }
//}

fn path_elem_rt(attr: NixAttr, state: &State) -> RichTerm {
    match attr {
        NixAttr::Ident(id) => Term::Str(id.to_string()).into(),
        NixAttr::Str(s) => s.translate(state),
        NixAttr::Dynamic(d) => d.expr().unwrap().translate(state),
    }
}

fn path_rts_from_nix<T>(n: rnix::ast::Attrpath, state: &State) -> T
where
    T: FromIterator<RichTerm>,
{
    n.attrs().map(|a| path_elem_rt(a, state)).collect()
}

fn id_from_nix(id: NixIdent, state: &State) -> Ident {
    let pos = id.syntax().text_range();
    let span = mk_span(state.file_id, pos.start().into(), pos.end().into());
    Ident::new_with_pos(id.to_string(), crate::position::TermPos::Original(span))
}

impl ToNickel for NixStr {
    fn translate(self, state: &State) -> RichTerm {
        use rnix::ast::InterpolPart;
        Term::StrChunks(
            self.parts()
                .enumerate()
                .map(|(i, c)| match c {
                    InterpolPart::Literal(s) => crate::term::StrChunk::Literal(s.to_string()),
                    InterpolPart::Interpolation(interp) => {
                        crate::term::StrChunk::Expr(interp.expr().unwrap().translate(state), i)
                    }
                })
                .collect(),
        )
        .into()
    }
}

impl ToNickel for NixUniOp {
    fn translate(self, state: &State) -> RichTerm {
        use rnix::ast::UnaryOpKind::*;
        let value = self.expr().unwrap().translate(state);
        match self.operator().unwrap() {
            Negate => make::op2(BinaryOp::Sub(), Term::Num(0.), value),
            Invert => make::op1(UnaryOp::BoolNot(), value),
        }
    }
}

impl ToNickel for NixBinOp {
    fn translate(self, state: &State) -> RichTerm {
        use rnix::ast::BinOpKind::*;
        let lhs = self.lhs().unwrap().translate(state);
        let rhs = self.rhs().unwrap().translate(state);
        match self.operator().unwrap() {
            Concat => make::op2(BinaryOp::ArrayConcat(), lhs, rhs),
            Update => unimplemented!(),

            Add => mk_app!(crate::stdlib::compat::add(), lhs, rhs),
            Sub => make::op2(BinaryOp::Sub(), lhs, rhs),
            Mul => make::op2(BinaryOp::Mult(), lhs, rhs),
            Div => make::op2(BinaryOp::Div(), lhs, rhs),

            Equal => make::op2(BinaryOp::Eq(), lhs, rhs),
            Less => make::op2(BinaryOp::LessThan(), lhs, rhs),
            More => make::op2(BinaryOp::GreaterThan(), lhs, rhs),
            LessOrEq => make::op2(BinaryOp::LessOrEq(), lhs, rhs),
            MoreOrEq => make::op2(BinaryOp::GreaterOrEq(), lhs, rhs),
            NotEqual => make::op1(UnaryOp::BoolNot(), make::op2(BinaryOp::Eq(), lhs, rhs)),

            Implication => if_then_else(lhs, rhs, Term::Bool(true)),

            And => mk_app!(Term::Op1(UnaryOp::BoolAnd(), lhs), rhs),
            Or => mk_app!(Term::Op1(UnaryOp::BoolOr(), lhs), rhs),
        }
    }
}

impl ToNickel for rnix::ast::Expr {
    fn translate(self, state: &State) -> RichTerm {
        use rnix::ast::Expr;
        let pos = self.syntax().text_range();
        let file_id = state.file_id;
        let span = mk_span(file_id, pos.start().into(), pos.end().into());
        println!("{:?}: {}", self, self);
        match self {
            Expr::Error(_) => {
                Term::ParseError(crate::error::ParseError::NixParseError(file_id)).into()
                // TODO: Improve error management
            }
            Expr::Root(n) => n.expr().unwrap().translate(state),
            Expr::Paren(n) => n.expr().unwrap().translate(state),

            Expr::Assert(_) => unimplemented!(),

            Expr::Literal(n) => match n.kind() {
                rnix::ast::LiteralKind::Float(v) => Term::Num(v.value().unwrap()),
                rnix::ast::LiteralKind::Integer(v) => Term::Num(v.value().unwrap() as f64),
                // TODO: How to manage Paths in nickel?
                rnix::ast::LiteralKind::Uri(v) => Term::Str(v.to_string()),
            }
            .into(),
            Expr::Str(n) => n.translate(state),
            Expr::List(n) => Term::Array(
                n.items().map(|elm| elm.translate(state)).collect(),
                Default::default(),
            )
            .into(),
            Expr::AttrSet(n) => {
                use crate::parser::utils::{build_record, elaborate_field_path};
                use rnix::ast::HasEntry;
                let fields: Vec<(_, _)> = n
                    .attrpath_values()
                    .map(|kv| {
                        let val = kv.value().unwrap().translate(state);
                        let p: Vec<_> = kv
                            .attrpath()
                            .unwrap()
                            .attrs()
                            .map(|p| match p {
                                rnix::ast::Attr::Ident(id) => {
                                    FieldPathElem::Ident(id_from_nix(id, state))
                                }
                                rnix::ast::Attr::Dynamic(d) => {
                                    FieldPathElem::Expr(d.expr().unwrap().translate(state))
                                }
                                rnix::ast::Attr::Str(s) => FieldPathElem::Expr(s.translate(state)),
                            })
                            .collect();
                        elaborate_field_path(p, val)
                    })
                    .collect();
                let inherited: Vec<(_, Option<RichTerm>)> = n
                    .inherits()
                    .map(|inh| {
                        let term_from = inh.from().map(|n| n.expr().unwrap().translate(state));
                        let ids: Vec<Ident> = inh.attrs().map(|id| id.to_string().into()).collect();
                        (ids, term_from)
                    })
                    .collect();

                build_record(fields, Default::default(), inherited).into()
            }

            // In nix it's allowed to define vars named `true`, `false` or `null`.
            // But we prefer to not support it. If we try to redefine one of these builtins, nickel
            // will panic (see below in the `LetIn` arm).
            Expr::Ident(id) => match id.to_string().as_str() {
                "true" => Term::Bool(true),
                "false" => Term::Bool(false),
                "null" => Term::Null,
                id_str => {
                    if state.env.contains(id_str) || state.with.is_empty() {
                        Term::Var(id_from_nix(id, state))
                    } else {
                        Term::App(
                            crate::stdlib::compat::with(state.with.clone().into_iter().collect()),
                            Term::Str(id.to_string()).into(),
                        )
                    }
                }
            }
            .into(),
            Expr::LegacyLet(_) => panic!("Legacy let form is not supported"), // Probably useless to suport it in a short term.
            // `let ... in` blocks are recursive in Nix and not in Nickel. To emulate this, we use
            // a `let <pattern> = <recrecord> in`. The record provide recursivity then the values
            // are destructured by the pattern.
            Expr::LetIn(n) => {
                use crate::destruct;
                use rnix::ast::HasEntry;
                let mut destruct_vec = Vec::new();
                let mut fields = HashMap::new();
                let mut state = state.clone();
                state.env.extend(n.attrpath_values().map(|kv| {
                    kv.attrpath().unwrap().attrs().next().unwrap().to_string() // TODO: does not work if the let contains Dynamic or Str. Is
                                                                               // it possible in Nix?
                }));
                for kv in n.attrpath_values() {
                    // In `let` blocks, the key is suposed to be a single ident so `Path` exactly one
                    // element.
                    let id = kv.attrpath().unwrap().attrs().next().unwrap();
                    // Check we don't try to redefine builtin values. Even if it's possible in Nix,
                    // we don't suport it.
                    let id: Ident = match id.to_string().as_str() {
                        "true" | "false" | "null" => panic!(
                            "`let {}` is forbiden. Can not redifine `true`, `false` or `nul`",
                            id
                        ),
                        s => s.into(),
                    };
                    let rt = kv.value().unwrap().translate(&state);
                    destruct_vec.push(destruct::Match::Simple(id, Default::default()));
                    fields.insert(id, rt);
                }
                make::let_pat::<Ident, _, _, _>(
                    None,
                    destruct::Destruct::Record {
                        matches: destruct_vec,
                        open: false,
                        rest: None,
                        span,
                    },
                    Term::RecRecord(RecordData::with_fields(fields), vec![], None, vec![]),
                    n.body().unwrap().translate(&state),
                )
            }
            Expr::With(n) => {
                let mut state = state.clone();
                state.with.push(n.namespace().unwrap().translate(&state));
                n.body().unwrap().translate(&state)
            }

            Expr::Lambda(n) => {
                match n.param().unwrap() {
                    rnix::ast::Param::IdentParam(idp) => Term::Fun(
                        idp.ident().unwrap().to_string().into(),
                        n.body().unwrap().translate(state),
                    ),
                    rnix::ast::Param::Pattern(pat) => {
                        use crate::destruct::*;
                        let at = pat
                            .pat_bind()
                            .map(|id| id.ident().unwrap().to_string().into());
                        // TODO: manage default values:
                        let matches = pat
                            .pat_entries()
                            .map(|e| {
                                let mv = if let Some(def) = e.default() {
                                    MetaValue {
                                        value: Some(def.translate(state)),
                                        priority: MergePriority::Bottom,
                                        ..Default::default()
                                    }
                                } else {
                                    Default::default()
                                };
                                Match::Simple(e.ident().unwrap().to_string().into(), mv)
                            })
                            .collect();
                        let dest = Destruct::Record {
                            matches,
                            open: pat.ellipsis_token().is_some(),
                            rest: None,
                            span,
                        };
                        Term::FunPattern(at, dest, n.body().unwrap().translate(state))
                    }
                }
            }
            .into(),

            Expr::Apply(n) => Term::App(
                n.lambda().unwrap().translate(state),
                n.argument().unwrap().translate(state),
            )
            .into(),
            Expr::IfElse(n) => if_then_else(
                n.condition().unwrap().translate(state),
                n.body().unwrap().translate(state),
                n.else_body().unwrap().translate(state),
            ),
            Expr::BinOp(n) => n.translate(state),
            Expr::UnaryOp(n) => n.translate(state),
            Expr::Select(n) => {
                n.attrpath()
                    .unwrap()
                    .attrs()
                    .fold(n.expr().unwrap().translate(state), |acc, i| {
                        match i {
                            rnix::ast::Attr::Ident(id) => {
                                Term::Op1(UnaryOp::StaticAccess(id.to_string().into()), acc)
                            }
                            rnix::ast::Attr::Dynamic(d) => Term::Op2(
                                BinaryOp::DynAccess(),
                                d.expr().unwrap().translate(state),
                                acc,
                            ),
                            rnix::ast::Attr::Str(s) => {
                                Term::Op2(BinaryOp::DynAccess(), s.translate(state), acc)
                            }
                        }
                        .into()
                    })
            }
            Expr::HasAttr(n) => {
                let path = path_rts_from_nix(n.attrpath().unwrap(), state);
                let path = Term::Array(path, Default::default());
                mk_app!(
                    crate::stdlib::compat::has_field_path(),
                    path,
                    n.expr().unwrap().translate(state)
                )
            }
            Expr::Path(_) => unimplemented!(), // TODO: What is the diff with select?
        }
        .with_pos(crate::position::TermPos::Original(span))
    }
}

pub fn parse(cache: &Cache, file_id: FileId) -> Result<RichTerm, rnix::parser::ParseError> {
    let source = cache.files().source(file_id);
    let root = rnix::Root::parse(source).ok()?; // TODO: we could return a list of errors calling
                                                // `errors()` to improve error management.
    Ok(root.expr().unwrap().to_nickel(file_id))
}
