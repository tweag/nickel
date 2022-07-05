use crate::cache::Cache;
use crate::convertion::ToNickel;
use crate::mk_app;
use crate::parser::utils::mk_span;
use crate::term::make::{self, if_then_else};
use crate::term::{BinaryOp, RecordData, UnaryOp};
use crate::term::{MergePriority, MetaValue, RichTerm, Term};
use codespan::FileId;
use rnix;
use rnix::types::{BinOp, EntryHolder, TokenWrapper, TypedNode, UnaryOp as UniOp};
use std::collections::HashMap;

impl ToNickel for UniOp {
    fn translate(self, file_id: FileId) -> RichTerm {
        use rnix::types::UnaryOpKind::*;
        let value = self.value().unwrap().translate(file_id);
        match self.operator() {
            Negate => make::op2(BinaryOp::Sub(), Term::Num(0.), value),
            Invert => make::op1(UnaryOp::BoolNot(), value),
        }
    }
}

impl ToNickel for BinOp {
    fn translate(self, file_id: FileId) -> RichTerm {
        use rnix::types::BinOpKind::*;
        let lhs = self.lhs().unwrap().translate(file_id);
        let rhs = self.rhs().unwrap().translate(file_id);
        match self.operator().unwrap() {
            // TODO: Should be fixed using a nickel function `compat.concat` of type `a -> a -> a`
            // using `str_conct` or `array_concat` in respect to `typeof a`.
            Concat => make::op2(BinaryOp::ArrayConcat(), lhs, rhs),
            IsSet => unimplemented!(),
            Update => unimplemented!(),

            Add => make::op2(BinaryOp::Plus(), lhs, rhs),
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

impl ToNickel for rnix::SyntaxNode {
    fn translate(self, file_id: FileId) -> RichTerm {
        use rnix::types::{self, ParsedType, Wrapper};
        use rnix::value;
        use rnix::SyntaxKind::*;
        use std::convert::{TryFrom, TryInto};
        let pos = self.text_range();
        let span = mk_span(file_id, pos.start().into(), pos.end().into());
        println!("{:?}: {}", self, self);
        let node: ParsedType = self.try_into().unwrap();
        match node {
            ParsedType::Error(_) => {
                Term::ParseError(crate::error::ParseError::NixParseError(file_id)).into()
            }
            ParsedType::Root(n) => n.inner().unwrap().translate(file_id),
            ParsedType::Paren(n) => n.inner().unwrap().translate(file_id),
            ParsedType::Dynamic(n) => n.inner().unwrap().translate(file_id),

            ParsedType::Assert(n) => unimplemented!(),

            ParsedType::Value(n) => match n.to_value().unwrap() {
                value::Value::Float(v) => Term::Num(v),
                value::Value::Integer(v) => Term::Num(v as f64),
                value::Value::String(v) => Term::Str(v),
                // TODO: How to manage Paths in nickel?
                value::Value::Path(a, v) => {
                    use value::Anchor::*;
                    let strpath = match a {
                        Absolute => format!("/{}", v),
                        Relative => format!("./{}", v),
                        Home => format!("~/{}", v),
                        Store => format!("<{}>", v),
                    };
                    Term::Str(strpath)
                }
            }
            .into(),
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
                                a.first_child().unwrap().translate(file_id),
                                i,
                            ),
                        })
                        .collect(),
                )
                .into()
            }
            ParsedType::List(n) => Term::Array(
                n.items().map(|elm| elm.translate(file_id)).collect(),
                Default::default(),
            )
            .into(),
            ParsedType::AttrSet(n) => {
                use crate::parser::utils::{build_record, elaborate_field_path, FieldPathElem};
                let fields: Vec<(_, _)> = n
                    .entries()
                    .map(|kv| {
                        let val = kv.value().unwrap().translate(file_id);
                        let p: Vec<_> = kv
                            .key()
                            .unwrap()
                            .path()
                            .map(|p| match p.kind() {
                                NODE_IDENT => FieldPathElem::Ident(
                                    rnix::types::Ident::cast(p).unwrap().as_str().into(),
                                ),
                                _ => FieldPathElem::Expr(p.translate(file_id)),
                            })
                            .collect();
                        elaborate_field_path(p, val)
                    })
                    .collect();
                build_record(fields, Default::default()).into()
            }

            // In nix it's allowed to define vars named `true`, `false` or `null`.
            // But we prefer to not suport it. If we try to redefine one of these builtins, nickel
            // will panic (see below in the `LetIn` arm.
            ParsedType::Ident(n) => match n.as_str() {
                "true" => Term::Bool(true),
                "false" => Term::Bool(false),
                "null" => Term::Null,
                id => Term::Var(id.into()),
            }
            .into(),
            ParsedType::LegacyLet(_) => panic!("Legacy let form is not supported"), // Probably useless to suport it in a short term.
            // `let ... in` blocks are recursive in Nix and not in Nickel. To emulate this, we use
            // a `let <pattern> = <recrecord> in`. The record provide recursivity then the values
            // are destructured by the pattern.
            ParsedType::LetIn(n) => {
                use crate::destruct;
                use crate::identifier::Ident;
                let mut destruct_vec = Vec::new();
                let mut fields = HashMap::new();
                for kv in n.entries() {
                    // In `let` blocks, the key is suposed to be a single ident so `Path` exactly one
                    // element.
                    let id = types::Ident::cast(kv.key().unwrap().path().next().unwrap()).unwrap();
                    // Check we don't try to redefine builtin values. Even if it's possible in Nix,
                    // we don't suport it.
                    let id: Ident = match id.as_str() {
                        "true" | "false" | "nul" => panic!(
                            "`let {}` is forbiden. Can not redifine `true`, `false` or `nul`",
                            id.as_str()
                        ),
                        s => s.into(),
                    };
                    let rt = kv.value().unwrap().translate(file_id);
                    destruct_vec.push(destruct::Match::Simple(id.clone(), Default::default()));
                    fields.insert(id.into(), rt);
                }
                make::let_pat::<Ident, _, _, _>(
                    None,
                    destruct::Destruct::Record {
                        matches: destruct_vec,
                        open: false,
                        rest: None,
                        span,
                    },
                    Term::RecRecord(RecordData::with_fields(fields), vec![], None).into(),
                    n.body().unwrap().translate(file_id),
                )
            }
            ParsedType::With(n) => unimplemented!(),

            ParsedType::Lambda(n) => {
                let arg = n.arg().unwrap();
                match arg.kind() {
                    NODE_IDENT => {
                        Term::Fun(arg.to_string().into(), n.body().unwrap().translate(file_id))
                    }
                    NODE_PATTERN => {
                        use crate::destruct::*;
                        let pat = rnix::types::Pattern::cast(arg).unwrap();
                        let at = pat.at().map(|id| id.as_str().into());
                        // TODO: manage default values:
                        let matches = pat
                            .entries()
                            .map(|e| {
                                let mv = if let Some(def) = e.default() {
                                    MetaValue {
                                        value: Some(def.translate(file_id)),
                                        priority: MergePriority::Bottom,
                                        ..Default::default()
                                    }
                                } else {
                                    Default::default()
                                };
                                Match::Simple(e.name().unwrap().as_str().into(), mv)
                            })
                            .collect();
                        let dest = Destruct::Record {
                            matches,
                            open: pat.ellipsis(),
                            rest: None,
                            span,
                        };
                        Term::FunPattern(at, dest, n.body().unwrap().translate(file_id))
                    }
                    _ => unreachable!(),
                }
            }
            .into(),

            ParsedType::Apply(n) => Term::App(
                n.lambda().unwrap().translate(file_id),
                n.value().unwrap().translate(file_id),
            )
            .into(),
            ParsedType::IfElse(n) => if_then_else(
                n.condition().unwrap().translate(file_id),
                n.body().unwrap().translate(file_id),
                n.else_body().unwrap().translate(file_id),
            ),
            ParsedType::BinOp(n) => n.translate(file_id),
            ParsedType::UnaryOp(n) => n.translate(file_id),
            ParsedType::Select(n) => match ParsedType::try_from(n.index().unwrap()).unwrap() {
                ParsedType::Ident(id) => Term::Op1(
                    UnaryOp::StaticAccess(id.as_str().into()),
                    n.set().unwrap().translate(file_id),
                )
                .into(),
                _ => Term::Op2(
                    BinaryOp::DynAccess(),
                    n.index().unwrap().translate(file_id),
                    n.set().unwrap().translate(file_id),
                )
                .into(),
            },
            ParsedType::OrDefault(n) => unimplemented!(),

            // TODO: what are these?
            ParsedType::PatBind(_) | ParsedType::PathWithInterpol(_) => {
                unimplemented!()
            }

            // Are not `RichTerm`s and are transformed as part of ones higher in the syntax tree.
            // Actualy, these variants are not "Terms" in the sens they can not be parsed independently of a Term. They can only be used in a term. Our translate function return a term so these can not be parsed at this level. they are parsed as parts of actual terms.
            ParsedType::Pattern(_)
            | ParsedType::PatEntry(_)
            | ParsedType::Inherit(..)
            | ParsedType::InheritFrom(..)
            | ParsedType::Key(..)
            | ParsedType::KeyValue(..) => unreachable!(),
        }
        .with_pos(crate::position::TermPos::Original(span))
    }
}

pub fn parse(cache: &Cache, file_id: FileId) -> Result<RichTerm, rnix::parser::ParseError> {
    let source = cache.files().source(file_id);
    let nixast = rnix::parse(source).as_result()?;
    Ok(nixast.node().translate(file_id))
}
