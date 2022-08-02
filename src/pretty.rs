use crate::destruct::{self, Destruct};
use crate::term::{BinaryOp, MetaValue, RichTerm, Term, UnaryOp};
use crate::types::{AbsType, Types};
pub use pretty::{DocAllocator, DocBuilder, Pretty};
use regex::Regex;
use std::collections::HashMap;

/// Helper to find the min number of `%` sign needed to interpolate a string containing this chunk.
fn min_interpolate_sign(text: &str) -> usize {
    let reg = Regex::new(r#"([%]+\{)|("[%]+m)"#).unwrap();
    reg.find_iter(text)
        .map(|m| {
            let d = m.end() - m.start();
            // We iterate over all sequences `%+{` and `"%+m`, which could clash with the interpolation
            // syntax, and return the maximum number of `%` insead each sequence.
            //
            // For the case of a closing delimiter `"%m`, we could actually be slightly smarter as we
            // don't necessarily need more `%`, but just a different number of `%`. For example, if the
            // string contains only one `"%%m`, then single `%` delimiters like `m%"` and `"%m` would
            // be fine. But picking the maximum
            //
            // TODO: Improve the `"%*m` case if necessary.
            if m.as_str().ends_with('{') {
                d
            } else {
                d - 1
            }
        })
        .max()
        .unwrap_or(1)
}

fn sorted_map<K: Ord, V>(m: &'_ HashMap<K, V>) -> Vec<(&'_ K, &'_ V)> {
    let mut ret: Vec<(&K, &V)> = m.iter().collect();
    ret.sort_by_key(|(k, _)| *k);
    ret
}

impl<'a, A: Clone + 'a> NickelAllocatorExt<'a, A> for pretty::BoxAllocator {}

trait NickelAllocatorExt<'a, A: 'a>: DocAllocator<'a, A> + Sized
where
    Self::Doc: Clone,
    A: Clone,
{
    fn quote_if_needed(&'a self, id: &crate::identifier::Ident) -> DocBuilder<'a, Self, A> {
        let reg = Regex::new("^_?[a-zA-Z][_a-zA-Z0-9-]*$").unwrap();
        if reg.is_match(id.as_ref()) {
            self.as_string(id)
        } else {
            self.as_string(id).double_quotes()
        }
    }

    fn escaped_string(&'a self, s: &str) -> DocBuilder<'a, Self, A> {
        let s = s
            .replace('\\', "\\\\")
            .replace("%{", "\\%{")
            .replace('\"', "\\\"");
        self.text(s)
    }

    fn metadata(&'a self, mv: &MetaValue, with_doc: bool) -> DocBuilder<'a, Self, A> {
        if let Some(types) = &mv.types {
            self.text(":")
                .append(self.space())
                .append(types.types.clone().pretty(self))
                .append(self.line())
        } else {
            self.nil()
        }
        .append(if with_doc {
            mv.doc
                .clone()
                .map(|doc| {
                    self.text("|")
                        .append(self.space())
                        .append(self.text("doc"))
                        .append(self.space())
                        .append(
                            self.hardline()
                                .append(self.intersperse(
                                    doc.lines().map(|d| self.escaped_string(d)),
                                    self.hardline().clone(),
                                ))
                                .double_quotes(),
                        )
                        .append(self.line())
                })
                .unwrap_or_else(|| self.nil())
        } else {
            self.nil()
        })
        .append(self.intersperse(
            mv.contracts.iter().map(|c| {
                self.text("|")
                    .append(self.space())
                    .append(c.to_owned().types.pretty(self))
            }),
            self.line().clone(),
        ))
        .append(if mv.priority == crate::term::MergePriority::Default {
            self.line().append(self.text("| default"))
        } else {
            self.nil()
        })
        .nest(2)
        .group()
    }

    fn atom(&'a self, rt: &RichTerm) -> DocBuilder<'a, Self, A> {
        if rt.as_ref().is_atom() {
            rt.to_owned().pretty(self)
        } else {
            rt.to_owned().pretty(self).parens()
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &UnaryOp
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        use UnaryOp::*;
        match self {
            ArrayGen() => allocator.text("%gen%").append(allocator.space()),
            ArrayMap() => allocator.text("%map%").append(allocator.space()),
            ArrayLength() => allocator.text("%length%").append(allocator.space()),
            ArrayTail() => allocator.text("%tail%").append(allocator.space()),
            ArrayHead() => allocator.text("%head%").append(allocator.space()),
            DeepSeq(_) => allocator.text("%deep_seq%").append(allocator.space()),
            Typeof() => allocator.text("%typeof%").append(allocator.space()),
            BoolNot() => allocator.text("!"),
            BoolAnd() => allocator.space().append(allocator.text("&&")),
            BoolOr() => allocator.space().append(allocator.text("||")),
            StaticAccess(id) => allocator.text(".").append(allocator.quote_if_needed(id)),
            Embed(id) => allocator
                .text("%embed%")
                .append(allocator.space())
                .append(allocator.as_string(id))
                .append(allocator.space()),
            op => allocator
                .text(format!("%{:?}%", op).to_lowercase())
                .append(allocator.space()),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &BinaryOp
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        use BinaryOp::*;
        match self {
            Plus() => allocator.text("+"),
            Sub() => allocator.text("-"),

            Mult() => allocator.text("*"),
            Div() => allocator.text("/"),
            Modulo() => allocator.text("%"),

            Eq() => allocator.text("=="),
            LessThan() => allocator.text("<"),
            GreaterThan() => allocator.text(">"),
            GreaterOrEq() => allocator.text(">="),
            LessOrEq() => allocator.text("<="),

            Merge() => allocator.text("&"),

            StrConcat() => allocator.text("++"),
            ArrayConcat() => allocator.text("@"),

            DynAccess() => allocator.text("."),
            ArrayElemAt() => allocator.text("%elem_at%"),

            op => allocator.as_string(format!("%{:?}%", op).to_lowercase()),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &Destruct
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        match self {
            Destruct::Record {
                matches,
                open,
                rest,
                ..
            } => allocator
                .intersperse(
                    matches.iter().map(|m| match m {
                        destruct::Match::Simple(id, meta) => allocator
                            .as_string(id)
                            .append(allocator.space())
                            .append(match meta.clone() {
                                MetaValue {
                                    types,
                                    contracts,
                                    priority: crate::term::MergePriority::Default,
                                    value: Some(value),
                                    ..
                                } => allocator
                                    .text("?")
                                    .append(allocator.space())
                                    .append(allocator.atom(&value))
                                    .append(allocator.metadata(
                                        &MetaValue {
                                            types,
                                            contracts,
                                            ..Default::default()
                                        },
                                        false,
                                    )),
                                m => allocator.metadata(&m, false),
                            }),
                        _ => unimplemented!(),
                    }),
                    allocator.text(",").append(allocator.space()),
                )
                .append(if *open {
                    allocator
                        .text(",")
                        .append(allocator.space())
                        .append(allocator.text(".."))
                        .append(if let Some(rest) = rest {
                            allocator.as_string(rest)
                        } else {
                            allocator.nil()
                        })
                } else {
                    allocator.nil()
                })
                .braces(),
            Destruct::Empty => allocator.nil(),
            _ => unimplemented!(),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for RichTerm
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        use Term::*;

        match self.as_ref() {
            Null => allocator.text("null"),
            Bool(v) => allocator.as_string(v),
            Num(v) => allocator.as_string(v),
            Str(v) => allocator.escaped_string(v),
            StrChunks(chunks) => {
                let multiline = chunks.len() > 1;
                let nb_perc = chunks
                    .iter()
                    .map(
                        |c| {
                            if let crate::term::StrChunk::Literal(s) = c {
                                min_interpolate_sign(s)
                            } else {
                                1
                            }
                        }, // be sure we have at least 1 `%` sign when an interpolation is present
                    )
                    .max()
                    .unwrap_or(1);
                let interp: String = "%".repeat(nb_perc);
                allocator
                    .intersperse(
                        chunks.iter().rev().map(|c| match c {
                            crate::term::StrChunk::Literal(s) => {
                                if multiline {
                                    allocator.as_string(s)
                                } else {
                                    allocator.escaped_string(s)
                                }
                            }
                            crate::term::StrChunk::Expr(e, _i) => allocator
                                .text(interp.clone())
                                .append(allocator.text("{"))
                                .append(e.to_owned().pretty(allocator))
                                .append(allocator.text("}")),
                        }),
                        allocator.nil(),
                    )
                    .double_quotes()
                    .enclose(
                        if multiline {
                            format!("m{}", interp)
                        } else {
                            "".to_string()
                        },
                        if multiline {
                            format!("{}m", interp)
                        } else {
                            "".to_string()
                        },
                    )
            }
            Fun(id, rt) => {
                let mut params = vec![id];
                let mut rt = rt;
                while let Fun(id, t) = rt.as_ref() {
                    params.push(id);
                    rt = t
                }
                allocator
                    .text("fun")
                    .append(allocator.space())
                    .append(allocator.intersperse(
                        params.iter().map(|p| allocator.as_string(p)),
                        allocator.space(),
                    ))
                    .append(allocator.space())
                    .append(allocator.text("=>"))
                    .append(allocator.softline())
                    .append(rt.to_owned().pretty(allocator).nest(2))
            }
            // TODO Pattern destructuring to implement.
            FunPattern(..) => {
                let mut params = vec![];
                let mut rt = &self;
                while let FunPattern(id, dst, t) = rt.as_ref() {
                    params.push(if let Some(id) = id {
                        allocator.as_string(id).append(if !dst.is_empty() {
                            allocator.text("@").append(dst.pretty(allocator))
                        } else {
                            allocator.nil()
                        })
                    } else {
                        dst.pretty(allocator)
                    });
                    rt = t;
                }
                allocator
                    .text("fun")
                    .append(allocator.space())
                    .append(allocator.intersperse(params, allocator.space()))
                    .append(allocator.space())
                    .append(allocator.text("=>"))
                    .append(allocator.softline())
                    .append(rt.to_owned().pretty(allocator).nest(2))
            }
            Lbl(_lbl) => allocator.text("# <label>").append(allocator.hardline()),
            Let(id, rt, body, _ty) => allocator
                .text("let")
                .append(allocator.space())
                .append(allocator.as_string(id))
                .append(if let MetaValue(ref mv) = rt.as_ref() {
                    allocator.space().append(allocator.metadata(mv, false))
                } else {
                    allocator.nil()
                })
                .append(allocator.space())
                .append(allocator.text("="))
                .append(allocator.line())
                .append(
                    if let MetaValue(crate::term::MetaValue {
                        value: Some(rt), ..
                    }) = rt.as_ref()
                    {
                        rt
                    } else {
                        rt
                    }
                    .to_owned()
                    .pretty(allocator)
                    .nest(2),
                )
                .append(allocator.line())
                .append(allocator.text("in"))
                .append(allocator.line_())
                .group()
                .append(allocator.line())
                .append(body.to_owned().pretty(allocator))
                .group(),
            LetPattern(opt_id, dst, rt, body) => allocator
                .text("let")
                .append(allocator.space())
                .append(
                    opt_id
                        .clone()
                        .map(|id| {
                            allocator.as_string(id).append(if dst.is_empty() {
                                allocator.nil()
                            } else {
                                allocator
                                    .space()
                                    .append(allocator.text("@"))
                                    .append(allocator.space())
                            })
                        })
                        .unwrap_or_else(|| allocator.nil()),
                )
                .append(dst.pretty(allocator))
                .append(if let MetaValue(ref mv) = rt.as_ref() {
                    allocator.space().append(allocator.metadata(mv, false))
                } else {
                    allocator.nil()
                })
                .append(allocator.space())
                .append(allocator.text("="))
                .append(allocator.line())
                .append(
                    if let MetaValue(crate::term::MetaValue {
                        value: Some(rt), ..
                    }) = rt.as_ref()
                    {
                        rt
                    } else {
                        rt
                    }
                    .to_owned()
                    .pretty(allocator)
                    .nest(2),
                )
                .append(allocator.line())
                .append(allocator.text("in"))
                .append(allocator.line_())
                .group()
                .append(allocator.line())
                .append(body.to_owned().pretty(allocator))
                .group(),
            App(rt1, rt2) => match rt1.as_ref() {
                Op1(crate::term::UnaryOp::Ite(), _) => rt1
                    .to_owned()
                    .pretty(allocator)
                    .append(allocator.space())
                    .append(allocator.text("then"))
                    .append(allocator.line())
                    .append(rt2.to_owned().pretty(allocator).nest(2))
                    .append(allocator.line())
                    .append(allocator.text("else"))
                    .group(),
                App(..) => rt1
                    .to_owned()
                    .pretty(allocator)
                    .append(allocator.line())
                    .append(allocator.atom(rt2))
                    .group(),
                _ => allocator
                    .atom(rt1)
                    .append(allocator.line())
                    .append(allocator.atom(rt2))
                    .group(),
            },
            Var(id) => allocator.as_string(id),
            Enum(id) => allocator.text("`").append(allocator.quote_if_needed(id)),
            Record(fields, attr) => allocator
                .line()
                .append(allocator.intersperse(
                    sorted_map(fields).iter().map(|&(id, rt)| {
                        allocator
                            .quote_if_needed(id)
                            .append(allocator.space())
                            .append(if let MetaValue(mv) = rt.as_ref() {
                                allocator
                                    .metadata(mv, true)
                                    .append(allocator.space())
                                    .append(
                                        mv.value
                                            .clone()
                                            .map(|v| {
                                                allocator
                                                    .text("=")
                                                    .append(allocator.space())
                                                    .append(v.pretty(allocator))
                                            })
                                            .unwrap_or_else(|| allocator.nil()),
                                    )
                            } else {
                                allocator
                                    .text("=")
                                    .append(allocator.line())
                                    .append(rt.to_owned().pretty(allocator).nest(2))
                            })
                            .append(allocator.text(","))
                    }),
                    allocator.line(),
                ))
                .append(if attr.open {
                    allocator.line().append(allocator.text(".."))
                } else {
                    allocator.nil()
                })
                .nest(2)
                .append(allocator.line())
                .group()
                .braces(),
            RecRecord(
                fields,
                dyn_fields, /* field whose name is defined by interpolation */
                attr,
                _deps, /* dependency tracking between fields. None before the free var pass */
            ) => allocator
                .line()
                .append(
                    allocator.intersperse(
                        sorted_map(fields)
                            .iter()
                            .map(|&(id, rt)| {
                                allocator
                                    .quote_if_needed(id)
                                    .append(allocator.space())
                                    .append(if let MetaValue(mv) = rt.as_ref() {
                                        allocator
                                            .metadata(mv, true)
                                            .append(allocator.space())
                                            .append(
                                                mv.value
                                                    .clone()
                                                    .map(|v| {
                                                        allocator
                                                            .text("=")
                                                            .append(allocator.space())
                                                            .append(v.pretty(allocator))
                                                    })
                                                    .unwrap_or_else(|| allocator.nil()),
                                            )
                                    } else {
                                        allocator
                                            .text("=")
                                            .append(allocator.line())
                                            .append(rt.to_owned().pretty(allocator).nest(2))
                                    })
                                    .append(allocator.text(","))
                            })
                            .chain(dyn_fields.iter().map(|(id, rt)| {
                                id.to_owned()
                                    .pretty(allocator)
                                    .append(allocator.space())
                                    .append(if let MetaValue(mv) = rt.as_ref() {
                                        allocator
                                            .metadata(mv, true)
                                            .append(allocator.space())
                                            .append(
                                                mv.value
                                                    .clone()
                                                    .map(|v| {
                                                        allocator
                                                            .text("=")
                                                            .append(v.pretty(allocator))
                                                    })
                                                    .unwrap_or_else(|| allocator.nil()),
                                            )
                                    } else {
                                        allocator
                                            .text("=")
                                            .append(allocator.space())
                                            .append(allocator.line())
                                            .append(rt.to_owned().pretty(allocator).nest(2))
                                    })
                                    .append(allocator.text(","))
                            })),
                        allocator.line(),
                    ),
                )
                .append(if attr.open {
                    allocator.line().append(allocator.text(".."))
                } else {
                    allocator.nil()
                })
                .nest(2)
                .append(allocator.line())
                .group()
                .braces(),
            Switch(tst, cases, def) => allocator
                .text("switch")
                .append(allocator.space())
                .append(
                    allocator
                        .intersperse(
                            sorted_map(cases).iter().map(|&(id, t)| {
                                allocator
                                    .text("`")
                                    .append(allocator.quote_if_needed(id))
                                    .append(allocator.space())
                                    .append(allocator.text("=>"))
                                    .append(allocator.space())
                                    .append(t.to_owned().pretty(allocator))
                                    .append(allocator.text(","))
                            }),
                            allocator.line(),
                        )
                        .append(def.clone().map_or(allocator.nil(), |d| {
                            allocator
                                .line()
                                .append(allocator.text("_"))
                                .append(allocator.space())
                                .append(allocator.text("=>"))
                                .append(allocator.space())
                                .append(d.pretty(allocator))
                        }))
                        .nest(2)
                        .append(allocator.line_())
                        .braces()
                        .group(),
                )
                .append(allocator.space())
                .append(allocator.atom(tst)),
            Array(fields, _) => allocator
                // NOTE: the Array attributes are ignored here.
                .line()
                .append(allocator.intersperse(
                    fields.iter().map(|rt| rt.to_owned().pretty(allocator)),
                    allocator.text(",").append(allocator.line()),
                ))
                .nest(2)
                .append(allocator.line())
                .group()
                .brackets(),

            Op1(op, rt) => match op.pos() {
                crate::term::OpPos::Prefix => op.pretty(allocator).append(allocator.atom(rt)),
                crate::term::OpPos::Postfix => allocator.atom(rt).append(op.pretty(allocator)),
                crate::term::OpPos::Infix => unreachable!(),
                crate::term::OpPos::Special => {
                    use UnaryOp::*;
                    match op {
                        Ite() => allocator
                            .text("if")
                            .append(allocator.space())
                            .append(rt.to_owned().pretty(allocator)),
                        op => panic!("pretty print is not impleented for {:?}", op),
                    }
                }
            },
            Op2(op, rtl, rtr) => if op == &BinaryOp::DynAccess() {
                rtr.to_owned()
                    .pretty(allocator)
                    .append(op.pretty(allocator))
                    .append(rtl.to_owned().pretty(allocator))
            } else {
                if (&BinaryOp::Sub(), &Num(0.0)) == (op, rtl.as_ref()) {
                    allocator.text("-")
                } else if let crate::term::OpPos::Prefix = op.pos() {
                    op.pretty(allocator)
                        .append(allocator.space())
                        .append(allocator.atom(rtl))
                } else {
                    allocator
                        .atom(rtl)
                        .append(allocator.space())
                        .append(op.pretty(allocator))
                }
                .append(allocator.line())
                .append(allocator.atom(rtr))
                .nest(2)
            }
            .group(),
            OpN(op, rts) => allocator
                .as_string(op)
                .append(allocator.line())
                .append(
                    allocator
                        .intersperse(rts.iter().map(|rt| allocator.atom(rt)), allocator.line())
                        .nest(2),
                )
                .group(),

            SealingKey(sym) => allocator
                .text(format!("#<sealing key: {}>", sym))
                .append(allocator.hardline()),

            // TODO
            Sealed(_i, _rt) => allocator.text("#<sealed>").append(allocator.hardline()),

            MetaValue(mv) => mv.to_owned().pretty(allocator),
            Import(f) => allocator
                .text("import")
                .append(allocator.space())
                .append(allocator.as_string(f.to_string_lossy()).double_quotes()),
            ResolvedImport(id) => allocator.text(format!("import <file_id: {:?}>", id)),
            ParseError => allocator
                .text("#<PARSE ERROR!>")
                .append(allocator.hardline()),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for MetaValue
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        if let Some(value) = &self.value {
            allocator.atom(value).append(allocator.space())
        } else {
            allocator.nil()
        }
        .append(allocator.metadata(&self, false))
    }
}

impl<'a, D, A> Pretty<'a, D, A> for Types
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        use AbsType::*;
        match self.0 {
            Dyn() => allocator.text("Dyn"),
            Num() => allocator.text("Num"),
            Bool() => allocator.text("Bool"),
            Str() => allocator.text("Str"),
            Array(ty) => allocator
                .text("Array")
                .group()
                .append(allocator.space())
                .append(if ty.fmt_is_atom() {
                    ty.pretty(allocator)
                } else {
                    ty.pretty(allocator).nest(2).parens()
                }),
            Sym() => allocator.text("Sym"),
            Flat(t) => t.pretty(allocator),
            Var(var) => allocator.as_string(var),
            Forall(id, ref ty) => {
                let mut curr = ty.as_ref();
                let mut foralls = vec![&id];
                while let Types(Forall(id, ref ty)) = curr {
                    foralls.push(id);
                    curr = ty;
                }
                allocator
                    .text("forall")
                    .append(allocator.line())
                    .group()
                    .append(allocator.intersperse(
                        foralls.iter().map(|i| allocator.as_string(i)),
                        allocator.space(), //allocator.softline(),
                    ))
                    .append(allocator.text("."))
                    .append(allocator.line())
                    .append(curr.to_owned().pretty(allocator))
            }
            Enum(row) => row.pretty(allocator).enclose("[|", "|]"),
            StaticRecord(row) => match &row.0 {
                AbsType::Var(id) => allocator
                    .space()
                    .append(allocator.text(";"))
                    .append(allocator.space())
                    .append(allocator.as_string(id))
                    .braces(),
                _ => row.pretty(allocator).braces(),
            },
            DynRecord(ty) => allocator
                .line()
                .append(allocator.text("_"))
                .append(allocator.space())
                .append(allocator.text(":"))
                .append(allocator.space())
                .append(ty.pretty(allocator))
                .append(allocator.line())
                .braces(),
            RowEmpty() => allocator.nil(),
            RowExtend(id, ty_opt, tail) => {
                let builder = if let Some(ty) = ty_opt {
                    allocator
                        .quote_if_needed(&id)
                        .append(allocator.text(":"))
                        .append(allocator.space())
                        .append(ty.pretty(allocator))
                } else {
                    allocator.text("`").append(allocator.quote_if_needed(&id))
                };

                match tail.0 {
                    AbsType::RowEmpty() => builder,
                    AbsType::Var(_) => builder
                        .append(allocator.space())
                        .append(allocator.text(";"))
                        .append(allocator.space()),
                    AbsType::Dyn() => {
                        return builder
                            .append(allocator.space())
                            .append(allocator.text(";"))
                            .append(allocator.space())
                            .append(allocator.text("Dyn"))
                    }
                    _ => builder
                        .append(allocator.text(","))
                        .append(allocator.space()),
                }
                .append(tail.pretty(allocator))
            }
            Arrow(dom, codom) => match dom.0 {
                Arrow(..) | Forall(..) => dom
                    .pretty(allocator)
                    .parens()
                    .append(allocator.softline())
                    .append(allocator.text("->"))
                    .append(allocator.space())
                    .append(codom.pretty(allocator)),
                _ => dom
                    .pretty(allocator)
                    .append(allocator.softline())
                    .append(allocator.text("->"))
                    .append(allocator.space())
                    .append(codom.pretty(allocator)),
            },
            Wildcard(_) => allocator.text("_"),
        }
    }
}
