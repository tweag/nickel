use crate::term::{BinaryOp, MetaValue, RecordAttrs, RichTerm, Term};
use crate::types::{AbsType, Types};
pub use pretty::{DocAllocator, DocBuilder, Pretty};

impl<'a, D, A> Pretty<'a, D, A> for BinaryOp
where
    D: DocAllocator<'a, A>,
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
            op => allocator.as_string(format!("%{:?}%", op).to_lowercase()),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for RichTerm
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        use Term::*;

        match self.as_ref() {
            Null => allocator.text("null"),
            Bool(v) => allocator.as_string(v),
            Num(v) => allocator.as_string(v),
            Str(v) => allocator.as_string(v),
            StrChunks(chunks) => allocator
                .text("m%\"")
                .append(allocator.intersperse(
                    chunks.iter().map(|c| {
                        match c {
                            crate::term::StrChunk::Literal(s) => allocator.as_string(s),
                            crate::term::StrChunk::Expr(e, i) => allocator
                                .text("%{")
                                .append(e.to_owned().pretty(allocator))
                                .append(allocator.text("}")),
                        }
                    }),
                    allocator.line_(),
                ))
                .append(allocator.text("\"%m")),
            Fun(id, rt) => {
                let mut params = vec![id];
                let mut rt = rt;
                while let Fun(id, t) = rt.as_ref() {
                    params.push(id);
                    rt = t
                }
                allocator
                    .text("fun ")
                    .append(allocator.intersperse(
                        params.iter().map(|p| allocator.as_string(p)),
                        allocator.space(),
                    ))
                    .append(allocator.text(" =>"))
                    .append(allocator.softline())
                    .append(rt.to_owned().pretty(allocator).nest(2))
            }
            // TODO
            FunPattern(..) => {
                println!("pretty fun pat");
                let mut params = vec![];
                let mut rt = &self;
                while let FunPattern(id, dst, t) = rt.as_ref() {
                    params.push(
                        if let Some(id) = id {
                            allocator.as_string(id)
                        } else {
                            allocator.nil()
                        }
                        .append(allocator.nil()),
                    );
                    rt = t;
                }
                allocator
                    .text("fun ")
                    .append(allocator.intersperse(params, allocator.space()))
                    .append(allocator.text(" =>"))
                    .append(allocator.softline())
                    .append(rt.to_owned().pretty(allocator).nest(2))
            }
            Lbl(lbl) => allocator.text(format!("<label: {:?}>", lbl)),
            Let(id, rt, body, ty) => allocator
                .text("let")
                .append(allocator.space())
                .append(allocator.as_string(id))
                .append(allocator.line())
                .append(allocator.text("="))
                .append(allocator.line())
                .append(rt.to_owned().pretty(allocator).nest(2))
                .append(allocator.line())
                .append(allocator.text("in"))
                .append(allocator.line_())
                .group()
                .append(allocator.line())
                .append(body.to_owned().pretty(allocator))
                .group(),
            LetPattern(opt_id, dst, rt, body) => {
                println!("pretty let pat");
                unimplemented!()
            }
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
                _ => rt1
                    .to_owned()
                    .pretty(allocator)
                    .append(allocator.line())
                    .append(if rt2.as_ref().is_atom() {
                        rt2.to_owned().pretty(allocator)
                    } else {
                        rt2.to_owned().pretty(allocator).parens()
                    })
                    .group(),
            },
            Var(id) => allocator.as_string(id),
            Enum(id) => allocator.text(format!("`{}", id)),
            Record(fields, attr) => allocator
                .line()
                .append(allocator.intersperse(
                    fields.iter().map(|(id, rt)| {
                        allocator
                            .as_string(id)
                            .append(
                                if let MetaValue(crate::term::MetaValue { value: None, .. }) =
                                    rt.as_ref()
                                {
                                    allocator.line_()
                                } else {
                                    allocator.text(" = ")
                                },
                            )
                            .append(rt.to_owned().pretty(allocator))
                    }),
                    allocator.text(",").append(allocator.line()),
                ))
                .append(if attr.open {
                    allocator.text("...")
                } else {
                    allocator.nil()
                })
                .nest(2)
                .append(allocator.line())
                .group()
                .braces(),
            RecRecord(
                fields,
                inter_fields, /* field whose name is defined by interpolation */
                attr,
                deps, /* dependency tracking between fields. None before the free var pass */
            ) => allocator
                .line()
                .append(allocator.intersperse(
                    fields.iter().map(|(id, rt)| {
                        allocator
                            .as_string(id)
                            .append(
                                if let MetaValue(crate::term::MetaValue { value: None, .. }) =
                                    rt.as_ref()
                                {
                                    allocator.line_()
                                } else {
                                    allocator.text(" = ")
                                },
                            )
                            .append(rt.to_owned().pretty(allocator))
                    }),
                    allocator.text(",").append(allocator.line()),
                ))
                .append(if attr.open {
                    allocator.text("...")
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
                            cases.iter().map(|(id, t)| {
                                allocator
                                    .text(format!("`{} => ", id))
                                    .append(t.to_owned().pretty(allocator))
                                    .append(allocator.text(","))
                            }),
                            allocator.line(),
                        )
                        .append(allocator.line())
                        .append(allocator.text("_ => "))
                        .append(def.to_owned().pretty(allocator))
                        .nest(2)
                        .append(allocator.line_())
                        .braces()
                        .group(),
                )
                .append(allocator.space())
                .append(tst.to_owned().pretty(allocator)),

            Array(fields) => allocator
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
                crate::term::OpPos::Infix => allocator
                    .as_string(op)
                    .append(allocator.space())
                    .append(rt.to_owned().pretty(allocator)),
                crate::term::OpPos::Postfix => rt
                    .to_owned()
                    .pretty(allocator)
                    .append(allocator.space())
                    .append(allocator.as_string(op)),
                crate::term::OpPos::Special => {
                    use crate::term::UnaryOp::*;
                    match op {
                        Ite() => allocator
                            .text("if ")
                            .append(rt.to_owned().pretty(allocator)),
                        op => panic!("pretty print is not impleented for {:?}", op),
                    }
                }
            },
            Op2(op, rtl, rtr) => rtl
                .to_owned()
                .pretty(allocator)
                .append(allocator.line())
                .append(op.clone().pretty(allocator))
                .append(allocator.space())
                .append(rtr.to_owned().pretty(allocator))
                .group(),
            OpN(NAryOp, rts) => unimplemented!(),

            Sym(sym) => allocator.text(format!("<symbol: {}>", sym)),

            Wrapped(i32, RichTerm) => unimplemented!(),

            MetaValue(mv) => mv.to_owned().pretty(allocator),
            Import(f) => allocator
                .text("import ")
                .append(allocator.as_string(f.to_string_lossy()).double_quotes()),
            ResolvedImport(id) => allocator.text(format!("import <file_id: {:?}>", id)),
            ParseError => allocator.text("## PARCE ERROR! ##"),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for MetaValue
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        let mut metas = vec![];
        if let Some(value) = self.value {
            metas.push(
                value
                    .pretty(allocator)
                    .append(if let Some(types) = self.types {
                        allocator
                            .line()
                            .append(allocator.text(": "))
                            .append(types.types.pretty(allocator))
                    } else {
                        allocator.nil()
                    }),
            );
        } else {
            metas.push(allocator.nil());
        }
        metas.append(
            &mut self
                .contracts
                .iter()
                .map(|c| c.to_owned().types.pretty(allocator))
                .collect(),
        );
        if self.priority == crate::term::MergePriority::Default {
            metas.push(allocator.text("default"));
        }
        allocator.intersperse(metas, allocator.line().append(allocator.text("| ")))
    }
}

impl<'a, D, A> Pretty<'a, D, A> for Types
where
    D: DocAllocator<'a, A>,
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
                        " ", //allocator.softline(),
                    ))
                    .append(allocator.text("."))
                    .append(allocator.line())
                    .append(curr.to_owned().pretty(allocator))
            }
            Enum(row) => row.pretty(allocator).enclose("[|", "|]"),
            StaticRecord(row) => row.pretty(allocator).braces().braces(),
            DynRecord(ty) => allocator
                .line()
                .append(allocator.text("_: "))
                .append(ty.pretty(allocator))
                .append(allocator.line())
                .braces()
                .braces(),
            RowEmpty() => allocator.nil(),
            RowExtend(id, ty_opt, tail) => {
                let mut builder = allocator.as_string(id);

                builder = if let Some(ty) = ty_opt {
                    builder
                        .append(allocator.text(": "))
                        .append(ty.pretty(allocator))
                } else {
                    builder
                };

                match tail.0 {
                    AbsType::RowEmpty() => builder,
                    AbsType::Var(_) => builder.append(allocator.text(" ; ")),
                    AbsType::Dyn() => return builder.append(allocator.text(" ; Dyn")),
                    _ => builder.append(allocator.text(", ")),
                }
                .append(tail.pretty(allocator))
            }
            Arrow(dom, codom) => match dom.0 {
                Arrow(_, _) => dom
                    .pretty(allocator)
                    .parens()
                    .append(allocator.softline())
                    .append(allocator.text("-> "))
                    .append(codom.pretty(allocator)),
                _ => dom
                    .pretty(allocator)
                    .append(allocator.softline())
                    .append(allocator.text("-> "))
                    .append(codom.pretty(allocator)),
            },
        }
    }
}
