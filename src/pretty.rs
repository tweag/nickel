use crate::term::{MetaValue, RecordAttrs, RichTerm, Term};
use crate::types::{AbsType, Types};
pub use pretty::{DocAllocator, DocBuilder, Pretty};
use std::io;
use std::str;

impl<'a, D, A> Pretty<'a, D, A> for RichTerm
where
    D: DocAllocator<'a, A>,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        use Term::*;

        match self.as_ref() {
            Null => allocator.text("null"),
            Bool(v) => allocator.as_string(v),
            Num(v) => allocator.as_string(v),
            Str(v) => allocator.as_string(v),
            // TODO
            StrChunks(chunks) => unimplemented!(),
            Fun(id, rt) => {
                println!("pretty fun");
                let mut params = vec![id];
                let mut rt = rt;
                while let Fun(id, t) = rt.as_ref() {
                    params.push(id);
                    rt = t
                }
                allocator
                    .text("fun ")
                    .append(
                        allocator.intersperse(params.iter().map(|p| allocator.as_string(p)), " "),
                    )
                    .append(allocator.text(" =>"))
                    .append(allocator.softline())
                    .append(rt.to_owned().pretty(allocator))
            }
            // TODO
            FunPattern(id, dst, rt) => unimplemented!(),
            Lbl(lbl) => allocator.nil(),
            Let(id, rt, body, ty) => allocator
                .text("let")
                .append(allocator.as_string(id))
                .append(allocator.text("=")), // TODO
            LetPattern(opt_id, dst, rt, body) => unimplemented!(),
            App(rt1, rt2) => unimplemented!(),
            Var(id) => allocator.as_string(id),
            Enum(id) => allocator.text(format!("`{}", id)),
            Record(fields, atr) => allocator
                .text("{")
                .append(allocator.intersperse(
                    fields.iter().map(|(id, rt)| {
                        allocator
                            .as_string(id)
                            .append(allocator.text(" = "))
                            .append(rt.to_owned().pretty(allocator))
                            .append(allocator.text(","))
                            .append(allocator.line_())
                    }),
                    "",
                ))
                .append(if atr.open {
                    allocator.text("...")
                } else {
                    allocator.nil()
                })
                .append(allocator.text("}")),
            RecRecord(
                fields,
                inter_fields, /* field whose name is defined by interpolation */
                attr,
                deps, /* dependency tracking between fields. None before the free var pass */
            ) => unimplemented!(), //TODO
            Switch(tst, cases, def) => unimplemented!(),

            Array(fields) => allocator
                .text("[")
                .append(
                    allocator
                        .intersperse(fields.iter().map(|rt| rt.to_owned().pretty(allocator)), ","),
                )
                .append(allocator.text("]")),

            Op1(UnaryOp, RichTerm) => unimplemented!(),
            Op2(BinaryOp, rtl, rtr) => unimplemented!(),
            OpN(NAryOp, rts) => unimplemented!(),

            Sym(i32) => unimplemented!(),

            Wrapped(i32, RichTerm) => unimplemented!(),

            MetaValue(mv) => mv.to_owned().pretty(allocator),
            Import(f) => allocator
                .text("import ")
                .append(allocator.as_string(f.to_string_lossy()).double_quotes()),
            ResolvedImport(FileId) => unimplemented!(),
            ParseError => allocator.text("## PARCE ERROR! ##"),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for MetaValue
where
    D: DocAllocator<'a, A>,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        match self.value {
            Some(rt) => rt.pretty(allocator),
            None => allocator.nil(),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for Types
where
    D: DocAllocator<'a, A>,
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
            Flat(ref t) => allocator.text("{}"), //t.as_ref().shallow_repr()),
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
            Enum(row) => row.pretty(allocator).enclose("|", "|").brackets(),
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
