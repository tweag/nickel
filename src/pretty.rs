use crate::destruct::{self, Destruct};
use crate::identifier::Ident;
use crate::parser::lexer::KEYWORDS;

use crate::term::{
    record::{Field, FieldMetadata},
    BinaryOp, MergePriority, RichTerm, StrChunk, Term, TypeAnnotation, UnaryOp,
};
use crate::types::{EnumRows, EnumRowsF, RecordRowF, RecordRows, RecordRowsF, TypeF, Types};
pub use pretty::{DocAllocator, DocBuilder, Pretty};
use regex::Regex;
use std::collections::HashMap;

#[derive(Clone, Copy, Eq, PartialEq)]
enum StringRenderStyle {
    Monoline,
    Multiline,
}

/// Helper to find the min number of `%` sign needed to interpolate a string containing this chunk.
fn min_interpolate_sign(text: &str) -> usize {
    let reg = Regex::new(r#"([%]+\{)|("[%]+m)"#).unwrap();
    reg.find_iter(text)
        .map(|m| {
            let d = m.end() - m.start();
            // We iterate over all sequences `%+{` and `"%+`, which could clash with the interpolation
            // syntax, and return the maximum number of `%` insead each sequence.
            //
            // For the case of a closing delimiter `"%`, we could actually be slightly smarter as we
            // don't necessarily need more `%`, but just a different number of `%`. For example, if the
            // string contains only one `"%%`, then single `%` delimiters like `m%"` and `"%` would
            // be fine. But picking the maximum results in a simpler algorithm for now, which we can
            // update later if necessary.
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
    /// Add quote around an ident if it contains characters that aren't normally allowed inside
    /// bare identifiers.
    fn quote_if_needed(&'a self, id: &Ident) -> DocBuilder<'a, Self, A> {
        let reg = Regex::new("^_?[a-zA-Z][_a-zA-Z0-9-]*$").unwrap();
        if reg.is_match(id.as_ref()) && !KEYWORDS.contains(&id.as_ref()) {
            self.as_string(id)
        } else {
            self.as_string(id).double_quotes()
        }
    }

    /// Escape the special characters in a string, including the newline character, so that it can
    /// be enclosed by double quotes a be a valid Nickel string.
    fn escaped_string(&'a self, s: &str) -> DocBuilder<'a, Self, A> {
        let s = s
            .replace('\\', "\\\\")
            .replace("%{", "\\%{")
            .replace('\"', "\\\"")
            .replace('\n', "\\n");
        self.text(s)
    }

    /// Print string chunks, either in the single line or multiline style.
    fn chunks(
        &'a self,
        chunks: &[StrChunk<RichTerm>],
        string_style: StringRenderStyle,
    ) -> DocBuilder<'a, Self, A> {
        let multiline = string_style == StringRenderStyle::Multiline;
        let nb_perc = chunks
            .iter()
            .map(
                |c| {
                    if let StrChunk::Literal(s) = c {
                        min_interpolate_sign(s)
                    } else {
                        1
                    }
                }, // be sure we have at least 1 `%` sign when an interpolation is present
            )
            .max()
            .unwrap_or(1);

        let interp: String = "%".repeat(nb_perc);

        let line_maybe = if multiline {
            self.hardline()
        } else {
            self.nil()
        };

        let start_delimiter = if multiline {
            format!("m{interp}")
        } else {
            String::new()
        };

        let end_delimiter = if multiline {
            interp.clone()
        } else {
            String::new()
        };

        line_maybe
            .clone()
            .append(self.intersperse(
                chunks.iter().cloned().rev().map(|c| {
                    match c {
                        StrChunk::Literal(s) => {
                            if multiline {
                                self.intersperse(
                                    s.lines().map(|s| self.text(s.to_owned())),
                                    self.hardline().clone(),
                                )
                            } else {
                                self.escaped_string(&s)
                            }
                        }
                        StrChunk::Expr(e, _i) => self
                            .text(interp.clone())
                            .append(self.text("{"))
                            .append(e.pretty(self))
                            .append(self.text("}")),
                    }
                }),
                self.nil(),
            ))
            .append(line_maybe)
            .double_quotes()
            .enclose(start_delimiter, end_delimiter)
    }

    fn field_metadata(
        &'a self,
        metadata: &FieldMetadata,
        with_doc: bool,
    ) -> DocBuilder<'a, Self, A> {
        self.annot_part(&metadata.annotation)
            .append(if with_doc {
                metadata
                    .doc
                    .clone()
                    .map(|doc| {
                        self.line()
                            .append(self.text("|"))
                            .append(self.space())
                            .append(self.text("doc"))
                            .append(self.space())
                            .append(
                                self.chunks(
                                    &[StrChunk::Literal(doc)],
                                    StringRenderStyle::Multiline,
                                ),
                            )
                    })
                    .unwrap_or_else(|| self.nil())
            } else {
                self.nil()
            })
            .append(if metadata.opt {
                self.line().append(self.text("| optional"))
            } else {
                self.nil()
            })
            .append(match metadata.priority {
                MergePriority::Bottom => self.line().append(self.text("| default")),
                MergePriority::Neutral => self.nil(),
                MergePriority::Numeral(p) => self
                    .line()
                    .append(self.text("| priority"))
                    .append(self.space())
                    .append(self.as_string(p)),
                MergePriority::Top => self.line().append(self.text("| force")),
            })
            .nest(2)
            .group()
    }

    fn field(&'a self, id: &Ident, field: &Field, with_doc: bool) -> DocBuilder<'a, Self, A> {
        self.quote_if_needed(id)
            .append(self.field_body(field, with_doc))
    }

    fn dyn_field(
        &'a self,
        id_expr: &RichTerm,
        field: &Field,
        with_doc: bool,
    ) -> DocBuilder<'a, Self, A> {
        id_expr
            .clone()
            .pretty(self)
            .append(self.field_body(field, with_doc))
    }

    fn field_body(&'a self, field: &Field, with_doc: bool) -> DocBuilder<'a, Self, A> {
        self.field_metadata(&field.metadata, with_doc)
            .append(if let Some(ref value) = field.value {
                let has_metadata = field.metadata != FieldMetadata::default();

                if has_metadata {
                    self.line()
                } else {
                    self.space()
                }
                .append(
                    self.text("=")
                        .append(self.softline())
                        .append(value.to_owned().pretty(self).nest(2)),
                )
                .nest(2)
            } else {
                self.nil()
            })
            .append(self.text(","))
    }

    fn fields(&'a self, fields: &HashMap<Ident, Field>, with_doc: bool) -> DocBuilder<'a, Self, A> {
        self.intersperse(
            sorted_map(fields)
                .iter()
                .map(|&(id, field)| self.field(id, field, with_doc)),
            self.line(),
        )
    }

    fn dyn_fields(
        &'a self,
        fields: &[(RichTerm, Field)],
        with_doc: bool,
    ) -> DocBuilder<'a, Self, A> {
        self.intersperse(
            fields
                .iter()
                .map(|(ref id_term, ref field)| self.dyn_field(id_term, field, with_doc)),
            self.line(),
        )
    }

    fn annot(&'a self, annot: &TypeAnnotation) -> DocBuilder<'a, Self, A> {
        self.annot_part(annot).nest(2).group()
    }

    fn annot_part(&'a self, annot: &TypeAnnotation) -> DocBuilder<'a, Self, A> {
        if let Some(types) = &annot.types {
            self.line()
                .append(self.text(":"))
                .append(self.space())
                .append(types.types.clone().pretty(self))
        } else {
            self.nil()
        }
        .append(if !annot.contracts.is_empty() {
            self.line()
        } else {
            self.nil()
        })
        .append(self.intersperse(
            annot.contracts.iter().map(|c| {
                self.text("|")
                    .append(self.space())
                    .append(c.to_owned().types.pretty(self))
            }),
            self.line(),
        ))
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
                        destruct::Match::Simple(id, field) => allocator
                            .as_string(id)
                            .append(allocator.space())
                            .append(match field {
                                Field {
                                    value: Some(value),
                                    metadata:
                                        FieldMetadata {
                                            annotation,
                                            priority: MergePriority::Bottom,
                                            ..
                                        },
                                } => allocator
                                    .text("?")
                                    .append(allocator.space())
                                    .append(allocator.atom(value))
                                    .append(allocator.field_metadata(
                                        &FieldMetadata {
                                            annotation: annotation.clone(),
                                            ..Default::default()
                                        },
                                        false,
                                    )),
                                field => allocator.field_metadata(&field.metadata, false),
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
            Str(v) => allocator.escaped_string(v).double_quotes(),
            StrChunks(chunks) => allocator.chunks(
                chunks,
                if chunks.len() > 1 {
                    StringRenderStyle::Multiline
                } else {
                    StringRenderStyle::Monoline
                },
            ),
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
                .append(if let Annotated(ref annot, _) = rt.as_ref() {
                    allocator.space().append(allocator.annot(annot))
                } else {
                    allocator.nil()
                })
                .append(allocator.space())
                .append(allocator.text("="))
                .append(allocator.line())
                .append(
                    if let Annotated(_, inner) = rt.as_ref() {
                        inner
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
                    (*opt_id)
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
                .append(if let Annotated(ref annot, _) = rt.as_ref() {
                    allocator.space().append(allocator.annot(annot))
                } else {
                    allocator.nil()
                })
                .append(allocator.space())
                .append(allocator.text("="))
                .append(allocator.line())
                .append(
                    if let Annotated(_, inner) = rt.as_ref() {
                        inner
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
            Record(record) => allocator
                .line()
                .append(allocator.fields(&record.fields, true))
                .append(if record.attrs.open {
                    allocator.line().append(allocator.text(".."))
                } else {
                    allocator.nil()
                })
                .nest(2)
                .append(allocator.line())
                .group()
                .braces(),
            RecRecord(record_data, dyn_fields, _) => allocator
                .line()
                .append(allocator.fields(&record_data.fields, true))
                .append(allocator.dyn_fields(dyn_fields, true))
                .append(if record_data.attrs.open {
                    allocator.line().append(allocator.text(".."))
                } else {
                    allocator.nil()
                })
                .nest(2)
                .append(allocator.line())
                .group()
                .braces(),
            Match { cases, default } => allocator.text("match").append(allocator.space()).append(
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
                    .append(default.clone().map_or(allocator.nil(), |d| {
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
            ),
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
            Sealed(_i, _rt, _lbl) => allocator.text("#<sealed>").append(allocator.hardline()),
            Annotated(annot, rt) => allocator
                .atom(rt)
                .append(allocator.space())
                .append(allocator.annot(annot)),
            Import(f) => allocator
                .text("import")
                .append(allocator.space())
                .append(allocator.as_string(f.to_string_lossy()).double_quotes()),
            ResolvedImport(id) => allocator.text(format!("import <file_id: {:?}>", id)),
            ParseError(_) => allocator.text("%<PARSE ERROR>"),
            RuntimeError(_) => allocator.text("%<RUNTIME ERROR>"),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for EnumRows
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        match self.0 {
            EnumRowsF::Empty => allocator.nil(),
            EnumRowsF::TailVar(id) => allocator
                .space()
                .append(allocator.text(";"))
                .append(allocator.space())
                .append(allocator.as_string(id)),
            EnumRowsF::Extend { row, tail } => {
                let builder = allocator.text("`").append(allocator.quote_if_needed(&row));
                let builder = if let EnumRowsF::Extend { .. } = tail.0 {
                    builder
                        .append(allocator.text(","))
                        .append(allocator.space())
                } else {
                    builder
                };

                builder.append(tail.pretty(allocator))
            }
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for RecordRows
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        match self.0 {
            RecordRowsF::Empty => allocator.nil(),
            RecordRowsF::TailDyn => allocator
                .space()
                .append(allocator.text(";"))
                .append(allocator.space())
                .append(allocator.text("Dyn")),
            RecordRowsF::TailVar(id) => allocator
                .space()
                .append(allocator.text(";"))
                .append(allocator.space())
                .append(allocator.as_string(id)),
            RecordRowsF::Extend {
                row: RecordRowF { id, types },
                tail,
            } => {
                let builder = allocator
                    .quote_if_needed(&id)
                    .append(allocator.text(":"))
                    .append(allocator.space())
                    .append(types.pretty(allocator));

                let builder = if let RecordRowsF::Extend { .. } = tail.0 {
                    builder
                        .append(allocator.text(","))
                        .append(allocator.space())
                } else {
                    builder
                };

                builder.append(tail.pretty(allocator))
            }
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for Types
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        use TypeF::*;
        match self.0 {
            Dyn => allocator.text("Dyn"),
            Num => allocator.text("Num"),
            Bool => allocator.text("Bool"),
            Str => allocator.text("Str"),
            Array(ty) => allocator
                .text("Array")
                .group()
                .append(allocator.space())
                .append(if ty.fmt_is_atom() {
                    ty.pretty(allocator)
                } else {
                    ty.pretty(allocator).nest(2).parens()
                }),
            Sym => allocator.text("Sym"),
            Flat(t) => t.pretty(allocator),
            Var(var) => allocator.as_string(var),
            Forall { var, ref body, .. } => {
                let mut curr = body.as_ref();
                let mut foralls = vec![&var];
                while let Types(Forall { var, ref body, .. }) = curr {
                    foralls.push(var);
                    curr = body;
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
            Enum(erows) => erows.pretty(allocator).enclose("[|", "|]"),
            Record(rrows) => rrows.pretty(allocator).braces(),
            Dict(ty) => allocator
                .line()
                .append(allocator.text("_"))
                .append(allocator.space())
                .append(allocator.text(":"))
                .append(allocator.space())
                .append(ty.pretty(allocator))
                .append(allocator.line())
                .braces(),
            Arrow(dom, codom) => match dom.0 {
                Arrow(..) | Forall { .. } => dom
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
