use std::fmt;

use crate::identifier::LocIdent;
use crate::parser::lexer::KEYWORDS;
use crate::term::{
    pattern::*,
    record::{Field, FieldMetadata, RecordData},
    *,
};
use crate::typ::*;

use malachite::num::{basic::traits::Zero, conversion::traits::ToSci};
use once_cell::sync::Lazy;
use pretty::docs;
pub use pretty::{DocAllocator, DocBuilder, Pretty};
use regex::Regex;

#[derive(Clone, Copy, Eq, PartialEq)]
enum StringRenderStyle {
    /// Never allow rendering as a multiline string
    ForceMonoline,
    /// Render as a multiline string if the string contains a newline
    Multiline,
}

/// Helper to find the min number of `%` sign needed to interpolate a string containing this chunk.
fn min_interpolate_sign(text: &str) -> usize {
    let reg = Regex::new(r#"([%]+\{)|("[%]+)"#).unwrap();
    reg.find_iter(text)
        .map(|m| {
            // We iterate over all sequences `%+{` and `"%+`, which could clash with the
            // interpolation syntax, and return the maximum number of `%` insead each sequence.
            //
            // For the case of a closing delimiter `"%`, we could actually be slightly smarter as we
            // don't necessarily need more `%`, but just a different number of `%`. For example, if
            // the string contains only one `"%%`, then single `%` delimiters like `m%"` and `"%`
            // would be fine. But picking the maximum results in a simpler algorithm for now, which
            // we can update later if necessary.
            m.end() - m.start()
        })
        .max()
        .unwrap_or(1)
}

fn sorted_map<K: Ord, V>(m: &'_ IndexMap<K, V>) -> Vec<(&'_ K, &'_ V)> {
    let mut ret: Vec<(&K, &V)> = m.iter().collect();
    ret.sort_by_key(|(k, _)| *k);
    ret
}

/// Escape a string to make it suitable for placing between quotes in Nickel
fn escape(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace("%{", "\\%{")
        .replace('\"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
}

static QUOTING_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new("^_*[a-zA-Z][_a-zA-Z0-9-]*$").unwrap());

/// Return the string representation of an identifier, and add enclosing double quotes if the
/// label isn't a valid identifier according to the parser, for example if it contains a
/// special character like a space.
pub fn ident_quoted(ident: &LocIdent) -> String {
    let label = ident.label();
    if QUOTING_REGEX.is_match(label) && !KEYWORDS.contains(&label) {
        String::from(label)
    } else {
        format!("\"{}\"", escape(label))
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
enum RecursivePriority {
    Default,
    Force,
    None,
}

impl RecursivePriority {
    fn is_present(&self) -> bool {
        !matches!(self, RecursivePriority::None)
    }
}

/// We need the field's value to inspect it for an application of
/// `$rec_default` or `$rec_force`. The recursive priority metadata
/// annotations `rec default` and `rec force` are converted at parsing
/// time into applications of the internal symbols `$rec_default` and
/// `$rec_force` respectively. These symbols are not valid identifiers in
/// Nickel's surface syntax, so we need to handle them specially.
///
/// If we find a recursive priority annotation we return the field's value
/// with the internal application removed.
fn split_recursive_priority(value: &RichTerm) -> (RecursivePriority, RichTerm) {
    if let Term::App(f, x) = value.as_ref() {
        match f.as_ref() {
            Term::Var(id) if id.label() == "$rec_default" => {
                return (RecursivePriority::Default, x.clone());
            }
            Term::Var(id) if id.label() == "$rec_force" => {
                return (RecursivePriority::Force, x.clone());
            }
            _ => (),
        }
    };
    (RecursivePriority::None, value.clone())
}

/// Does a sequence of `StrChunk`s contain a literal newline?
fn contains_newline<T>(chunks: &[StrChunk<T>]) -> bool {
    chunks.iter().any(|chunk| match chunk {
        StrChunk::Literal(str) => str.contains('\n'),
        StrChunk::Expr(_, _) => false,
    })
}

/// Does a sequence of `StrChunk`s contain a carriage return? Lone carriage
/// returns are forbidden in Nickel's surface syntax.
fn contains_carriage_return<T>(chunks: &[StrChunk<T>]) -> bool {
    chunks.iter().any(|chunk| match chunk {
        StrChunk::Literal(str) => str.contains('\r'),
        StrChunk::Expr(_, _) => false,
    })
}

/// Determines if a type to be printed in type position needs additional parentheses.
///
/// Terms in type position don't need to be atoms: for example, we should pretty print `foo |
/// Contract arg1 arg2` without parentheses instead of `foo | (Contract arg1 arg2)`.
///
/// However, some terms (i.e. contracts) in type position still need parentheses in some cases, for
/// example when said term is a function function. This function precisely determines if the given
/// type is such a term.
fn needs_parens_in_type_pos(typ: &Type) -> bool {
    if let TypeF::Flat(term) = &typ.typ {
        matches!(
            term.as_ref(),
            Term::Fun(..)
                | Term::FunPattern(..)
                | Term::Let(..)
                | Term::LetPattern(..)
                | Term::Op1(UnaryOp::IfThenElse, _)
                | Term::Import(..)
                | Term::ResolvedImport(..)
        )
    } else {
        false
    }
}

pub fn fmt_pretty<'a, T>(value: &T, f: &mut fmt::Formatter) -> fmt::Result
where
    T: Pretty<'a, pretty::BoxAllocator, ()> + Clone,
{
    let doc: DocBuilder<_, ()> = value.clone().pretty(&pretty::BoxAllocator);
    doc.render_fmt(80, f)
}

impl<'a, A: Clone + 'a> NickelAllocatorExt<'a, A> for pretty::BoxAllocator {}

trait NickelAllocatorExt<'a, A: 'a>: DocAllocator<'a, A> + Sized
where
    Self::Doc: Clone,
    A: Clone,
{
    /// Escape the special characters in a string, including the newline character, so that it can
    /// be enclosed by double quotes a be a valid Nickel string.
    fn escaped_string(&'a self, s: &str) -> DocBuilder<'a, Self, A> {
        self.text(escape(s))
    }

    /// Print string chunks, either in the single line or multiline style.
    fn chunks(
        &'a self,
        chunks: &[StrChunk<RichTerm>],
        string_style: StringRenderStyle,
    ) -> DocBuilder<'a, Self, A> {
        let multiline = string_style == StringRenderStyle::Multiline
            && contains_newline(chunks)
            && !contains_carriage_return(chunks);

        let nb_perc = if multiline {
            chunks
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
                .unwrap_or(1)
        } else {
            1
        };

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
            .append(self.concat(chunks.iter().rev().map(|c| {
                match c {
                    StrChunk::Literal(s) => {
                        if multiline {
                            self.concat(
                                // We do this manually instead of using
                                // `str::lines` because we need to be careful
                                // about whether a trailing newline appears at
                                // the end of the last line.
                                s.split_inclusive('\n').map(|line| {
                                    if let Some(s) = line.strip_suffix('\n') {
                                        self.text(s.to_owned()).append(self.hardline())
                                    } else {
                                        self.text(line.to_owned())
                                    }
                                }),
                            )
                        } else {
                            self.escaped_string(s)
                        }
                    }
                    StrChunk::Expr(e, _i) => docs![self, interp.clone(), "{", e, "}"],
                }
            })))
            .nest(if multiline { 2 } else { 0 })
            .append(line_maybe)
            .double_quotes()
            .enclose(start_delimiter, end_delimiter)
    }

    fn field_metadata(
        &'a self,
        metadata: &FieldMetadata,
        with_doc: bool,
    ) -> DocBuilder<'a, Self, A> {
        docs![
            self,
            &metadata.annotation,
            if with_doc {
                metadata
                    .doc
                    .clone()
                    .map(|doc| {
                        docs![
                            self,
                            self.line(),
                            "| doc ",
                            self.chunks(&[StrChunk::Literal(doc)], StringRenderStyle::Multiline),
                        ]
                    })
                    .unwrap_or_else(|| self.nil())
            } else {
                self.nil()
            },
            if metadata.opt {
                docs![self, self.line(), "| optional"]
            } else {
                self.nil()
            },
            match &metadata.priority {
                MergePriority::Bottom => docs![self, self.line(), "| default"],
                MergePriority::Neutral => self.nil(),
                MergePriority::Numeral(p) =>
                    docs![self, self.line(), "| priority ", p.to_sci().to_string()],
                MergePriority::Top => docs![self, self.line(), "| force"],
            }
        ]
    }

    fn field(&'a self, id: &LocIdent, field: &Field) -> DocBuilder<'a, Self, A> {
        self.text(ident_quoted(id))
            .append(self.field_body(field))
            .group()
    }

    fn dyn_field(&'a self, id_expr: &RichTerm, field: &Field) -> DocBuilder<'a, Self, A> {
        match id_expr.as_ref() {
            // Nickel will not parse a multiline string literal in this position
            Term::StrChunks(chunks) => self.chunks(chunks, StringRenderStyle::ForceMonoline),
            _ => unimplemented!("Dynamic record fields must be StrChunks currently"),
        }
        .append(self.field_body(field))
        .group()
    }

    fn field_body(&'a self, field: &Field) -> DocBuilder<'a, Self, A> {
        docs![
            self,
            self.field_metadata(&field.metadata, true),
            if let Some((priority, value)) = field.value.as_ref().map(split_recursive_priority) {
                let has_metadata =
                    field.metadata != FieldMetadata::default() || priority.is_present();

                docs![
                    self,
                    priority,
                    if has_metadata {
                        docs![self, self.line(), "= "]
                    } else {
                        docs![self, " =", self.line()]
                    },
                    value.pretty(self).nest(2)
                ]
            } else {
                self.nil()
            },
            ","
        ]
        .nest(2)
    }

    fn fields(&'a self, fields: &IndexMap<LocIdent, Field>) -> DocBuilder<'a, Self, A> {
        self.intersperse(
            sorted_map(fields)
                .iter()
                .map(|(id, field)| self.field(id, field)),
            self.line(),
        )
    }

    fn dyn_fields(&'a self, fields: &[(RichTerm, Field)]) -> DocBuilder<'a, Self, A> {
        self.intersperse(
            fields
                .iter()
                .map(|(id_term, field)| self.dyn_field(id_term, field)),
            self.line(),
        )
    }

    fn record(
        &'a self,
        record_data: &RecordData,
        dyn_fields: &[(RichTerm, Field)],
    ) -> DocBuilder<'a, Self, A> {
        // Print empty non-open records specially to avoid double newlines and extra spaces
        if record_data.fields.is_empty() && dyn_fields.is_empty() && !record_data.attrs.open {
            return self.text("{}");
        }

        docs![
            self,
            self.line(),
            self.fields(&record_data.fields),
            if !dyn_fields.is_empty() {
                docs![self, self.line(), self.dyn_fields(dyn_fields)]
            } else {
                self.nil()
            },
            if record_data.attrs.open {
                docs![self, self.line(), ".."]
            } else {
                self.nil()
            }
        ]
        .nest(2)
        .append(self.line())
        .braces()
        .group()
    }

    fn atom(&'a self, rt: &RichTerm) -> DocBuilder<'a, Self, A> {
        rt.pretty(self).parens_if(!rt.as_ref().is_atom())
    }

    /// Almost identical to calling `typ.pretty(self)`, but adds parentheses when the type is
    /// actually a contract that has a top-level form that needs parentheses (let-binding,
    /// if-then-else, etc.).
    ///
    /// Although terms can appear in type position as contracts, the parenthesis rules are slightly
    /// more restrictive than for a generic term: for example, `{foo | let x = Contract in x}` is
    /// not valid Nickel. It must be parenthesised as `{foo | (let x = Contract in x)}`.
    ///
    /// This method must be used whenever a type is rendered either as component of another type or
    /// in the position of an annotation. Rendering stand-alone types (for example as part of error
    /// messages) can avoid those parentheses and directly call to `typ.pretty(allocator)` instead.
    fn type_part(&'a self, typ: &Type) -> DocBuilder<'a, Self, A> {
        typ.pretty(self).parens_if(needs_parens_in_type_pos(typ))
    }

    /// Pretty printing of a restricted patterns that requires enum variant patterns and or
    /// patterns to be parenthesized (typically function pattern arguments). The only difference
    /// with a general pattern is that for a function, a top-level enum variant pattern with an
    /// enum tag as an argument such as `'Foo 'Bar` must be parenthesized, because `fun 'Foo 'Bar
    /// => ...` is parsed as a function of two arguments, which are bare enum tags `'Foo` and
    /// `'Bar`. We must print `fun ('Foo 'Bar) => ..` instead.
    fn pat_with_parens(&'a self, pattern: &Pattern) -> DocBuilder<'a, Self, A> {
        pattern.pretty(self).parens_if(matches!(
            pattern.data,
            PatternData::Enum(EnumPattern {
                pattern: Some(_),
                ..
            }) | PatternData::Or(_)
        ))
    }
}

trait NickelDocBuilderExt<'a, D, A> {
    /// Call `self.parens()` but only if `parens` is `true`.
    fn parens_if(self, parens: bool) -> Self;
}

impl<'a, D: DocAllocator<'a, A>, A> NickelDocBuilderExt<'a, D, A> for DocBuilder<'a, D, A> {
    fn parens_if(self, parens: bool) -> Self {
        if parens {
            self.parens()
        } else {
            self
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for RecursivePriority
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        match self {
            RecursivePriority::Default => allocator.text("| rec default"),
            RecursivePriority::Force => allocator.text("| rec force"),
            RecursivePriority::None => allocator.nil(),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &TypeAnnotation
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        docs![
            allocator,
            if let Some(typ) = &self.typ {
                docs![
                    allocator,
                    allocator.line(),
                    ": ",
                    allocator.type_part(&typ.typ)
                ]
            } else {
                allocator.nil()
            },
            if !self.contracts.is_empty() {
                allocator.line()
            } else {
                allocator.nil()
            },
            allocator.intersperse(
                self.contracts
                    .iter()
                    .map(|c| { docs![allocator, "| ", allocator.type_part(&c.typ)] }),
                allocator.line(),
            )
        ]
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
            BoolNot => allocator.text("!"),
            BoolAnd | BoolOr | RecordAccess(_) => {
                unreachable!(
                    "These are handled specially since they are actually encodings \
                    of binary operators (`BoolAnd` and `BoolOr`) or need special \
                    formatting (`StaticAccess`). This currently happens in the `App` \
                    branch of `Term::pretty`"
                )
            }
            EnumEmbed(id) => docs![
                allocator,
                "%enum/embed%",
                docs![allocator, allocator.line(), id.to_string()].nest(2)
            ],
            op => allocator.text(format!("%{op}%")).append(allocator.space()),
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
            Plus => allocator.text("+"),
            Sub => allocator.text("-"),

            Mult => allocator.text("*"),
            Div => allocator.text("/"),
            Modulo => allocator.text("%"),

            Eq => allocator.text("=="),
            LessThan => allocator.text("<"),
            GreaterThan => allocator.text(">"),
            GreaterOrEq => allocator.text(">="),
            LessOrEq => allocator.text("<="),

            Merge(_) => allocator.text("&"),

            StringConcat => allocator.text("++"),
            ArrayConcat => allocator.text("@"),

            RecordGet => allocator.text("."),

            op => allocator.as_string(format!("%{op}%")),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &NAryOp
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        allocator.as_string(format!("%{self}%"))
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &Pattern
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        let alias_prefix = if let Some(alias) = self.alias {
            docs![
                allocator,
                alias.to_string(),
                allocator.space(),
                "@",
                allocator.space()
            ]
        } else {
            allocator.nil()
        };

        docs![allocator, alias_prefix, &self.data]
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &PatternData
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        match self {
            PatternData::Wildcard => allocator.text("_"),
            PatternData::Any(id) => allocator.as_string(id),
            PatternData::Record(rp) => rp.pretty(allocator),
            PatternData::Array(ap) => ap.pretty(allocator),
            PatternData::Enum(evp) => evp.pretty(allocator),
            PatternData::Constant(cp) => cp.pretty(allocator),
            PatternData::Or(op) => op.pretty(allocator),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &ConstantPattern
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        self.data.pretty(allocator)
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &ConstantPatternData
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        match self {
            ConstantPatternData::Bool(b) => allocator.as_string(b),
            ConstantPatternData::Number(n) => allocator.as_string(format!("{}", n.to_sci())),
            ConstantPatternData::String(s) => allocator.escaped_string(s).double_quotes(),
            ConstantPatternData::Null => allocator.text("null"),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &EnumPattern
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        docs![
            allocator,
            "'",
            ident_quoted(&self.tag),
            if let Some(ref arg_pat) = self.pattern {
                docs![
                    allocator,
                    allocator.line(),
                    allocator.pat_with_parens(arg_pat)
                ]
            } else {
                allocator.nil()
            }
        ]
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &RecordPattern
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        let RecordPattern {
            patterns: matches,
            tail,
            ..
        } = self;
        docs![
            allocator,
            allocator.line(),
            allocator.intersperse(
                matches.iter().map(|field_pat| {
                    docs![
                        allocator,
                        field_pat.matched_id.to_string(),
                        allocator.field_metadata(
                            &FieldMetadata {
                                annotation: field_pat.annotation.clone(),
                                ..Default::default()
                            },
                            false
                        ),
                        if let Some(default) = field_pat.default.as_ref() {
                            docs![allocator, allocator.line(), "? ", allocator.atom(default),]
                        } else {
                            allocator.nil()
                        },
                        match &field_pat.pattern.data {
                            PatternData::Any(id) if *id == field_pat.matched_id => allocator.nil(),
                            _ => docs![allocator, allocator.line(), "= ", &field_pat.pattern],
                        },
                        ","
                    ]
                    .nest(2)
                }),
                allocator.line()
            ),
            match tail {
                TailPattern::Empty => allocator.nil(),
                TailPattern::Open => docs![allocator, allocator.line(), ".."],
                TailPattern::Capture(id) =>
                    docs![allocator, allocator.line(), "..", id.ident().to_string()],
            },
        ]
        .nest(2)
        .append(allocator.line())
        .braces()
        .group()
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &ArrayPattern
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        docs![
            allocator,
            allocator.intersperse(
                self.patterns.iter(),
                docs![allocator, ",", allocator.line()],
            ),
            if !self.patterns.is_empty() && self.is_open() {
                docs![allocator, ",", allocator.line()]
            } else {
                allocator.nil()
            },
            match self.tail {
                TailPattern::Empty => allocator.nil(),
                TailPattern::Open => allocator.text(".."),
                TailPattern::Capture(id) => docs![allocator, "..", id.ident().to_string()],
            },
        ]
        .nest(2)
        .brackets()
        .group()
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &OrPattern
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        docs![
            allocator,
            allocator.intersperse(
                self.patterns
                    .iter()
                    .map(|pat| allocator.pat_with_parens(pat)),
                docs![allocator, allocator.line(), "or", allocator.space()],
            ),
        ]
        .group()
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &RichTerm
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        self.as_ref().pretty(allocator)
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &Term
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        use Term::*;

        match self {
            Null => allocator.text("null"),
            Bool(v) => allocator.as_string(v),
            Num(n) => allocator.as_string(format!("{}", n.to_sci())),
            Str(v) => allocator.escaped_string(v).double_quotes(),
            StrChunks(chunks) => allocator.chunks(chunks, StringRenderStyle::Multiline),
            Fun(id, rt) => {
                let mut params = vec![id];
                let mut rt = rt;
                while let Fun(id, t) = rt.as_ref() {
                    params.push(id);
                    rt = t
                }
                docs![
                    allocator,
                    "fun",
                    allocator.line(),
                    allocator.intersperse(
                        params.iter().map(|p| allocator.as_string(p)),
                        allocator.line()
                    ),
                    allocator.line(),
                    "=>",
                    allocator.line(),
                    rt
                ]
                .nest(2)
                .group()
            }
            FunPattern(..) => {
                let mut params = vec![];
                let mut rt = self;
                while let FunPattern(pat, t) = rt {
                    params.push(allocator.pat_with_parens(pat));
                    rt = t.as_ref();
                }
                docs![
                    allocator,
                    "fun",
                    allocator.line(),
                    allocator.intersperse(params, allocator.line()),
                    allocator.line(),
                    "=>",
                    allocator.line(),
                    rt
                ]
                .nest(2)
                .group()
            }
            Lbl(_lbl) => allocator.text("%<label>").append(allocator.line()),
            Let(id, rt, body, attrs) => docs![
                allocator,
                "let ",
                if attrs.rec {
                    allocator.text("rec ")
                } else {
                    allocator.nil()
                },
                id.to_string(),
                if let Annotated(annot, _) = rt.as_ref() {
                    annot.pretty(allocator)
                } else {
                    allocator.nil()
                },
                allocator.line(),
                "= ",
                if let Annotated(_, inner) = rt.as_ref() {
                    inner.pretty(allocator)
                } else {
                    rt.pretty(allocator)
                },
                allocator.line(),
                "in",
            ]
            .nest(2)
            .append(allocator.line())
            .append(body.pretty(allocator).nest(2))
            .group(),
            LetPattern(pattern, rt, body) => docs![
                allocator,
                "let ",
                pattern,
                if let Annotated(annot, _) = rt.as_ref() {
                    annot.pretty(allocator)
                } else {
                    allocator.nil()
                },
                allocator.line(),
                "= ",
                if let Annotated(_, inner) = rt.as_ref() {
                    inner
                } else {
                    rt
                },
                allocator.line(),
                "in",
            ]
            .nest(2)
            .append(allocator.line())
            .append(body.pretty(allocator).nest(2))
            .group(),
            App(rt1, rt2) => match rt1.as_ref() {
                App(iop, t) if matches!(iop.as_ref(), Op1(UnaryOp::IfThenElse, _)) => {
                    match iop.as_ref() {
                        Op1(UnaryOp::IfThenElse, i) => docs![
                            allocator,
                            "if ",
                            i,
                            " then",
                            docs![allocator, allocator.line(), t].nest(2),
                            allocator.line(),
                            "else",
                            docs![allocator, allocator.line(), rt2].nest(2)
                        ]
                        .group(),
                        _ => unreachable!(),
                    }
                }
                Op1(op @ (UnaryOp::BoolAnd | UnaryOp::BoolOr), rt1) => docs![
                    allocator,
                    allocator.atom(rt1),
                    allocator.line(),
                    match op {
                        UnaryOp::BoolAnd => "&& ",
                        UnaryOp::BoolOr => "|| ",
                        _ => unreachable!(),
                    },
                    allocator.atom(rt2)
                ]
                .group(),
                App(..) => docs![
                    allocator,
                    rt1,
                    docs![allocator, allocator.line(), allocator.atom(rt2)]
                        .nest(2)
                        .group()
                ],
                _ => docs![
                    allocator,
                    allocator.atom(rt1),
                    docs![allocator, allocator.line(), allocator.atom(rt2)]
                        .nest(2)
                        .group()
                ],
            },
            Var(id) => allocator.as_string(id),
            Enum(id) => allocator.text("'").append(allocator.text(ident_quoted(id))),
            EnumVariant { tag, arg, attrs: _ } => allocator
                .text("'")
                .append(allocator.text(ident_quoted(tag)))
                .append(
                    docs![allocator, allocator.line(), allocator.atom(arg)]
                        .nest(2)
                        .group(),
                ),
            Record(record_data) => allocator.record(record_data, &[]),
            RecRecord(record_data, dyn_fields, _) => allocator.record(record_data, dyn_fields),
            Match(data) => docs![
                allocator,
                "match ",
                docs![
                    allocator,
                    allocator.line(),
                    allocator.intersperse(
                        data.branches.iter().map(|branch| docs![
                            allocator,
                            branch.pretty(allocator),
                            ","
                        ]),
                        allocator.line(),
                    ),
                ]
                .nest(2)
                .append(allocator.line())
                .braces()
            ]
            .group(),
            Array(fields, _) =>
            // NOTE: the Array attributes are ignored here. They contain only
            // information that has no surface syntax.
            {
                docs![
                    allocator,
                    allocator.line(),
                    allocator.intersperse(
                        fields.iter().map(|rt| rt.pretty(allocator)),
                        allocator.text(",").append(allocator.line()),
                    ),
                ]
                .nest(2)
                .append(allocator.line())
                .brackets()
                .group()
            }

            Op1(UnaryOp::RecordAccess(id), rt) => {
                docs![allocator, allocator.atom(rt), ".", ident_quoted(id)]
            }
            Op1(UnaryOp::BoolNot, rt) => docs![allocator, "!", allocator.atom(rt)],

            Op1(UnaryOp::BoolAnd | UnaryOp::BoolOr | UnaryOp::IfThenElse, _) => unreachable!(),
            Op1(op, rt) => match op.pos() {
                OpPos::Prefix => docs![
                    allocator,
                    op,
                    docs![allocator, allocator.line(), allocator.atom(rt)].nest(2)
                ]
                .group(),
                OpPos::Special | OpPos::Postfix | OpPos::Infix => {
                    panic!("pretty print is not implemented for {op:?}")
                }
            },
            Op2(BinaryOp::RecordGet, rtl, rtr) => {
                docs![allocator, rtr, ".", rtl]
            }
            Op2(op, rtl, rtr) => docs![
                allocator,
                if (&BinaryOp::Sub, &Num(Number::ZERO)) == (op, rtl.as_ref()) {
                    allocator.text("-")
                } else if op.pos() == OpPos::Prefix {
                    op.pretty(allocator).append(
                        docs![
                            allocator,
                            allocator.line(),
                            allocator.atom(rtl),
                            allocator.line()
                        ]
                        .nest(2),
                    )
                } else {
                    docs![allocator, allocator.atom(rtl), allocator.line(), op, " "]
                },
                allocator.atom(rtr)
            ]
            .group(),
            OpN(op, rts) => docs![
                allocator,
                op,
                docs![
                    allocator,
                    allocator.line(),
                    allocator
                        .intersperse(rts.iter().map(|rt| allocator.atom(rt)), allocator.line())
                ]
                .nest(2)
            ]
            .group(),
            ForeignId(_) => allocator.text("%<foreign>"),
            SealingKey(sym) => allocator.text(format!("%<sealing key: {sym}>")),
            Sealed(_i, _rt, _lbl) => allocator.text("%<sealed>"),
            Annotated(annot, rt) => allocator.atom(rt).append(annot.pretty(allocator)),
            Import(f) => allocator
                .text("import ")
                .append(allocator.as_string(f.to_string_lossy()).double_quotes()),
            ResolvedImport(id) => allocator.text(format!("import <file_id: {id:?}>")),
            // This type is in term position, so we don't need to add parentheses.
            Type(ty) => ty.pretty(allocator),
            ParseError(_) => allocator.text("%<PARSE ERROR>"),
            RuntimeError(_) => allocator.text("%<RUNTIME ERROR>"),
            Closure(idx) => allocator.text(format!("%<closure@{idx:p}>")),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &EnumRows
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        match &self.0 {
            EnumRowsF::Empty => allocator.nil(),
            EnumRowsF::TailVar(id) => docs![allocator, ";", allocator.line(), id.to_string()],
            EnumRowsF::Extend { row, tail } => {
                let mut result = row.pretty(allocator);

                if let EnumRowsF::Extend { .. } = tail.0 {
                    result = result.append(allocator.text(",").append(allocator.line()));
                }

                result.append(tail.as_ref())
            }
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &EnumRow
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        let mut result = allocator
            .text("'")
            .append(allocator.text(ident_quoted(&self.id)));

        if let Some(typ) = self.typ.as_ref() {
            let ty_parenthesized = if typ.fmt_is_atom() {
                typ.pretty(allocator)
            } else {
                allocator
                    .text("(")
                    .append(allocator.line_())
                    .append(typ.pretty(allocator))
                    .append(allocator.line_())
                    .append(")")
            };

            result = result.append(allocator.text(" ")).append(ty_parenthesized);
        }

        result
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &RecordRows
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        match &self.0 {
            RecordRowsF::Empty => allocator.nil(),
            RecordRowsF::TailDyn => docs![allocator, ";", allocator.line(), "Dyn"],
            RecordRowsF::TailVar(id) => docs![allocator, ";", allocator.line(), id.to_string()],
            RecordRowsF::Extend { row, tail } => docs![
                allocator,
                row,
                if let RecordRowsF::Extend { .. } = tail.0 {
                    docs![allocator, ",", allocator.line()]
                } else {
                    allocator.nil()
                },
                tail.as_ref()
            ],
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &RecordRow
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        docs![
            allocator,
            ident_quoted(&self.id),
            " : ",
            allocator.type_part(self.typ.as_ref()),
        ]
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &Type
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        use TypeF::*;
        match &self.typ {
            Dyn => allocator.text("Dyn"),
            Number => allocator.text("Number"),
            Bool => allocator.text("Bool"),
            String => allocator.text("String"),
            Array(ty) => if ty.fmt_is_atom() {
                docs![allocator, "Array", allocator.line(), ty.as_ref()].nest(2)
            } else {
                docs![
                    allocator,
                    "Array (",
                    docs![allocator, allocator.line_(), ty.as_ref()].nest(2),
                    allocator.line_(),
                    ")"
                ]
            }
            .group(),
            ForeignId => allocator.text("ForeignId"),
            Symbol => allocator.text("Symbol"),
            Flat(t) => t.pretty(allocator),
            Var(var) => allocator.as_string(var),
            Forall { var, ref body, .. } => {
                let mut curr = body.as_ref();
                let mut foralls = vec![var];
                while let Type {
                    typ: Forall { var, ref body, .. },
                    ..
                } = curr
                {
                    foralls.push(var);
                    curr = body;
                }
                docs![
                    allocator,
                    "forall",
                    allocator.line(),
                    allocator.intersperse(
                        foralls.iter().map(|i| allocator.as_string(i)),
                        allocator.line(),
                    ),
                    ".",
                    allocator.line(),
                    allocator.type_part(curr)
                ]
                .nest(2)
                .group()
            }
            Enum(erows) => docs![allocator, allocator.line(), erows]
                .nest(2)
                .append(allocator.line())
                .enclose("[|", "|]")
                .group(),
            Record(rrows) => docs![allocator, allocator.line(), rrows]
                .nest(2)
                .append(allocator.line())
                .braces()
                .group(),
            Dict {
                type_fields: ty,
                flavour: attrs,
            } => docs![
                allocator,
                allocator.line(),
                "_ ",
                match attrs {
                    DictTypeFlavour::Type => ":",
                    DictTypeFlavour::Contract => "|",
                },
                " ",
                allocator.type_part(ty.as_ref()),
            ]
            .nest(2)
            .append(allocator.line())
            .braces()
            .group(),
            Arrow(dom, codom) => docs![
                allocator,
                allocator
                    .type_part(dom)
                    .parens_if(matches!(dom.typ, Arrow(..) | Forall { .. }))
                    .nest(2),
                allocator.line(),
                "-> ",
                allocator
                    .type_part(codom)
                    .parens_if(matches!(codom.typ, Forall { .. }))
            ]
            .group(),
            Wildcard(_) => allocator.text("_"),
        }
    }
}

impl<'a, D, A> Pretty<'a, D, A> for &MatchBranch
where
    D: NickelAllocatorExt<'a, A>,
    D::Doc: Clone,
    A: Clone + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        let guard = if let Some(guard) = &self.guard {
            docs![allocator, allocator.line(), "if", allocator.space(), guard]
        } else {
            allocator.nil()
        };

        docs![
            allocator,
            &self.pattern,
            guard,
            allocator.space(),
            "=>",
            docs![allocator, allocator.line(), self.body.pretty(allocator),].nest(2),
        ]
    }
}

/// Generate an implementation of `fmt::Display` for types that implement `Pretty`.
#[macro_export]
macro_rules! impl_display_from_pretty {
    ($ty:ty) => {
        impl std::fmt::Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                $crate::pretty::fmt_pretty(&self, f)
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use pretty::BoxAllocator;

    use crate::parser::lexer::Lexer;
    use crate::parser::{
        grammar::{FixedTypeParser, TermParser},
        ErrorTolerantParser,
    };
    use codespan::Files;

    use super::*;
    use indoc::indoc;

    /// Parse a type represented as a string.
    fn parse_type(s: &str) -> Type {
        let id = Files::new().add("<test>", s);

        FixedTypeParser::new()
            .parse_strict(id, Lexer::new(s))
            .unwrap()
    }

    /// Parse a term represented as a string.
    fn parse_term(s: &str) -> RichTerm {
        let id = Files::new().add("<test>", s);

        TermParser::new().parse_strict(id, Lexer::new(s)).unwrap()
    }

    /// Parse a string representation `long` of a type, and assert that
    /// formatting it gives back `long`, if the line length is set to `80`, or
    /// alternatively results in `short`, if the line length is set to `0`
    #[track_caller]
    fn assert_long_short_type(long: &str, short: &str) {
        let ty = parse_type(long);
        let doc: DocBuilder<'_, BoxAllocator, ()> = ty.pretty(&BoxAllocator);

        let mut long_lines = String::new();
        doc.render_fmt(usize::MAX, &mut long_lines).unwrap();

        let mut short_lines = String::new();
        doc.render_fmt(0, &mut short_lines).unwrap();

        assert_eq!(long_lines, long);
        assert_eq!(short_lines, short);
    }

    /// Parse a string representation `long` of a Nickel term, and assert that
    /// formatting it gives back `long`, if the line length is set to `80`, or
    /// alternatively results in `short`, if the line length is set to `0`
    #[track_caller]
    fn assert_long_short_term(long: &str, short: &str) {
        let term = parse_term(long);
        let doc: DocBuilder<'_, BoxAllocator, ()> = term.pretty(&BoxAllocator);

        let mut long_lines = String::new();
        doc.render_fmt(160, &mut long_lines).unwrap();

        let mut short_lines = String::new();
        doc.render_fmt(0, &mut short_lines).unwrap();

        assert_eq!(long_lines, long);
        assert_eq!(short_lines, short);
    }

    #[test]
    fn pretty_array_type() {
        assert_long_short_type("Array String", "Array\n  String");
        assert_long_short_type(
            "Array (Number -> Array Dyn)",
            indoc! {"
                Array (
                  Number
                  -> Array
                    Dyn
                )"
            },
        );
    }

    #[test]
    fn pretty_arrow_type() {
        assert_long_short_type("Number -> Number", "Number\n-> Number");
        assert_long_short_type(
            "(Number -> Number -> Dyn) -> Number",
            indoc! {"
                (Number
                  -> Number
                  -> Dyn)
                -> Number"
            },
        );
    }

    #[test]
    fn pretty_dict_type() {
        assert_long_short_type(
            "{ _ : Number }",
            indoc! {"
                {
                  _ : Number
                }"
            },
        );
        assert_long_short_type(
            "{ _ : { x : Number, y : String } }",
            indoc! {"
                {
                  _ : {
                    x : Number,
                    y : String
                  }
                }"
            },
        );
    }

    #[test]
    fn pretty_record_type() {
        assert_long_short_type(
            "{ x : Number, y : String; Dyn }",
            indoc! {"
                {
                  x : Number,
                  y : String;
                  Dyn
                }"
            },
        );
    }

    #[test]
    fn pretty_enum_type() {
        assert_long_short_type(
            "forall r. [| 'tag1, 'tag2, 'tag3; r |]",
            indoc! {"
                forall
                  r.
                  [|
                    'tag1,
                    'tag2,
                    'tag3;
                    r
                  |]"
            },
        )
    }

    #[test]
    fn pretty_forall_type() {
        assert_long_short_type(
            "forall a r. a -> { foo : a; r }",
            indoc! {"
                forall
                  a
                  r.
                  a
                  -> {
                    foo : a;
                    r
                  }"
            },
        );
    }

    #[test]
    fn pretty_opn() {
        assert_long_short_term(
            "%string/replace% string pattern replace",
            indoc! {"
                %string/replace%
                  string
                  pattern
                  replace"
            },
        );
    }

    #[test]
    fn pretty_binop() {
        assert_long_short_term(
            "a + b",
            indoc! {"
                a
                + b"
            },
        );
        assert_long_short_term(
            "%string/split% string sep",
            indoc! {"
                %string/split%
                  string
                  sep"
            },
        );
        assert_long_short_term("-5", "-5");
        assert_long_short_term(
            "a - (-b)",
            indoc! {"
                a
                - (-b)"
            },
        );
    }

    #[test]
    fn pretty_unop() {
        assert_long_short_term("!xyz", "!xyz");
        assert_long_short_term(
            "a && b",
            indoc! {"
                a
                && b"
            },
        );
        assert_long_short_term(
            "(a && b) && c",
            indoc! {"
                (a
                && b)
                && c"
            },
        );
        assert_long_short_term(
            "a || b",
            indoc! {"
                a
                || b"
            },
        );
        assert_long_short_term(
            "if true then false else not",
            indoc! {"
                if true then
                  false
                else
                  not"
            },
        );
        assert_long_short_term(
            "%enum/embed% foo bar",
            indoc! {"
                %enum/embed%
                  foo
                  bar"
            },
        );
    }

    #[test]
    fn pretty_arrays() {
        assert_long_short_term(
            "[ 1, 2, 3, 4 ]",
            indoc! {"
                [
                  1,
                  2,
                  3,
                  4
                ]"
            },
        );
    }

    #[test]
    fn pretty_match() {
        assert_long_short_term(
            "match { 'A => a, 'B => b, 'C => c, }",
            indoc! {"
                match {
                  'A =>
                    a,
                  'B =>
                    b,
                  'C =>
                    c,
                }"
            },
        );
    }

    #[test]
    fn pretty_record() {
        assert_long_short_term("{}", "{}");
        assert_long_short_term(
            "{ a = b, c = d, }",
            indoc! {"
                {
                  a =
                    b,
                  c =
                    d,
                }"
            },
        );
        assert_long_short_term(
            r#"{ a | String | force = b, c | Number | doc "" = d, }"#,
            indoc! {r#"
                {
                  a
                    | String
                    | force
                    = b,
                  c
                    | Number
                    | doc ""
                    = d,
                }"#
            },
        );
        assert_long_short_term(
            "{ a = b, .. }",
            indoc! {"
                {
                  a =
                    b,
                  ..
                }"
            },
        );
        assert_long_short_term(
            r#"{ a = b, "%{a}" = c, .. }"#,
            indoc! {r#"
                {
                  a =
                    b,
                  "%{a}" =
                    c,
                  ..
                }"#
            },
        );
        assert_long_short_term(
            r#"{ "=" = a, }"#,
            indoc! {r#"
                {
                  "=" =
                    a,
                }"#
            },
        );
    }

    #[test]
    fn pretty_let() {
        assert_long_short_term(
            "let rec foo | String = c in {}",
            indoc! {"
                let rec foo
                  | String
                  = c
                  in
                {}"
            },
        );
        assert_long_short_term(
            "let foo = c bar in {}",
            indoc! {"
                let foo
                  = c
                    bar
                  in
                {}"
            },
        );
        assert_long_short_term(
            "let foo | String = c bar in {}",
            indoc! {"
                let foo
                  | String
                  = c
                    bar
                  in
                {}"
            },
        );
    }

    #[test]
    fn pretty_multiline_strings() {
        // The string `"\n1."` contains a newline, so it will be pretty-printed using Nickel's
        // multiline string syntax. The result looks like:
        // ```
        // m%"
        //
        //   1.
        // "%
        // ```
        // The newline after `m%"` and the newline before `"%` are removed by the parser, as is the
        // indentation. Unfortunately, we can't use `indoc!` in this test because `pretty.rs`
        // insists on putting two spaces after every newline, even if the line is otherwise empty.
        // But `indoc!` would rightfully strip those empty spaces.
        let t: RichTerm = Term::StrChunks(vec![StrChunk::Literal("\n1.".to_owned())]).into();
        assert_eq!(format!("{t}"), "m%\"\n  \n  1.\n\"%");

        let t: RichTerm = Term::StrChunks(vec![StrChunk::Literal(
            "a multiline string\n\n\n\n".to_owned(),
        )])
        .into();
        assert_eq!(
            format!("{t}"),
            "m%\"\n  a multiline string\n  \n  \n  \n  \n\"%"
        );
    }

    #[test]
    fn pretty_let_pattern() {
        assert_long_short_term(
            "let foo @ { a | Bool ? true = a', b ? false, } = c in {}",
            indoc! {"
                let foo @ {
                    a
                      | Bool
                      ? true
                      = a',
                    b
                      ? false,
                  }
                  = c
                  in
                {}"
            },
        );
        assert_long_short_term(
            "let foo @ { a = a', b = e @ { foo, .. }, } = c in {}",
            indoc! {"
                let foo @ {
                    a
                      = a',
                    b
                      = e @ {
                        foo,
                        ..
                      },
                  }
                  = c
                  in
                {}"
            },
        );
        assert_long_short_term(
            "let foo @ { a = a', b, } | String = c in {}",
            indoc! {"
                let foo @ {
                    a
                      = a',
                    b,
                  }
                  | String
                  = c
                  in
                {}"
            },
        );
    }

    #[test]
    fn pretty_fun() {
        assert_long_short_term(
            "fun x y z => x y z",
            indoc! {"
                fun
                  x
                  y
                  z
                  =>
                  x
                    y
                    z"
            },
        );
        assert_long_short_term(
            "fun x @ { foo, bar ? true, } y @ { baz, } => x y z",
            indoc! {"
                fun
                  x @ {
                    foo,
                    bar
                      ? true,
                  }
                  y @ {
                    baz,
                  }
                  =>
                  x
                    y
                    z"
            },
        );
    }

    #[test]
    fn pretty_app() {
        assert_long_short_term(
            "x y z",
            indoc! {"
                x
                  y
                  z"
            },
        );
    }

    /// Take a string representation of a type, parse it, and assert that formatting it gives the
    /// same string as the original argument.
    ///
    /// Note that there are infinitely many string representations of the same type since, for
    /// example, spaces are ignored: for the outcome of this function to be meaningful, the
    /// original type must be written in the same way as types are formatted.
    #[track_caller]
    fn assert_format_eq(s: &str) {
        let ty = parse_type(s);
        assert_eq!(s, &format!("{ty}"));
    }

    #[test]
    fn types_pretty_printing() {
        assert_format_eq("Number");
        assert_format_eq("Number -> Number");
        assert_format_eq("(Number -> Number) -> (Number -> Number) -> Number -> Number");
        assert_format_eq("((Number -> Number) -> Number) -> Number");
        assert_format_eq("Number -> (forall a. a -> String) -> String");

        assert_format_eq("{ _ : String }");
        assert_format_eq("{ _ : (String -> String) -> String }");
        assert_format_eq("{ _ | String }");
        assert_format_eq("{ _ | (String -> String) -> String }");

        assert_format_eq("{ x : (Bool -> Bool) -> Bool, y : Bool }");
        assert_format_eq("forall r. { x : Bool, y : Bool, z : Bool; r }");
        assert_format_eq("{ x : Bool, y : Bool, z : Bool }");

        assert_format_eq("[| 'a, 'b, 'c, 'd |]");
        assert_format_eq("forall r. [| 'tag1, 'tag2, 'tag3; r |]");

        assert_format_eq("Array Number");
        assert_format_eq("Array (Array Number)");
        assert_format_eq("Number -> Array (Array String) -> Number");
        assert_format_eq("Array (Number -> Number)");
        assert_format_eq("Array (Array (Array Dyn) -> Number)");

        assert_format_eq("_");
        assert_format_eq("_ -> _");
        assert_format_eq("{ x : _, y : Bool }");
        assert_format_eq("{ _ : _ }");
    }
}
