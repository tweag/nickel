use std::cell::Cell;
use std::fmt;
use std::sync::LazyLock;

use crate::identifier::{Ident, LocIdent};
use crate::input_format::InputFormat;
use crate::lexer::KEYWORDS;
use crate::typ::*;

use indexmap::IndexMap;
use malachite::base::num::{basic::traits::Zero, conversion::traits::ToSci};
use pretty::docs;
pub use pretty::{DocAllocator, DocBuilder, Pretty};
use regex::Regex;

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum StringRenderStyle {
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

static QUOTING_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new("^_*[a-zA-Z][_a-zA-Z0-9-]*$").unwrap());

/// Return the string representation of an identifier, and add enclosing double quotes if the
/// label isn't a valid identifier according to the parser, for example if it contains a
/// special character like a space.
pub fn ident_quoted(ident: impl Into<Ident>) -> String {
    let ident = ident.into();
    let label = ident.label();
    if QUOTING_REGEX.is_match(label) && !KEYWORDS.contains(&label) {
        String::from(label)
    } else {
        format!("\"{}\"", escape(label))
    }
}

/// Return a string representation of an identifier, adding enclosing double quotes if
/// the label isn't valid for an enum tag. This is like `ident_quoted` except that keywords
/// aren't wrapped in quotes (because `'if` is a valid enum tag, for example).
pub fn enum_tag_quoted(ident: impl Into<Ident>) -> String {
    let ident = ident.into();
    let label = ident.label();
    if QUOTING_REGEX.is_match(label) {
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

pub fn fmt_pretty<T>(value: &T, f: &mut fmt::Formatter) -> fmt::Result
where
    T: for<'a> Pretty<'a, Allocator, ()> + Clone,
{
    let allocator = Allocator::default();
    let doc: DocBuilder<_, ()> = value.clone().pretty(&allocator);
    doc.render_fmt(80, f)
}

#[derive(Clone, Copy, Debug, Default)]
struct SizeBound {
    depth: usize,
    size: usize,
}

/// A pretty-printing allocator that supports rough bounds on the
/// size of the output.
///
/// When a pretty-printed object is too large, it will be abbreviated.
/// For example, a record will be abbreviated as "{…}".
///
/// The bounds are "rough" in that the depth bound only (currently; this might
/// be extended in the future) constrains the number of nested records: you can
/// still have deeply nested terms of other kinds. The size bound only constrains
/// the number of children of nested records. As such, neither constraint gives
/// precise control over the size of the output.
pub struct Allocator {
    inner: pretty::BoxAllocator,
    bound: Option<Cell<SizeBound>>,
}

/// The default `BoundedAllocator` imposes no constraints.
impl Default for Allocator {
    fn default() -> Self {
        Self {
            inner: pretty::BoxAllocator,
            bound: None,
        }
    }
}

impl Allocator {
    /// Creates a `BoundedAllocator` with constraints.
    pub fn bounded(max_depth: usize, max_size: usize) -> Self {
        Self {
            inner: pretty::BoxAllocator,
            bound: Some(Cell::new(SizeBound {
                depth: max_depth,
                size: max_size,
            })),
        }
    }

    /// Runs a callback with a "smaller" allocator.
    fn shrunken<'a, F: FnOnce(&'a Allocator) -> DocBuilder<'a, Self>>(
        &'a self,
        child_size: usize,
        f: F,
    ) -> DocBuilder<'a, Self> {
        if let Some(bound) = self.bound.as_ref() {
            let old = bound.get();
            bound.set(SizeBound {
                depth: old.depth.saturating_sub(1),
                size: child_size,
            });

            let ret = f(self);

            bound.set(old);

            ret
        } else {
            f(self)
        }
    }

    fn depth_constraint(&self) -> usize {
        self.bound.as_ref().map_or(usize::MAX, |b| b.get().depth)
    }

    fn size_constraint(&self) -> usize {
        self.bound.as_ref().map_or(usize::MAX, |b| b.get().size)
    }
}

impl<'a> DocAllocator<'a> for Allocator {
    type Doc = pretty::BoxDoc<'a>;

    fn alloc(&'a self, doc: pretty::Doc<'a, Self::Doc>) -> Self::Doc {
        self.inner.alloc(doc)
    }

    fn alloc_column_fn(
        &'a self,
        f: impl Fn(usize) -> Self::Doc + 'a,
    ) -> <Self::Doc as pretty::DocPtr<'a, ()>>::ColumnFn {
        self.inner.alloc_column_fn(f)
    }

    fn alloc_width_fn(
        &'a self,
        f: impl Fn(isize) -> Self::Doc + 'a,
    ) -> <Self::Doc as pretty::DocPtr<'a, ()>>::WidthFn {
        self.inner.alloc_width_fn(f)
    }
}

trait NickelDocBuilderExt {
    /// Call `self.parens()` but only if `parens` is `true`.
    fn parens_if(self, parens: bool) -> Self;
}

impl NickelDocBuilderExt for DocBuilder<'_, Allocator> {
    fn parens_if(self, parens: bool) -> Self {
        if parens {
            self.parens()
        } else {
            self
        }
    }
}

impl<'a> Pretty<'a, Allocator> for LocIdent {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        allocator.text(self.into_label())
    }
}

impl<'a> Pretty<'a, Allocator> for RecursivePriority {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        match self {
            RecursivePriority::Default => allocator.text("| rec default"),
            RecursivePriority::Force => allocator.text("| rec force"),
            RecursivePriority::None => allocator.nil(),
        }
    }
}

/// Provide a method to pretty-print a long term, type, etc. (anything that implements `ToString`,
/// really) capped to a maximum length.
pub trait PrettyPrintCap: ToString {
    /// Pretty print an object capped to a given max length (in characters). Useful to limit the
    /// size of terms reported e.g. in typechecking errors. If the output of pretty printing is
    /// greater than the bound, the string is truncated to `max_width` and the last character after
    /// truncate is replaced by the ellipsis unicode character U+2026.
    fn pretty_print_cap(&self, max_width: usize) -> String {
        let output = self.to_string();

        if output.len() <= max_width {
            output
        } else {
            let (end, _) = output.char_indices().nth(max_width).unwrap();
            let mut truncated = String::from(&output[..end]);

            if max_width >= 2 {
                truncated.pop();
                truncated.push('\u{2026}');
            }

            truncated
        }
    }
}
