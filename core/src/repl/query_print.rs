//! Rendering of the results of a metadata query.
use crate::identifier::{Ident, LocIdent};
use crate::term::{
    record::{Field, FieldMetadata},
    MergePriority, Term,
};
use std::{io, io::Write};

/// The maximum width for pretty-printing default values. Beyond this limit, the content is cut and
/// an ellipsis is appended.
const TERM_MAX_WIDTH: usize = 80;

/// A query printer. The implementation may differ depending on the activation of markdown
/// support.
pub trait QueryPrinter {
    /// Print a metadata attribute.
    fn write_metadata(&self, out: &mut impl Write, attr: &str, value: &str) -> io::Result<()>;
    /// Print the documentation attribute.
    fn write_doc(&self, out: &mut impl Write, content: &str) -> io::Result<()>;
    /// Print the list of fields of a record.
    fn write_fields<I>(&self, out: &mut impl Write, fields: I) -> io::Result<()>
    where
        I: Iterator<Item = Ident>;
}

#[cfg(feature = "markdown")]
pub struct MarkdownRenderer {
    skin: termimad::MadSkin,
}

pub struct SimpleRenderer {}

/// Helper to render the result of the `query` sub-command without markdown support.
impl QueryPrinter for SimpleRenderer {
    fn write_metadata(&self, out: &mut impl Write, attr: &str, value: &str) -> io::Result<()> {
        writeln!(out, "* {attr}: {value}")
    }

    fn write_doc(&self, out: &mut impl Write, content: &str) -> io::Result<()> {
        if content.find('\n').is_none() {
            self.write_metadata(out, "documentation", content)
        } else {
            writeln!(out, "* documentation\n")?;
            writeln!(out, "{content}")
        }
    }

    fn write_fields<I>(&self, out: &mut impl Write, fields: I) -> io::Result<()>
    where
        I: Iterator<Item = Ident>,
    {
        writeln!(out, "Available fields:")?;

        for field in fields {
            writeln!(out, " - {field}")?;
        }

        Ok(())
    }
}

#[cfg(feature = "markdown")]
impl MarkdownRenderer {
    pub fn new() -> Self {
        MarkdownRenderer {
            skin: termimad::MadSkin::default(),
        }
    }
}

#[cfg(feature = "markdown")]
impl Default for MarkdownRenderer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "markdown")]
fn termimad_to_io(err: termimad::Error) -> io::Error {
    match err {
        termimad::Error::IO(err) => err,
        // Not an IO error per se, but creating a new error type and chaning the signatures of the
        // query printer functions just for this variant that is specific to the termimad backend
        // doesn't seem to worth it.
        termimad::Error::InsufficientWidth(err) => {
            io::Error::new(io::ErrorKind::Other, format!("{err}"))
        }
    }
}

/// Helper to render the result of the `query` sub-command with markdown support on the
/// terminal.
#[cfg(feature = "markdown")]
impl QueryPrinter for MarkdownRenderer {
    fn write_metadata(&self, out: &mut impl Write, attr: &str, value: &str) -> io::Result<()> {
        use minimad::*;
        use termimad::*;

        let mut expander = OwningTemplateExpander::new();
        let template = TextTemplate::from("* **${attr}**: *${value}*");

        expander.set("attr", attr);
        expander.set("value", value);
        let text = expander.expand(&template);
        let (width, _) = terminal_size();
        let fmt_text = FmtText::from_text(&self.skin, text, Some(width as usize));
        write!(out, "{fmt_text}")
    }

    fn write_doc(&self, out: &mut impl Write, content: &str) -> io::Result<()> {
        if content.find('\n').is_none() {
            self.skin
                .write_text_on(out, &format!("* **documentation**: {content}"))
                .map_err(termimad_to_io)
        } else {
            self.skin
                .write_text_on(out, "* **documentation**\n\n")
                .map_err(termimad_to_io)?;
            self.skin
                .write_text_on(out, content)
                .map_err(termimad_to_io)
        }
    }

    fn write_fields<I>(&self, out: &mut impl Write, fields: I) -> io::Result<()>
    where
        I: Iterator<Item = Ident>,
    {
        use minimad::*;
        use termimad::*;

        let (width, _) = terminal_size();
        let mut expander = OwningTemplateExpander::new();
        let template = TextTemplate::from("* ${field}");

        self.skin
            .write_text_on(out, "## Available fields")
            .map_err(termimad_to_io)?;

        for field in fields {
            expander.set("field", field.to_string());
            let text = expander.expand(&template);
            let fmt_text = FmtText::from_text(&self.skin, text, Some(width as usize));
            write!(out, "{fmt_text}")?;
        }

        Ok(())
    }
}

/// Represent which metadata attributes are requested by a query.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Attributes {
    pub doc: bool,
    pub contract: bool,
    pub typ: bool,
    pub default: bool,
    pub value: bool,
}

// By default, show all available metadata.
impl Default for Attributes {
    fn default() -> Self {
        Attributes {
            doc: true,
            contract: true,
            typ: true,
            default: true,
            value: true,
        }
    }
}

/// Render the result of a metadata query, automatically selecting an adapted query printer at
/// compile time.
///
/// Return `true` if some metadata were found (according to the selected attributes) and printed,
/// and `false` otherwise.
pub fn write_query_result(
    out: &mut impl Write,
    field: &Field,
    selected_attrs: Attributes,
) -> io::Result<bool> {
    #[cfg(feature = "markdown")]
    let renderer = MarkdownRenderer::new();

    #[cfg(not(feature = "markdown"))]
    let renderer = SimpleRenderer {};

    render_query_result(out, field, selected_attrs, &renderer)
}

/// Render the result of a metadata query.
///
/// Return `true` if some metadata were found (according to the selected attributes) and printed,
/// and `false` otherwise.
fn render_query_result<R: QueryPrinter>(
    out: &mut impl Write,
    field: &Field,
    selected_attrs: Attributes,
    renderer: &R,
) -> io::Result<bool> {
    // Print a list the fields of a term if it is a record, or do nothing otherwise.
    fn write_fields<R: QueryPrinter>(
        out: &mut impl Write,
        renderer: &R,
        value: &Term,
    ) -> io::Result<()> {
        writeln!(out)?;

        match value {
            Term::Record(record) if !record.fields.is_empty() => {
                let mut fields: Vec<_> = record.fields.keys().collect();
                fields.sort();
                renderer.write_fields(out, fields.into_iter().map(LocIdent::ident))
            }
            Term::RecRecord(record, dyn_fields, ..) if !record.fields.is_empty() => {
                let mut fields: Vec<_> = record.fields.keys().map(LocIdent::ident).collect();
                fields.sort();
                let dynamic = Ident::from("<dynamic>");
                fields.extend(dyn_fields.iter().map(|_| dynamic));
                renderer.write_fields(out, fields.into_iter())
            }
            Term::Record(..) | Term::RecRecord(..) => renderer.write_metadata(out, "value", "{}"),
            _ => Ok(()),
        }
    }

    let mut found = false;
    let metadata = &field.metadata;

    if selected_attrs.contract && !metadata.annotation.contracts.is_empty() {
        let ctrs: Vec<String> = metadata
            .annotation
            .contracts
            .iter()
            // We use the original user-written type stored in the label. Using
            // `ctr.typ` instead is unreadable most of the time, as it can have been
            // altered by closurizations or other run-time rewriting
            .map(|ctr| ctr.label.typ.to_string())
            .collect();
        renderer.write_metadata(out, "contract", &ctrs.join(","))?;
        found = true;
    }

    if selected_attrs.typ && metadata.annotation.typ.is_some() {
        renderer.write_metadata(
            out,
            "type",
            &metadata
                .annotation
                .typ
                .as_ref()
                .unwrap()
                // We use the original type here, as well.
                .label
                .typ
                .to_string(),
        )?;
        found = true;
    }

    match &field {
        Field {
            metadata:
                FieldMetadata {
                    priority: MergePriority::Bottom,
                    ..
                },
            value: Some(t),
            ..
        } if selected_attrs.default => {
            renderer.write_metadata(out, "default", &t.pretty_print_cap(TERM_MAX_WIDTH))?;
            found = true;
        }
        Field {
            metadata:
                FieldMetadata {
                    priority: MergePriority::Numeral(n),
                    ..
                },
            value: Some(t),
            ..
        } if selected_attrs.value => {
            renderer.write_metadata(out, "priority", &format!("{n}"))?;
            renderer.write_metadata(out, "value", &t.pretty_print_cap(TERM_MAX_WIDTH))?;
            found = true;
        }
        _ => (),
    }

    match metadata.doc {
        Some(ref s) if selected_attrs.doc => {
            renderer.write_doc(out, s)?;
            found = true;
        }
        _ => (),
    }

    match field.value {
        Some(ref value) if selected_attrs.value => write_fields(out, renderer, value.as_ref())?,
        _ => (),
    };

    Ok(found)
}
