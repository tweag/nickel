//! Rendering of the results of a metadata query.
use crate::identifier::Ident;
use crate::term::{MergePriority, MetaValue, Term};
use std::{io, io::Write};

/// A query printer. The implementation may differ depending on the activation of markdown
/// support.
pub trait QueryPrinter {
    /// Print a metadata attribute.
    fn write_metadata(&self, out: &mut impl Write, attr: &str, value: &str) -> io::Result<()>;
    /// Print the documentation attribute.
    fn write_doc(&self, out: &mut impl Write, content: &str) -> io::Result<()>;
    /// Print the list of fields of a record.
    fn write_fields<'a, I>(&self, out: &mut impl Write, fields: I) -> io::Result<()>
    where
        I: Iterator<Item = &'a Ident>;
}

#[cfg(feature = "markdown")]
pub struct MarkdownRenderer {
    skin: termimad::MadSkin,
}

pub struct SimpleRenderer {}

/// Helper to render the result of the `query` sub-command without markdown support.
impl QueryPrinter for SimpleRenderer {
    fn write_metadata(&self, out: &mut impl Write, attr: &str, value: &str) -> io::Result<()> {
        writeln!(out, "* {}: {}", attr, value)
    }

    fn write_doc(&self, out: &mut impl Write, content: &str) -> io::Result<()> {
        if content.find('\n').is_none() {
            self.write_metadata(out, "documentation", content)
        } else {
            writeln!(out, "* documentation\n")?;
            writeln!(out, "{}", content)
        }
    }

    fn write_fields<'a, I>(&self, out: &mut impl Write, fields: I) -> io::Result<()>
    where
        I: Iterator<Item = &'a Ident>,
    {
        writeln!(out, "Available fields:")?;

        for field in fields {
            writeln!(out, " - {}", field)?;
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
fn termimad_to_io(err: termimad::Error) -> io::Error {
    match err {
        termimad::Error::IO(err) => err,
        // Not an IO error per se, but creating a new error type and chaning the signatures of the
        // query printer functions just for this variant that is specific to the termimad backend
        // doesn't seem to worth it.
        termimad::Error::InsufficientWidth(err) => {
            io::Error::new(io::ErrorKind::Other, format!("{}", err))
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
        write!(out, "{}", fmt_text)
    }

    fn write_doc(&self, out: &mut impl Write, content: &str) -> io::Result<()> {
        if content.find('\n').is_none() {
            self.skin
                .write_text_on(out, &format!("* **documentation**: {}", content))
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

    fn write_fields<'a, I>(&self, out: &mut impl Write, fields: I) -> io::Result<()>
    where
        I: Iterator<Item = &'a Ident>,
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
            write!(out, "{}", fmt_text)?;
        }

        Ok(())
    }
}

/// Represent which metadata attributes are requested by a query.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Attributes {
    pub doc: bool,
    pub contract: bool,
    pub types: bool,
    pub default: bool,
    pub value: bool,
}

// By default, show all available metadata.
impl Default for Attributes {
    fn default() -> Self {
        Attributes {
            doc: true,
            contract: true,
            types: true,
            default: true,
            value: true,
        }
    }
}

/// Print the result of a metadata query, which is a "weakly" evaluated term (see
/// [`crate::eval::eval_meta`] and [`crate::program::query`]).
///
/// Wrapper around `write_query_result_` that selects an adapated query printer at compile time.
pub fn write_query_result(
    out: &mut impl Write,
    term: &Term,
    selected_attrs: Attributes,
) -> io::Result<()> {
    #[cfg(feature = "markdown")]
    let renderer = MarkdownRenderer::new();

    #[cfg(not(feature = "markdown"))]
    let renderer = SimpleRenderer {};

    write_query_result_(out, term, selected_attrs, &renderer)
}

/// Print the result of a metadata query, which is a "weakly" evaluated term (see
/// [`crate::eval::eval_meta`] and [`crate::program::query`]).
fn write_query_result_<R: QueryPrinter>(
    out: &mut impl Write,
    term: &Term,
    selected_attrs: Attributes,
    renderer: &R,
) -> io::Result<()> {
    // Print a list the fields of a term if it is a record, or do nothing otherwise.
    fn write_fields<R: QueryPrinter>(
        out: &mut impl Write,
        renderer: &R,
        t: &Term,
    ) -> io::Result<()> {
        writeln!(out)?;
        match t {
            Term::Record(record) if !record.fields.is_empty() => {
                let mut fields: Vec<_> = record.fields.keys().collect();
                fields.sort();
                renderer.write_fields(out, fields.into_iter())
            }
            Term::RecRecord(map, dyn_fields, ..) if !map.is_empty() => {
                let mut fields: Vec<_> = map.keys().collect();
                fields.sort();
                let dynamic = Ident::from("<dynamic>");
                fields.extend(dyn_fields.iter().map(|_| &dynamic));
                renderer.write_fields(out, fields.into_iter())
            }
            Term::Record(..) | Term::RecRecord(..) => renderer.write_metadata(out, "value", "{}"),
            _ => Ok(()),
        }
    }

    match term {
        Term::MetaValue(meta) => {
            let mut found = false;
            if selected_attrs.contract && !meta.contracts.is_empty() {
                let ctrs: Vec<String> = meta
                    .contracts
                    .iter()
                    // We use the original user-written type stored in the label. Using
                    // `ctr.types` instead is unreadable most of the time, as it can have been
                    // altered by closurizations or other run-time rewriting
                    .map(|ctr| ctr.label.types.to_string())
                    .collect();
                renderer.write_metadata(out, "contract", &ctrs.join(","))?;
                found = true;
            }

            if selected_attrs.types && meta.types.is_some() {
                renderer.write_metadata(
                    out,
                    "type",
                    &meta.types.as_ref().unwrap().types.to_string(),
                )?;
                found = true;
            }

            match &meta {
                MetaValue {
                    priority: MergePriority::Bottom,
                    value: Some(t),
                    ..
                } if selected_attrs.default => {
                    renderer.write_metadata(out, "default", &t.as_ref().shallow_repr())?;
                    found = true;
                }
                MetaValue {
                    priority: MergePriority::Numeral(n),
                    value: Some(t),
                    ..
                } if selected_attrs.value => {
                    renderer.write_metadata(out, "priority", &format!("{}", n))?;
                    renderer.write_metadata(out, "value", &t.as_ref().shallow_repr())?;
                    found = true;
                }
                _ => (),
            }

            match meta.doc {
                Some(ref s) if selected_attrs.doc => {
                    renderer.write_doc(out, s)?;
                    found = true;
                }
                _ => (),
            }

            if !found {
                println!("Requested metadata were not found for this value.");
                meta.value
                    .iter()
                    .try_for_each(|rt| write_fields(out, renderer, rt.as_ref()))?;
            }

            meta.value
                .iter()
                .try_for_each(|rt| write_fields(out, renderer, rt.as_ref()))?;
        }
        t @ Term::Record(..) | t @ Term::RecRecord(..) => {
            writeln!(out, "No metadata found for this value.")?;
            write_fields(out, renderer, t)?;
        }
        t => {
            writeln!(out, "No metadata found for this value.\n")?;
            if selected_attrs.value {
                renderer.write_metadata(out, "value", &t.shallow_repr())?;
            }
        }
    };

    Ok(())
}
