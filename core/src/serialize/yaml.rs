//! Support for parsing YAML and converting it to Nickel.
//!
//! We use the `saphyr` crate because it looks like the best-maintained
//! option right now, and it supports reporting locations. We use their
//! lower-level API so that we can convert direction to a `RichTerm`
//! without passing through `saphyr`'s in-memory `Yaml` structure.
//! (Also, this lets us support recursive aliases.)

use std::{borrow::Cow, collections::BTreeMap, num::NonZeroUsize};

use codespan::ByteIndex;
use indexmap::IndexMap;
use saphyr_parser::{BufferedInput, Parser, ScalarStyle, SpannedEventReceiver, Tag};

use crate::{
    error::ParseError,
    files::FileId,
    identifier::{Ident, LocIdent},
    position::{RawSpan, TermPos},
    term::{
        array::{Array, ArrayAttrs},
        record::{Field, RecordAttrs, RecordData},
        LetAttrs, Number, RichTerm, Term,
    },
};

fn mk_pos(file_id: Option<FileId>, start: usize, end: usize) -> TermPos {
    file_id
        .map(|src_id| {
            TermPos::Original(RawSpan {
                src_id,
                start: ByteIndex::from(start as u32),
                end: ByteIndex::from(end as u32),
            })
        })
        .unwrap_or_default()
}

#[derive(Clone, Debug)]
enum Node {
    Array {
        array: Array,
        pos: TermPos,
    },
    Map {
        map: IndexMap<LocIdent, Field>,
        cur_key: Option<LocIdent>,
        pos: TermPos,
    },
    Scalar(RichTerm),
}

impl Node {
    fn pos(&self) -> &TermPos {
        match self {
            Node::Array { pos, .. } | Node::Map { pos, .. } => pos,
            Node::Scalar(rt) => &rt.pos,
        }
    }

    fn finish(self) -> RichTerm {
        match self {
            Node::Array { array, pos } => {
                RichTerm::new(Term::Array(array, ArrayAttrs::default()), pos)
            }
            Node::Map { map, cur_key, pos } => {
                debug_assert!(cur_key.is_none());
                RichTerm::new(
                    Term::Record(RecordData {
                        fields: map,
                        attrs: RecordAttrs::default(),
                        sealed_tail: None,
                    }),
                    pos,
                )
            }
            Node::Scalar(rt) => rt,
        }
    }

    fn with_end_pos(mut self, end: ByteIndex) -> Self {
        match &mut self {
            Node::Array { pos, .. } | Node::Map { pos, .. } => {
                *pos = pos.map(|sp| RawSpan { end, ..sp });
            }
            Node::Scalar(rt) => {
                rt.pos = rt.pos.map(|sp| RawSpan { end, ..sp });
            }
        }
        self
    }
}

/// Keep track of anchors.
///
/// We turn anchor references into Nickel variable references: every node
/// with an anchor gets inserted into a big top-level `let` block, and every
/// reference to an anchor gets turned into a `Term::Var` node.
///
/// This is different from saphyr's approach, where they clone the referent
/// every time it's referenced. In addition to avoiding unnecessary clones,
/// our version supports recursive anchor references.
///
/// As an example, we turn
///
/// ```yaml
/// bar: &ref
///     - 1
///     - 2
/// baz: *ref
/// ```
///
/// into
///
/// ```nickel
/// let rec %0 = [ 1, 2 ] in { bar = %0, baz = %0, }
/// ```
///
/// Note that we generate the reference even if it's never
/// dereferenced: if the `baz` in the example above were
/// missing, we'd generate
///
/// ```nickel
/// let rec %0 = [ 1, 2 ] in { bar = %0, }
/// ```
///
/// This could be changed in the future, but the current behavior is nice
/// because we can do it all in one pass.
#[derive(Default)]
struct AnchorMap {
    map: BTreeMap<NonZeroUsize, (Ident, RichTerm)>,
}

impl AnchorMap {
    /// Assign a unique variable name to this anchor id.
    ///
    /// This should be called when opening a container (i.e. an array or map),
    /// and before recursing into its children. That way the variable name will
    /// be available to any children that reference the parent.
    fn reserve(&mut self, anchor_id: Option<NonZeroUsize>) {
        if let Some(aid) = anchor_id {
            self.map.insert(aid, (Ident::fresh(), Term::Null.into()));
        }
    }

    fn var(&self, anchor_id: NonZeroUsize) -> RichTerm {
        Term::Var(self.map[&anchor_id].0.into()).into()
    }

    /// If a `RichTerm` has an anchor id, replace it with a `Term::Var`.
    ///
    /// (The original `RichTerm` will get hoisted up to the top-level recursive let block.)
    fn anchorify(&mut self, anchor_id: Option<NonZeroUsize>, rt: RichTerm) -> RichTerm {
        if let Some(aid) = anchor_id {
            let (ident, slot) = self
                .map
                .entry(aid)
                .or_insert((Ident::fresh(), Term::Null.into()));
            *slot = rt;
            Term::Var((*ident).into()).into()
        } else {
            rt
        }
    }
}

#[derive(Default)]
struct YamlLoader {
    /// Keeps track of the anchors we've encountered so far.
    anchor_map: AnchorMap,
    /// The stack of incomplete containers, and their anchor ids.
    ///
    /// The top of the stack is the container currently being filled;
    /// as we encounter new nodes, we add them to that container.
    ///
    /// There is one situation in which a non-container `Node` will
    /// be on the stack: if the document contains nothing but a single
    /// scalar.
    doc_stack: Vec<(Node, Option<NonZeroUsize>)>,
    /// All the docs that we've already finished loading.
    docs: Vec<RichTerm>,
    /// If the input came from a file, here is it's id.
    file_id: Option<FileId>,
    /// Saphyr errors early when it encounters a YAML parse error, but it
    /// doesn't provide a way for the loader to exit on error. We have a few
    /// error cases that aren't caught by the yaml parser (for example, because
    /// Nickel records only allow string keys). Because there's no way to
    /// early-exit, we keep track of those errors by stashing them here.
    err: Option<ParseError>,
}

impl YamlLoader {
    fn push_node(&mut self, node: Node, anchor_id: Option<NonZeroUsize>) {
        if let Some((parent, _)) = self.doc_stack.last_mut() {
            match parent {
                Node::Array { array, pos: _ } => {
                    let node = self.anchor_map.anchorify(anchor_id, node.finish());
                    array.push(node);
                }
                Node::Map {
                    map,
                    cur_key,
                    pos: _,
                } => {
                    if let Some(key) = cur_key.take() {
                        let node = self.anchor_map.anchorify(anchor_id, node.finish());
                        map.insert(key, node.into());
                    } else {
                        self.err = Some(ParseError::ExternalFormatError(
                            "yaml".into(),
                            "Nickel records only support string keys".into(),
                            node.pos().as_opt_ref().cloned(),
                        ));
                    }
                }
                Node::Scalar(_) => {
                    // The parser shouldn't allow us to get into this state.
                    debug_assert!(false, "scalars can't have children");
                }
            }
        } else {
            self.doc_stack.push((node, anchor_id));
        }
    }

    fn push_scalar_with_err(
        &mut self,
        value: Cow<'_, str>,
        style: ScalarStyle,
        anchor_id: Option<NonZeroUsize>,
        tag: Option<&Cow<'_, Tag>>,
        pos: TermPos,
    ) -> Result<(), ParseError> {
        // Returns Ok(Some(f)) if it succeeds, Ok(None) if the argument wasn't a float, or
        // Err it it was a float that Nickel doesn't support (i.e. inf or nan).
        //
        // The motivation here is that we want an error on ".inf" instead of silently treating
        // it as a string.
        let parse_float = |v: &str| -> Result<Option<Term>, ParseError> {
            match v {
                ".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" | "-.inf" | "-.Inf"
                | "-.INF" | ".nan" | ".NaN" | ".NAN" => Err(ParseError::ExternalFormatError(
                    "yaml".into(),
                    "Nickel numbers cannot be inf or NaN".into(),
                    pos.into_opt(),
                )),
                // `f64::from_str` will succeed on "NaN" and "inf". Since those aren't
                // valid YAML floats, we want to skip them.
                _ if v.as_bytes().iter().any(u8::is_ascii_digit) => {
                    let f = v.parse::<f64>().ok();
                    // unwrap: we've already checked for inf and nan
                    Ok(f.map(|f| Term::Num(Number::try_from_float_simplest(f).unwrap())))
                }
                _ => Ok(None),
            }
        };

        // Parse a YAML scalar, inferring the type from the value itself.
        let parse = |v: Cow<'_, str>| -> Result<Term, ParseError> {
            if let Some(number) = v.strip_prefix("0x") {
                if let Ok(i) = i64::from_str_radix(number, 16) {
                    return Ok(Term::Num(i.into()));
                }
            } else if let Some(number) = v.strip_prefix("0o") {
                if let Ok(i) = i64::from_str_radix(number, 8) {
                    return Ok(Term::Num(i.into()));
                }
            } else if let Some(number) = v.strip_prefix('+') {
                if let Ok(i) = number.parse::<i64>() {
                    return Ok(Term::Num(i.into()));
                }
            }
            match &*v {
                "~" | "null" | "Null" | "NULL" => Ok(Term::Null),
                "true" | "True" | "TRUE" => Ok(Term::Bool(true)),
                "false" | "False" | "FALSE" => Ok(Term::Bool(false)),
                _ => {
                    if let Ok(i) = v.parse::<i64>() {
                        Ok(Term::Num(i.into()))
                    } else if let Some(f) = parse_float(&v)? {
                        Ok(f)
                    } else {
                        Ok(Term::Str(v.into()))
                    }
                }
            }
        };

        let t = if style != ScalarStyle::Plain {
            // Any quoted scalar is a string.
            Term::Str(value.into())
        } else if let Some(tag) = tag.map(Cow::as_ref) {
            if tag.is_yaml_core_schema() {
                let t = match tag.suffix.as_ref() {
                    "bool" => value
                        .parse::<bool>()
                        .map(Term::Bool)
                        .map_err(|_| "invalid bool scalar"),
                    "int" => value
                        .parse::<i64>()
                        .map(|i| Term::Num(i.into()))
                        .map_err(|_| "invalid int scalar"),
                    "float" => parse_float(&value)?.ok_or_else(|| "invalid float scalar"),
                    "null" => match value.as_ref() {
                        "~" | "null" => Ok(Term::Null),
                        _ => Err("invalid null scalar"),
                    },
                    "str" => Ok(Term::Str(value.into())),
                    _ => Err("unknown tag"),
                };
                t.map_err(|msg| {
                    ParseError::ExternalFormatError("yaml".into(), msg.into(), pos.into_opt())
                })?
            } else {
                // If it isn't a core schema tag, try to infer the type.
                parse(value)?
            }
        } else {
            // If there's no tag, try to infer the type.
            parse(value)?
        };

        let node = Node::Scalar(RichTerm::new(t, pos));
        self.push_node(node, anchor_id);
        Ok(())
    }

    fn push_scalar(
        &mut self,
        value: Cow<'_, str>,
        style: ScalarStyle,
        anchor_id: Option<NonZeroUsize>,
        tag: Option<&Cow<'_, Tag>>,
        pos: TermPos,
    ) {
        if let Err(e) = self.push_scalar_with_err(value, style, anchor_id, tag, pos) {
            self.err = Some(e);
        }
    }

    /// If we're expecting the next item to be a map key, return somewhere to put it.
    ///
    /// The goal here is to delay parsing so that we can interpret map keys as strings as much as
    /// possible. For example, we want to parse
    ///
    /// ```yaml
    /// 1.00: foo
    /// ```
    ///
    /// as having a string key, with value "1.00". If we were to call
    /// `Scalar::parse_...` too early, it would parse the "1.00" as a number, and
    /// then lose information because it will store the number as "1" and forget
    /// that the original string was "1.00".
    ///
    /// So the idea is that you call `key_slot` to check whether the next value
    /// needs to be interpreted as a key. If so, you parse it as a string; if not,
    /// you let yaml decide how to interpret it.
    ///
    /// This only works for scalar keys: given the input
    ///
    /// ```yaml
    /// [bar, baz]: foo
    /// ```
    ///
    /// saphyr will produce a map whose key is a list (which we don't support in Nickel).
    fn key_slot(&mut self) -> Option<&mut Option<LocIdent>> {
        match self.doc_stack.last_mut() {
            Some((Node::Map { cur_key, .. }, _)) if cur_key.is_none() => Some(cur_key),
            _ => None,
        }
    }
}

impl<'input> SpannedEventReceiver<'input> for YamlLoader {
    fn on_event(&mut self, ev: saphyr_parser::Event<'input>, span: saphyr_parser::Span) {
        // saphyr-parser doesn't provide a way for the loader to signal an
        // error. So we store an error in our internal state and just refuse to
        // process anything once we have one.
        if self.err.is_some() {
            return;
        }

        use saphyr_parser::Event::*;
        let pos = mk_pos(self.file_id, span.start.index(), span.end.index());
        match ev {
            DocumentStart(_) | Nothing | StreamStart | StreamEnd => {}
            DocumentEnd => {
                let doc = match self.doc_stack.len() {
                    0 => RichTerm::new(Term::Null, pos),
                    1 => self.doc_stack.pop().unwrap().0.finish(),
                    _ => unreachable!(),
                };
                self.docs.push(doc);
            }
            Alias(id) => {
                // unwrap: saphyr uses id zero to represent no id, so it should
                // never be the payload of the Alias variant.
                let id = NonZeroUsize::new(id).unwrap();
                self.push_node(Node::Scalar(self.anchor_map.var(id)), None);
            }
            Scalar(value, style, anchor_id, tag) => {
                if let Some(key_slot) = self.key_slot() {
                    let key = LocIdent::from(value.as_ref()).with_pos(pos);
                    *key_slot = Some(key);
                } else {
                    let aid = NonZeroUsize::new(anchor_id);
                    self.push_scalar(value, style, aid, tag.as_ref(), pos);
                }
            }
            SequenceStart(anchor_id, _tag) => {
                let anchor_id = NonZeroUsize::new(anchor_id);
                self.anchor_map.reserve(anchor_id);
                let node = Node::Array {
                    array: Array::default(),
                    pos,
                };
                self.doc_stack.push((node, anchor_id));
            }
            MappingStart(anchor_id, _tag) => {
                let anchor_id = NonZeroUsize::new(anchor_id);
                self.anchor_map.reserve(anchor_id);
                let node = Node::Map {
                    map: IndexMap::default(),
                    cur_key: None,
                    pos,
                };
                self.doc_stack.push((node, anchor_id));
            }
            SequenceEnd | MappingEnd => {
                let end_idx = ByteIndex::from(span.end.index() as u32);
                let (node, anchor_id) = self.doc_stack.pop().unwrap();
                let node = node.with_end_pos(end_idx);
                self.push_node(node, anchor_id);
            }
        }
    }
}

/// Parse a YAML string and convert it to a [`RichTerm`].
///
/// If `file_id` is provided, the `RichTerm` will have its positions filled out.
pub fn load_yaml(s: &str, file_id: Option<FileId>) -> Result<RichTerm, ParseError> {
    let mut loader = YamlLoader {
        file_id,
        ..YamlLoader::default()
    };
    let mut parser = Parser::new(BufferedInput::new(s.chars()));
    parser
        .load(&mut loader, true)
        .map_err(|e| ParseError::from_yaml(e, file_id))?;
    if let Some(e) = loader.err {
        return Err(e);
    }
    let mut yaml = loader.docs;

    let main_term = if yaml.is_empty() {
        RichTerm::new(Term::Null, mk_pos(file_id, 0, 0))
    } else if yaml.len() == 1 {
        yaml.pop().unwrap()
    } else {
        RichTerm::new(
            Term::Array(yaml.into_iter().collect(), ArrayAttrs::default()),
            mk_pos(file_id, 0, s.len()),
        )
    };

    if loader.anchor_map.map.is_empty() {
        Ok(main_term)
    } else {
        let bindings = loader
            .anchor_map
            .map
            .into_values()
            .map(|(id, rt)| (LocIdent::from(id), rt))
            .collect();
        Ok(Term::Let(
            bindings,
            main_term,
            LetAttrs {
                rec: true,
                ..Default::default()
            },
        )
        .into())
    }
}

#[cfg(test)]
mod tests {
    use regex::Regex;

    use super::*;

    // Turn a YAML string into a Nickel string, by converting to RichTerm
    // and pretty-printing. This is more convenient for testing than
    // comparing RichTerms, because there are annoying differences between
    // StrChunks/String and RecRecord/Record.
    fn yaml_to_ncl(s: &str) -> String {
        load_yaml(s, None).unwrap().to_string()
    }

    #[test]
    fn basic_yaml_loading() {
        assert_eq!(&yaml_to_ncl("[1, 2]"), "[ 1, 2 ]");
        assert_eq!(&yaml_to_ncl("{a: 1, b: 2}"), "{ a = 1, b = 2, }");
        assert_eq!(&yaml_to_ncl(""), "null");

        // Multiple yaml documents get turned into a list.
        let multi_doc = r#"
1
---
2
"#;
        assert_eq!(&yaml_to_ncl(multi_doc), "[ 1, 2 ]");
    }

    #[test]
    fn yaml_refs() {
        let fresh_re = Regex::new("%[0-9]+").unwrap();

        // We can't predict what the generated fresh identifiers will be,
        // so replace them all with "%gen".
        let yaml_to_censored_ncl =
            |s: &str| -> String { fresh_re.replace_all(&yaml_to_ncl(s), "%gen").into_owned() };

        let basic_ref = r#"
bar: &ref
    - 1
    - 2
baz: *ref
"#;
        assert_eq!(
            &yaml_to_censored_ncl(basic_ref),
            "let rec %gen = [ 1, 2 ] in { bar = %gen, baz = %gen, }"
        );

        let recursive_ref = r#"
foo: &ref
    - 1
    - bar: *ref
"#;
        assert_eq!(
            &yaml_to_censored_ncl(recursive_ref),
            "let rec %gen = [ 1, { bar = %gen, } ] in { foo = %gen, }"
        );
    }
}
