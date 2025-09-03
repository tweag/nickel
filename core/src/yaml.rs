use std::{borrow::Cow, collections::BTreeMap, num::NonZeroUsize};

use codespan::ByteIndex;
use indexmap::IndexMap;
use saphyr::{Scalar, ScalarStyle, Tag};
use saphyr_parser::{BufferedInput, Parser, SpannedEventReceiver};

use crate::{
    error::ParseError,
    files::FileId,
    identifier::LocIdent,
    position::{RawSpan, TermPos},
    term::{
        array::{Array, ArrayAttrs},
        record::{Field, RecordAttrs, RecordData},
        Number, RichTerm, Term,
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
enum EltInProgress<'input> {
    Elt(Elt),
    AmbiguousScalar {
        value: Cow<'input, str>,
        style: ScalarStyle,
        tag: Option<Cow<'input, Tag>>,
        pos: TermPos,
    },
}

impl EltInProgress<'_> {
    // TODO: we only use this in conjunction with finish
    fn into_elt(self) -> Result<Elt, ParseError> {
        match self {
            EltInProgress::Elt(e) => Ok(e),
            EltInProgress::AmbiguousScalar {
                value,
                style,
                tag,
                pos,
            } => {
                let scalar = Scalar::parse_from_cow_and_metadata(value, style, tag.as_ref())
                    .ok_or_else(|| {
                        ParseError::ExternalFormatError(
                            "yaml".into(),
                            "TODO".into(),
                            pos.into_opt(),
                        )
                    })?;
                let t = match scalar {
                    Scalar::Null => Term::Null,
                    Scalar::Boolean(b) => Term::Bool(b),
                    Scalar::Integer(i) => Term::Num(i.into()),
                    Scalar::FloatingPoint(f) => Term::Num(
                        Number::try_from_float_simplest(f.into_inner()).map_err(|_| {
                            ParseError::ExternalFormatError(
                                "yaml".into(),
                                "Nickel numbers cannot be inf or NaN".into(),
                                pos.into_opt(),
                            )
                        })?,
                    ),
                    Scalar::String(s) => Term::Str(s.into_owned().into()),
                };

                Ok(Elt::Scalar(RichTerm::new(t, pos)))
            }
        }
    }

    fn into_key(self) -> Result<LocIdent, ParseError> {
        match self {
            EltInProgress::AmbiguousScalar { pos, value, .. } => {
                Ok(LocIdent::from(value.as_ref()).with_pos(pos))
            }
            EltInProgress::Elt(e) => Err(ParseError::ExternalFormatError(
                "yaml".into(),
                "Nickel records only support string keys".into(),
                e.pos().as_opt_ref().cloned(),
            )),
        }
    }
}

#[derive(Clone, Debug)]
enum Elt {
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

impl Elt {
    fn pos(&self) -> &TermPos {
        match self {
            Elt::Array { pos, .. } | Elt::Map { pos, .. } => pos,
            Elt::Scalar(rt) => &rt.pos,
        }
    }

    fn finish(self) -> RichTerm {
        match self {
            Elt::Array { array, pos } => {
                RichTerm::new(Term::Array(array, ArrayAttrs::default()), pos)
            }
            Elt::Map { map, cur_key, pos } => {
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
            Elt::Scalar(rt) => rt,
        }
    }

    fn with_end_pos(mut self, end: ByteIndex) -> Self {
        match &mut self {
            Elt::Array { pos, .. } | Elt::Map { pos, .. } => {
                *pos = pos.map(|sp| RawSpan { end, ..sp });
            }
            Elt::Scalar(rt) => {
                rt.pos = rt.pos.map(|sp| RawSpan { end, ..sp });
            }
        }
        self
    }
}

#[derive(Default)]
struct YamlLoader {
    anchor_map: BTreeMap<NonZeroUsize, RichTerm>,
    doc_stack: Vec<(Elt, Option<NonZeroUsize>)>,
    docs: Vec<RichTerm>,
    file_id: Option<FileId>,
    err: Option<ParseError>,
}

impl YamlLoader {
    fn push_elt_with_err(
        &mut self,
        elt: EltInProgress<'_>,
        anchor_id: Option<NonZeroUsize>,
    ) -> Result<(), ParseError> {
        if let Some((parent, _)) = self.doc_stack.last_mut() {
            match parent {
                Elt::Array { array, pos: _ } => {
                    let elt = elt.into_elt()?.finish();
                    if let Some(aid) = anchor_id {
                        self.anchor_map.insert(aid, elt.clone());
                    }
                    array.push(elt);
                }
                Elt::Map {
                    map,
                    cur_key,
                    pos: _,
                } => {
                    if let Some(key) = cur_key.take() {
                        let elt = elt.into_elt()?.finish();
                        if let Some(aid) = anchor_id {
                            self.anchor_map.insert(aid, elt.clone());
                        }
                        map.insert(key, elt.into());
                    } else {
                        *cur_key = Some(elt.into_key()?);
                    }
                }
                Elt::Scalar(_) => {
                    // The parser shouldn't allow us to get into this state.
                    debug_assert!(false, "got a child of a scalar???");
                }
            }
        } else {
            self.doc_stack.push((elt.into_elt()?, anchor_id));
        }
        Ok(())
    }

    fn push_elt(&mut self, elt: EltInProgress<'_>, anchor_id: Option<NonZeroUsize>) {
        if let Err(e) = self.push_elt_with_err(elt, anchor_id) {
            if self.err.is_none() {
                self.err = Some(e);
            }
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
            Alias(_) => todo!(),
            Scalar(value, style, anchor_id, tag) => {
                self.push_elt(
                    EltInProgress::AmbiguousScalar {
                        value,
                        style,
                        tag,
                        pos,
                    },
                    NonZeroUsize::new(anchor_id),
                );
            }
            SequenceStart(anchor_id, _tag) => {
                let elt = Elt::Array {
                    array: Array::default(),
                    pos,
                };
                self.doc_stack.push((elt, NonZeroUsize::new(anchor_id)));
            }
            MappingStart(anchor_id, _tag) => {
                let elt = Elt::Map {
                    map: IndexMap::default(),
                    cur_key: None,
                    pos,
                };
                self.doc_stack.push((elt, NonZeroUsize::new(anchor_id)));
            }
            SequenceEnd | MappingEnd => {
                // TODO: we could handle recursive anchor ids (saphyr doesn't) by building
                // a recursive let block...
                let end_idx = ByteIndex::from(span.end.index() as u32);
                let (elt, anchor_id) = self.doc_stack.pop().unwrap();
                let elt = elt.with_end_pos(end_idx);
                self.push_elt(EltInProgress::Elt(elt), anchor_id);
            }
        }
    }
}

/// Parse a Yaml string and convert it to a [`RichTerm`].
///
/// If `file_id` is provided, the `RichTerm` will have its positions filled out.
pub fn load_yaml(s: &str, file_id: Option<FileId>) -> Result<RichTerm, ParseError> {
    let mut loader = YamlLoader {
        file_id,
        ..YamlLoader::default()
    };
    let mut parser = Parser::new(BufferedInput::new(s.chars()));
    // TODO: put this doc somewhere better.
    // Leave things unparsed, so that we can interpret map keys as strings as much as
    // possible. Without setting `early_parse`, saphyr will parse
    //
    // ```
    // 1.00: foo
    // ```
    //
    // as having a number key, and then it will lose information because it will
    // store the number as "1" and forget that the original string was "1.00".
    // By setting `early_parse` to `true`, we tell saphyr to produce a
    // `YamlData::Representation` for the key `1.00` instead of trying to infer
    // the type.
    //
    // This only works for scalar keys: given the input
    //
    // ```
    // [bar, baz]: foo
    // ```
    //
    // saphyr will produce a map whose key is a list (which we don't support in Nickel).
    parser
        .load(&mut loader, true)
        .map_err(|e| ParseError::from_yaml(e, file_id))?;
    if let Some(e) = loader.err {
        return Err(e);
    }
    let mut yaml = loader.docs;

    if yaml.is_empty() {
        Ok(RichTerm::new(Term::Null, mk_pos(file_id, 0, 0)))
    } else if yaml.len() == 1 {
        Ok(yaml.pop().unwrap())
    } else {
        Ok(RichTerm::new(
            Term::Array(yaml.into_iter().collect(), ArrayAttrs::default()),
            mk_pos(file_id, 0, s.len()),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::files::Files;
    use crate::parser::{lexer::Lexer, ErrorTolerantParserCompat};

    fn parse(s: &str) -> RichTerm {
        let id = Files::new().add("<test>", String::from(s));

        crate::parser::grammar::TermParser::new()
            .parse_strict_compat(id, Lexer::new(s))
            .unwrap()
            .without_pos()
    }

    #[test]
    fn basic_yaml_loading() {
        assert_eq!(load_yaml("[1, 2]", None).unwrap(), parse("[1, 2]"));
        // TODO: more test cases. It's a bit painful, because the Nickel parser
        // likes to produce StrChunks and RecRecords where our yaml loader likes
        // to produce Strings and Records.
    }
}
