use std::collections::BTreeMap;
use std::num::NonZero;
use std::rc::Rc;

use codespan::{ByteIndex, ByteOffset, FileId};
use indexmap::IndexMap;
use yaml_rust2::parser::{Event, MarkedEventReceiver, Tag};
use yaml_rust2::scanner::{Marker, ScanError, TScalarStyle};

use crate::identifier::LocIdent;
use crate::position::RawSpan;
use crate::term::array::{Array, ArrayAttrs};
use crate::term::record::{Field, RecordData};
use crate::term::{Number, RichTerm, Term};

// parse f64 as Core schema
// See: https://github.com/chyh1990/yaml-rust/issues/51
// FIXME: nickel doesn't support inf/nan anyway...
fn parse_f64(v: &str) -> Option<f64> {
    match v {
        ".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" => Some(f64::INFINITY),
        "-.inf" | "-.Inf" | "-.INF" => Some(f64::NEG_INFINITY),
        ".nan" | "NaN" | ".NAN" => Some(f64::NAN),
        _ => v.parse::<f64>().ok(),
    }
}

// yaml_rust2 uses usizes for optional anchor ids, with zero meaning "no id".
// We represent this using Option<NonZero<usize>> instead.
type AnchorId = NonZero<usize>;

enum Entry {
    Array {
        start: ByteIndex,
        elems: Vec<RichTerm>,
    },
    Map {
        start: ByteIndex,
        // We only support string keys.
        fields: IndexMap<LocIdent, Field>,
        last_key: Option<LocIdent>,
    },
    Scalar(RichTerm),
}

impl Entry {
    fn new_array(start: ByteIndex) -> Self {
        Self::Array {
            start,
            elems: Vec::new(),
        }
    }

    fn new_map(start: ByteIndex) -> Self {
        Self::Map {
            start,
            fields: IndexMap::new(),
            last_key: None,
        }
    }

    fn finish(self, src_id: FileId, end: ByteIndex) -> Result<RichTerm, ScanError> {
        match self {
            Entry::Array { start, elems } => {
                let term = Term::Array(
                    Array::new(Rc::from(elems.into_boxed_slice())),
                    ArrayAttrs::default(),
                );
                Ok(RichTerm::from(term).with_pos(RawSpan { src_id, start, end }.into()))
            }
            Entry::Map {
                start,
                fields,
                last_key,
            } => {
                if last_key.is_some() {
                    panic!();
                }
                let term = Term::Record(RecordData {
                    fields,
                    ..Default::default()
                });
                Ok(RichTerm::from(term).with_pos(RawSpan { src_id, start, end }.into()))
            }
            Entry::Scalar(rt) => Ok(rt),
        }
    }
}

struct YamlLoader {
    docs: Vec<RichTerm>,
    src_id: FileId,
    // states
    doc_stack: Vec<(Entry, Option<AnchorId>)>,
    anchor_map: BTreeMap<AnchorId, RichTerm>,
    /// An error, if one was encountered.
    error: Option<ScanError>,
}

pub fn load(input: &str, src_id: FileId) -> Result<RichTerm, LoadError> {
    let mut loader = YamlLoader {
        docs: Vec::new(),
        src_id,
        doc_stack: Vec::new(),
        anchor_map: BTreeMap::new(),
        error: None,
    };

    let mut parser = yaml_rust2::parser::Parser::new_from_str(input);
    parser.load(&mut loader, true).map_err(LoadError::Scan)?;

    // YAML files can contain multiple documents. If there is only
    // one we transparently deserialize it. If there are multiple,
    // we deserialize the file as an array.
    if loader.docs.is_empty() {
        Ok(RichTerm::from(Term::Null).with_pos(
            RawSpan {
                src_id,
                start: 0.into(),
                end: 0.into(),
            }
            .into(),
        ))
    } else if loader.docs.len() == 1 {
        Ok(loader.docs.pop().unwrap())
    } else {
        let term = Term::Array(
            Array::new(Rc::from(loader.docs.into_boxed_slice())),
            Default::default(),
        );
        Ok(term.into()) // TODO: attach a position
    }
}

impl MarkedEventReceiver for YamlLoader {
    fn on_event(&mut self, ev: Event, mark: Marker) {
        if self.error.is_some() {
            return;
        }
        if let Err(e) = self.on_event_impl(ev, mark) {
            self.error = Some(e);
        }
    }
}

/// An error that happened when loading a YAML document.
#[derive(Debug)]
pub enum LoadError {
    /// An I/O error.
    IO(std::io::Error),
    /// An error within the scanner. This indicates a malformed YAML input.
    Scan(ScanError),
    /// A decoding error (e.g.: Invalid UTF-8).
    Decode(std::borrow::Cow<'static, str>),
}

impl From<std::io::Error> for LoadError {
    fn from(error: std::io::Error) -> Self {
        LoadError::IO(error)
    }
}

impl YamlLoader {
    fn key_slot(&mut self) -> Option<&mut Option<LocIdent>> {
        match self.doc_stack.last_mut() {
            Some((
                Entry::Map {
                    last_key: x @ None, ..
                },
                _,
            )) => Some(x),
            _ => None,
        }
    }

    fn on_event_impl(&mut self, ev: Event, mark: Marker) -> Result<(), ScanError> {
        let byte = ByteIndex::from(mark.index() as u32);
        match ev {
            Event::DocumentStart | Event::Nothing | Event::StreamStart | Event::StreamEnd => {
                // do nothing
            }
            Event::DocumentEnd => {
                assert_eq!(self.doc_stack.len(), 1);
                let doc = self.doc_stack.pop().unwrap().0;
                match doc {
                    Entry::Scalar(s) => self.docs.push(s),
                    _ => panic!(),
                }
            }
            Event::SequenceStart(aid, _) => {
                self.doc_stack
                    .push((Entry::new_array(byte), AnchorId::new(aid)));
            }
            Event::MappingStart(aid, _) => {
                self.doc_stack
                    .push((Entry::new_map(byte), AnchorId::new(aid)));
            }
            Event::SequenceEnd | Event::MappingEnd => {
                let (node, aid) = self.doc_stack.pop().unwrap();
                self.insert_new_node(node.finish(self.src_id, byte)?, aid, mark)?;
            }
            Event::Scalar(v, style, aid, tag) => {
                let aid = AnchorId::new(aid);
                let end = byte + ByteOffset::from(v.len() as i64);
                let span = RawSpan {
                    src_id: self.src_id,
                    start: byte,
                    end,
                };
                if let Some(slot) = self.key_slot() {
                    *slot = Some(LocIdent::new(v).with_pos(span.into()));
                } else {
                    let term = if style != TScalarStyle::Plain {
                        Term::Str(v.into())
                    } else if let Some(Tag {
                        ref handle,
                        ref suffix,
                    }) = tag
                    {
                        if handle == "tag:yaml.org,2002:" {
                            match suffix.as_ref() {
                                "bool" => match v.parse::<bool>() {
                                    Err(_) => todo!(),
                                    Ok(v) => Term::Bool(v),
                                },
                                "int" => match v.parse::<i64>() {
                                    Err(_) => todo!(),
                                    Ok(v) => Term::Num(v.into()),
                                },
                                "float" => match parse_f64(&v) {
                                    Some(n) => {
                                        Term::Num(Number::try_from_float_simplest(n).unwrap())
                                    } // FIXME
                                    None => todo!(),
                                },
                                "null" => match v.as_ref() {
                                    "~" | "null" => Term::Null,
                                    _ => todo!(),
                                },
                                _ => Term::Str(v.into()),
                            }
                        } else {
                            Term::Str(v.into())
                        }
                    } else if let Ok(b) = v.parse::<bool>() {
                        Term::Bool(b)
                    } else if let Ok(i) = v.parse::<i64>() {
                        Term::Num(i.into())
                    } else if let Some(n) = parse_f64(&v) {
                        Term::Num(Number::try_from_float_simplest(n).unwrap())
                    } else {
                        // Datatype is not specified, or unrecognized
                        Term::Str(v.into())
                    };

                    let rt = RichTerm::from(term).with_pos(
                        RawSpan {
                            src_id: self.src_id,
                            start: byte,
                            end,
                        }
                        .into(),
                    );

                    self.insert_new_node(rt, aid, mark)?;
                }
            }
            Event::Alias(id) => {
                let n = match AnchorId::new(id).and_then(|id| self.anchor_map.get(&id)) {
                    Some(v) => v.clone(),
                    None => todo!(),
                };
                self.insert_new_node(n, None, mark)?;
            }
        }
        Ok(())
    }

    fn insert_new_node(
        &mut self,
        node: RichTerm,
        aid: Option<AnchorId>,
        mark: Marker,
    ) -> Result<(), ScanError> {
        // valid anchor id starts from 1
        if let Some(aid) = aid {
            self.anchor_map.insert(aid, node.clone());
        }
        if self.doc_stack.is_empty() {
            self.doc_stack.push((Entry::Scalar(node), None));
        } else {
            let parent = self.doc_stack.last_mut().unwrap();
            match &mut parent.0 {
                Entry::Array { elems, .. } => elems.push(node),
                Entry::Map {
                    fields, last_key, ..
                } => {
                    let Some(key) = last_key.take() else {
                        panic!();
                    };
                    if fields.insert(key, node.into()).is_some() {
                        panic!("duplicated value");
                    }
                }
                Entry::Scalar(_) => unreachable!(),
            }
        }
        Ok(())
    }
}
