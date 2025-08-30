use codespan::ByteIndex;
use saphyr::{MarkedYaml, Scalar, YamlData, YamlLoader};
use saphyr_parser::{BufferedInput, Parser};

use crate::{
    error::ParseError,
    files::FileId,
    identifier::LocIdent,
    position::{RawSpan, TermPos},
    term::{array::ArrayAttrs, record::RecordData, Number, RichTerm, Term},
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

fn yaml_to_rich_term(
    mut yaml: MarkedYaml,
    file_id: Option<FileId>,
) -> Result<RichTerm, ParseError> {
    let to_pos = |s: saphyr_parser::Span| mk_pos(file_id, s.start.index(), s.end.index());
    let pos = to_pos(yaml.span);
    let term = match yaml.data {
        YamlData::Representation(..) => {
            yaml.data.parse_representation();
            return yaml_to_rich_term(yaml, file_id);
        }
        YamlData::Value(Scalar::Null) => Term::Null,
        YamlData::Value(Scalar::Boolean(b)) => Term::Bool(b),
        YamlData::Value(Scalar::Integer(i)) => Term::Num(i.into()),
        YamlData::Value(Scalar::FloatingPoint(f)) => Term::Num(
            Number::try_from_float_simplest(f.into_inner()).map_err(|_| {
                ParseError::ExternalFormatError(
                    "yaml".into(),
                    "Nickel numbers cannot be inf or NaN".into(),
                    pos.into_opt(),
                )
            })?,
        ),
        YamlData::Value(Scalar::String(s)) => Term::Str(s.into_owned().into()),
        YamlData::Sequence(items) => Term::Array(
            items
                .into_iter()
                .map(|it| yaml_to_rich_term(it, file_id))
                .collect::<Result<_, _>>()?,
            ArrayAttrs::default(),
        ),
        YamlData::Mapping(map) => {
            let fields = map
                .into_iter()
                .map(|(k, v)| {
                    let k_pos = to_pos(k.span);
                    let k = match k.data {
                        YamlData::Representation(s, ..) => LocIdent::from(s).with_pos(k_pos),
                        YamlData::Value(Scalar::String(s)) => LocIdent::from(s).with_pos(k_pos),
                        _ => {
                            return Err(ParseError::ExternalFormatError(
                                "yaml".into(),
                                "Nickel records only support string keys".into(),
                                k_pos.into_opt(),
                            ));
                        }
                    };
                    let v = yaml_to_rich_term(v, file_id)?;
                    Ok((k, v.into()))
                })
                .collect::<Result<_, ParseError>>()?;
            Term::Record(RecordData {
                fields,
                ..Default::default()
            })
        }
        YamlData::Tagged(_, inner) => {
            return yaml_to_rich_term(*inner, file_id);
        }
        YamlData::Alias(_) => todo!(),
        YamlData::BadValue => {
            return Err(ParseError::ExternalFormatError(
                "yaml".into(),
                "malformed yaml".into(),
                pos.into_opt(),
            ));
        }
    };
    Ok(RichTerm::new(term, pos))
}

pub fn load_yaml(s: &str, file_id: Option<FileId>) -> Result<RichTerm, ParseError> {
    let mut loader = YamlLoader::<MarkedYaml>::default();
    let mut parser = Parser::new(BufferedInput::new(s.chars()));
    // Leave things unparsed, so that we can always interpret map keys as strings.
    loader.early_parse(false);
    parser
        .load(&mut loader, true)
        .map_err(|e| ParseError::from_yaml(e, file_id))?;
    let mut yaml = loader.into_documents();

    if yaml.is_empty() {
        Ok(RichTerm::new(Term::Null, mk_pos(file_id, 0, 0)))
    } else if yaml.len() == 1 {
        yaml_to_rich_term(yaml.remove(0), file_id)
    } else {
        Ok(RichTerm::new(
            Term::Array(
                yaml.into_iter()
                    .map(|x| yaml_to_rich_term(x, file_id))
                    .collect::<Result<_, ParseError>>()?,
                ArrayAttrs::default(),
            ),
            mk_pos(file_id, 0, s.len()),
        ))
    }
}
