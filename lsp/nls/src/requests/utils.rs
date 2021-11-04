use codespan::ByteIndex;
use log::debug;
use nickel::{position::TermPos, typecheck::linearization};

pub fn find_linearization_index(
    linearization: &Vec<linearization::LinearizationItem<nickel::types::Types>>,
    locator: (codespan::FileId, ByteIndex),
) -> Option<usize> {
    let (file_id, start) = locator;
    let index = match linearization.binary_search_by_key(&locator, |item| match item.pos {
        TermPos::Original(span) | TermPos::Inherited(span) => (span.src_id, span.start),
        TermPos::None => unreachable!(),
    }) {
        Ok(index) => linearization[index..]
            .iter()
            .enumerate()
            .take_while(|(_, item)| {
                let pos = match item.pos {
                    TermPos::Original(span) | TermPos::Inherited(span) => (span.src_id, span.start),
                    TermPos::None => unreachable!(),
                };
                pos == (file_id, start)
            })
            .inspect(|(offset, item)| debug!("taken: {:?} @ {}", item, index + offset))
            .map(|(offset, _)| index + offset)
            .last(),
        Err(index) => {
            linearization[..index]
                .iter()
                .enumerate()
                .rfold(None, |acc, (index, item)| {
                    let pos = match item.pos {
                        TermPos::Original(pos) | TermPos::Inherited(pos) => {
                            Some((pos.start, pos.end, pos.src_id))
                        }
                        TermPos::None => None,
                    };

                    acc.or_else(|| {
                        if pos == None {
                            return None;
                        }
                        let (istart, iend, ifile) = pos.unwrap();

                        debug!(
                            "{} < {} < {} in {:?} = {:?}",
                            istart, start, iend, file_id, ifile
                        );

                        if file_id == ifile && start > istart && start < iend {
                            return Some(index);
                        }

                        return None;
                    })
                })
        }
    };
    index
}
