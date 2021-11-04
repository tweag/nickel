use codespan::ByteIndex;
use log::debug;
use nickel::{position::TermPos, typecheck::linearization};

/// Finds the index of a linearization item for a given location
/// The linearization is a list of items that are sorted by their physical occurence.
/// - Each element has a corresponding span in the source
/// - Spans are either equal (same starting point, same length)
///   or shorter but never intersecting
///   
///   (start_element_2 >= start_element_1 AND end_element_2 <= end_element_1)
///
/// For any location a binary search is used to efficiently find the index
/// of the *last* element that starts at this position.
/// This corresponds to the most concrete Element as the linearization is
/// 1. produced by a stable sort and
/// 2. lower elements are more concrete
///
/// If a perfect match cannot be found, the binary search still provides an
/// anchor point from which we reversely find the first element that *contains*
/// the location looked up
///
/// If neither is possible `None` is returned as no corresponding linearization
/// item could be found.
///  
pub fn find_linearization_index(
    linearization: &Vec<linearization::LinearizationItem<nickel::types::Types>>,
    locator: (codespan::FileId, ByteIndex),
) -> Option<usize> {
    let (file_id, start) = locator;
    let index = match linearization.binary_search_by_key(&locator, |item| match item.pos {
        TermPos::Original(span) | TermPos::Inherited(span) => (span.src_id, span.start),
        TermPos::None => unreachable!(),
    }) {
        // Found item(s) starting at `locator`
        // search for most precise element
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
        // No perfect match found
        // iterate back finding the first wrapping linearization item
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
                    // Returning the stored item directly ensures we return the first (reversly) found item
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
