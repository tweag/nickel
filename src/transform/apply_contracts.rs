//! Generate contract applications from annotations.
//!
//! During the evaluation, the following invariant is enforced: any contract (be it the type
//! annotation or a contract annotation) contained in a `MetaValue` must have been applied to the
//! inner value of this metavalue. This invariant is false just after parsing, as there's merely no
//! direct `Assume` in the output AST. This transformation makes it true after program
//! transformations by generating corresponding assume.
//!
//! It must be run before `share_normal_form` to avoid rechecking contracts each time the inner
//! value is unwrapped.
use crate::term::{RichTerm, Term};
use crate::{match_sharedterm, mk_app};

/// If the top-level node of the AST is a meta-value, apply the meta-value's contracts to the inner
/// value.  Otherwise, return the term unchanged.
pub fn transform_one(rt: RichTerm) -> RichTerm {
    let pos = rt.pos;
    match_sharedterm! {rt.term,
        with {
            Term::MetaValue(meta) if meta.value.is_some() => {
                let mut meta = meta;
                let inner = meta.types.iter().chain(meta.contracts.iter()).fold(
                    meta.value.take().unwrap(),
                    |acc, ctr| {
                        mk_app!(
                            ctr.types.clone().contract(),
                            Term::Lbl(ctr.label.clone()),
                            acc
                        )
                        .with_pos(pos)
                    },
                );

                meta.value.replace(inner);
                RichTerm::new(Term::MetaValue(meta), pos)
            }
        } else rt
    }
}
