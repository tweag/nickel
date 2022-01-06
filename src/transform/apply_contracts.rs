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
use crate::mk_app;
use crate::term::{RichTerm, Term};

/// If the top-level node of the AST is a meta-value, apply the meta-value's contracts to the inner
/// value.  Otherwise, return the term unchanged.
pub fn transform_one(rt: RichTerm) -> RichTerm {
    let RichTerm { term, pos } = rt;

    match *term {
        Term::MetaValue(mut meta) if meta.value.is_some() => {
            let inner = meta.types.iter().chain(meta.contracts.iter()).fold(
                meta.value.take().unwrap(),
                |acc, ctr| {
                    mk_app!(
                        ctr.types.clone().contract(),
                        Term::Lbl(ctr.label.clone()),
                        acc
                    )
                    .with_pos(pos.into_inherited())
                },
            );

            meta.value.replace(inner);
            RichTerm::new(Term::MetaValue(meta), pos.into_inherited())
        }
        t => RichTerm::new(t, pos),
    }
}
