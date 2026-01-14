use rkyv::{
    Archive,
    rend::u64_le,
    tuple::ArchivedTuple3,
    vec::{ArchivedVec, VecResolver},
    with::ArchiveWith,
};

use crate::ast::Number;

pub struct NumberStash;

impl ArchiveWith<Number> for NumberStash {
    type Archived = ArchivedTuple3<bool, ArchivedVec<u64_le>, ArchivedVec<u64_le>>;
    type Resolver = ((), VecResolver, VecResolver);

    fn resolve_with(n: &Number, resolver: Self::Resolver, out: rkyv::Place<Self::Archived>) {
        let sign = n >= &0;
        // TODO: with newer malachite (and some more code), we could do this without copying the number data.
        let (num, denom) = n.clone().into_numerator_and_denominator();
        (sign, num.into_limbs_asc(), denom.into_limbs_asc()).resolve(resolver, out);
    }
}
