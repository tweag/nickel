use malachite::Natural;
use nickel_lang_parser::ast::Number;
use rkyv::{
    Archive, Deserialize, Serialize,
    rc::{ArchivedRc, Flavor, RcResolver},
};

use crate::{position::PosIdx, term::Term};

use super::{
    ArrayData, CustomContractData, EnumVariantData, ForeignIdData, LabelData, NickelValue,
    NumberData, RecordData, SealingKeyData, StringData, Thunk, TypeData, ValueContent,
    lens::TermContent,
};

#[derive(Archive)]
#[rkyv(bytecheck(bounds(
    __C: rkyv::validation::ArchiveContext,
    __C: rkyv::validation::shared::SharedContext,
    <__C as rkyv::rancor::Fallible>::Error: rkyv::rancor::Source,
)))]
pub struct ValueOwned {
    pos_idx: PosIdx,
    #[rkyv(omit_bounds)]
    payload: ValuePayload,
}

#[derive(Archive)]
pub enum ValuePayload {
    Null,
    Bool(bool),
    Number(NumberStash),
    Array(ArrayData),
    Record(RecordData),
    String(StringData),
    Thunk(Thunk),
    Term(Term),
    Label(LabelData),
    EnumVariant(EnumVariantData),
    ForeignId(ForeignIdData),
    SealingKey(SealingKeyData),
    CustomContract(CustomContractData),
    Type(TypeData),
}

impl From<NickelValue> for ValueOwned {
    fn from(value: NickelValue) -> Self {
        let pos_idx = value.pos_idx();
        let payload = match value.content() {
            ValueContent::Null(_) => ValuePayload::Null,
            ValueContent::Bool(lens) => ValuePayload::Bool(lens.take()),
            ValueContent::Number(lens) => ValuePayload::Number(lens.take().into()),
            ValueContent::Array(lens) => ValuePayload::Array(lens.take().unwrap_or_alloc()),
            ValueContent::Record(lens) => ValuePayload::Record(lens.take().unwrap_or_alloc()),
            ValueContent::String(lens) => ValuePayload::String(lens.take()),
            ValueContent::Thunk(lens) => ValuePayload::Thunk(lens.take()),
            ValueContent::Term(term) => ValuePayload::Term(term.take()),
            ValueContent::Label(lens) => ValuePayload::Label(lens.take()),
            ValueContent::EnumVariant(lens) => ValuePayload::EnumVariant(lens.take()),
            ValueContent::ForeignId(lens) => ValuePayload::ForeignId(lens.take()),
            ValueContent::SealingKey(lens) => ValuePayload::SealingKey(lens.take()),
            ValueContent::CustomContract(lens) => ValuePayload::CustomContract(lens.take()),
            ValueContent::Type(lens) => ValuePayload::Type(lens.take()),
        };

        ValueOwned { pos_idx, payload }
    }
}

// TODO: with newer malachite (and some more code), we could do this without copying the number data.
#[derive(Archive, Serialize, Deserialize)]
struct NumberStash {
    sign: bool,
    num_limbs: Vec<u64>,
    denom_limbs: Vec<u64>,
}

impl From<Number> for NumberStash {
    fn from(n: Number) -> Self {
        let sign = n >= 0;
        let (num, denom) = n.into_numerator_and_denominator();
        NumberStash {
            sign,
            num_limbs: num.into_limbs_asc(),
            denom_limbs: denom.into_limbs_asc(),
        }
    }
}

impl From<NumberStash> for Number {
    fn from(nd: NumberStash) -> Self {
        Number::from_sign_and_naturals(
            nd.sign,
            Natural::from_owned_limbs_asc(nd.num_limbs),
            Natural::from_owned_limbs_asc(nd.denom_limbs),
        )
    }
}

pub struct NickelValueFlavor;

impl Flavor for NickelValueFlavor {
    // FIXME: what does this actually enable?
    const ALLOW_CYCLES: bool = true;
}

impl Archive for NickelValue {
    type Archived = ArchivedRc<<ValueOwned as Archive>::Archived, NickelValueFlavor>;

    type Resolver = RcResolver;

    fn resolve(&self, resolver: Self::Resolver, out: rkyv::Place<Self::Archived>) {
        todo!()
    }
}
