//! Runtime representation of Nickel values.
//!
//! This module implements a custom memory layout for a memory-efficient representation of Nickel
//! values. See
//! [RFC007](https://github.com/tweag/nickel/blob/master/rfcs/007-bytecode-interpreter.md) for more
//! details.
use crate::{
    identifier::LocIdent,
    impl_display_from_pretty,
    label::Label,
    metrics::increment,
    position::{PosIdx, PosTable, TermPos},
    term::{
        ForeignIdPayload, IndexMap, Number, RecordOpKind, RuntimeContract, SealingKey, Term,
        record::Field, string::NickelString,
    },
    traverse::{Traverse, TraverseControl, TraverseOrder},
    typ::Type,
};

use malachite::base::num::conversion::traits::ToSci as _;
use nickel_lang_vector::Slice;
use std::{
    alloc::{Layout, alloc, dealloc},
    borrow::ToOwned,
    cell::{Cell, RefCell},
    convert::Infallible,
    fmt,
    mem::{ManuallyDrop, size_of, transmute},
    ptr::{self, NonNull},
};

pub use super::cache::lazy::{self, Thunk};
pub use crate::term::record::RecordData;

pub mod lens;

/// A Nickel array.
pub type Array = Slice<NickelValue, 32>;

/// A mismatch between an expected tag and the tag found during the decoding process of a value.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TagMismatchError;

/// The unified representation of Nickel values.
///
/// A tagged pointer to a [reference-counted Nickel value block][ValueBlockRc], or an inline value.
/// The two least significant bits of the pointer are used as the tag. See [ValueTag] for more
/// details.
#[derive(Eq)]
pub struct NickelValue {
    data: usize,
    // On 64-bits pointer-width archs, we can fit everything into one word. Otherwise, we stay safe
    // and use a separate field for the position index of inline values.
    #[cfg(not(target_pointer_width = "64"))]
    pos_idx: PosIdx,
}

// PartialEq is mostly used for tests, when it's handy to compare something to an expected result.
// Most of the instances aren't really meaningful to use outside of very simple cases, and you
// should avoid comparing terms directly.
impl PartialEq for NickelValue {
    fn eq(&self, other: &Self) -> bool {
        match (self.content_ref(), other.content_ref()) {
            (ValueContentRef::Null, ValueContentRef::Null) => true,
            (ValueContentRef::Bool(b1), ValueContentRef::Bool(b2)) => b1 == b2,

            (ValueContentRef::Number(number1), ValueContentRef::Number(number2)) => {
                number1 == number2
            }
            (ValueContentRef::Array(array1), ValueContentRef::Array(array2)) => array1 == array2,
            (ValueContentRef::Record(record1), ValueContentRef::Record(record2)) => {
                record1 == record2
            }
            (ValueContentRef::String(string1), ValueContentRef::String(string2)) => {
                string1 == string2
            }
            (ValueContentRef::Thunk(thunk1), ValueContentRef::Thunk(thunk2)) => thunk1 == thunk2,
            (ValueContentRef::Term(term1), ValueContentRef::Term(term2)) => term1 == term2,
            (ValueContentRef::Label(label1), ValueContentRef::Label(label2)) => label1 == label2,
            (
                ValueContentRef::EnumVariant(enum_variant1),
                ValueContentRef::EnumVariant(enum_variant2),
            ) => enum_variant1 == enum_variant2,
            (ValueContentRef::ForeignId(foreign_id1), ValueContentRef::ForeignId(foreign_id2)) => {
                foreign_id1 == foreign_id2
            }
            (
                ValueContentRef::SealingKey(sealing_key1),
                ValueContentRef::SealingKey(sealing_key2),
            ) => sealing_key1 == sealing_key2,
            (
                ValueContentRef::CustomContract(custom_contract1),
                ValueContentRef::CustomContract(custom_contract2),
            ) => custom_contract1 == custom_contract2,
            (ValueContentRef::Type(type1), ValueContentRef::Type(type2)) => type1 == type2,
            _ => false,
        }
    }
}

impl fmt::Debug for NickelValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NickelValue {{ pos_idx: {:?}, data: ", self.pos_idx())?;

        match self.content_ref() {
            ValueContentRef::Null => write!(f, "InlineValue(null)"),
            ValueContentRef::Bool(b) => write!(f, "InlineValue({b})"),
            ValueContentRef::Number(number) => write!(f, "Number({number})"),
            ValueContentRef::Array(container) => write!(f, "Array({container:?})"),
            ValueContentRef::Record(container) => write!(f, "Record({container:?})"),
            ValueContentRef::String(string) => write!(f, "String({string})"),
            ValueContentRef::Thunk(thunk) => write!(f, "Thunk({thunk:?})"),
            ValueContentRef::Term(term) => write!(f, "Term({term:?})"),
            ValueContentRef::Label(label) => write!(f, "Label({label:?})"),
            ValueContentRef::EnumVariant(enum_variant_data) => {
                write!(f, "EnumVariant({enum_variant_data:?})")
            }
            ValueContentRef::ForeignId(foreign_id) => {
                write!(f, "ForeignId({foreign_id:?})")
            }
            ValueContentRef::SealingKey(sealing_key) => {
                write!(f, "SealingKey({sealing_key:?})")
            }
            ValueContentRef::CustomContract(custom_contract) => {
                write!(f, "CustomContract({custom_contract:?})")
            }
            ValueContentRef::Type(type_data) => write!(f, "Type({type_data:?})"),
        }?;

        write!(f, "}}")
    }
}

impl std::fmt::Pointer for NickelValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_inline() {
            write!(f, "<inline>")
        } else {
            std::fmt::Pointer::fmt(&(self.data as *const ()), f)
        }
    }
}

// pointer-width-specific implementation
#[cfg(target_pointer_width = "64")]
impl NickelValue {
    /// The mask for the position index bits in an inline value.
    const INLINE_POS_IDX_MASK: usize = 0xFF_FF_FF_FF_00_00_00_00;

    /// Returns `self` with the inline position index set to `idx` if `self` is an inline value, or
    /// returns `self` unchanged otherwise.
    fn with_inline_pos_idx(mut self, idx: PosIdx) -> Self {
        if self.tag() == ValueTag::Inline {
            self.data = (self.data & !Self::INLINE_POS_IDX_MASK) | (usize::from(idx) << 32);
        }
        self
    }

    /// Returns the position index of `self` if it is an inline value, or `None` otherwise.
    #[inline]
    fn inline_pos_idx(&self) -> Option<PosIdx> {
        (self.tag() == ValueTag::Inline)
            .then(|| PosIdx::from_usize_truncate((self.data & Self::INLINE_POS_IDX_MASK) >> 32))
    }

    /// Creates a new inline value with an associated position index.
    #[inline]
    pub const fn inline(inline: InlineValue, idx: PosIdx) -> Self {
        NickelValue {
            data: inline as usize | (idx.to_usize() << 32),
        }
    }

    /// Converts `self` to an inline value bypassing any safety checks.
    ///
    /// # Safety
    ///
    /// `self` must hold a valid inline value.
    #[inline]
    pub unsafe fn as_inline_unchecked(&self) -> InlineValue {
        // Safety: `InlineValue` is `#[repr(usize)]`, ensuring that it has the same layout as
        // `self.0`. The precondition of the function ensures that `self.0` is a valid value for
        // [InlineValue].
        //
        // Since `!Self::inline_pos_idx_MASK` is `FF_FF_FF_FF`, `AND`ing `self.data` with this mask
        // ensures that the result fit in a u32, so we can use `as` fearlessly.
        unsafe { transmute::<u32, InlineValue>((self.data & !Self::INLINE_POS_IDX_MASK) as u32) }
    }

    /// Makes a raw byte-to-byte copy of another Nickel value, entirely ignoring reference counting
    /// operations.
    ///
    /// # Safety
    ///
    /// - the ref count of the value block must be adjusted manually to compensate for dropping `self`. Possible solutions are:
    ///   - manually increment the reference count
    ///   - prevent `other` from being dropped (e.g. using [std::mem::ManuallyDrop])
    ///   - prevent `self` from being dropped
    #[inline]
    unsafe fn raw_copy(other: &Self) -> Self {
        NickelValue { data: other.data }
    }

    /// Create a Nickel value from a raw pointer to a value block.
    ///
    /// # Safety
    ///
    /// - `ptr` must be a valid pointer to a [ValueBlockRc].
    /// - the ref count of the value block must be adjusted manually to compensate for dropping `self`. Possible solutions are:
    ///   - manually increment the reference count
    ///   - prevent the original block or value of `ptr` from being dropped (e.g. using
    ///     [std::mem::ManuallyDrop])
    #[inline]
    unsafe fn block(ptr: NonNull<u8>) -> Self {
        NickelValue {
            data: ptr.as_ptr() as usize,
        }
    }
}

// pointer-width-specific implementation
#[cfg(not(target_pointer_width = "64"))]
impl NickelValue {
    /// Returns `self` with the position index set to `idx` if `self` is an inline value, or
    /// returns `self` unchanged otherwise.
    #[inline]
    fn with_inline_pos_idx(mut self, idx: PosIdx) -> Self {
        // Although the inline position index is just dead data for value blocks on non-64 bits
        // architecture, we unconditionally update it to avoid branching
        self.pos_idx = idx;
        self
    }

    /// Returns the inline position index of `self` if it's an inline value, or `None` otherwise.
    fn inline_pos_idx(&self) -> Option<PosIdx> {
        if self.tag() == ValueTag::Inline {
            Some(self.pos_idx)
        } else {
            None
        }
    }

    /// Creates a new inline value.
    #[inline]
    pub const fn inline(inline: InlineValue, pos_idx: PosIdx) -> Self {
        NickelValue {
            data: inline as usize,
            pos_idx,
        }
    }

    /// Converts the inner value of `self` to an inline value bypassing any safety checks.
    ///
    /// # Safety
    ///
    /// `self` must hold a valid inline value.
    #[inline]
    pub unsafe fn as_inline_unchecked(&self) -> InlineValue {
        // Safety: `InlineValue` is `#[repr(usize)]`, ensuring that it has the same layout as
        // `self.0`. The precondition of the function ensures that `self.0` is a valid value for
        // [InlineValue].
        unsafe { transmute::<usize, InlineValue>(self.data) }
    }

    /// Makes a raw byte-to-byte copy of another Nickel value, entirely ignoring reference counting
    /// operations.
    ///
    /// # Safety
    ///
    /// - the ref count of the value block must be adjusted manually to compensate for dropping `self`. Possible solutions are:
    ///   - manually increment the reference count
    ///   - prevent `other` from being dropped (e.g. using [std::mem::ManuallyDrop])
    ///   - prevent `self` from being dropped
    #[inline]
    unsafe fn raw_copy(other: &Self) -> Self {
        NickelValue {
            data: other.data,
            pos_idx: other.pos_idx,
        }
    }

    /// Creates a Nickel value from a raw pointer to a value block.
    ///
    /// # Safety
    ///
    /// - `ptr` must be a valid pointer to a [ValueBlockRc].
    /// - the ref count of the value block must be adjusted manually to compensate for dropping `self`. Possible solutions are:
    ///   - manually increment the reference count,
    ///   - prevent `self` from being dropped (e.g. using [std::mem::ManuallyDrop])
    ///   - use a `ptr` coming from a call to [ValueBlockRc::into_raw]
    ///   - derive `ptr` from a value or a value block that won't be dropped
    #[inline]
    unsafe fn block(ptr: NonNull<u8>) -> Self {
        NickelValue {
            data: ptr.as_ptr() as usize,
            pos_idx: PosIdx::NONE,
        }
    }
}

impl NickelValue {
    /// The mask for the tag bits in a value pointer.
    const VALUE_TAG_MASK: usize = 0b11;

    /// Returns the tag bits of this value.
    #[inline]
    pub fn tag(&self) -> ValueTag {
        (self.data & Self::VALUE_TAG_MASK).try_into().unwrap()
    }

    /// Creates a new inline value without an associated position index. Same as
    /// `Self::inline(inline, PosIdx::NONE)`.
    #[inline]
    pub const fn inline_posless(inline: InlineValue) -> Self {
        Self::inline(inline, PosIdx::NONE)
    }

    /// Creates a new null value with the index set to [PosIdx::NONE].
    #[inline]
    pub const fn null() -> Self {
        Self::inline_posless(InlineValue::Null)
    }

    /// Creates a new true value with the index set to [PosIdx::NONE].
    #[inline]
    pub const fn bool_true() -> Self {
        Self::inline_posless(InlineValue::True)
    }

    /// Creates a new false value with the index set to [PosIdx::NONE].
    #[inline]
    pub const fn bool_false() -> Self {
        Self::inline_posless(InlineValue::False)
    }

    /// Creates a new boolean value.
    pub fn bool_value(value: bool, pos_idx: PosIdx) -> Self {
        if value {
            Self::inline(InlineValue::True, pos_idx)
        } else {
            Self::inline(InlineValue::False, pos_idx)
        }
    }

    /// Creates a new boolean value with the index set to [PosIdx::NONE].
    #[inline]
    pub fn bool_value_posless(value: bool) -> Self {
        Self::bool_value(value, PosIdx::NONE)
    }

    /// Creates a new empty array with the index set to [PosIdx::NONE].
    #[inline]
    pub const fn empty_array() -> Self {
        Self::inline_posless(InlineValue::EmptyArray)
    }

    /// Creates a new empty record with the index set to [PosIdx::NONE].
    #[inline]
    pub const fn empty_record() -> Self {
        Self::inline_posless(InlineValue::EmptyRecord)
    }

    /// Allocates a new number value.
    #[inline]
    pub fn number(value: impl Into<Number>, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(value.into(), pos_idx).into()
    }

    /// Allocates a new number value without any position set. Equivalent to `Self::number(value,
    /// PosIdx::NONE`
    #[inline]
    pub fn number_posless(value: impl Into<Number>) -> Self {
        Self::number(value, PosIdx::NONE)
    }

    /// Allocates a new string value.
    #[inline]
    pub fn string(value: impl Into<NickelString>, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(value.into(), pos_idx).into()
    }

    /// Allocates a new string value without any position set. Equivalent to `Self::string(value,
    /// PosIdx::NONE`
    #[inline]
    pub fn string_posless(value: impl Into<NickelString>) -> Self {
        Self::string(value, PosIdx::NONE)
    }

    /// Allocates a new array value. If the array is empty, it is automatically inlined as
    /// [InlineValue::EmptyArray].
    pub fn array(value: Array, pending_contracts: Vec<RuntimeContract>, pos_idx: PosIdx) -> Self {
        if value.is_empty() {
            Self::inline(InlineValue::EmptyArray, pos_idx)
        } else {
            ValueBlockRc::encode(
                ArrayData {
                    array: value,
                    pending_contracts,
                },
                pos_idx,
            )
            .into()
        }
    }

    /// Allocates a new array value without any position set. If the array is empty, it is
    /// automatically inlined as [InlineValue::EmptyArray].
    #[inline]
    pub fn array_posless(value: Array, pending_contracts: Vec<RuntimeContract>) -> Self {
        Self::array(value, pending_contracts, PosIdx::NONE)
    }

    /// Creates a new empty array, but doesn't inline it. This forces the allocation of a value
    /// block. [Self::empty_array_block] can be useful when one needs to have a pre-allocated array
    /// that will be mutated in a second step.
    #[inline]
    pub fn empty_array_block(pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(ArrayData::default(), pos_idx).into()
    }

    /// Allocates a new record value. If the record is empty, it is automatically inlined as
    /// [InlineValue::EmptyRecord].
    pub fn record(value: RecordData, pos_idx: PosIdx) -> Self {
        // We need to exclude empty open records here, because we would otherwise lose this bit of
        // information: an inline empty record is assumed to be closed.
        if value.is_empty() && !value.attrs.open {
            Self::inline(InlineValue::EmptyRecord, pos_idx)
        } else {
            ValueBlockRc::encode(value, pos_idx).into()
        }
    }

    /// Allocates a new record value without any position set. If the record is empty, it is
    /// automatically inlined as [InlineValue::EmptyRecord].
    pub fn record_posless(value: RecordData) -> Self {
        if value.is_empty() {
            Self::empty_record()
        } else {
            ValueBlockRc::encode(value, PosIdx::NONE).into()
        }
    }

    /// Creates a new empty record, but doesn't inline it. This forces the allocation of a value
    /// block. [Self::empty_record] can be useful when one needs to have a pre-allocated record
    /// that will be mutated in a second step.
    #[inline]
    pub fn empty_record_block(pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(RecordData::empty(), pos_idx).into()
    }

    /// Allocates a new thunk value.
    #[inline]
    pub fn thunk(data: lazy::ThunkData, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(RefCell::new(data), pos_idx).into()
    }

    /// Allocates a new thunk value without any position set. Equivalent to `Self::thunk(value,
    /// PosIdx::NONE)`.
    #[inline]
    pub fn thunk_posless(data: lazy::ThunkData) -> Self {
        Self::thunk(data, PosIdx::NONE)
    }

    /// Allocates a new term value.
    #[inline]
    pub fn term(value: Term, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(value, pos_idx).into()
    }

    /// Allocates a new term value without any position set. Equivalent to `Self::term(value,
    /// PosIdx::NONE)`.
    #[inline]
    pub fn term_posless(value: Term) -> Self {
        Self::term(value, PosIdx::NONE)
    }

    /// Allocates a new label value.
    #[inline]
    pub fn label(value: Label, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(value, pos_idx).into()
    }

    /// Allocates a new label value without any position set. Equivalent to `Self::label(value,
    /// PosIdx::NONE)`.
    #[inline]
    pub fn label_posless(value: Label) -> Self {
        Self::label(value, PosIdx::NONE)
    }

    /// Allocates a new enum variant value.
    pub fn enum_variant(
        tag: impl Into<LocIdent>,
        arg: Option<NickelValue>,
        pos_idx: PosIdx,
    ) -> Self {
        ValueBlockRc::encode(
            EnumVariantData {
                tag: tag.into(),
                arg,
            },
            pos_idx,
        )
        .into()
    }

    /// Allocates a new enum variant value without any position set. Equivalent to
    /// `Self::enum_variant(tag, arg, PosIdx::NONE)`.
    #[inline]
    pub fn enum_variant_posless(tag: impl Into<LocIdent>, arg: Option<NickelValue>) -> Self {
        Self::enum_variant(tag.into(), arg, PosIdx::NONE)
    }

    /// Allocates a new enum tag value. Same as `Self::enum_variant(tag, None, pos_idx)`.
    #[inline]
    pub fn enum_tag(tag: impl Into<LocIdent>, pos_idx: PosIdx) -> Self {
        Self::enum_variant(tag.into(), None, pos_idx)
    }

    /// Allocates a new enum tag value without any position set. Equivalent to `Self::enum_tag(tag,
    /// arg, PosIdx::NONE)`.
    #[inline]
    pub fn enum_tag_posless(tag: impl Into<LocIdent>) -> Self {
        Self::enum_tag(tag.into(), PosIdx::NONE)
    }

    /// Allocates a new foreign ID value.
    #[inline]
    pub fn foreign_id(value: ForeignIdPayload, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(value, pos_idx).into()
    }

    /// Allocates a new foreign ID value without any position set. Equivalent to
    /// `Self::foreign_id(value, PosIdx::NONE)`.
    #[inline]
    pub fn foreign_id_posless(value: ForeignIdPayload) -> Self {
        Self::foreign_id(value, PosIdx::NONE)
    }

    /// Allocates a new sealing key value.
    #[inline]
    pub fn sealing_key(value: SealingKey, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(value, pos_idx).into()
    }

    /// Allocates a new sealing key value without any position set. Equivalent to
    /// `Self::sealing_key(value, PosIdx::NONE)`.
    #[inline]
    pub fn sealing_key_posless(value: SealingKey) -> Self {
        Self::sealing_key(value, PosIdx::NONE)
    }

    /// Allocates a new custom contract value.
    #[inline]
    pub fn custom_contract(value: NickelValue, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(value, pos_idx).into()
    }

    /// Allocates a new custom contract value without any position set. Equivalent to
    /// `Self::custom_contract(value, PosIdx::NONE)`.
    #[inline]
    pub fn custom_contract_posless(value: NickelValue) -> Self {
        Self::custom_contract(value, PosIdx::NONE)
    }

    /// Allocates a new type.
    #[inline]
    pub fn typ(typ: Type, contract: NickelValue, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(TypeData { typ, contract }, pos_idx).into()
    }

    /// Returns the inner value of `self` if it's an inline value, or `None` otherwise.
    #[inline]
    pub fn as_inline(&self) -> Option<InlineValue> {
        InlineValue::try_from(self).ok()
    }

    /// Returns the inner value of `self` if it's a (inlined) boolean value, or `None` otherwise.
    #[inline]
    pub fn as_bool(&self) -> Option<bool> {
        match self.as_inline()? {
            InlineValue::True => Some(true),
            InlineValue::False => Some(false),
            _ => None,
        }
    }

    /// Returns a reference to the inner number stored in this value if `self` is a value block
    /// with tag number, or `None` otherwise.
    #[inline]
    pub fn as_number(&self) -> Option<&NumberData> {
        self.as_value_data()
    }

    /// Returns a reference to the inner string stored in this value if `self` is a value block
    /// with tag string, or `None` otherwise.
    #[inline]
    pub fn as_string(&self) -> Option<&StringData> {
        self.as_value_data()
    }

    /// Returns a reference to the inner array stored in this value if `self` is a value block
    /// with tag array or an inline empty array. Returns `None` otherwise.
    #[inline]
    pub fn as_array(&self) -> Option<Container<&ArrayData>> {
        if self.is_inline_empty_array() {
            Some(Container::Empty)
        } else {
            self.as_value_data().map(Container::Alloc)
        }
    }

    /// Returns a reference to the inner record stored in this value if `self` is a value block
    /// with tag record or an inline empty record. Returns `None` otherwise.
    pub fn as_record(&self) -> Option<Container<&RecordData>> {
        if self.is_inline_empty_record() {
            Some(Container::Empty)
        } else {
            self.as_value_data().map(Container::Alloc)
        }
    }

    /// Returns a reference to the inner thunk data stored in this value if `self` is a value block
    /// with tag record or an inline empty record. Returns `None` otherwise.
    ///
    /// # Safety
    ///
    /// `self` must be a thunk value block, that is `self.data_tag()` must be [DataTag::Thunk].
    #[inline]
    pub unsafe fn as_thunk_data_unchecked(&self) -> &ThunkData {
        // Safety: function safety pre-condition
        unsafe {
            ValueBlockRc::decode_from_raw_unchecked(NonNull::new_unchecked(self.data as *mut u8))
        }
    }

    /// Returns `&self` seen as a thunk if `self` is a value block with tag thunk, or
    /// `None` otherwise.
    ///
    /// While it would be more in line with other `as_xxx` functions to return [ThunkData], in
    /// practice you often want [Thunk] instead, the latter being harder to work with.
    pub fn as_thunk(&self) -> Option<&Thunk> {
        if let Some(DataTag::Thunk) = self.data_tag() {
            // Safety: function safety pre-condition
            unsafe { Some(self.as_thunk_unchecked()) }
        } else {
            None
        }
    }

    /// Returns the same value but wrapped as a [crate::eval::cache::lazy::Thunk] if `self` is a
    /// value block with tag thunk, or `Err(self)` otherwise.
    pub fn try_into_thunk(self) -> Result<Thunk, NickelValue> {
        if let Some(DataTag::Thunk) = self.data_tag() {
            Ok(Thunk(self))
        } else {
            Err(self)
        }
    }

    /// Same as [Self::as_thunk], but doesn't check that the value is a block nor that the data tag
    /// is [DataTag::Thunk].
    ///
    /// This method is mostly intended to be used internally and from [Thunk] only. You should
    /// almost always use [Self::as_thunk] instead.
    ///
    /// # Safety
    ///
    /// `self` must be a value block with a thunk inside, that is `self.data_tag()` must be
    /// [DataTag::Thunk].
    #[inline]
    pub(super) unsafe fn as_thunk_unchecked(&self) -> &Thunk {
        // Safety: `Thunk` is a struct with a single field that is a `NickelValue` and has the
        // `transparent` representation, guaranteeing the same layout as `NickelValue`.
        unsafe { transmute::<&NickelValue, &Thunk>(self) }
    }

    /// Mutable version of [Self::as_thunk_unchecked].
    ///
    /// # Safety
    ///
    /// `self` must be a value block with a thunk inside, that is `self.data_tag()` must be
    /// [DataTag::Thunk].
    #[inline]
    pub(super) unsafe fn as_thunk_mut_unchecked(&mut self) -> &mut Thunk {
        // Safety: `Thunk` is a struct with a single field that is a `NickelValue` and has the
        // `transparent` representation, guaranteeing the same layout as `NickelValue`.
        unsafe { transmute::<&mut NickelValue, &mut Thunk>(self) }
    }

    /// Interprets this value as a thunk, without checking that the value is a block nor that the
    /// data tag is [DataTag::Thunk].
    ///
    /// This method is mostly intended to be used internally and from [Thunk] only. You should
    /// almost always use [Self::as_thunk] instead.
    ///
    /// # Safety
    ///
    /// `self` must be a value block with a thunk inside, that is `self.data_tag()` must be
    /// [DataTag::Thunk].
    #[inline]
    pub(super) unsafe fn into_thunk_unchecked(self) -> Thunk {
        // Safety: this function pre-condition.
        Thunk(self)
    }

    /// Returns a reference to the inner term stored in this value if `self` is a value block with
    /// a tag term, or `None` otherwise.
    #[inline]
    pub fn as_term(&self) -> Option<&TermData> {
        self.as_value_data()
    }

    /// Returns a reference to the inner label stored in this value if `self` is a value block with
    /// tag label, or `None` otherwise.
    #[inline]
    pub fn as_label(&self) -> Option<&LabelData> {
        self.as_value_data()
    }

    /// Returns a reference to the inner enum variant stored in this value if `self` is a value
    /// block with tag enum variant, or `None` otherwise.
    #[inline]
    pub fn as_enum_variant(&self) -> Option<&EnumVariantData> {
        self.as_value_data()
    }

    /// Returns a reference to the inner enum tag stored in this value if `self` is a value is a
    /// block with an enum variant inside, which has no arguments, or `None` otherwise.
    pub fn as_enum_tag(&self) -> Option<LocIdent> {
        let enum_var = self.as_enum_variant()?;
        enum_var.arg.is_none().then_some(enum_var.tag)
    }

    /// Returns a reference to the inner sealing key stored in this value if `self` is a value
    /// block with sealing key inside, or `None` otherwise.
    #[inline]
    pub fn as_sealing_key(&self) -> Option<&SealingKeyData> {
        self.as_value_data()
    }

    /// Returns a reference to the inner type stored in this value if `self` is a value with a type
    /// inside, or `None` otherwise.
    #[inline]
    pub fn as_type(&self) -> Option<&TypeData> {
        self.as_value_data()
    }

    /// Returns a reference to the inner foreign id stored in this value if `self` is a value
    /// block with a foreign value inside, or `None` otherwise.
    #[inline]
    pub fn as_foreign_id(&self) -> Option<&ForeignIdData> {
        self.as_value_data()
    }

    /// Returns the value block pointed to by this Nickel value if it's a value block, or `Err`
    /// otherwise. This is equivalent to `<Self as TryInto<ValueBlockRc>>::try_into`, but is more
    /// succinct and type-inference-friendly. Since this method must consume `self` in the case
    /// it's a value block, in order to keep the reference count correct, the caller can get `self`
    /// back in case of error.
    #[inline]
    pub fn into_block(self) -> Result<ValueBlockRc, Self> {
        ValueBlockRc::try_from(self)
    }

    /// Returns a reference to the content of this value if it's a value block of type `T`, or
    /// `None` otherwise.
    fn as_value_data<T: ValueBlockData>(&self) -> Option<&T> {
        if self.tag() == ValueTag::Pointer {
            // Safety: if `tag` is `Pointer`, the content of `self` must be a valid non-null
            // pointer to a value block.
            //
            // Since the value block won't be dropped until self is dropped, and since the output
            // lifetime of `&T` is tied to `&self`, `&T` won't outlive the value block.
            unsafe {
                ValueBlockRc::try_decode_from_raw(NonNull::new_unchecked(self.data as *mut u8))
            }
        } else {
            None
        }
    }

    /// Checks if this value is an inline value or is an allocated block.
    #[inline]
    pub fn is_inline(&self) -> bool {
        self.tag() == ValueTag::Inline
    }

    /// Returns a typed reference to the content of the value, or the content of the inline value
    /// directly.
    pub fn content_ref(&self) -> ValueContentRef<'_> {
        match self.tag() {
            // Safety: if `self.tag()` is `Pointer`, then the content of self must be a valid
            // non-null pointer to a value block.
            ValueTag::Pointer => unsafe {
                let as_ptr = NonNull::new_unchecked(self.data as *mut u8);
                let tag = ValueBlockRc::tag_from_raw(as_ptr);

                // Safety: additionally, the lifetime of the return `ValueContentRef<'_>` is tied to
                // `&self`, so the former won't outlive the value block.
                match tag {
                    DataTag::Number => {
                        ValueContentRef::Number(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    DataTag::Array => ValueContentRef::Array(Container::Alloc(
                        ValueBlockRc::decode_from_raw_unchecked(as_ptr),
                    )),
                    DataTag::Record => ValueContentRef::Record(Container::Alloc(
                        ValueBlockRc::decode_from_raw_unchecked(as_ptr),
                    )),
                    DataTag::String => {
                        ValueContentRef::String(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    DataTag::Thunk => {
                        // Safety: we checked that value's tag is `DataTag::Thunk`
                        ValueContentRef::Thunk(self.as_thunk_unchecked())
                    }
                    DataTag::Term => {
                        ValueContentRef::Term(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    DataTag::Label => {
                        ValueContentRef::Label(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    DataTag::EnumVariant => ValueContentRef::EnumVariant(
                        ValueBlockRc::decode_from_raw_unchecked(as_ptr),
                    ),
                    DataTag::ForeignId => {
                        ValueContentRef::ForeignId(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    DataTag::SealingKey => {
                        ValueContentRef::SealingKey(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    DataTag::CustomContract => ValueContentRef::CustomContract(
                        ValueBlockRc::decode_from_raw_unchecked(as_ptr),
                    ),
                    DataTag::Type => {
                        ValueContentRef::Type(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                }
            },
            ValueTag::Inline => {
                // Safety: `self.tag()` is `ValueTag::Inline`
                match unsafe { self.as_inline_unchecked() } {
                    InlineValue::EmptyArray => ValueContentRef::Array(Container::Empty),
                    InlineValue::EmptyRecord => ValueContentRef::Record(Container::Empty),
                    InlineValue::Null => ValueContentRef::Null,
                    InlineValue::True => ValueContentRef::Bool(true),
                    InlineValue::False => ValueContentRef::Bool(false),
                }
            }
        }
    }

    /// Returns a mutable typed reference to the content of the value. Returns `None` if the
    /// underlying value block has a reference count greater than one. See [Self::content_make_mut]
    /// in this case.
    ///
    /// # Thunks
    ///
    /// [ValueContentRefMut::Thunk] is special in that it returns a wrapper around `self`, and not
    /// a reference into the block. It's the only case that doesn't actually require to
    /// copy-on-write, and thus this method always return `Some` for thunks even if the block is
    /// shared.
    pub fn content_mut(&mut self) -> Option<ValueContentRefMut<'_>> {
        match self.tag() {
            // Safety: if `self.tag()` is `Pointer`, then the content of self must be a valid
            // non-null pointer to a value block.
            ValueTag::Pointer => unsafe {
                let as_ptr = NonNull::new_unchecked(self.data as *mut u8);
                let header = ValueBlockRc::header_from_raw(as_ptr);

                // [^thunk-copy-on-mut-ref]: We must be careful with thunks. Not only we don't need
                // to copy-on-write, since we're handing back a `Thunk` which is just a wrapper
                // around `self` (instead of a mutable reference inside the block), but we must
                // actually _avoid_ making copies, which would break the sharing of thunks.
                if header.ref_count() != 1 && header.tag() != DataTag::Thunk {
                    return None;
                }

                // Safety:
                //  - additionally, the lifetime of the return `ValueContentRef<'_>` is tied to
                //    `&mut self`, so the former won't outlive the value block.
                //  - we've checked above that `ref_count` is `1`, and we hold a mutable borrow of
                //  `  self`, so there can't be other active mutable borrows to the block
                Some(match header.tag() {
                    DataTag::Number => ValueContentRefMut::Number(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    DataTag::Array => ValueContentRefMut::Array(Container::Alloc(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    )),
                    DataTag::Record => ValueContentRefMut::Record(Container::Alloc(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    )),
                    DataTag::String => ValueContentRefMut::String(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    // Safety: `Thunk` is a struct with a single field that is a `NickelValue` and has the
                    // `transparent` representation, guaranteeing the same layout as `NickelValue`.
                    DataTag::Thunk => ValueContentRefMut::Thunk(self.as_thunk_mut_unchecked()),
                    DataTag::Term => ValueContentRefMut::Term(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    DataTag::Label => ValueContentRefMut::Label(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    DataTag::EnumVariant => ValueContentRefMut::EnumVariant(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    DataTag::ForeignId => ValueContentRefMut::ForeignId(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    DataTag::SealingKey => ValueContentRefMut::SealingKey(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    DataTag::CustomContract => ValueContentRefMut::CustomContract(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    DataTag::Type => ValueContentRefMut::Type(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                })
            },
            ValueTag::Inline => {
                // Safety: `self.tag()` is `ValueTag::Inline`
                Some(match unsafe { self.as_inline_unchecked() } {
                    InlineValue::EmptyArray => ValueContentRefMut::Array(Container::Empty),
                    InlineValue::EmptyRecord => ValueContentRefMut::Record(Container::Empty),
                    InlineValue::Null => ValueContentRefMut::Null(self),
                    InlineValue::True => ValueContentRefMut::Bool(self),
                    InlineValue::False => ValueContentRefMut::Bool(self),
                })
            }
        }
    }

    /// Returns a lazy, owned handle to the content of this value.
    pub fn content(self) -> ValueContent {
        use lens::{TermContent, ValueLens};

        match self.tag() {
            ValueTag::Pointer => {
                // Safety: if `self.tag()` is `ValueTag::Pointer`, `self.data` must be valid
                // pointer to a block.
                let as_ptr = unsafe { NonNull::new_unchecked(self.data as *mut u8) };
                // Safety: ditto
                let header = unsafe { ValueBlockRc::header_from_raw(as_ptr) };

                // Safety: in each branch, the tag of the value block matches the type parameter of
                // the lens built with `content_lens()`.
                unsafe {
                    match header.tag() {
                        DataTag::Number => ValueContent::Number(ValueLens::content_lens(self)),
                        DataTag::Array => ValueContent::Array(ValueLens::container_lens(self)),
                        DataTag::Record => ValueContent::Record(ValueLens::container_lens(self)),
                        DataTag::String => ValueContent::String(ValueLens::content_lens(self)),
                        DataTag::Thunk => ValueContent::Thunk(ValueLens::thunk_lens(self)),
                        DataTag::Term => {
                            let term: &TermData = ValueBlockRc::decode_from_raw_unchecked(as_ptr);

                            ValueContent::Term(match term {
                                Term::StrChunks(_) => {
                                    TermContent::StrChunks(ValueLens::term_str_chunks_lens(self))
                                }
                                Term::Fun(..) => TermContent::Fun(ValueLens::term_fun_lens(self)),
                                Term::FunPattern(..) => {
                                    TermContent::FunPattern(ValueLens::term_fun_pat_lens(self))
                                }
                                Term::Let(..) => TermContent::Let(ValueLens::term_let_lens(self)),
                                Term::LetPattern(..) => {
                                    TermContent::LetPattern(ValueLens::term_let_pat_lens(self))
                                }
                                Term::App(..) => TermContent::App(ValueLens::term_app_lens(self)),
                                Term::Var(..) => TermContent::Var(ValueLens::term_var_lens(self)),
                                Term::RecRecord(..) => {
                                    TermContent::RecRecord(ValueLens::term_rec_record_lens(self))
                                }
                                Term::Closurize(_) => {
                                    TermContent::Closurize(ValueLens::term_closurize_lens(self))
                                }
                                Term::Match(_) => {
                                    TermContent::Match(ValueLens::term_match_lens(self))
                                }
                                Term::Op1(..) => TermContent::Op1(ValueLens::term_op1_lens(self)),
                                Term::Op2(..) => TermContent::Op2(ValueLens::term_op2_lens(self)),
                                Term::OpN(..) => TermContent::OpN(ValueLens::term_opn_lens(self)),
                                Term::Sealed(..) => {
                                    TermContent::Sealed(ValueLens::term_sealed_lens(self))
                                }
                                Term::Annotated(..) => {
                                    TermContent::Annotated(ValueLens::term_annotated_lens(self))
                                }
                                Term::Import(_) => {
                                    TermContent::Import(ValueLens::term_import_lens(self))
                                }
                                Term::ResolvedImport(_) => TermContent::ResolvedImport(
                                    ValueLens::term_resolved_import_lens(self),
                                ),
                                Term::ParseError(_) => {
                                    TermContent::ParseError(ValueLens::term_parse_error_lens(self))
                                }
                                Term::RuntimeError(_) => TermContent::RuntimeError(
                                    ValueLens::term_runtime_error_lens(self),
                                ),
                            })
                        }
                        DataTag::Label => ValueContent::Label(ValueLens::content_lens(self)),
                        DataTag::EnumVariant => {
                            ValueContent::EnumVariant(ValueLens::content_lens(self))
                        }
                        DataTag::ForeignId => {
                            ValueContent::ForeignId(ValueLens::content_lens(self))
                        }
                        DataTag::SealingKey => {
                            ValueContent::SealingKey(ValueLens::content_lens(self))
                        }
                        DataTag::CustomContract => {
                            ValueContent::CustomContract(ValueLens::content_lens(self))
                        }
                        DataTag::Type => ValueContent::Type(ValueLens::content_lens(self)),
                    }
                }
            }
            ValueTag::Inline => {
                // Safety:
                //
                // as_inline_unchecked: `self.tag()` is `ValueTag::Inline`
                //
                // ValueLens::xxx_lens: in each branch, the type of the inline value matches the
                // type of the lens built with `xxx_lens()`.
                unsafe {
                    match self.as_inline_unchecked() {
                        InlineValue::Null => ValueContent::Null(ValueLens::null_lens(self)),
                        InlineValue::True | InlineValue::False => {
                            ValueContent::Bool(ValueLens::bool_lens(self))
                        }
                        InlineValue::EmptyArray => {
                            ValueContent::Array(ValueLens::container_lens(self))
                        }
                        InlineValue::EmptyRecord => {
                            ValueContent::Record(ValueLens::container_lens(self))
                        }
                    }
                }
            }
        }
    }

    /// Returns a mutable typed reference to the content of the value. This method is
    /// copy-on-write, same as [std::rc::Rc::make_mut] and [ValueBlockRc::make_mut]: if the
    /// underlying value block has a reference count greater than one, `self` is assigned to a fresh
    /// copy which is guaranteed to be 1-reference counted, and a mutable reference to the content
    /// of this copy is returned. Thunks are exception, and will never be copied; see
    /// [Self::content_mut].
    pub fn content_make_mut(&mut self) -> ValueContentRefMut<'_> {
        // Safety: `value.tag()` must be `Pointer` and `value.data_tag()` must be equal to `T::Tag`
        unsafe fn make_mut<T: ValueBlockData + Clone>(value: &mut NickelValue) -> &mut T {
            unsafe {
                // Safety: if `value.tag()` is `Pointer`, `value.data` is a non-null pointer (precondition)
                let mut as_ptr = NonNull::new_unchecked(value.data as *mut u8);
                let header = ValueBlockRc::header_from_raw(as_ptr);

                if header.ref_count() != 1 {
                    increment!("value::content_make_mut::strong clone");
                    let unique = ValueBlockRc::encode(
                        // Safety: `value.data_tag()` is `T::Tag` (precondition)
                        ValueBlockRc::decode_from_raw_unchecked::<T>(as_ptr).clone(),
                        header.pos_idx,
                    );
                    as_ptr = unique.0;
                    *value = unique.into();
                } else {
                    increment!("value::content_make_mut::no clone");
                }

                // Safety: we've made sure `value` is now unique
                ValueBlockRc::decode_mut_from_raw_unchecked::<T>(as_ptr)
            }
        }

        match self.tag() {
            // Safety: if `self.tag()` is `Pointer`, then the content of self must be a valid
            // non-null pointer to a value block.
            ValueTag::Pointer => unsafe {
                let as_ptr = NonNull::new_unchecked(self.data as *mut u8);
                let tag = ValueBlockRc::tag_from_raw(as_ptr);

                match tag {
                    DataTag::Number => ValueContentRefMut::Number(make_mut(self)),
                    DataTag::Array => ValueContentRefMut::Array(Container::Alloc(make_mut(self))),
                    DataTag::Record => ValueContentRefMut::Record(Container::Alloc(make_mut(self))),
                    DataTag::String => ValueContentRefMut::String(make_mut(self)),
                    // See [^thunk-copy-on-mut-ref].
                    DataTag::Thunk => {
                        // Safety: we just checked that the value's tag is `Thunk`.
                        ValueContentRefMut::Thunk(self.as_thunk_mut_unchecked())
                    }
                    DataTag::Term => ValueContentRefMut::Term(make_mut(self)),
                    DataTag::Label => ValueContentRefMut::Label(make_mut(self)),
                    DataTag::EnumVariant => ValueContentRefMut::EnumVariant(make_mut(self)),
                    DataTag::ForeignId => ValueContentRefMut::ForeignId(make_mut(self)),
                    DataTag::SealingKey => ValueContentRefMut::SealingKey(make_mut(self)),
                    DataTag::CustomContract => ValueContentRefMut::CustomContract(make_mut(self)),
                    DataTag::Type => ValueContentRefMut::Type(make_mut(self)),
                }
            },
            ValueTag::Inline => {
                // Safety: `self.tag()` is `ValueTag::Inline`
                match unsafe { self.as_inline_unchecked() } {
                    InlineValue::Null => ValueContentRefMut::Null(self),
                    InlineValue::True | InlineValue::False => ValueContentRefMut::Bool(self),
                    InlineValue::EmptyArray => ValueContentRefMut::Array(Container::Empty),
                    InlineValue::EmptyRecord => ValueContentRefMut::Record(Container::Empty),
                }
            }
        }
    }

    /// Returns the data tag of the underlying value block, or `None` if `self` is an inline value.
    pub fn data_tag(&self) -> Option<DataTag> {
        (self.tag() == ValueTag::Pointer).then(|| {
            // Safety: if `self.tag()` is `Pointer`, then `self.data` must be valid pointer to a
            // value block.
            unsafe {
                let as_ptr = NonNull::new_unchecked(self.data as *mut u8);
                ValueBlockRc::tag_from_raw(as_ptr)
            }
        })
    }

    /// Checks if this value is the inline empty array. Caution: note that `self` could also be an
    /// empty array, but allocated as a block. In that case, [Self::is_inline_empty_array] would
    /// still return `false`.
    pub fn is_inline_empty_array(&self) -> bool {
        self.as_inline()
            .is_some_and(|inl| matches!(inl, InlineValue::EmptyArray))
    }

    /// Checks if this value is the inline empty record. Caution: note that `self` could also be an
    /// empty record, but allocated as a block. In that case, [Self::is_inline_empty_record] would still
    /// return `false`.
    pub fn is_inline_empty_record(&self) -> bool {
        self.as_inline()
            .is_some_and(|inl| matches!(inl, InlineValue::EmptyRecord))
    }

    /// Checks if this value is the inlined boolean `true`.
    pub fn is_bool_true(&self) -> bool {
        self.as_inline()
            .is_some_and(|inl| matches!(inl, InlineValue::True))
    }

    /// Checks if this value is the inlined boolean `false`.
    pub fn is_bool_false(&self) -> bool {
        self.as_inline()
            .is_some_and(|inl| matches!(inl, InlineValue::False))
    }

    /// Checks if this value is the inlined constant `null`.
    pub fn is_null(&self) -> bool {
        self.as_inline()
            .is_some_and(|inl| matches!(inl, InlineValue::Null))
    }

    /// Determines if a value is in evaluated form, called weak head normal form (WHNF). See
    /// [crate::term::Term::is_whnf] for more detais.
    #[inline]
    pub fn is_whnf(&self) -> bool {
        !matches!(self.data_tag(), Some(DataTag::Thunk | DataTag::Term))
    }

    /// Returns the class of an expression in WHNF.
    ///
    /// The class of an expression is an approximation of its type used in error reporting. Class
    /// and type coincide for constants (numbers, strings and booleans) and arrays. Otherwise the
    /// class is less precise than the type and indicates the general shape of the term: `Record`
    /// for records, `Array` for arrays, etc. If the term is not a WHNF, `None` is returned.
    pub fn type_of(&self) -> Option<&'static str> {
        match self.content_ref() {
            ValueContentRef::Null => Some("Other"),
            ValueContentRef::Bool(_) => Some("Bool"),
            ValueContentRef::Number(_) => Some("Number"),
            ValueContentRef::Array(_) => Some("Array"),
            ValueContentRef::Record(_) => Some("Record"),
            ValueContentRef::String(_) => Some("String"),
            ValueContentRef::Term(term) => match term {
                Term::Closurize(v) => v.type_of(),
                Term::RecRecord(..) => Some("Record"),
                Term::Fun(..) | Term::FunPattern(..) => Some("Function"),
                Term::Match { .. } => Some("MatchExpression"),
                Term::Sealed(..) => Some("Sealed"),
                Term::Annotated(..) => Some("Annotated"),
                Term::Let(..)
                | Term::LetPattern(..)
                | Term::App(_)
                | Term::Var(_)
                | Term::Op1(_)
                | Term::Op2(_)
                | Term::OpN(_)
                | Term::Import(_)
                | Term::ResolvedImport(_)
                | Term::StrChunks(_)
                | Term::ParseError(_)
                | Term::RuntimeError(_) => None,
            },
            ValueContentRef::Label(_) => Some("Label"),
            ValueContentRef::EnumVariant(EnumVariantData { arg: None, tag: _ }) => Some("EnumTag"),
            ValueContentRef::EnumVariant(EnumVariantData {
                arg: Some(_),
                tag: _,
            }) => Some("EnumVariant"),
            ValueContentRef::ForeignId(_) => Some("ForeignId"),
            ValueContentRef::SealingKey(_) => Some("SealingKey"),
            ValueContentRef::CustomContract(_) => Some("CustomContract"),
            ValueContentRef::Type(_) => Some("Type"),
            _ => None,
        }
    }

    /// Determines if a term is a constant.
    ///
    /// In this context, a constant is an atomic literal of the language that mustn't contain any
    /// other sub-expression: null, a boolean, a number, a string, a label, an enum tag, an empty
    /// container (array or record), a sealing key or a foreign id.
    pub fn is_constant(&self) -> bool {
        match self.content_ref() {
            ValueContentRef::Null
            | ValueContentRef::Bool(_)
            | ValueContentRef::Array(Container::Empty)
            | ValueContentRef::Record(Container::Empty)
            | ValueContentRef::Number(_)
            | ValueContentRef::Label(_)
            | ValueContentRef::ForeignId(_)
            | ValueContentRef::SealingKey(_)
            | ValueContentRef::String(_) => true,
            ValueContentRef::EnumVariant(enum_variant) => enum_variant.arg.is_none(),
            ValueContentRef::Array(_)
            | ValueContentRef::Record(_)
            | ValueContentRef::Thunk(_)
            | ValueContentRef::Term(_)
            | ValueContentRef::CustomContract(_)
            | ValueContentRef::Type(_) => false,
        }
    }

    /// Determines if a value is an atom of the surface syntax. Atoms are basic elements of the
    /// syntax that can freely substituted without being parenthesized.
    pub fn fmt_is_atom(&self) -> bool {
        let Some(data_tag) = self.data_tag() else {
            // All inline values are atoms
            // Caution: if we inline some integers in the future as well, negative integers are NOT
            // atoms, so this path should be updated.
            return true;
        };

        match data_tag {
            DataTag::Array
            | DataTag::Record
            | DataTag::ForeignId
            | DataTag::SealingKey
            | DataTag::Label
            | DataTag::String => true,
            DataTag::Number => self.as_value_data::<NumberData>().unwrap() >= &0,
            DataTag::EnumVariant => self
                .as_value_data::<EnumVariantData>()
                .unwrap()
                .arg
                .is_none(),
            DataTag::Thunk => false,
            DataTag::Term => self.as_value_data::<TermData>().unwrap().fmt_is_atom(),
            DataTag::CustomContract => self
                .as_value_data::<CustomContractData>()
                .unwrap()
                .fmt_is_atom(),
            DataTag::Type => self.as_value_data::<TypeData>().unwrap().typ.fmt_is_atom(),
        }
    }

    /// Converts a primitive value (number, string, boolean, enum tag or null) to a Nickel string,
    /// or returns `None` if the value isn't primitive.
    pub fn to_nickel_string(&self) -> Option<NickelString> {
        match self.content_ref() {
            ValueContentRef::Null => Some("null".into()),
            ValueContentRef::Bool(b) => Some(b.to_string().into()),
            ValueContentRef::String(s) => Some(s.clone()),
            ValueContentRef::EnumVariant(EnumVariantData { tag, arg: None }) => Some((*tag).into()),
            ValueContentRef::Number(n) => Some(format!("{}", n.to_sci()).into()),
            _ => None,
        }
    }

    /// Checks for physical equality of two Nickel values. This is a very fast check that is
    /// complete for inline values but partial otherwise (i.e. it only returns `true` for pointers
    /// if the values physically point to the same value block).
    ///
    /// For inline values, this check **ignores** the position index.
    pub fn phys_eq(&self, other: &Self) -> bool {
        match self.tag() {
            ValueTag::Pointer => self.data == other.data,
            // Safety: the tag is checked to be `Inline`
            ValueTag::Inline => {
                other.is_inline()
                    && unsafe {
                        // Using `as_inline_unchecked` instead of `self.data` directly sidesteps the
                        // platform-specific differences
                        self.as_inline_unchecked() == other.as_inline_unchecked()
                    }
            }
        }
    }

    /// Updates the position of this value. First allocates a position index in the position table
    /// and use it as the new index.
    #[inline]
    pub fn with_pos(self, pos_table: &mut PosTable, pos: TermPos) -> Self {
        self.with_pos_idx(pos_table.push(pos))
    }

    /// Updates the position index of this value. If the value is a shared block (the ref
    /// count is greater than one), the result will be a new copy of the original value with the
    /// position set.
    pub fn with_pos_idx(self, pos_idx: PosIdx) -> Self {
        match self.tag() {
            ValueTag::Pointer => {
                // unwrap(): if the tag is `Pointer`, `ValueBlocRc::try_from(self)` must succeed.
                let mut block: ValueBlockRc = self.try_into().unwrap();
                block.make_unique();
                // Safety: `make_unique()` ensures there's no sharing, and we have the exclusive
                // access (ownership) of the block.
                unsafe { block.0.cast::<ValueBlockHeader>().as_mut().pos_idx = pos_idx }
                block.into()
            }
            // Safety: the tag is checked to be `Inline`
            ValueTag::Inline => self.with_inline_pos_idx(pos_idx),
        }
    }

    /// Returns the position index of this value, whether it is inline or not.
    pub fn pos_idx(&self) -> PosIdx {
        match self.tag() {
            // Safety: if `self.tag()` is `Pointer`, then `self.data` must be a valid non-null
            // pointer to a value block
            ValueTag::Pointer => unsafe {
                ValueBlockRc::header_from_raw(NonNull::new_unchecked(self.data as *mut u8)).pos_idx
            },
            // unwrap(): if the tag is `Inline`, then `inline_pos_idx()` must be `Some`
            ValueTag::Inline => self.inline_pos_idx().unwrap(),
        }
    }

    /// Returns the position associated to this value.
    #[inline]
    pub fn pos(&self, table: &PosTable) -> TermPos {
        table.get(self.pos_idx())
    }

    /// Returns the same value with all the positions (recursively) cleared (set to
    /// [crate::position::PosIdx::NONE]).
    ///
    /// This is currently only used in test code, but because it's used from integration
    /// tests we cannot hide it behind `#[cfg(test)]`.
    pub fn without_pos(self) -> Self {
        self.traverse(
            &mut |t: Type| {
                Ok::<_, Infallible>(Type {
                    pos: TermPos::None,
                    ..t
                })
            },
            TraverseOrder::BottomUp,
        )
        .unwrap()
        .traverse(
            &mut |val: NickelValue| {
                //unwrap(): will go away soon
                Ok::<_, Infallible>(val.with_pos_idx(PosIdx::NONE))
            },
            TraverseOrder::BottomUp,
        )
        .unwrap()
    }
}

impl Default for NickelValue {
    #[inline]
    fn default() -> Self {
        Self::null()
    }
}

// Since a `NickelValue` can be a reference-counted pointer in disguise, we can't just copy it
// blindly. We need to make sure the reference count is incremented accordingly.
impl Clone for NickelValue {
    #[inline]
    fn clone(&self) -> Self {
        if self.tag() == ValueTag::Pointer {
            unsafe {
                // Safety: if `self.tag()` is `Pointer`, `self.0` must be a valid non-null pointer
                // to a value block.
                //
                // We need to prevent this value block from being dropped as this would decrement
                // the refcount, nullifying our increment.
                ValueBlockRc::header_from_raw(NonNull::new_unchecked(self.data as *mut u8))
                    .inc_ref_count();
            }
        }

        // Safety: we incremented the ref count above, if the inner value is a block.
        unsafe { NickelValue::raw_copy(self) }
    }
}

// Same as for `Clone`: since we might be a reference-counted pointer in disguise, we need to
// properly decrement the underlying ref count.
impl Drop for NickelValue {
    #[inline]
    fn drop(&mut self) {
        if self.tag() == ValueTag::Pointer {
            unsafe {
                // Safety: if `self.tag()` is `Pointer`, `self.0` must be valid non-null pointer to
                // a value block
                let _ = ValueBlockRc::from_raw_unchecked(self.data as *mut u8);
            }
        }
    }
}

impl From<InlineValue> for NickelValue {
    #[inline]
    fn from(inline: InlineValue) -> Self {
        NickelValue::inline_posless(inline)
    }
}

impl<'a> TryFrom<&'a NickelValue> for InlineValue {
    type Error = TagMismatchError;

    fn try_from(value: &'a NickelValue) -> Result<Self, Self::Error> {
        if value.tag() == ValueTag::Inline {
            // Safety: we checked that the tag is `ValueTag::Inline`
            unsafe { Ok(value.as_inline_unchecked()) }
        } else {
            Err(TagMismatchError)
        }
    }
}

impl TryFrom<NickelValue> for ValueBlockRc {
    /// To avoid consuming the original value needlessly, it is returned upon error.
    type Error = NickelValue;

    fn try_from(value: NickelValue) -> Result<Self, Self::Error> {
        if value.tag() == ValueTag::Pointer {
            Ok(unsafe {
                // We need to prevent value from being dropped, or this will decrease the refcount.
                let value = ManuallyDrop::new(value);
                // Safety: if `self.tag()` is `Pointer`, `self.0` must be a valid non-null pointer
                // to a value block
                ValueBlockRc::from_raw_unchecked(value.data as *mut u8)
            })
        } else {
            Err(value)
        }
    }
}

impl From<ValueBlockRc> for NickelValue {
    // Converts this value block to a [NickelValue] pointer. To keep the reference count
    // consistent, this function consumes the value block without decrementing the reference
    // count, which is left unchanged by the conversion.
    #[inline]
    fn from(block: ValueBlockRc) -> Self {
        // We must avoid dropping `block` here, which would decrement the reference count.
        let this = ManuallyDrop::new(block);
        // Safety: this is a valid pointer to a `ValueBlockRc`, and the usage of manually drop
        // ensures the ref count is left unchanged.
        unsafe { NickelValue::block(this.0) }
    }
}

impl_display_from_pretty!(NickelValue);

/// Pointer tag used by [NickelValue] to discriminate between the pointer and non-pointer kind of Nickel values.
#[repr(usize)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
///////////
// CAUTION
///////////
// unsafe conversion functions from and to numeric types are relying on the precise values
// and range of `ValueTag`. If you add or remove tags, make sure to update all the corresponding
// code, in particular the `Self::MAX` constant.
//
// Values must be consecutive.
pub enum ValueTag {
    /// A heap-allocated value, meaning the tagged data is a valid pointer to [ValueBlockRc].
    Pointer = 0,
    /// The tag for an [InlineValue], which is not a pointer.
    Inline = 1,
}

impl ValueTag {
    /// The highest possible value when a tag is seen as a usize.
    pub const MAX: usize = ValueTag::Inline as usize;
}

impl From<ValueTag> for usize {
    #[inline]
    fn from(tag: ValueTag) -> Self {
        tag as usize
    }
}

/// Out of bounds error when trying to convert a numeric type to a tag.
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct TagOutOfBoundsError;

impl TryFrom<usize> for ValueTag {
    type Error = TagOutOfBoundsError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        if value <= ValueTag::MAX {
            // Safety: `#[repr(usize)]` on [ValueTag] guarantees that the enum is safe to transmute
            // to and from `usize`, as long as we are in the range of valid tags.
            Ok(unsafe { transmute::<usize, ValueTag>(value) })
        } else {
            Err(TagOutOfBoundsError)
        }
    }
}

/// Encode an inline value as a tagged pointer representation (a [NickelValue]).
const fn tag_inline(inline: u32) -> u32 {
    // Ensures the two highest bits of the inline value aren't set, so that `content << 2` doesn't
    // lose any information.
    debug_assert!(inline <= (u32::MAX >> 2));
    (inline << 2) | (ValueTag::Inline as u32)
}

/// Small Nickel values that can be inlined in the higher bits (excluding the tag) of the compact
/// representation of a Nickel value. Their numeric value is directly encoded with the tag
/// included, so that no bit shifting is needed at all when reading or writing them.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InlineValue {
    Null = tag_inline(0),
    True = tag_inline(1),
    False = tag_inline(2),
    EmptyArray = tag_inline(3),
    EmptyRecord = tag_inline(4),
}

/// The discriminating tag for the different kinds of data that can be store in a value block.
///////////
// CAUTION
///////////
// unsafe conversion functions from and to numeric types are relying on the `u8` representation,
// and precise values and range of `ValueTag`. If you add or remove tags, make sure to update all
// the corresponding code, in particular the `Self::MAX` constant.
//
// Values must be consecutive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum DataTag {
    Number = 0,
    Array = 1,
    Record = 2,
    String = 3,
    Thunk = 4,
    /// This is a temporary optimization. We could always store a term as a thunk with an empty
    /// environment, but this requires additional allocation, indirection, etc. As long as we keep
    /// the hybrid term/value representation, we'll use [Self::Term] to inline a term with an empty
    /// environment, typically when converting the new AST to the runtime representation.
    Term = 5,
    Label = 6,
    EnumVariant = 7,
    ForeignId = 8,
    SealingKey = 9,
    CustomContract = 10,
    Type = 11,
}

impl DataTag {
    /// Returns the offset of the data in a value block from the start pointer (the header). Calls
    /// to [ValueBlockRc::data_offset] under the hood instantiated with the right type.
    const fn data_offset(&self) -> usize {
        match self {
            DataTag::Number => ValueBlockRc::data_offset::<NumberData>(),
            DataTag::String => ValueBlockRc::data_offset::<StringData>(),
            DataTag::Array => ValueBlockRc::data_offset::<ArrayData>(),
            DataTag::Record => ValueBlockRc::data_offset::<RecordData>(),
            DataTag::Thunk => ValueBlockRc::data_offset::<ThunkData>(),
            DataTag::Term => ValueBlockRc::data_offset::<TermData>(),
            DataTag::Label => ValueBlockRc::data_offset::<LabelData>(),
            DataTag::EnumVariant => ValueBlockRc::data_offset::<EnumVariantData>(),
            DataTag::ForeignId => ValueBlockRc::data_offset::<ForeignIdData>(),
            DataTag::SealingKey => ValueBlockRc::data_offset::<SealingKeyData>(),
            DataTag::CustomContract => ValueBlockRc::data_offset::<CustomContractData>(),
            DataTag::Type => ValueBlockRc::data_offset::<TypeData>(),
        }
    }

    /// Returns the layout to be used for (de)allocation of a whole value block for a given type of
    /// data. Calls to [ValueBlockRc::block_layout] under the hood instantiated with the right
    /// type.
    const fn block_layout(&self) -> Layout {
        match self {
            DataTag::Number => ValueBlockRc::block_layout::<NumberData>(),
            DataTag::String => ValueBlockRc::block_layout::<StringData>(),
            DataTag::Array => ValueBlockRc::block_layout::<ArrayData>(),
            DataTag::Record => ValueBlockRc::block_layout::<RecordData>(),
            DataTag::Thunk => ValueBlockRc::block_layout::<ThunkData>(),
            DataTag::Term => ValueBlockRc::block_layout::<TermData>(),
            DataTag::Label => ValueBlockRc::block_layout::<LabelData>(),
            DataTag::EnumVariant => ValueBlockRc::block_layout::<EnumVariantData>(),
            DataTag::ForeignId => ValueBlockRc::block_layout::<ForeignIdData>(),
            DataTag::SealingKey => ValueBlockRc::block_layout::<SealingKeyData>(),
            DataTag::CustomContract => ValueBlockRc::block_layout::<CustomContractData>(),
            DataTag::Type => ValueBlockRc::block_layout::<TypeData>(),
        }
    }

    /// The highest possible value (included) for [Self].
    const MAX: u8 = DataTag::Type as u8;
}

impl From<DataTag> for u8 {
    #[inline]
    fn from(tag: DataTag) -> Self {
        tag as u8
    }
}

impl TryFrom<u8> for DataTag {
    type Error = TagOutOfBoundsError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value <= DataTag::MAX {
            // Safety: `#[repr(u8)]` on `DataTag` guarantees that the enum is safe to transmute to
            // and from `u8`, as long as we are in the range of valid tags.
            Ok(unsafe { transmute::<u8, DataTag>(value) })
        } else {
            Err(TagOutOfBoundsError)
        }
    }
}

/// The header for a heap-allocated Nickel value which is laid out at the beginning of a value
/// block directly followed by the value data.
// We set a minimal alignment of `4` bytes, so that pointers to the content of a value block
// (which are aligned to the max of the alignment of `ValueBlockHeader` and the content) is
// guaranteed to have at least the last 2 bits free for tagging (although we currently only use one
// bit).
#[repr(Rust, align(4))]
#[derive(Debug, Clone, PartialEq, Eq)]
struct ValueBlockHeader {
    /// The tag determining the type and the layout of the data following the header in a value
    /// block, and the (strong) reference count of the value, packed in 64 bits. The 8 most
    /// significant bits represent the tag, and the remaining 56 bits the reference count.
    ///
    /// This packed representation proved to perform better (or play better with optimizations at
    /// least) than having `data: DataTag` followed by `ref_count: [u8; 7]`. This makes [Self]
    /// 8-bytes aligned, meaning that currently we waste 4 unused bytes in the header. However, it
    /// was already the case before with a 12 bytes, 4-bytes aligned header (since all data but
    /// [SealingKeyData] are 8-bytes aligned anyway, and the latter is very rare). We can also use
    /// this space later to store more information.
    tag_and_ref_count: Cell<u64>,
    /// The position index of the source position of the value.
    pos_idx: PosIdx,
}

impl ValueBlockHeader {
    const TAG_MASK: u64 = 0xFF_00_00_00_00_00_00_00;
    const MAX_REF_COUNT: u64 = 0x00_FF_FF_FF_FF_FF_FF_FF;

    /// Packs a data tagand a reference count together.
    ///
    /// # Safety
    ///
    /// This function doesn't check that the reference count is representable on 56 bits.
    /// `ref_count` must be smaller or equal to [Self::MAX_REF_COUNT], or the data tag might be
    /// later decoded to an invalid value.
    const unsafe fn pack(tag: DataTag, ref_count: u64) -> u64 {
        ((tag as u64) << 56) | ref_count
    }

    /// Returns the [DataTag] of this value block.
    #[inline]
    pub fn tag(&self) -> DataTag {
        // Safety: `DataTag` is `repr(u8)`, and we maintain the invariant that the higher 8 bits
        // of `self.tag_and_ref_count` stores a valid `DataTag` value.
        unsafe { transmute::<u8, DataTag>((self.tag_and_ref_count.get() >> 56) as u8) }
    }

    /// Creates a new header for a value block with the given tag, a reference count of 1 and the
    /// given position index.
    #[inline]
    pub fn new(tag: DataTag, pos_idx: PosIdx) -> Self {
        Self {
            // Safety: 1 is smaller than `Self::MAX_REF_COUNT`
            tag_and_ref_count: unsafe { Cell::new(Self::pack(tag, 1)) },
            pos_idx,
        }
    }

    #[inline]
    fn ref_count(&self) -> u64 {
        self.tag_and_ref_count.get() & !Self::TAG_MASK
    }

    #[inline]
    fn set_ref_count(&self, count: u64) {
        // Safety: we checked above that `count` is smaller or equal to `Self::MAX_REF_COUNT`.
        unsafe {
            self.tag_and_ref_count.set(Self::pack(self.tag(), count));
        }

        if count > Self::MAX_REF_COUNT {
            panic!("reference count overfow");
        }
    }

    #[inline]
    fn inc_ref_count(&self) {
        // The code and the tricks of this method are inspired (or downright copied) from
        // `std::rc::Rc`.
        let count = self.ref_count();

        // We insert an `assume` here to hint LLVM at an otherwise missed optimization.
        // SAFETY: The reference count will never be zero when this is called.
        unsafe {
            std::hint::assert_unchecked(count != 0);
        }

        self.set_ref_count(count + 1);
    }

    #[inline]
    fn dec_ref_count(&self) {
        self.set_ref_count(self.ref_count() - 1);
    }
}

/// Marker trait for the data that can be stored in a Nickel value block after the header.
pub trait ValueBlockData {
    const TAG: DataTag;
}

pub type NumberData = Number;
pub type StringData = NickelString;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct ArrayData {
    pub array: Array,
    /// Arrays implement lazy contract application for performance reasons: contracts applied to
    /// this array are lazily accumulated in this field and only applied when an element is
    /// extracted.
    pub pending_contracts: Vec<RuntimeContract>,
}

pub type ThunkData = RefCell<lazy::ThunkData>;
pub type TermData = Term;
pub type LabelData = Label;

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariantData {
    pub tag: LocIdent,
    pub arg: Option<NickelValue>,
}

pub type ForeignIdData = ForeignIdPayload;
pub type CustomContractData = NickelValue;
pub type SealingKeyData = SealingKey;

#[derive(Clone, Debug, PartialEq)]
pub struct TypeData {
    /// The static type.
    pub typ: Type,
    /// The conversion of this type to a contract, that is, `typ.contract()?`. This field
    /// serves as a caching mechanism so we only run the contract generation code once per type
    /// written by the user.
    pub contract: NickelValue,
}

impl ValueBlockData for NumberData {
    const TAG: DataTag = DataTag::Number;
}

impl ValueBlockData for ArrayData {
    const TAG: DataTag = DataTag::Array;
}

impl ValueBlockData for RecordData {
    const TAG: DataTag = DataTag::Record;
}

impl ValueBlockData for StringData {
    const TAG: DataTag = DataTag::String;
}

impl ValueBlockData for ThunkData {
    const TAG: DataTag = DataTag::Thunk;
}

impl ValueBlockData for TermData {
    const TAG: DataTag = DataTag::Term;
}

impl ValueBlockData for LabelData {
    const TAG: DataTag = DataTag::Label;
}

impl ValueBlockData for EnumVariantData {
    const TAG: DataTag = DataTag::EnumVariant;
}

impl ValueBlockData for ForeignIdData {
    const TAG: DataTag = DataTag::ForeignId;
}

impl ValueBlockData for CustomContractData {
    const TAG: DataTag = DataTag::CustomContract;
}

impl ValueBlockData for SealingKeyData {
    const TAG: DataTag = DataTag::SealingKey;
}

impl ValueBlockData for TypeData {
    const TAG: DataTag = DataTag::Type;
}

/// A pointer to a heap-allocated, reference-counted Nickel value of variable size, although the
/// size is a deterministic function of the tag stored in the header (first word of the block).
///
/// # Layout
///
/// In the following, the type `T : ValueBlockData` is uniquely determined by the tag in the
/// header.
///
/// A value block is laid out as follows:
///
/// ```text
/// -------------------+---------+---+
/// | ValueBlockHeader | padding | T |
/// -------------------+---------+---+
/// ```
///
/// The base address (`self.0`) is `max(align_of::<ValueBlockHeader>(), align_of::<T>())`-aligned.
/// The padding is determined statically and is only necessary if `size_of::<ValueBlockHeader>()`
/// isn't a multiple of the total alignment. Currently, for example, there is no padding on x86_64
/// in practice. It might happen if the alignment of `T` grows larger than the size of the header.
///
/// The content of padding is left uninitialized and should not be accessed.
pub struct ValueBlockRc(NonNull<u8>);

impl ValueBlockRc {
    /// Creates a new [Self] from a raw pointer without checking for null.
    ///
    /// # Safety
    ///
    /// `ptr` must be a valid, non-null pointer to a value block.
    #[inline]
    pub unsafe fn from_raw_unchecked(ptr: *mut u8) -> Self {
        // Safety: function pre-condition
        unsafe { ValueBlockRc(NonNull::new_unchecked(ptr)) }
    }

    /// Returns the header of this value block.
    #[inline]
    fn header(&self) -> &ValueBlockHeader {
        // Safety: self.0 is always a valid pointer into a value block, and the lifetime of the
        // returned reference is tied to `&self`, guaranteeing that that the block will live as
        // long as the `&ValueBlockHeader` reference.
        unsafe { Self::header_from_raw(self.0) }
    }

    /// Returns the tag in the header of this value block.
    #[inline]
    fn tag(&self) -> DataTag {
        self.header().tag()
    }

    /// Returns the position index of this value.
    #[inline]
    pub fn pos_idx(&self) -> PosIdx {
        self.header().pos_idx
    }

    /// Returns a reference to the header of a value block pointed to by `ptr`.
    ///
    /// # Safety
    ///
    /// - `ptr` must be a valid pointer into a value block.
    /// - the corresponding value block must remain alive for `'a`.
    #[inline]
    unsafe fn header_from_raw<'a>(ptr: NonNull<u8>) -> &'a ValueBlockHeader {
        // Safety: the safety precondition of this function
        unsafe { ptr.cast::<ValueBlockHeader>().as_ref() }
    }

    /// Returns the tag in the header of a value block pointed to by `ptr`.
    ///
    /// # Safety
    ///
    /// - `ptr` must be a valid pointer into a value block.
    #[inline]
    unsafe fn tag_from_raw(ptr: NonNull<u8>) -> DataTag {
        // Safety: the safety precondition of this function
        unsafe { Self::header_from_raw(ptr).tag() }
    }

    /// Same as [std::rc::Rc::get_mut] but for a value block. Mutably borrows the value block.
    /// Returns an error if the tag doesn't match `T::Tag`.
    pub fn try_get_mut<T: ValueBlockData>(&mut self) -> Result<Option<&mut T>, TagMismatchError> {
        let header = self.header();

        if header.tag() != T::TAG {
            Err(TagMismatchError)
        } else if header.ref_count() != 1 {
            Ok(None)
        } else {
            // Safety: we know that the value block is unique, and we have a mutable borrow to the
            // only pointer to it, so we can safely decode the content to a mutable reference
            // without any risk of aliasing.
            unsafe { Ok(Some(self.decode_mut_unchecked::<T>())) }
        }
    }

    /// Panicking variant of [Self::try_get_mut]. Equivalent to
    /// `self.try_get_mut().unwrap().unwrap()`.
    #[inline]
    pub fn get_mut<T: ValueBlockData>(&mut self) -> &mut T {
        self.try_get_mut().unwrap().unwrap()
    }

    /// Same as [std::rc::Rc::make_mut] but for a value block. Returns `None` if the tag doesn't
    /// match `T::Tag`.
    pub fn try_make_mut<T: ValueBlockData + Clone>(&mut self) -> Option<&mut T> {
        if self.tag() != T::TAG {
            return None;
        }

        self.make_unique();
        // Safety: we know that the value block is unique, so we can safely decode the content
        // without any risk of aliasing.
        unsafe { Some(self.decode_mut_unchecked::<T>()) }
    }

    /// Panicking variant of [Self::try_make_mut]. Equivalent to `self.try_make_mut().unwrap()`.
    #[inline]
    pub fn make_mut<T: ValueBlockData + Clone>(&mut self) -> &mut T {
        self.try_make_mut().unwrap()
    }

    /// Makes `self` a unique (non shared) block, that is a 1-reference counted block with the same
    /// data. Similar to [Self::make_mut] in spirit but it doesn't require to specify the `T` and
    /// doesn't return a mutable reference.
    ///
    /// This is a no-op if `self` is 1-reference counted. Otherwise, a fresh (strong) copy is
    /// allocated and assigned to `self`.
    pub fn make_unique(&mut self) {
        if self.header().ref_count() == 1 {
            increment!("value::make_unique::no clone");
        } else {
            increment!("value::make_unique::strong clone");
            *self = self.strong_clone();
        }
    }

    /// Creates a copy of the content of this value block. As opposed to [Self::clone], which just
    /// increments the reference count but encapsulates a pointer to the same block in memory (as
    /// [std::rc::Rc::clone]), this method allocates a fresh block with a clone of the content and
    /// return a value that is 1-reference counted.
    pub fn strong_clone(&self) -> Self {
        match self.tag() {
            DataTag::Number => Self::encode(self.decode::<NumberData>().clone(), self.pos_idx()),
            DataTag::Array => Self::encode(self.decode::<ArrayData>().clone(), self.pos_idx()),
            DataTag::Record => Self::encode(self.decode::<RecordData>().clone(), self.pos_idx()),
            DataTag::String => Self::encode(self.decode::<StringData>().clone(), self.pos_idx()),
            DataTag::Thunk => Self::encode(self.decode::<ThunkData>().clone(), self.pos_idx()),
            DataTag::Term => Self::encode(self.decode::<TermData>().clone(), self.pos_idx()),
            DataTag::Label => Self::encode(self.decode::<LabelData>().clone(), self.pos_idx()),
            DataTag::EnumVariant => {
                Self::encode(self.decode::<EnumVariantData>().clone(), self.pos_idx())
            }
            DataTag::ForeignId => Self::encode(*self.decode::<ForeignIdData>(), self.pos_idx()),
            DataTag::SealingKey => Self::encode(*self.decode::<SealingKeyData>(), self.pos_idx()),
            DataTag::CustomContract => {
                Self::encode(self.decode::<CustomContractData>().clone(), self.pos_idx())
            }
            DataTag::Type => Self::encode(self.decode::<TypeData>().clone(), self.pos_idx()),
        }
    }

    /// Returns the required padding in bytes between the header and the data in [ValueBlockRc]
    /// depending on the tag. Calls to [ValueBlockRc::padding] under the hood instantiated with the
    /// right type.
    const fn padding<T: ValueBlockData>() -> usize {
        let align = Self::block_align::<T>();
        let leftover = size_of::<ValueBlockHeader>() % align;

        (align - leftover) % align
    }

    /// Returns the offset to add to the start of a value block (the address of the block header)
    /// to reach the data (including padding). Offsetting [Self::0] by [Self::data_offset]
    /// yields a valid pointer to a `T`.
    const fn data_offset<T: ValueBlockData>() -> usize {
        size_of::<ValueBlockHeader>() + Self::padding::<T>()
    }

    /// Returns the alignment in bytes of a value block for a given value content type `T`. This is
    /// the maximum of the alignment of the header and the alignment of `T`, and is guaranteed to
    /// be at least 4 bytes.
    const fn block_align<T: ValueBlockData>() -> usize {
        // Note: we can't use `std::cmp::max` in a const context
        if align_of::<ValueBlockHeader>() < align_of::<T>() {
            align_of::<T>()
        } else {
            align_of::<ValueBlockHeader>()
        }
    }

    /// Determines the layout for allocation or de-allocation of value blocks for a given value
    /// content type `T`.
    const fn block_layout<T: ValueBlockData>() -> Layout {
        let header_layout = Layout::new::<ValueBlockHeader>();
        let data_layout = Layout::new::<T>();
        let size = header_layout.size() + Self::padding::<T>() + data_layout.size();

        // The check is not tight (technically, there are a few valid size values for Layout that
        // will fail this assert) but it's simpler and the few corner cases don't matter in
        // practice.
        assert!(
            size + Self::block_align::<T>() <= isize::MAX as usize,
            "value block size overflow"
        );

        // Safety: align is the max of existing valid alignments, so it's a power of 2. The assert
        // above ensures that the nearest multiple of the alignment after `size` is less than
        // `isize::MAX`.
        unsafe { Layout::from_size_align_unchecked(size, Self::block_align::<T>()) }
    }

    /// Allocates a new value block with the given value `T` as content.
    fn encode<T: ValueBlockData>(value: T, pos_idx: PosIdx) -> Self {
        unsafe {
            // Safety: the layout of a block verifies `size > 0`
            let start = alloc(Self::block_layout::<T>());

            if start.is_null() {
                panic!("out of memory: failed to allocate memory for Nickel value")
            }

            let header_ptr = start as *mut ValueBlockHeader;
            header_ptr.write(ValueBlockHeader::new(T::TAG, pos_idx));

            // Safety: layout computations are correct (hopefully)
            let data_layout = start.add(Self::data_offset::<T>()) as *mut T;
            data_layout.write(value);

            // Safety: we abort if `start.is_null()` above, so `start` is not null
            Self(NonNull::new_unchecked(start))
        }
    }

    /// Tries to decode this value block as a reference to a value of type `T`. Returns `None` if
    /// the tag of this value block is not `T::TAG`.
    #[inline]
    fn try_decode<T: ValueBlockData>(&self) -> Option<&T> {
        // Safety: we decode only if `self.tag()` is `T::TAG`
        (self.tag() == T::TAG).then(|| unsafe { self.decode_unchecked() })
    }

    /// Panicking variant of [Self::try_decode]. Same as `self.try_decode().unwrap()`.
    #[inline]
    #[track_caller]
    fn decode<T: ValueBlockData>(&self) -> &T {
        self.try_decode().unwrap()
    }

    /// Unsafe variant of [Self::try_decode]. Doesn't perform any tag check, and blindly tries to
    /// decode the content of this block to a `&T`.
    ///
    /// # Safety
    ///
    /// The content of this value block must have been encoded from a value of type `T`, that is
    /// `self.tag() == T::TAG`.
    #[inline]
    unsafe fn decode_unchecked<T: ValueBlockData>(&self) -> &T {
        // Safety: preconditions
        unsafe { Self::decode_from_raw_unchecked(self.0) }
    }

    /// Mutable variant of [Self::decode_unchecked] (or unsafe variant of [Self::get_mut]).
    ///
    /// # Safety
    ///
    /// - The content of this value block must have been encoded from a value of type `T`, that is
    ///   `self.tag() == T::TAG`.
    /// - You must ensure that there is no active mutable reference inside this value block as long
    ///   as the returned mutable reference is alive. This is typically the case if the reference
    ///   count of the value block is 1.
    #[inline]
    unsafe fn decode_mut_unchecked<T: ValueBlockData>(&mut self) -> &mut T {
        // Safety: preconditions
        unsafe { Self::decode_mut_from_raw_unchecked(self.0) }
    }

    /// Given a pointer into a value block, blindly tries to decode the content to a `T` bypassing all safety checks.
    ///
    /// # Safety
    ///
    /// - The content of this value block must have been encoded from a value of type `T`, that is
    ///   `self.tag() == T::TAG`.
    /// - The lifetime `'a` of the returned reference must not outlive the value block.
    /// - The value block content must not be mutably borrowed during the lifetime `'a`.
    #[inline]
    unsafe fn decode_from_raw_unchecked<'a, T: ValueBlockData>(ptr: NonNull<u8>) -> &'a T {
        // Safety: preconditions
        unsafe { ptr.add(Self::data_offset::<T>()).cast::<T>().as_ref() }
    }

    /// Mutable variant of [Self::decode_from_raw_unchecked].
    ///
    /// # Safety
    ///
    /// - The content of this value block must have been encoded from a value of type `T`, that is
    ///   `self.tag() == T::TAG`.
    /// - The lifetime `'a` of the returned reference must not outlive the value block.
    /// - You must ensure that there is no active mutable reference inside this value block as long
    ///   as the returned mutable reference is alive (during `'a`). This is typically satisfied if
    ///   the reference count of the value block is `1`.
    #[inline]
    unsafe fn decode_mut_from_raw_unchecked<'a, T: ValueBlockData>(ptr: NonNull<u8>) -> &'a mut T {
        // Safety: preconditions
        unsafe { ptr.add(Self::data_offset::<T>()).cast::<T>().as_mut() }
    }

    /// Given a pointer into a value block, tries to decode the content to a `T`. Returns `None` if
    /// the tag of this value block is not `T::TAG`.
    ///
    /// Same as [Self::try_decode], but takes a raw (non-null) pointer instead of a value block.
    ///
    /// # Safety
    ///
    /// - The lifetime `'a` of the returned reference must not outlive the value block
    /// - The value block content must not be mutably borrowed during the lifetime `'a`.
    #[inline]
    unsafe fn try_decode_from_raw<'a, T: ValueBlockData>(ptr: NonNull<u8>) -> Option<&'a T> {
        // Safety: we've checked that the tag matched `T`. The rest of the safety conditions are
        // the pre-conditions of the current function `try_decode`.
        unsafe { (Self::tag_from_raw(ptr) == T::TAG).then(|| Self::decode_from_raw_unchecked(ptr)) }
    }

    // Non-inlined part of drop. Splitting the inlined and slow part is inspired from `std::rc::Rc`
    // code.
    #[inline(never)]
    unsafe fn drop_slow(&mut self) {
        unsafe {
            let tag = self.tag();
            // Safety: the value block is guaranteed to have been allocated with a size of
            // `size_of::<ValueBlockHeader>()` + `padding` + `size_of::<T>()`.
            let data_ptr = self.0.as_ptr().add(tag.data_offset());

            // Safety: `data_ptr` is a valid pointer for the corresponding type and it hasn't
            // been dropped before, as it's only dropped once when the last reference goes out
            // of scope.
            match tag {
                DataTag::Number => {
                    ptr::drop_in_place(data_ptr as *mut NumberData);
                }
                DataTag::Array => {
                    ptr::drop_in_place(data_ptr as *mut ArrayData);
                }
                DataTag::Record => {
                    ptr::drop_in_place(data_ptr as *mut RecordData);
                }
                DataTag::String => {
                    ptr::drop_in_place(data_ptr as *mut StringData);
                }
                DataTag::Thunk => {
                    ptr::drop_in_place(data_ptr as *mut ThunkData);
                }
                DataTag::Term => {
                    ptr::drop_in_place(data_ptr as *mut TermData);
                }
                DataTag::Label => {
                    ptr::drop_in_place(data_ptr as *mut LabelData);
                }
                DataTag::EnumVariant => {
                    ptr::drop_in_place(data_ptr as *mut EnumVariantData);
                }
                DataTag::ForeignId => {
                    ptr::drop_in_place(data_ptr as *mut ForeignIdData);
                }
                DataTag::CustomContract => {
                    ptr::drop_in_place(data_ptr as *mut CustomContractData);
                }
                DataTag::SealingKey => {
                    ptr::drop_in_place(data_ptr as *mut SealingKeyData);
                }
                DataTag::Type => {
                    ptr::drop_in_place(data_ptr as *mut TypeData);
                }
            };

            dealloc(self.0.as_ptr(), tag.block_layout());
        }
    }
}

impl Drop for ValueBlockRc {
    #[inline]
    fn drop(&mut self) {
        self.header().dec_ref_count();

        if self.header().ref_count() == 0 {
            unsafe {
                self.drop_slow();
            }
        }
    }
}

impl Clone for ValueBlockRc {
    #[inline]
    fn clone(&self) -> Self {
        self.header().inc_ref_count();
        Self(self.0)
    }
}

/// Empty containers are inlined, but this should be an implementation detail as much as possible.
/// This generic handler is providing a uniform interface to a container, that can either be
/// inlined as empty, or allocated (note that an allocated container can still be empty!).
#[derive(Clone, PartialEq, Debug, Copy)]
pub enum Container<C> {
    /// An empty, inlined value.
    Empty,
    /// An allocated container. **Note that an allocated container can still be empty as a container**:
    /// for example, an open record `{..}` needs to be allocated to store the open attribute, but
    /// will have an empty underlying map.
    Alloc(C),
}

impl<C> Container<C> {
    /// Converts this container to an optional. Returns `None` if `self` is [Container::Empty], and
    /// `Some` otherwise.
    pub fn into_opt(self) -> Option<C> {
        match self {
            Container::Empty => None,
            Container::Alloc(c) => Some(c),
        }
    }

    /// Unwrap the underlying allocation, or panics if `self` is [Self::Empty].
    #[inline]
    pub fn unwrap_alloc(self) -> C {
        self.into_opt().unwrap()
    }
}

impl<C: Default> Container<C> {
    /// Unwrap the underlying container, or allocate a new empty one (via [Default]) if `self` is
    /// [Container::Empty].
    #[inline]
    pub fn unwrap_or_alloc(self) -> C {
        self.into_opt().unwrap_or_default()
    }
}

impl<C: ToOwned> Container<&C> {
    /// Clones the underlying container if `self` is [Self::Alloc], or returns [Self::Empty]
    /// otherwise.
    pub fn to_owned(&self) -> Container<C::Owned> {
        match self {
            Container::Empty => Container::Empty,
            Container::Alloc(c) => Container::Alloc((*c).to_owned()),
        }
    }
}

impl Container<&RecordData> {
    /// Retrieves a field from [crate::term::record::RecordData::fields], or returns `None` if `self`
    /// is [Self::Empty].
    pub fn get(&self, id: LocIdent) -> Option<&Field> {
        self.into_opt().and_then(|record| record.fields.get(&id))
    }

    /// Returns a vector of all the fields' names of this record sorted alphabetically. Returns an
    /// empty vector if `self` is [Self::Empty].
    pub fn field_names(&self, op_kind: RecordOpKind) -> Vec<LocIdent> {
        self.into_opt()
            .map(|record| record.field_names(op_kind))
            .unwrap_or_default()
    }

    /// Returns the length of the underlying record.
    pub fn len(&self) -> usize {
        self.into_opt().map_or(0, |record| record.fields.len())
    }

    /// Checks if this record is [Self::Empty], or is [Self::Alloc] where the underlying record is
    /// empty (is such that [RecordData::is_empty] returns `true`).
    pub fn is_empty(&self) -> bool {
        match self {
            Container::Empty => true,
            Container::Alloc(record) => record.is_empty(),
        }
    }

    /// Checks if this record is [Self::Empty], or is [Self::Alloc] where the underlying record is
    /// empty or has only empty optional fields (is such that [RecordData::has_only_empty_opts]
    /// returns `true`).
    pub fn has_only_empty_opts(&self) -> bool {
        match self {
            Container::Empty => true,
            Container::Alloc(record) => record.has_only_empty_opts(),
        }
    }
}

impl Container<&ArrayData> {
    /// Iterates over the elements of the array.
    pub fn iter(&self) -> impl Iterator<Item = &NickelValue> {
        self.into_opt()
            .into_iter()
            .flat_map(|array_data| array_data.array.iter())
    }

    /// Iterates over the pending contracts.
    pub fn iter_pending_contracts(&self) -> impl Iterator<Item = &RuntimeContract> {
        self.into_opt()
            .into_iter()
            .flat_map(|array_data| array_data.pending_contracts.iter())
    }

    /// Retrieves an element from the underlying array at the given index, or returns `None` if `self`
    /// is [Self::Empty].
    pub fn get(&self, idx: usize) -> Option<&NickelValue> {
        self.into_opt()
            .and_then(|array_data| array_data.array.get(idx))
    }

    /// Returns the length of the underlying array.
    pub fn len(&self) -> usize {
        self.into_opt()
            .map_or(0, |array_data| array_data.array.len())
    }

    /// Checks if this record is [Self::Empty], or is [Self::Alloc] where the underlying array is
    /// empty (is such that [Array::is_empty] returns `true`).
    pub fn is_empty(&self) -> bool {
        match self {
            Container::Empty => true,
            Container::Alloc(array_data) => array_data.array.is_empty(),
        }
    }
}

/// An enum representation of a decoded Nickel value. This is useful to match on a general value in
/// a typed way.
///
/// Note that we only ever store references to the content of a value block here, or 1-word inline
/// values, so this enum is compact and doesn't defeat the purpose of the whole value encoding
/// (beside the fact that [ValueContentRef] is only materialized temporarily by helpers and not a
/// standard value representation). This is still true for `Container<ref>` values, thanks to the
/// niche optimizatio of rustc: [Self] is 16 bytes wide.
#[derive(Clone, Copy, Debug)]
pub enum ValueContentRef<'a> {
    Null,
    Bool(bool),
    Number(&'a NumberData),
    Array(Container<&'a ArrayData>),
    Record(Container<&'a RecordData>),
    String(&'a StringData),
    Thunk(&'a Thunk),
    Term(&'a TermData),
    Label(&'a LabelData),
    EnumVariant(&'a EnumVariantData),
    ForeignId(&'a ForeignIdData),
    SealingKey(&'a SealingKeyData),
    CustomContract(&'a CustomContractData),
    Type(&'a TypeData),
}

/// Mutable version of [ValueContentRef].
pub enum ValueContentRefMut<'a> {
    /// Given the encoding of inline values, it's a bad idea to provide a bare Rust reference to
    /// the underlying data encoding the inline value. While it's possible currently, we might use
    /// a more exotic layout in the future making it impossible to produce a valid Rust reference
    /// to an [InlineValue].
    ///
    /// A mutable reference is mostly useful for value blocks anyway. For inline values, we just
    /// return the original value back. It can be overridden directly with a new inline value.
    ///
    /// Note that the empty array case and the empty record case are handled by the [Container]
    /// part in [Self::Array] and [Self::Record].
    Null(&'a mut NickelValue),
    Bool(&'a mut NickelValue),
    Number(&'a mut NumberData),
    Array(Container<&'a mut ArrayData>),
    Record(Container<&'a mut RecordData>),
    String(&'a mut StringData),
    Thunk(&'a mut Thunk),
    Term(&'a mut TermData),
    Label(&'a mut LabelData),
    EnumVariant(&'a mut EnumVariantData),
    ForeignId(&'a mut ForeignIdData),
    SealingKey(&'a mut SealingKeyData),
    CustomContract(&'a mut CustomContractData),
    Type(&'a mut TypeData),
}

/// A lazy handle to the owned content of a value block.
///
/// It's a common pattern to need to move the content out of a block (copying the content if it's
/// shared, but avoiding copy if it's unique, like [std::rc::Rc::unwrap_or_clone]) only if a value
/// matches some pattern. Typically during program transformations, where blocks should always be
/// 1-reference counted, and where one transformation only affects specific nodes.
///
/// This is the *lazy* part: while the type of the data is known, the content is hidden behind a
/// lazy handle, which can either be unwrapped further - consuming the original value irreversibly
/// and producing an owned version of the data - or reverted back to the original value.
pub enum ValueContent {
    /// It can seem useless to carry a lens here, since there's no real data to take out. However,
    /// it's important to keep the ability to restore a [Self] value to the original [NickelValue],
    /// which is precisely the role of the lens.
    Null(lens::ValueLens<()>),
    Bool(lens::ValueLens<bool>),
    Number(lens::ValueLens<NumberData>),
    Array(lens::ValueLens<Container<ArrayData>>),
    Record(lens::ValueLens<Container<RecordData>>),
    String(lens::ValueLens<StringData>),
    Thunk(lens::ValueLens<Thunk>),
    Term(lens::TermContent),
    Label(lens::ValueLens<LabelData>),
    EnumVariant(lens::ValueLens<EnumVariantData>),
    ForeignId(lens::ValueLens<ForeignIdData>),
    SealingKey(lens::ValueLens<SealingKeyData>),
    CustomContract(lens::ValueLens<CustomContractData>),
    Type(lens::ValueLens<TypeData>),
}

impl ValueContent {
    /// Do not access the content and restore the original value unchanged.
    pub fn restore(self) -> NickelValue {
        match self {
            ValueContent::Null(lens) => lens.restore(),
            ValueContent::Bool(lens) => lens.restore(),
            ValueContent::Number(lens) => lens.restore(),
            ValueContent::Array(lens) => lens.restore(),
            ValueContent::Record(lens) => lens.restore(),
            ValueContent::String(lens) => lens.restore(),
            ValueContent::Thunk(lens) => lens.restore(),
            ValueContent::Term(lens) => lens.restore(),
            ValueContent::Label(lens) => lens.restore(),
            ValueContent::EnumVariant(lens) => lens.restore(),
            ValueContent::ForeignId(lens) => lens.restore(),
            ValueContent::SealingKey(lens) => lens.restore(),
            ValueContent::CustomContract(lens) => lens.restore(),
            ValueContent::Type(lens) => lens.restore(),
        }
    }
}

impl Traverse<NickelValue> for NickelValue {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(NickelValue) -> Result<NickelValue, E>,
    {
        let value = match order {
            TraverseOrder::TopDown => f(self)?,
            TraverseOrder::BottomUp => self,
        };

        let pos_idx = value.pos_idx();

        let result = match value.content() {
            lens @ (ValueContent::Null(_)
            | ValueContent::Bool(_)
            | ValueContent::Number(_)
            | ValueContent::String(_)
            | ValueContent::Label(_)
            | ValueContent::Thunk(_)
            | ValueContent::SealingKey(_)
            | ValueContent::ForeignId(_)) => lens.restore(),
            ValueContent::Array(lens) if lens.value().is_inline_empty_array() => lens.restore(),
            ValueContent::Record(lens) if lens.value().is_inline_empty_record() => lens.restore(),
            ValueContent::Term(lens) => {
                let term = lens.take();
                let term = term.traverse(f, order)?;
                NickelValue::term(term, pos_idx)
            }
            ValueContent::Record(lens) => {
                //unwrap(): we already treated the empty case above, so if we end up here, the
                //container must be non-empty, and `into_opt()` must return `Some`.
                let record = lens.take().into_opt().unwrap();

                // The annotation on `fields_res` uses Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let fields: Result<IndexMap<LocIdent, Field>, E> = record
                    .fields
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,RichTerm), E>
                    .map(|(id, field)| Ok((id, field.traverse(f, order)?)))
                    .collect();
                NickelValue::record(
                    RecordData::new_shared_tail(fields?, record.attrs, record.sealed_tail),
                    pos_idx,
                )
            }
            ValueContent::Array(lens) => {
                //unwrap(): we already treated the empty case above, so if we end up here, the
                //container must be non-empty, and `into_opt()` must return `Some`.
                let ArrayData {
                    array,
                    pending_contracts,
                } = lens.take().into_opt().unwrap();

                let array = array
                    .into_iter()
                    .map(|t| t.traverse(f, order))
                    .collect::<Result<Array, _>>()?;

                // We don't recurse into pending contracts currently
                NickelValue::array(array, pending_contracts, pos_idx)
            }
            ValueContent::Type(lens) => {
                let TypeData { typ, contract } = lens.take();
                let typ = typ.traverse(f, order)?;
                let contract = contract.traverse(f, order)?;

                NickelValue::typ(typ, contract, pos_idx)
            }
            ValueContent::EnumVariant(lens) => {
                let EnumVariantData { tag, arg } = lens.take();
                let arg = arg.map(|arg| arg.traverse(f, order)).transpose()?;
                NickelValue::enum_variant(tag, arg, pos_idx)
            }
            ValueContent::CustomContract(lens) => {
                let ctr = lens.take();
                let ctr = ctr.traverse(f, order)?;
                NickelValue::custom_contract(ctr, pos_idx)
            }
        };

        match order {
            TraverseOrder::TopDown => Ok(result),
            TraverseOrder::BottomUp => f(result),
        }
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&NickelValue, &S) -> TraverseControl<S, U>,
        scope: &S,
    ) -> Option<U> {
        let child_scope = match f(self, scope) {
            TraverseControl::Continue => None,
            TraverseControl::ContinueWithScope(s) => Some(s),
            TraverseControl::SkipBranch => {
                return None;
            }
            TraverseControl::Return(ret) => {
                return Some(ret);
            }
        };

        let scope = child_scope.as_ref().unwrap_or(scope);

        match self.content_ref() {
            ValueContentRef::Null
            | ValueContentRef::Bool(_)
            | ValueContentRef::Array(Container::Empty)
            | ValueContentRef::Record(Container::Empty)
            | ValueContentRef::Number(_)
            | ValueContentRef::String(_)
            | ValueContentRef::Label(_)
            | ValueContentRef::Thunk(_)
            | ValueContentRef::SealingKey(_)
            | ValueContentRef::EnumVariant(EnumVariantData { arg: None, .. })
            | ValueContentRef::ForeignId(_) => None,
            ValueContentRef::EnumVariant(EnumVariantData { arg: Some(v), .. })
            | ValueContentRef::CustomContract(v) => v.traverse_ref(f, scope),
            ValueContentRef::Array(Container::Alloc(array_data)) => array_data
                .array
                .iter()
                .find_map(|t| t.traverse_ref(f, scope)),
            ValueContentRef::Type(TypeData { typ, contract }) => {
                typ.traverse_ref(f, scope)?;
                contract.traverse_ref(f, scope)
            }
            ValueContentRef::Record(Container::Alloc(data)) => data
                .fields
                .values()
                .find_map(|field| field.traverse_ref(f, scope)),
            ValueContentRef::Term(term) => term.traverse_ref(f, scope),
        }
    }
}

impl Traverse<Type> for NickelValue {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<NickelValue, E>
    where
        F: FnMut(Type) -> Result<Type, E>,
    {
        self.traverse(
            &mut |value: NickelValue| {
                let pos_idx = value.pos_idx();

                match value.content() {
                    ValueContent::Type(lens) => {
                        let TypeData { typ, contract } = lens.take();
                        let typ = typ.traverse(f, order)?;
                        Ok(NickelValue::typ(typ, contract, pos_idx))
                    }
                    lens => Ok(lens.restore()),
                }
            },
            order,
        )
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&Type, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        self.traverse_ref(
            &mut |value: &NickelValue, state: &S| {
                if let Some(TypeData { typ, contract: _ }) = value.as_type() {
                    typ.traverse_ref(f, state).into()
                } else {
                    TraverseControl::Continue
                }
            },
            state,
        )
    }
}

impl From<Term> for NickelValue {
    fn from(term: Term) -> Self {
        NickelValue::term_posless(term)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::position::RawSpan;

    #[test]
    fn inline_values() {
        use crate::files::Files;

        let inline_null = NickelValue::null();
        let inline_true = NickelValue::bool_true();
        let inline_false = NickelValue::bool_false();
        let inline_empty_array = NickelValue::empty_array();
        let inline_empty_record = NickelValue::empty_record();

        assert_eq!(inline_null.tag(), ValueTag::Inline);
        assert_eq!(inline_true.tag(), ValueTag::Inline);
        assert_eq!(inline_false.tag(), ValueTag::Inline);
        assert_eq!(inline_empty_array.tag(), ValueTag::Inline);
        assert_eq!(inline_empty_record.tag(), ValueTag::Inline);

        assert_eq!(inline_null.as_inline(), Some(InlineValue::Null));
        assert_eq!(inline_true.as_inline(), Some(InlineValue::True));
        assert_eq!(inline_false.as_inline(), Some(InlineValue::False));
        assert_eq!(
            inline_empty_array.as_inline(),
            Some(InlineValue::EmptyArray)
        );
        assert_eq!(
            inline_empty_record.as_inline(),
            Some(InlineValue::EmptyRecord)
        );

        let dummy_pos = TermPos::Original(RawSpan {
            src_id: Files::empty().add("<test>", String::from("empty")),
            start: 0.into(),
            end: 1.into(),
        });

        let mut pos_table = PosTable::new();

        // Makes sure the values are what we expect, and that `phys_eq` properly ignores position
        // information (we create a fresh position index for each value).
        assert!(
            inline_null
                .clone()
                .with_inline_pos_idx(pos_table.push(dummy_pos))
                .phys_eq(&NickelValue::null().with_inline_pos_idx(pos_table.push(dummy_pos)))
        );
        assert!(
            inline_true
                .with_inline_pos_idx(pos_table.push(dummy_pos))
                .phys_eq(&NickelValue::bool_true().with_inline_pos_idx(pos_table.push(dummy_pos)))
        );
        assert!(
            inline_false
                .with_inline_pos_idx(pos_table.push(dummy_pos))
                .phys_eq(&NickelValue::bool_false().with_inline_pos_idx(pos_table.push(dummy_pos)))
        );
        assert!(
            inline_empty_array
                .with_inline_pos_idx(pos_table.push(dummy_pos))
                .phys_eq(
                    &NickelValue::empty_array().with_inline_pos_idx(pos_table.push(dummy_pos))
                )
        );
        assert!(
            inline_empty_record
                .with_inline_pos_idx(pos_table.push(dummy_pos))
                .phys_eq(
                    &NickelValue::empty_record().with_inline_pos_idx(pos_table.push(dummy_pos))
                )
        );

        assert!(!inline_null.phys_eq(&NickelValue::bool_true()));
    }

    #[test]
    fn basic_value_blocks() {
        let number_value = NickelValue::number_posless(42);
        let string_value = NickelValue::string_posless("Hello, World!");

        assert_eq!(number_value.tag(), ValueTag::Pointer);
        assert_eq!(string_value.tag(), ValueTag::Pointer);

        assert_eq!(number_value.as_number().unwrap(), &Number::from(42));
        assert_eq!(
            string_value.as_string().unwrap(),
            &NickelString::from("Hello, World!")
        );

        assert!(number_value.as_string().is_none());
        assert!(string_value.as_number().is_none());
    }

    #[test]
    fn ref_counting() {
        let number_value = NickelValue::number_posless(42);

        let mut copies =
            std::array::from_fn::<_, 20, _>(|_| ManuallyDrop::new(number_value.clone()));

        let mut as_val = number_value.into_block().unwrap();
        assert_eq!(as_val.header().ref_count(), 21);

        let mut block_copy = ManuallyDrop::new(as_val.clone());

        assert_eq!(as_val.header().ref_count(), 22);

        assert!(as_val.try_get_mut::<NumberData>().unwrap().is_none());

        for copy in copies.iter_mut().take(10) {
            unsafe {
                ManuallyDrop::drop(copy);
            }
        }

        assert_eq!(as_val.header().ref_count(), 12);

        for copy in copies.iter_mut().skip(10) {
            unsafe {
                ManuallyDrop::drop(copy);
            }
        }

        let mut cow = as_val.clone();
        *cow.make_mut() = Number::from(0);

        // The original value wasn't 1-RCed, so it should be left unchanged and cow must be a
        // separate copy.
        assert_eq!(as_val.header().ref_count(), 2);
        assert_eq!(as_val.decode::<NumberData>(), &Number::from(42));
        assert_eq!(cow.header().ref_count(), 1);
        assert_eq!(cow.decode::<NumberData>(), &Number::from(0));

        unsafe { ManuallyDrop::drop(&mut block_copy) }

        *as_val.get_mut() = Number::from(100);
        assert_eq!(as_val.header().ref_count(), 1);
        assert_eq!(as_val.decode::<NumberData>(), &Number::from(100));
    }

    #[test]
    fn in_place_modification() {
        // We make the containers non-empty so that they're not inlined.
        let mut record_data = RecordData::default();
        record_data
            .fields
            .insert(LocIdent::from("hello"), Default::default());
        let mut array_data = Array::default();
        array_data.push(NickelValue::null());

        let record = NickelValue::record_posless(record_data);
        let array = NickelValue::array_posless(array_data, Vec::new());

        let mut record_value = record.into_block().unwrap();
        let mut array_value = array.into_block().unwrap();

        assert_eq!(record_value.decode::<RecordData>().fields.len(), 1);
        assert_eq!(array_value.decode::<ArrayData>().array.len(), 1);

        record_value
            .get_mut::<RecordData>()
            .fields
            .insert(LocIdent::from("world"), Default::default());
        array_value
            .get_mut::<ArrayData>()
            .array
            .push(NickelValue::null());

        let array_copy = array_value.clone();
        let record_copy = record_value.clone();

        assert_eq!(record_copy.decode::<RecordData>().fields.len(), 2);
        assert_eq!(array_copy.decode::<ArrayData>().array.len(), 2);
    }

    #[test]
    fn in_place_modification_with_content_ref() {
        // We make the containers non-empty so that they're not inlined.
        let mut record_data = RecordData::default();
        record_data
            .fields
            .insert(LocIdent::from("hello"), Default::default());
        let mut array_data = Array::default();
        array_data.push(NickelValue::null());

        let mut record = NickelValue::record_posless(record_data);
        let mut array = NickelValue::array_posless(array_data, Vec::new());

        assert_eq!(record.as_record().unwrap().unwrap_alloc().fields.len(), 1);
        assert_eq!(array.as_array().unwrap().unwrap_alloc().array.len(), 1);

        if let Some(ValueContentRefMut::Record(Container::Alloc(record))) = record.content_mut() {
            record
                .fields
                .insert(LocIdent::from("world"), Default::default());
        } else {
            panic!("Expected RecordData");
        }

        if let Some(ValueContentRefMut::Array(Container::Alloc(array_data))) = array.content_mut() {
            array_data.array.push(NickelValue::null());
        } else {
            panic!("Expected ArrayData");
        }

        let array_copy = array.clone();
        let record_copy = record.clone();

        assert_eq!(record_copy.as_record().unwrap().len(), 2);
        assert_eq!(array_copy.as_array().unwrap().len(), 2);
    }

    #[test]
    fn content_make_mut() {
        // We make the containers non-empty so that they're not inlined.
        let mut record_data = RecordData::default();
        record_data
            .fields
            .insert(LocIdent::from("hello"), Default::default());
        let mut array_data = Array::default();
        array_data.push(NickelValue::null());

        let mut record = NickelValue::record_posless(record_data);
        let mut array = NickelValue::array_posless(array_data, Vec::new());
        let array_copy = array.clone();

        assert_eq!(record.as_record().unwrap().unwrap_alloc().fields.len(), 1);
        assert_eq!(array.as_array().unwrap().unwrap_alloc().array.len(), 1);

        if let ValueContentRefMut::Record(Container::Alloc(record)) = record.content_make_mut() {
            record
                .fields
                .insert(LocIdent::from("world"), Default::default());
        } else {
            panic!("Expected RecordData");
        }

        if let ValueContentRefMut::Array(Container::Alloc(array_data)) = array.content_make_mut() {
            array_data.array.push(NickelValue::null());
        } else {
            panic!("Expected ArrayData");
        }

        let record_copy = record.clone();

        assert_eq!(record.as_record().unwrap().len(), 2);
        assert_eq!(record_copy.as_record().unwrap().len(), 2);
        assert_eq!(array.as_array().unwrap().len(), 2);
        // The copy was made before the call to `content_make_mut`, so it must have been preserved.
        assert_eq!(array_copy.as_array().unwrap().len(), 1);
    }

    #[test]
    fn empty_containers_are_inlined() {
        let empty_record = NickelValue::record_posless(RecordData::default());
        let empty_array = NickelValue::array_posless(Array::default(), Vec::new());

        assert_eq!(empty_record.tag(), ValueTag::Inline);
        assert_eq!(empty_array.tag(), ValueTag::Inline);

        assert_eq!(empty_record.as_inline(), Some(InlineValue::EmptyRecord));
        assert_eq!(empty_array.as_inline(), Some(InlineValue::EmptyArray));
    }
}
