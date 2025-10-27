//! Runtime representation of Nickel values.
//!
//! This module implements a custom memory layout for a memory-efficient representation of Nickel
//! values. See
//! [RFC007](https://github.com/tweag/nickel/blob/master/rfcs/007-bytecode-interpreter.md) for more
//! details.
// Temporary, since this module isn't used yet
#![allow(dead_code, unused_variables, unused_imports)]

use crate::{
    eval::cache::CacheIndex,
    identifier::LocIdent,
    impl_display_from_pretty,
    label::Label,
    position::{PosIdx, PosTable, RawSpan, TermPos},
    term::{
        ForeignIdPayload, Number, RecordOpKind, RuntimeContract, SealingKey, Term,
        record::{Field, RecordData},
        string::NickelString,
    },
    traverse::{Traverse as _, TraverseOrder},
    typ::Type,
};
use malachite::base::num::conversion::traits::ToSci as _;
use nickel_lang_vector::Slice;
use std::{
    alloc::{Layout, alloc, dealloc},
    cmp::max,
    convert::Infallible,
    fmt,
    marker::PhantomData,
    mem::{ManuallyDrop, size_of, transmute},
    ptr::{self, NonNull},
};

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

            (ValueContentRef::Number(number_body1), ValueContentRef::Number(number_body2)) => {
                number_body1 == number_body2
            }
            (ValueContentRef::Array(array_body1), ValueContentRef::Array(array_body2)) => {
                array_body1 == array_body2
            }
            (ValueContentRef::Record(record_body1), ValueContentRef::Record(record_body2)) => {
                record_body1 == record_body2
            }
            (ValueContentRef::String(string_body1), ValueContentRef::String(string_body2)) => {
                string_body1 == string_body2
            }
            (ValueContentRef::Thunk(thunk_body1), ValueContentRef::Thunk(thunk_body2)) => {
                thunk_body1 == thunk_body2
            }
            (ValueContentRef::Term(term_body1), ValueContentRef::Term(term_body2)) => {
                term_body1 == term_body2
            }
            (ValueContentRef::Label(label_body1), ValueContentRef::Label(label_body2)) => {
                label_body1 == label_body2
            }
            (
                ValueContentRef::EnumVariant(enum_variant_body1),
                ValueContentRef::EnumVariant(enum_variant_body2),
            ) => enum_variant_body1 == enum_variant_body2,
            (
                ValueContentRef::ForeignId(foreign_id_body1),
                ValueContentRef::ForeignId(foreign_id_body2),
            ) => foreign_id_body1 == foreign_id_body2,
            (
                ValueContentRef::SealingKey(sealing_key_body1),
                ValueContentRef::SealingKey(sealing_key_body2),
            ) => sealing_key_body1 == sealing_key_body2,
            (
                ValueContentRef::CustomContract(custom_contract_body1),
                ValueContentRef::CustomContract(custom_contract_body2),
            ) => custom_contract_body1 == custom_contract_body2,
            (ValueContentRef::Type(type_body1), ValueContentRef::Type(type_body2)) => {
                type_body1 == type_body2
            }
            _ => false,
        }
    }
}

impl fmt::Debug for NickelValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NickelValue(pos_idx: {:?}, ", self.pos_idx())?;

        match self.content_ref() {
            ValueContentRef::Null => write!(f, "content: null"),
            ValueContentRef::Bool(b) => write!(f, "content: {b}"),
            ValueContentRef::Number(number_body) => write!(f, "content: {}", number_body.0),
            ValueContentRef::Array(container) => write!(f, "array: {container:?}"),
            ValueContentRef::Record(container) => write!(f, "record: {container:?}"),
            ValueContentRef::String(string_body) => write!(f, "{}", string_body.0),
            ValueContentRef::Thunk(thunk_body) => write!(f, "thunk: {:?}", thunk_body.0),
            ValueContentRef::Term(term_body) => write!(f, "{:?}", term_body.0),
            ValueContentRef::Label(label_body) => write!(f, "{:?}", label_body.0),
            ValueContentRef::EnumVariant(enum_variant_body) => {
                write!(f, "{:?}", enum_variant_body)
            }
            ValueContentRef::ForeignId(foreign_id_body) => write!(f, "foreign id: {:?}", foreign_id_body.0),
            ValueContentRef::SealingKey(sealing_key_body) => {
                write!(f, "sealing key: {:?}", sealing_key_body.0)
            }
            ValueContentRef::CustomContract(custom_contract_body) => {
                write!(f, "custom contract: {:?}", custom_contract_body.0)
            }
            ValueContentRef::Type(type_body) => write!(f, "{type_body:?}"),
        }?;

        write!(f, ")")
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
    fn inline_pos_idx(&self) -> Option<PosIdx> {
        (self.tag() == ValueTag::Inline)
            .then(|| PosIdx::from_usize_truncate((self.data & Self::INLINE_POS_IDX_MASK) >> 32))
    }

    /// Creates a new inline value with an associated position index.
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
    unsafe fn raw_copy(other: &Self) -> Self {
        NickelValue { data: other.data }
    }

    /// Create a Nickel value from a raw pointer to a value block.
    ///
    /// # Safety
    ///
    /// - `ptr` must be a valid pointer to a [ValueBlockRc].
    /// - the ref count of the value block must be adjusted manually to compensate for dropping `self`. Possible solutions are:
    ///   - manually increment the reference count,
    ///   - prevent `self` from being dropped (e.g. using [std::mem::ManuallyDrop])
    ///   - use a `ptr` coming from a call to [ValueBlockRc::into_raw]
    ///   - derive `ptr` from a value or a value block that won't be dropped
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
    unsafe fn block(ptr: NonNull<u8>) -> Self {
        NickelValue {
            data: ptr.as_ptr() as usize,
            pos_idx: InlinePosIdx::NONE,
        }
    }
}

impl NickelValue {
    /// The mask for the tag bits in a value pointer.
    const VALUE_TAG_MASK: usize = 0b11;

    /// Returns the tag bits of this value.
    pub fn tag(&self) -> ValueTag {
        (self.data & Self::VALUE_TAG_MASK).try_into().unwrap()
    }

    /// Creates a new inline value without an associated position index. Same as
    /// `Self::inline(inline, InlinePosIdx::NONE)`.
    pub const fn inline_posless(inline: InlineValue) -> Self {
        Self::inline(inline, PosIdx::NONE)
    }

    /// Creates a new null value with the index set to [InlinePosIdx::NONE].
    pub const fn null() -> Self {
        Self::inline_posless(InlineValue::Null)
    }

    /// Creates a new true value with the index set to [InlinePosIdx::NONE].
    pub const fn bool_true() -> Self {
        Self::inline_posless(InlineValue::True)
    }

    /// Creates a new false value with the index set to [InlinePosIdx::NONE].
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

    /// Creates a new boolean value with the index set to [InlinePosIdx::NONE].
    pub fn bool_value_posless(value: bool) -> Self {
        Self::bool_value(value, PosIdx::NONE)
    }

    /// Creates a new empty array with the index set to [InlinePosIdx::NONE].
    pub const fn empty_array() -> Self {
        Self::inline_posless(InlineValue::EmptyArray)
    }

    /// Creates a new empty record with the index set to [InlinePosIdx::NONE].
    pub const fn empty_record() -> Self {
        Self::inline_posless(InlineValue::EmptyRecord)
    }

    /// Allocates a new number value.
    pub fn number(value: impl Into<Number>, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(NumberBody(value.into()), pos_idx).into()
    }

    /// Allocates a new number value without any position set. Equivalent to `Self::number(value,
    /// PosIdx::NONE`
    pub fn number_posless(value: impl Into<Number>) -> Self {
        Self::number(value, PosIdx::NONE)
    }

    /// Allocates a new string value.
    pub fn string(value: impl Into<NickelString>, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(StringBody(value.into()), pos_idx).into()
    }

    /// Allocates a new string value without any position set. Equivalent to `Self::string(value,
    /// PosIdx::NONE`
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
                    ArrayBody {
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
    pub fn array_posless(value: Array, pending_contracts: Vec<RuntimeContract>) -> Self {
        if value.is_empty() {
            Self::inline(InlineValue::EmptyArray, PosIdx::NONE)
        } else {
            ValueBlockRc::encode(
                ArrayBody {
                    array: value,
                    pending_contracts,
                },
                PosIdx::NONE,
            )
            .into()
        }
    }

    /// Creates a new empty array, but doesn't inline it. This forces the allocation of a value
    /// block. [Self::empty_array_block] can be useful when one needs to have a pre-allocated array
    /// that will be mutated in a second step.
    pub fn empty_array_block(pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(
            ArrayBody {
                array: Array::default(),
                pending_contracts: Vec::new(),
            },
            pos_idx,
        )
        .into()
    }

    /// Allocates a new record value. If the record is empty, it is automatically inlined as
    /// [InlineValue::EmptyRecord].
    pub fn record(value: RecordData, pos_idx: PosIdx) -> Self {
        // We need to exclude empty open records here, because we would otherwise lose this bit of
        // information: an inline empty record is assumed to be closed.
        if value.is_empty() && !value.attrs.open {
            Self::inline(InlineValue::EmptyRecord, pos_idx)
        } else {
            ValueBlockRc::encode(RecordBody(value), pos_idx).into()
        }
    }

    /// Allocates a new record value without any position set. If the record is empty, it is
    /// automatically inlined as [InlineValue::EmptyRecord].
    pub fn record_posless(value: RecordData) -> Self {
        if value.is_empty() {
            Self::empty_record()
        } else {
            ValueBlockRc::encode(RecordBody(value), PosIdx::NONE).into()
        }
    }

    /// Creates a new empty record, but doesn't inline it. This forces the allocation of a value
    /// block. [Self::empty_record] can be useful when one needs to have a pre-allocated record
    /// that will be mutated in a second step.
    pub fn empty_record_block(pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(RecordBody(RecordData::empty()), pos_idx).into()
    }

    /// Allocates a new thunk value.
    pub fn thunk(value: CacheIndex, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(ThunkBody(value), pos_idx).into()
    }

    /// Allocates a new thunk value without any position set. Equivalent to `Self::thunk(value,
    /// PosIdx::NONE)`.
    pub fn thunk_posless(value: CacheIndex) -> Self {
        Self::thunk(value, PosIdx::NONE)
    }

    /// Allocates a new term value.
    pub fn term(value: Term, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(TermBody(value), pos_idx).into()
    }

    /// Allocates a new term value without any position set. Equivalent to `Self::term(value,
    /// PosIdx::NONE)`.
    pub fn term_posless(value: Term) -> Self {
        Self::term(value, PosIdx::NONE)
    }

    /// Allocates a new label value.
    pub fn label(value: Label, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(LabelBody(value), pos_idx).into()
    }

    /// Allocates a new label value without any position set. Equivalent to `Self::label(value,
    /// PosIdx::NONE)`.
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
            EnumVariantBody {
                tag: tag.into(),
                arg,
            },
            pos_idx,
        )
        .into()
    }

    /// Allocates a new enum variant value without any position set. Equivalent to
    /// `Self::enum_variant(tag, arg, PosIdx::NONE)`.
    pub fn enum_variant_posless(tag: impl Into<LocIdent>, arg: Option<NickelValue>) -> Self {
        Self::enum_variant(tag.into(), arg, PosIdx::NONE)
    }

    /// Allocates a new enum tag value. Same as `Self::enum_variant(tag, None, pos_idx)`.
    pub fn enum_tag(tag: impl Into<LocIdent>, pos_idx: PosIdx) -> Self {
        Self::enum_variant(tag.into(), None, pos_idx)
    }

    /// Allocates a new enum tag value without any position set. Equivalent to `Self::enum_tag(tag,
    /// arg, PosIdx::NONE)`.
    pub fn enum_tag_posless(tag: impl Into<LocIdent>) -> Self {
        Self::enum_tag(tag.into(), PosIdx::NONE)
    }

    /// Allocates a new foreign ID value.
    pub fn foreign_id(value: ForeignIdPayload, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(ForeignIdBody(value), pos_idx).into()
    }

    /// Allocates a new foreign ID value without any position set. Equivalent to
    /// `Self::foreign_id(value, PosIdx::NONE)`.
    pub fn foreign_id_posless(value: ForeignIdPayload) -> Self {
        Self::foreign_id(value, PosIdx::NONE)
    }

    /// Allocates a new sealing key value.
    pub fn sealing_key(value: SealingKey, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(SealingKeyBody(value), pos_idx).into()
    }

    /// Allocates a new sealing key value without any position set. Equivalent to
    /// `Self::sealing_key(value, PosIdx::NONE)`.
    pub fn sealing_key_posless(value: SealingKey) -> Self {
        Self::sealing_key(value, PosIdx::NONE)
    }

    /// Allocates a new custom contract value.
    pub fn custom_contract(value: NickelValue, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(CustomContractBody(value), pos_idx).into()
    }

    /// Allocates a new custom contract value without any position set. Equivalent to
    /// `Self::custom_contract(value, PosIdx::NONE)`.
    pub fn custom_contract_posless(value: NickelValue) -> Self {
        Self::custom_contract(value, PosIdx::NONE)
    }

    /// Allocates a new type.
    pub fn typ(typ: Type, contract: NickelValue, pos_idx: PosIdx) -> Self {
        ValueBlockRc::encode(TypeBody { typ, contract }, pos_idx).into()
    }

    /// Returns the inner value of `self` if it's an inline value, or `None` otherwise.
    pub fn as_inline(&self) -> Option<InlineValue> {
        InlineValue::try_from(self).ok()
    }

    /// Returns the inner value of `self` if it's a (inlined) boolean value, or `None` otherwise.
    pub fn as_bool(&self) -> Option<bool> {
        match self.as_inline()? {
            InlineValue::True => Some(true),
            InlineValue::False => Some(false),
            _ => None,
        }
    }

    /// Returns a reference to the inner number stored in this value if `self` is a value block
    /// with tag number, or `None` otherwise.
    pub fn as_number(&self) -> Option<&NumberBody> {
        self.as_value_body()
    }

    /// Returns a reference to the inner string stored in this value if `self` is a value block
    /// with tag string, or `None` otherwise.
    pub fn as_string(&self) -> Option<&StringBody> {
        self.as_value_body()
    }

    /// Returns a reference to the inner array stored in this value if `self` is a value block
    /// with tag array, or `None` otherwise.
    pub fn as_array(&self) -> Option<Container<&ArrayBody>> {
        if self.is_empty_array() {
            Some(Container::Empty)
        } else {
            self.as_value_body().map(Container::Alloc)
        }
    }

    /// Returns a reference to the inner record stored in this value if `self` is a value block
    /// with tag record, or `None` otherwise.
    pub fn as_record(&self) -> Option<Container<&RecordBody>> {
        if self.is_empty_record() {
            Some(Container::Empty)
        } else {
            self.as_value_body().map(Container::Alloc)
        }
    }

    /// Returns a reference to the inner thunk stored in this value if `self` is a value block with
    /// tag thunk, or `None` otherwise.
    pub fn as_thunk(&self) -> Option<&ThunkBody> {
        self.as_value_body()
    }

    /// Returns a reference to the inner term stored in this value if `self` is a value block with
    /// a tag term, or `None` otherwise.
    pub fn as_term(&self) -> Option<&TermBody> {
        self.as_value_body()
    }

    /// Returns a reference to the inner label stored in this value if `self` is a value block with
    /// tag label, or `None` otherwise.
    pub fn as_label(&self) -> Option<&LabelBody> {
        self.as_value_body()
    }

    /// Returns a reference to the inner enum variant stored in this value if `self` is a value
    /// block with tag enum variant, or `None` otherwise.
    pub fn as_enum_variant(&self) -> Option<&EnumVariantBody> {
        self.as_value_body()
    }

    /// Returns a reference to the inner enum tag stored in this value if `self` is a value is a
    /// block with an enum variant inside, which has no arguments, or `None` otherwise.
    pub fn as_enum_tag(&self) -> Option<LocIdent> {
        let enum_var = self.as_enum_variant()?;
        enum_var.arg.is_none().then(|| enum_var.tag)
    }

    /// Returns a reference to the inner sealing key stored in this value if `self` is a value
    /// block with sealing key inside, or `None` otherwise.
    pub fn as_sealing_key(&self) -> Option<&SealingKeyBody> {
        self.as_value_body()
    }

    /// Returns a reference to the inner type stored in this value if `self` is a value with a type
    /// inside, or `None` otherwise.
    pub fn as_type(&self) -> Option<&TypeBody> {
        self.as_value_body()
    }

    /// Returns a reference to the inner foreign id stored in this value if `self` is a value
    /// block with a foreign value inside, or `None` otherwise.
    pub fn as_foreign_id(&self) -> Option<&ForeignIdBody> {
        self.as_value_body()
    }

    /// Returns the value block pointed to by this Nickel value if it's a value block, or `Err`
    /// otherwise. This is equivalent to `<Self as TryInto<ValueBlockRc>>::try_into`, but is more
    /// succinct and type-inference-friendly. Since this method must consume `self` in the case
    /// it's a value block, in order to keep the reference count correct, the caller can get `self`
    /// back in case of error.
    pub fn into_block(self) -> Result<ValueBlockRc, Self> {
        ValueBlockRc::try_from(self)
    }

    /// Returns a reference to the content of this value if it's a value block of type `T`, or
    /// `None` otherwise.
    fn as_value_body<T: ValueBlockBody>(&self) -> Option<&T> {
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
                    BodyTag::Number => {
                        ValueContentRef::Number(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    BodyTag::Array => ValueContentRef::Array(Container::Alloc(
                        ValueBlockRc::decode_from_raw_unchecked(as_ptr),
                    )),
                    BodyTag::Record => ValueContentRef::Record(Container::Alloc(
                        ValueBlockRc::decode_from_raw_unchecked(as_ptr),
                    )),
                    BodyTag::String => {
                        ValueContentRef::String(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    BodyTag::Thunk => {
                        ValueContentRef::Thunk(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    BodyTag::Term => {
                        ValueContentRef::Term(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    BodyTag::Label => {
                        ValueContentRef::Label(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    BodyTag::EnumVariant => ValueContentRef::EnumVariant(
                        ValueBlockRc::decode_from_raw_unchecked(as_ptr),
                    ),
                    BodyTag::ForeignId => {
                        ValueContentRef::ForeignId(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    BodyTag::SealingKey => {
                        ValueContentRef::SealingKey(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    BodyTag::CustomContract => ValueContentRef::CustomContract(
                        ValueBlockRc::decode_from_raw_unchecked(as_ptr),
                    ),
                    BodyTag::Type => {
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
    pub fn content_mut(&mut self) -> Option<ValueContentRefMut<'_>> {
        match self.tag() {
            // Safety: if `self.tag()` is `Pointer`, then the content of self must be a valid
            // non-null pointer to a value block.
            ValueTag::Pointer => unsafe {
                let as_ptr = NonNull::new_unchecked(self.data as *mut u8);
                let header = ValueBlockRc::header_from_raw(as_ptr);

                if header.ref_count() != 1 {
                    return None;
                }

                // Safety:
                //  - additionally, the lifetime of the return `ValueContentRef<'_>` is tied to
                //    `&mut self`, so the former won't outlive the value block.
                //  - we've checked above that `ref_count` is `1`, and we hold a mutable borrow of
                //  `  self`, so there can't be other active mutable borrows to the block
                Some(match header.tag {
                    BodyTag::Number => ValueContentRefMut::Number(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    BodyTag::Array => ValueContentRefMut::Array(Container::Alloc(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    )),
                    BodyTag::Record => ValueContentRefMut::Record(Container::Alloc(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    )),
                    BodyTag::String => ValueContentRefMut::String(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    BodyTag::Thunk => ValueContentRefMut::Thunk(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    BodyTag::Term => ValueContentRefMut::Term(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    BodyTag::Label => ValueContentRefMut::Label(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    BodyTag::EnumVariant => ValueContentRefMut::EnumVariant(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    BodyTag::ForeignId => ValueContentRefMut::ForeignId(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    BodyTag::SealingKey => ValueContentRefMut::SealingKey(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    BodyTag::CustomContract => ValueContentRefMut::CustomContract(
                        ValueBlockRc::decode_mut_from_raw_unchecked(as_ptr),
                    ),
                    BodyTag::Type => ValueContentRefMut::Type(
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
                // the lens built with `body_lens`.
                unsafe {
                    match header.tag {
                        BodyTag::Number => ValueContent::Number(ValueLens::body_lens(self)),
                        BodyTag::Array => ValueContent::Array(ValueLens::container_lens(self)),
                        BodyTag::Record => ValueContent::Record(ValueLens::container_lens(self)),
                        BodyTag::String => ValueContent::String(ValueLens::body_lens(self)),
                        BodyTag::Thunk => ValueContent::Thunk(ValueLens::body_lens(self)),
                        BodyTag::Term => {
                            let term: &TermBody = ValueBlockRc::decode_from_raw_unchecked(as_ptr);

                            ValueContent::Term(match term.0 {
                                Term::Value(_) => {
                                    TermContent::Value(ValueLens::term_value_lens(self))
                                }
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
                        BodyTag::Label => ValueContent::Label(ValueLens::body_lens(self)),
                        BodyTag::EnumVariant => {
                            ValueContent::EnumVariant(ValueLens::body_lens(self))
                        }
                        BodyTag::ForeignId => ValueContent::ForeignId(ValueLens::body_lens(self)),
                        BodyTag::SealingKey => ValueContent::SealingKey(ValueLens::body_lens(self)),
                        BodyTag::CustomContract => {
                            ValueContent::CustomContract(ValueLens::body_lens(self))
                        }
                        BodyTag::Type => ValueContent::Type(ValueLens::body_lens(self)),
                    }
                }
            }
            ValueTag::Inline => {
                // Safety:
                //
                // as_inline_unchecked: `self.tag()` is `ValueTag::Inline`
                //
                // ValueLens::xxx_lens: in each branch, the type of the inline value matches the
                // type of the lens built with `body_lens`.
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
    /// of this copy is returned.
    pub fn content_make_mut(&mut self) -> ValueContentRefMut<'_> {
        // Safety: `value.tag()` must be `Pointer` and `value.body_tag()` must be equal to `T::Tag`
        unsafe fn make_mut<T: ValueBlockBody + Clone>(value: &mut NickelValue) -> &mut T {
            unsafe {
                // Safety: if `value.tag()` is `Pointer`, `value.data` is a non-null pointer (precondition)

                let mut as_ptr = NonNull::new_unchecked(value.data as *mut u8);
                // Safety: if `value.tag()` is `Pointer`, `value.data` is a non-null pointer (precondition)
                let header = ValueBlockRc::header_from_raw(as_ptr);

                if header.ref_count() != 1 {
                    let unique = ValueBlockRc::encode(
                        // Safety: `value.body_tag()` is `T::Tag` (precondition)
                        ValueBlockRc::decode_from_raw_unchecked::<T>(as_ptr).clone(),
                        header.pos_idx,
                    );
                    as_ptr = unique.0;
                    *value = unique.into();
                }

                // Safety: we've made sure `value` is unique
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
                    BodyTag::Number => ValueContentRefMut::Number(make_mut(self)),
                    BodyTag::Array => ValueContentRefMut::Array(Container::Alloc(make_mut(self))),
                    BodyTag::Record => ValueContentRefMut::Record(Container::Alloc(make_mut(self))),
                    BodyTag::String => ValueContentRefMut::String(make_mut(self)),
                    BodyTag::Thunk => ValueContentRefMut::Thunk(make_mut(self)),
                    BodyTag::Term => ValueContentRefMut::Term(make_mut(self)),
                    BodyTag::Label => ValueContentRefMut::Label(make_mut(self)),
                    BodyTag::EnumVariant => ValueContentRefMut::EnumVariant(make_mut(self)),
                    BodyTag::ForeignId => ValueContentRefMut::ForeignId(make_mut(self)),
                    BodyTag::SealingKey => ValueContentRefMut::SealingKey(make_mut(self)),
                    BodyTag::CustomContract => ValueContentRefMut::CustomContract(make_mut(self)),
                    BodyTag::Type => ValueContentRefMut::Type(make_mut(self)),
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

    /// Returns the body tag of the underlying value block, or `None` if `self` is an inline value.
    pub fn body_tag(&self) -> Option<BodyTag> {
        (self.tag() == ValueTag::Pointer).then(|| {
            // Safety: if `self.tag()` is `Pointer`, then `self.data` must be valid pointer to a
            // value  block.
            unsafe {
                let as_ptr = NonNull::new_unchecked(self.data as *mut u8);
                ValueBlockRc::tag_from_raw(as_ptr)
            }
        })
    }

    /// Checks if this value is the inlined empty array. Caution: note that `self` could also be an
    /// empty array, but allocated as a block. In that case, [Self::is_empty_array] would still
    /// return `false`.
    pub fn is_empty_array(&self) -> bool {
        self.as_inline()
            .is_some_and(|inl| matches!(inl, InlineValue::EmptyArray))
    }

    /// Checks if this value is the inlined empty record. Caution: note that `self` could also be
    /// an empty record, but allocated as a block. In that case, [Self::is_empty_record] would
    /// still return `false`.
    pub fn is_empty_record(&self) -> bool {
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
    pub fn is_whnf(&self) -> bool {
        !matches!(self.body_tag(), Some(BodyTag::Thunk | BodyTag::Term))
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
            ValueContentRef::Term(term_body) => match &term_body.0 {
                Term::Value(v) | Term::Closurize(v) => v.type_of(),
                Term::RecRecord(..) => Some("Record"),
                Term::Fun(..) | Term::FunPattern(..) => Some("Function"),
                Term::Match { .. } => Some("MatchExpression"),
                Term::Sealed(..) => Some("Sealed"),
                Term::Annotated(..) => Some("Annotated"),
                Term::Let(..)
                | Term::LetPattern(..)
                | Term::App(_, _)
                | Term::Var(_)
                | Term::Op1(_, _)
                | Term::Op2(_, _, _)
                | Term::OpN(..)
                | Term::Import(_)
                | Term::ResolvedImport(_)
                | Term::StrChunks(_)
                | Term::ParseError(_)
                | Term::RuntimeError(_) => None,
            },
            ValueContentRef::Label(_) => Some("Label"),
            ValueContentRef::EnumVariant(EnumVariantBody { arg: None, tag: _ }) => Some("EnumTag"),
            ValueContentRef::EnumVariant(EnumVariantBody {
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
            ValueContentRef::EnumVariant(enum_variant_body) => enum_variant_body.arg.is_none(),
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
        let Some(body_tag) = self.body_tag() else {
            // All inline values are atoms
            // Caution: if we inline some integers in the future as well, negative integers are NOT
            // atoms, so this path should be updated.
            return true;
        };

        match body_tag {
            BodyTag::Array
            | BodyTag::Record
            | BodyTag::ForeignId
            | BodyTag::SealingKey
            | BodyTag::Label
            | BodyTag::String => true,
            BodyTag::Number => self.as_value_body::<NumberBody>().unwrap().0 >= 0,
            BodyTag::EnumVariant => self
                .as_value_body::<EnumVariantBody>()
                .unwrap()
                .arg
                .is_none(),
            BodyTag::Thunk => false,
            BodyTag::Term => self.as_value_body::<TermBody>().unwrap().0.fmt_is_atom(),
            BodyTag::CustomContract => self
                .as_value_body::<CustomContractBody>()
                .unwrap()
                .0
                .fmt_is_atom(),
            BodyTag::Type => self.as_value_body::<TypeBody>().unwrap().typ.fmt_is_atom(),
        }
    }

    /// Converts a primitive value (number, string, boolean, enum tag or null) to a Nickel string,
    /// or returns `None` if the value isn't primitive.
    pub fn to_nickel_string(&self) -> Option<NickelString> {
        match self.content_ref() {
            ValueContentRef::Null => Some("null".into()),
            ValueContentRef::Bool(b) => Some(b.to_string().into()),
            ValueContentRef::String(s) => Some(s.0.clone()),
            ValueContentRef::EnumVariant(EnumVariantBody { tag, arg: None }) => Some((*tag).into()),
            ValueContentRef::Number(n) => Some(format!("{}", n.0.to_sci()).into()),
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

    /// Updates the position of this value. First allocates a position index of the proper type
    /// (inline or not) in the position table, and then use it as the new index.
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
                let block: ValueBlockRc = self.try_into().unwrap();
                let unique = block.make_unique();
                // Safety: `make_unique()` ensures there's no sharing, and we have the exclusive
                // access (ownership) of the block.
                unsafe { (*unique.header_mut()).pos_idx = pos_idx }
                unique.into()
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
            ValueTag::Inline => self.inline_pos_idx().unwrap().into(),
        }
    }

    /// Returns the position associated to this value.
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

// Since a `NickelValue` can be a reference-counted pointer in disguise, we can't just copy it
// blindly. We need to make sure the reference count is incremented accordingly.
impl Clone for NickelValue {
    fn clone(&self) -> Self {
        if self.tag() == ValueTag::Pointer {
            unsafe {
                // Safety: if `self.tag()` is `Pointer`, `self.0` must be a valid non-null pointer
                // to a value block.
                //
                // We need to prevent this value block from being dropped as this would decrement
                // the refcount, nullifying our increment.
                let block_ptr =
                    ManuallyDrop::new(ValueBlockRc::from_raw_unchecked(self.data as *mut u8));
                block_ptr.incr_ref_count();
            }
        }

        // Safety: we incremented the ref count above, if the inner value is a block.
        unsafe { NickelValue::raw_copy(self) }
    }
}

// Same as for `Clone`: since we might be a reference-counted pointer in disguise, we need to
// properly decrement the underlying ref count.
impl Drop for NickelValue {
    fn drop(&mut self) {
        if self.tag() == ValueTag::Pointer {
            unsafe {
                // Safety: if `self.tag()` is `Pointer`, `self.0` must be valid non-null pointer to
                // a value block
                std::mem::drop(ValueBlockRc::from_raw_unchecked(self.data as *mut u8));
            }
        }
    }
}

impl From<InlineValue> for NickelValue {
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
    fn from(block: ValueBlockRc) -> Self {
        // We must avoid dropping `Self` here, which would decrement the reference count.
        let this = ManuallyDrop::new(block);
        // Safety: this is a valid pointer to a `ValueBlockRc`, and the usage of manually drop
        // ensures the ref count is properly adjusted.
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
    fn from(tag: ValueTag) -> Self {
        // Safety: `#[repr(usize)]` on [ValueTag] guarantees that the enum is represented in memory
        // with the exact same layout as `usize`
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
// unsafe fconversion functions from and to numeric typesunctions are relying on the precise values
// and range of `ValueTag`. If you add or remove tags, make sure to update all the corresponding
// code, in particular the `Self::MAX` constant.
//
// Values must be consecutive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum BodyTag {
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

impl BodyTag {
    /// Returns the padding required in bytes between the header and the body in [ValueBlockRc] for
    /// a given type of data. Calls to [ValueBlockRc::padding] under the hood instantiated with the
    /// right type.
    fn padding(&self) -> usize {
        match self {
            BodyTag::Number => ValueBlockRc::padding::<NumberBody>(),
            BodyTag::String => ValueBlockRc::padding::<StringBody>(),
            BodyTag::Array => ValueBlockRc::padding::<ArrayBody>(),
            BodyTag::Record => ValueBlockRc::padding::<RecordBody>(),
            BodyTag::Thunk => ValueBlockRc::padding::<ThunkBody>(),
            BodyTag::Term => ValueBlockRc::padding::<TermBody>(),
            BodyTag::Label => ValueBlockRc::padding::<LabelBody>(),
            BodyTag::EnumVariant => ValueBlockRc::padding::<EnumVariantBody>(),
            BodyTag::ForeignId => ValueBlockRc::padding::<ForeignIdBody>(),
            BodyTag::SealingKey => ValueBlockRc::padding::<SealingKeyBody>(),
            BodyTag::CustomContract => ValueBlockRc::padding::<CustomContractBody>(),
            BodyTag::Type => ValueBlockRc::padding::<TypeBody>(),
        }
    }

    /// Returns the offset of the body of a value block from start pointer (the header). Calls to
    /// [ValueBlockRc::body_offset] under the hood instantiated with the right type.
    fn body_offset(&self) -> usize {
        match self {
            BodyTag::Number => ValueBlockRc::body_offset::<NumberBody>(),
            BodyTag::String => ValueBlockRc::body_offset::<StringBody>(),
            BodyTag::Array => ValueBlockRc::body_offset::<ArrayBody>(),
            BodyTag::Record => ValueBlockRc::body_offset::<RecordBody>(),
            BodyTag::Thunk => ValueBlockRc::body_offset::<ThunkBody>(),
            BodyTag::Term => ValueBlockRc::body_offset::<TermBody>(),
            BodyTag::Label => ValueBlockRc::body_offset::<LabelBody>(),
            BodyTag::EnumVariant => ValueBlockRc::body_offset::<EnumVariantBody>(),
            BodyTag::ForeignId => ValueBlockRc::body_offset::<ForeignIdBody>(),
            BodyTag::SealingKey => ValueBlockRc::body_offset::<SealingKeyBody>(),
            BodyTag::CustomContract => ValueBlockRc::body_offset::<CustomContractBody>(),
            BodyTag::Type => ValueBlockRc::body_offset::<TypeBody>(),
        }
    }

    /// Returns the layout to be used for (de)allocation of a whole value block for a given type of
    /// data. Calls to [ValueBlockRc::block_layout] under the hood instantiated with the right
    /// type.
    fn block_layout(&self) -> Layout {
        match self {
            BodyTag::Number => ValueBlockRc::block_layout::<NumberBody>(),
            BodyTag::String => ValueBlockRc::block_layout::<StringBody>(),
            BodyTag::Array => ValueBlockRc::block_layout::<ArrayBody>(),
            BodyTag::Record => ValueBlockRc::block_layout::<RecordBody>(),
            BodyTag::Thunk => ValueBlockRc::block_layout::<ThunkBody>(),
            BodyTag::Term => ValueBlockRc::block_layout::<TermBody>(),
            BodyTag::Label => ValueBlockRc::block_layout::<LabelBody>(),
            BodyTag::EnumVariant => ValueBlockRc::block_layout::<EnumVariantBody>(),
            BodyTag::ForeignId => ValueBlockRc::block_layout::<ForeignIdBody>(),
            BodyTag::SealingKey => ValueBlockRc::block_layout::<SealingKeyBody>(),
            BodyTag::CustomContract => ValueBlockRc::block_layout::<CustomContractBody>(),
            BodyTag::Type => ValueBlockRc::block_layout::<TypeBody>(),
        }
    }

    /// The highest possible value for a [BodyTag].
    const MAX: u8 = BodyTag::Type as u8;
}

impl From<BodyTag> for u8 {
    fn from(tag: BodyTag) -> Self {
        tag as u8
    }
}

impl TryFrom<u8> for BodyTag {
    type Error = TagOutOfBoundsError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value <= BodyTag::MAX {
            // Safety: `#[repr(u8)]` on `BodyTag` guarantees that the enum is safe to transmute to
            // and from `u8`, as long as we are in the range of valid tags.
            Ok(unsafe { transmute::<u8, BodyTag>(value) })
        } else {
            Err(TagOutOfBoundsError)
        }
    }
}

/// A 56-bits ref-count represented as a sequence of 7 bytes (little endian).
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct RefCount([u8; 7]);

impl RefCount {
    /// The maximum representable ref count as a `u64`.
    pub const MAX: u64 = 0x00_FF_FF_FF_FF_FF_FF_FF;
    /// A ref count equals to one.
    pub const ONE: Self = RefCount([1, 0, 0, 0, 0, 0, 0]);

    /// Creates a ref count from a `u64` value. As opposed to the [TryFrom] instance that is
    /// failing for `value > RefCount::MAX`, this method truncates the higher bits and will return
    /// `RefCount::MAX` in those cases instead.
    pub fn from_u64_truncate(value: u64) -> Self {
        let mut data = [0; 7];
        let bytes = value.to_le_bytes();
        data.copy_from_slice(&bytes[0..7]);
        Self(data)
    }
}

impl From<RefCount> for u64 {
    fn from(value: RefCount) -> Self {
        let mut data = [0; 8];
        // Since ref count is in little endian representation, we should copy the first 7 bytes and
        // keep the last one at zero.
        data[..7].copy_from_slice(&value.0);
        u64::from_le_bytes(data)
    }
}

impl TryFrom<u64> for RefCount {
    type Error = ();

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        let mut data = [0; 7];
        let bytes = value.to_le_bytes();

        if bytes[7] == 0 {
            data.copy_from_slice(&bytes[0..7]);
            Ok(RefCount(data))
        } else {
            Err(())
        }
    }
}

/// The header for a heap-allocated Nickel value which is laid out at the beginning of a value
/// block directly followed by the value data.
// We set a minimal alignment of `4` bytes, so that pointers to the content of a value block
// (which are aligned to the max of the alignement of `ValueBlockHeader` and the content) is
// guaranteed to have at least the last 2 bits free for tagging (although we currently only use one
// bits).
#[repr(Rust, align(4))]
#[derive(Debug, Clone, PartialEq, Eq)]
struct ValueBlockHeader {
    /// The tag determining the type and the layout of the data following the header in a value
    /// block.
    tag: BodyTag,
    /// The (strong) reference count of the value.
    ref_count: RefCount,
    /// The position index of the source position of the value.
    pos_idx: PosIdx,
}

impl ValueBlockHeader {
    /// Creates a new header for a value block with the given tag, a reference count of 1 and the
    /// given position index.
    pub fn new(tag: BodyTag, pos_idx: PosIdx) -> Self {
        Self {
            tag,
            // 1 in little endian representation
            ref_count: RefCount::ONE,
            pos_idx,
        }
    }

    /// Creates a new header for a value block with the given tag, a reference count of 1 and a
    /// position index set to [PosIdx::NONE].
    pub fn new_posless(tag: BodyTag) -> Self {
        Self::new(tag, PosIdx::NONE)
    }

    fn ref_count(&self) -> u64 {
        self.ref_count.into()
    }

    fn set_ref_count(&mut self, count: u64) {
        self.ref_count = RefCount::try_from(count).expect("reference count overflow");
    }

    fn incr_ref_count(&mut self) {
        self.set_ref_count(self.ref_count() + 1);
    }

    fn decr_ref_count(&mut self) {
        self.set_ref_count(self.ref_count() - 1);
    }
}

/// Marker trait for the data that can be stored in a Nickel value block after the header.
pub trait ValueBlockBody {
    const TAG: BodyTag;
}

#[derive(Clone, Debug, PartialEq)]
pub struct NumberBody(pub Number);

#[derive(Clone, Debug, PartialEq)]
pub struct StringBody(pub NickelString);

#[derive(Clone, Debug, PartialEq, Default)]
pub struct ArrayBody {
    pub array: Array,
    /// Arrays implement lazy contract application for performance reasons: contracts appiled to
    /// this array are lazily accumulated in this field and only applied when an element is
    /// extracted.
    pub pending_contracts: Vec<RuntimeContract>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct RecordBody(pub RecordData);

#[derive(Clone, Debug)]
pub struct ThunkBody(pub CacheIndex);

impl PartialEq for ThunkBody {
    fn eq(&self, other: &Self) -> bool {
        // We don't compare closure, because we can't, without the evaluation cache at hand. It's
        // ok even if the cache index are the same: we implement PartialEq, so we can have `x !=
        // x`. In practice, this case shouldn't even be triggered, because tests usually compare
        // simple terms without closures in it (or terms where closures have been substituted for
        // their value).
        false
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TermBody(pub Term);

#[derive(Clone, Debug, PartialEq)]
pub struct LabelBody(pub Label);

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariantBody {
    pub tag: LocIdent,
    pub arg: Option<NickelValue>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ForeignIdBody(pub ForeignIdPayload);

#[derive(Clone, Debug, PartialEq)]
pub struct CustomContractBody(pub NickelValue);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SealingKeyBody(pub SealingKey);

#[derive(Clone, Debug, PartialEq)]
pub struct TypeBody {
    /// The static type.
    pub typ: Type,
    /// The conversion of this type to a contract, that is, `typ.contract()?`. This field
    /// serves as a caching mechanism so we only run the contract generation code once per type
    /// written by the user.
    pub contract: NickelValue,
}

impl ValueBlockBody for NumberBody {
    const TAG: BodyTag = BodyTag::Number;
}

impl ValueBlockBody for ArrayBody {
    const TAG: BodyTag = BodyTag::Array;
}

impl ValueBlockBody for RecordBody {
    const TAG: BodyTag = BodyTag::Record;
}

impl ValueBlockBody for StringBody {
    const TAG: BodyTag = BodyTag::String;
}

impl ValueBlockBody for ThunkBody {
    const TAG: BodyTag = BodyTag::Thunk;
}

impl ValueBlockBody for TermBody {
    const TAG: BodyTag = BodyTag::Term;
}

impl ValueBlockBody for LabelBody {
    const TAG: BodyTag = BodyTag::Label;
}

impl ValueBlockBody for EnumVariantBody {
    const TAG: BodyTag = BodyTag::EnumVariant;
}

impl ValueBlockBody for ForeignIdBody {
    const TAG: BodyTag = BodyTag::ForeignId;
}

impl ValueBlockBody for CustomContractBody {
    const TAG: BodyTag = BodyTag::CustomContract;
}

impl ValueBlockBody for SealingKeyBody {
    const TAG: BodyTag = BodyTag::SealingKey;
}

impl ValueBlockBody for TypeBody {
    const TAG: BodyTag = BodyTag::Type;
}

/// A pointer to a heap-allocated, reference-counted Nickel value of variable size, although the
/// size is a deterministic function of the tag stored in the header (first word of the block).
///
/// # Layout
///
/// In the following, the type `T : ValueBlockBody` is uniquely determined by the tag in the
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
/// The base address (self.0) is `max(align_of::<ValueBlockHeader>(), align_of::<T>())`-aligned.
/// The padding is determined statically and is only necessary if `size_of::<ValueBlockHeader>()`
/// isn't a multiple of the total alignment. Currently, for example, there is no padding on x86_64
/// in practice. It might happen if the alignment of `T` grows larger than the size of the header.
///
/// The content of padding is left uninitialized and should not be accessed.
pub struct ValueBlockRc(NonNull<u8>);

impl ValueBlockRc {
    /// Converts a raw pointer back to a value block.
    ///
    /// # Safety
    ///
    /// Similar safety conditions as for [std::rc::Rc::from_raw]. `ptr` must have been obtained
    /// from a previous call to `ValueBlockRc::into_raw`, and the value block must not have been
    /// deallocated since then. Those conditions are typically met when the pointer has been
    /// obtained from `ValueBlockRc::into_raw` and hasn't been converted back to a `ValueBlockRc`
    /// in between. Note that if the same raw pointer is converted back to a `ValueBlockRc` which is
    /// then dropped, this pointer becomes invalid and mustn't be used anymore (and in particular be
    /// passed to this function).
    pub unsafe fn from_raw(ptr: *mut u8) -> Option<Self> {
        NonNull::new(ptr).map(ValueBlockRc)
    }

    /// Creates a new [Self] from a raw pointer without checking for null.
    ///
    /// # Safety
    ///
    /// Same conditions as for [ValueBlockRc::from_raw].
    pub unsafe fn from_raw_unchecked(ptr: *mut u8) -> Self {
        // Safety: if the pointer is coming from a call to `into_raw` (precondition), it can't be
        // nulll (invariant of `ValueBlockRc`)
        unsafe { ValueBlockRc(NonNull::new_unchecked(ptr)) }
    }

    /// Converts a pointer to a value block to a raw pointer. See [Self::from_raw].
    pub fn into_raw(self) -> *mut u8 {
        // We must avoid dropping `Self` here, which would decrement the reference count.
        let this = ManuallyDrop::new(self);
        this.0.as_ptr()
    }

    /// Returns the header of this value block.
    fn header(&self) -> ValueBlockHeader {
        // Safety: self.0 is always a valid pointer into a value block
        unsafe { Self::header_from_raw(self.0) }
    }

    /// Returns the tag in the header of this value block.
    fn tag(&self) -> BodyTag {
        // Safety: self.0 is always a valid pointer into a value block
        unsafe { Self::tag_from_raw(self.0) }
    }

    /// Returns the position index of this value.
    pub fn pos_idx(&self) -> PosIdx {
        // Safety: self.0 is always a valid pointer into a value block
        unsafe { Self::header_from_raw(self.0).pos_idx }
    }

    /// Returns the header of a value block pointed to by `ptr`.
    ///
    /// # Safety
    ///
    /// - `ptr` must be a valid pointer into a value block.
    unsafe fn header_from_raw(ptr: NonNull<u8>) -> ValueBlockHeader {
        // Safety: the safety precondition of this function
        unsafe { ptr.cast::<ValueBlockHeader>().as_ref().clone() }
    }

    /// Returns the tag in the header of a value block pointed to by `ptr`.
    ///
    /// # Safety
    ///
    /// - `ptr` must be a valid pointer into a value block.
    unsafe fn tag_from_raw(ptr: NonNull<u8>) -> BodyTag {
        // Safety: the safety precondition of this function
        unsafe { ptr.cast::<ValueBlockHeader>().as_ref().tag }
    }

    /// Returns a mutable pointer to the header of this value block. [Self::header_mut] returns a
    /// mutable pointer instead of a reference to remain safe and avoid the possibility of handing
    /// over multiple mutable references.
    fn header_mut(&self) -> *mut ValueBlockHeader {
        self.0.cast::<ValueBlockHeader>().as_ptr()
    }

    /// Increments the reference count of this value block.
    fn incr_ref_count(&self) {
        // Safety: we never hand over mutable references into a value block unless there is an
        // active mutable borrow to `self` and the reference is 1-counted, but this is excluded
        // here since we borrow `self` immutably.
        unsafe { (*self.header_mut()).incr_ref_count() }
    }

    /// Decrements the reference count of this value block.
    fn decr_ref_count(&self) {
        // Safety: we never hand over mutable references into a value block unless there is an
        // active mutable borrow to `self` and the reference is 1-counted, but this excluded here
        // since we borrow `self` immutably.
        unsafe { (*self.header_mut()).decr_ref_count() }
    }

    /// Same as [std::rc::Rc::get_mut] but for a value block. Mutably borrows the value block.
    /// Returns an error if the tag doesn't match `T::Tag`.
    pub fn try_get_mut<T: ValueBlockBody>(&mut self) -> Result<Option<&mut T>, TagMismatchError> {
        if self.tag() != T::TAG {
            Err(TagMismatchError)
        } else if self.header().ref_count() != 1 {
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
    pub fn get_mut<T: ValueBlockBody>(&mut self) -> &mut T {
        self.try_get_mut().unwrap().unwrap()
    }

    /// Same as [Self::try_make_mut] but doesn't check that `self.tag()` matches `T::Tag`, and
    /// operate on a raw pointer to a value block.
    ///
    /// `ptr` isn't wrapped in a [ValueBlockRc] or a [NickelValue], so it won't de-allocate the
    /// block when it goes out of scope. It is the responsibility of the caller to wrap this
    /// pointer in a a block or a value, e.g. by using [Self::from_raw], to avoid leaks.
    ///
    /// # Safety
    ///
    /// - `ptr` must be a valid non-null pointer to a value block
    /// - the tag in the header of the pointee block must be equal to `T::Tag`
    /// - the pointee block must be alive for the duration of `'a`
    /// - the reference count of the pointee block must not change during `'a`. For example, one
    ///   could extract the underlying pointer of a 1-reference counted value (while keeping the
    ///   value alive), make a mutable reference through this method, and while the mutable
    ///   reference is still alive, clone the original value. We would then have mutable aliasing,
    ///   which is undefined behavior.
    pub unsafe fn make_mut_from_raw_unchecked<'a, T: ValueBlockBody + Clone>(
        ptr: &mut NonNull<u8>,
    ) -> &'a mut T {
        unsafe {
            // Safety: `ptr` is a valid non-null pointer to a value block (precondition)
            let header = Self::header_from_raw(*ptr);

            if header.ref_count() == 1 {
                // Safety: we know that the value block is unique, so we can safely decode the content
                // without any risk of aliasing.
                Self::decode_mut_from_raw_unchecked::<T>(*ptr)
            } else {
                // Safety: `ptr` is a valid pointer to a block with a `T` inside (precondition)
                let unique = ManuallyDrop::new(ValueBlockRc::encode(
                    Self::decode_from_raw_unchecked::<T>(*ptr).clone(),
                    header.pos_idx,
                ));
                *ptr = unique.0;
                // Safety: we just made a unique block, so we can safely decode the content without any
                // risk of aliasing.
                Self::decode_mut_from_raw_unchecked::<T>(*ptr)
            }
        }
    }

    /// Same as [std::rc::Rc::make_mut] but for a value block. Returns an error if the tag doesn't
    /// match `T::Tag`.
    pub fn try_make_mut<T: ValueBlockBody + Clone>(&mut self) -> Result<&mut T, TagMismatchError> {
        if self.tag() != T::TAG {
            Err(TagMismatchError)
        } else if self.header().ref_count() == 1 {
            // Safety: we know that the value block is unique, so we can safely decode the content
            // without any risk of aliasing.
            unsafe { Ok(self.decode_mut_unchecked::<T>()) }
        } else {
            let unique = ValueBlockRc::encode(self.decode::<T>().clone(), self.pos_idx());
            *self = unique;
            // Safety: `self.tag()` is `T::TAG`
            unsafe { Ok(self.decode_mut_unchecked::<T>()) }
        }
    }

    /// Panicking variant of [Self::try_make_mut]. Equivalent to `self.try_make_mut().unwrap()`.
    pub fn make_mut<T: ValueBlockBody + Clone>(&mut self) -> &mut T {
        self.try_make_mut().unwrap()
    }

    /// Make a unique (non shared) value block out of `self`, that is a 1-reference counted block
    /// with the same data. Similar to [Self::make_mut] in spirit but it doesn't require to specify
    /// the `T` and doesn't return a mutable reference.
    ///
    /// This is a no-op if `self` is 1-reference counted. Otherwise, a new (strong) copy is
    /// allocated and returned.
    pub fn make_unique(self) -> Self {
        if self.header().ref_count() == 1 {
            self
        } else {
            self.strong_clone()
        }
    }

    /// Creates a copy of the content of this value block. As opposed to [Self::clone], which just
    /// increments the reference count but encapsulates a pointer to the same block in memory (as
    /// [std::rc::Rc::clone]), this method allocates a fresh block with a clone of the content and
    /// return a value that is 1-reference counted.
    pub fn strong_clone(&self) -> Self {
        match self.tag() {
            BodyTag::Number => Self::encode(self.decode::<NumberBody>().clone(), self.pos_idx()),
            BodyTag::Array => Self::encode(self.decode::<ArrayBody>().clone(), self.pos_idx()),
            BodyTag::Record => Self::encode(self.decode::<RecordBody>().clone(), self.pos_idx()),
            BodyTag::String => Self::encode(self.decode::<StringBody>().clone(), self.pos_idx()),
            BodyTag::Thunk => Self::encode(self.decode::<ThunkBody>().clone(), self.pos_idx()),
            BodyTag::Term => Self::encode(self.decode::<TermBody>().clone(), self.pos_idx()),
            BodyTag::Label => Self::encode(self.decode::<LabelBody>().clone(), self.pos_idx()),
            BodyTag::EnumVariant => {
                Self::encode(self.decode::<EnumVariantBody>().clone(), self.pos_idx())
            }
            BodyTag::ForeignId => {
                Self::encode(self.decode::<ForeignIdBody>().clone(), self.pos_idx())
            }
            BodyTag::SealingKey => {
                Self::encode(self.decode::<SealingKeyBody>().clone(), self.pos_idx())
            }
            BodyTag::CustomContract => {
                Self::encode(self.decode::<CustomContractBody>().clone(), self.pos_idx())
            }
            BodyTag::Type => Self::encode(self.decode::<TypeBody>().clone(), self.pos_idx()),
        }
    }

    /// Returns the required padding in bytes between the header and the body in [ValueBlockRc]
    /// depending on the tag. Calls to [ValueBlockRc::padding] under the hood instantiated with the
    /// right type.
    const fn padding<T: ValueBlockBody>() -> usize {
        let align = Self::block_align::<T>();
        let leftover = size_of::<ValueBlockHeader>() % align;

        (align - leftover) % align
    }

    /// Returns the offset to add to the start of a value block (the address of the block header)
    /// to reach the body (including padding). Offsetting [Self::0] by [Self::body_offset]
    /// yields a valid pointer to a `T`.
    const fn body_offset<T: ValueBlockBody>() -> usize {
        size_of::<ValueBlockHeader>() + Self::padding::<T>()
    }

    /// Returns the alignment in bytes of a value block for a given value content type `T`. This is
    /// the maximum of the alignment of the header and the alignment of `T`, and is guaranteed to
    /// be at least 4 bytes.
    const fn block_align<T: ValueBlockBody>() -> usize {
        // Note: we can't use `std::cmp::max` in a const context
        if align_of::<ValueBlockHeader>() < align_of::<T>() {
            align_of::<T>()
        } else {
            align_of::<ValueBlockHeader>()
        }
    }

    /// Determines the layout for allocation or de-allocation of value blocks for a given value
    /// content type `T`.
    const fn block_layout<T: ValueBlockBody>() -> Layout {
        let header_layout = Layout::new::<ValueBlockHeader>();
        let body_layout = Layout::new::<T>();
        let size = header_layout.size() + Self::padding::<T>() + body_layout.size();

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
    fn encode<T: ValueBlockBody>(value: T, pos_idx: PosIdx) -> Self {
        unsafe {
            // Safety: the layout of a block verifies `size > 0`
            let start = alloc(Self::block_layout::<T>());

            if start.is_null() {
                panic!("out of memory: failed to allocate memory for Nickel value")
            }

            let header_ptr = start as *mut ValueBlockHeader;
            header_ptr.write(ValueBlockHeader::new(T::TAG, pos_idx));

            // Safety: layout computations are correct (hopefully)
            let body_ptr = start.add(Self::body_offset::<T>()) as *mut T;
            body_ptr.write(value);

            // Safety: we abort if `start.is_null()` above, so `start` is not null
            Self(NonNull::new_unchecked(start))
        }
    }

    /// Tries to decode this value block as a reference to a value of type `T`. Returns `None` if
    /// the tag of this value block is not `T::TAG`.
    fn try_decode<T: ValueBlockBody>(&self) -> Option<&T> {
        // Safety: we decode only if `self.tag()` is `T::TAG`
        (self.tag() == T::TAG).then(|| unsafe { self.decode_unchecked() })
    }

    /// Panicking variant of [Self::try_decode]. Same as `self.try_decode().unwrap()`.
    #[track_caller]
    fn decode<T: ValueBlockBody>(&self) -> &T {
        self.try_decode().unwrap()
    }

    /// Unsafe variant of [Self::try_decode]. Doesn't perform any tag check, and blindly tries to
    /// decode the content of this block to a `&T`.
    ///
    /// # Safety
    ///
    /// The content of this value block must have been encoded from a value of type `T`, that is
    /// `self.tag() == T::TAG`.
    unsafe fn decode_unchecked<T: ValueBlockBody>(&self) -> &T {
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
    unsafe fn decode_mut_unchecked<T: ValueBlockBody>(&mut self) -> &mut T {
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
    unsafe fn decode_from_raw_unchecked<'a, T: ValueBlockBody>(ptr: NonNull<u8>) -> &'a T {
        // Safety: preconditions
        unsafe { ptr.add(Self::body_offset::<T>()).cast::<T>().as_ref() }
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
    unsafe fn decode_mut_from_raw_unchecked<'a, T: ValueBlockBody>(ptr: NonNull<u8>) -> &'a mut T {
        // Safety: preconditions
        unsafe { ptr.add(Self::body_offset::<T>()).cast::<T>().as_mut() }
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
    unsafe fn try_decode_from_raw<'a, T: ValueBlockBody>(ptr: NonNull<u8>) -> Option<&'a T> {
        // Safety: we've checked that the tag matched `T`. The rest of the safety conditions are
        // the pre-conditions of the current function `try_decode`.
        unsafe { (Self::tag_from_raw(ptr) == T::TAG).then(|| Self::decode_from_raw_unchecked(ptr)) }
    }
}

impl Drop for ValueBlockRc {
    fn drop(&mut self) {
        if self.header().ref_count() == 1 {
            unsafe {
                let tag = self.tag();
                // Safety: the value block is guaranteed to have been allocated with a size of
                // `size_of::<ValueBlockHeader>()` + `tag.padding()` + `size_of::<T>()`.
                let body_ptr = self.0.as_ptr().add(tag.body_offset());

                // Safety: `body_ptr` is a valid pointer for the corresponding type and it hasn't
                // been dropped before, as it's only dropped once when the last reference goes out
                // of scope.
                match tag {
                    BodyTag::Number => {
                        ptr::drop_in_place(body_ptr as *mut NumberBody);
                    }
                    BodyTag::Array => {
                        ptr::drop_in_place(body_ptr as *mut ArrayBody);
                    }
                    BodyTag::Record => {
                        ptr::drop_in_place(body_ptr as *mut RecordBody);
                    }
                    BodyTag::String => {
                        ptr::drop_in_place(body_ptr as *mut StringBody);
                    }
                    BodyTag::Thunk => {
                        ptr::drop_in_place(body_ptr as *mut ThunkBody);
                    }
                    BodyTag::Term => {
                        ptr::drop_in_place(body_ptr as *mut TermBody);
                    }
                    BodyTag::Label => {
                        ptr::drop_in_place(body_ptr as *mut LabelBody);
                    }
                    BodyTag::EnumVariant => {
                        ptr::drop_in_place(body_ptr as *mut EnumVariantBody);
                    }
                    BodyTag::ForeignId => {
                        ptr::drop_in_place(body_ptr as *mut ForeignIdBody);
                    }
                    BodyTag::CustomContract => {
                        ptr::drop_in_place(body_ptr as *mut CustomContractBody);
                    }
                    BodyTag::SealingKey => {
                        ptr::drop_in_place(body_ptr as *mut SealingKeyBody);
                    }
                    BodyTag::Type => {
                        ptr::drop_in_place(body_ptr as *mut TypeBody);
                    }
                };

                dealloc(self.0.as_ptr(), tag.block_layout());
            }
        } else {
            self.decr_ref_count();
        }
    }
}

impl Clone for ValueBlockRc {
    fn clone(&self) -> Self {
        self.incr_ref_count();
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
    pub fn unwrap_alloc(self) -> C {
        self.into_opt().unwrap()
    }
}

impl<C: Default> Container<C> {
    /// Unwrap the underlying container, or allocate a new empty one (via [Default]) if `self` is
    /// [Container::Empty].
    pub fn unwrap_or_alloc(self) -> C {
        self.into_opt().unwrap_or_default()
    }
}

impl Container<&RecordBody> {
    /// Retrieves a field from [crate::term::record::RecordData::fields], or returns `None` if `self`
    /// is [Self::Empty].
    pub fn get_field(&self, id: LocIdent) -> Option<&Field> {
        self.into_opt().and_then(|record| record.0.fields.get(&id))
    }

    /// Returns a vector of all the fields' names of this record sorted alphabetically. Returns an
    /// empty vector if `self` is [Self::Empty].
    pub fn field_names(&self, op_kind: RecordOpKind) -> Vec<LocIdent> {
        self.into_opt()
            .map(|record| record.0.field_names(op_kind))
            .unwrap_or_default()
    }

    /// Returns the length of the underlying record.
    pub fn len(&self) -> usize {
        self.into_opt().map_or(0, |record| record.0.fields.len())
    }

    /// Checks if this record is [Self::Empty], or is [Self::Alloc] where the underlying record is
    /// empty (is such that [crate::term::record::RecordData::is_empty] returns `true`).
    pub fn is_empty(&self) -> bool {
        match self {
            Container::Empty => true,
            Container::Alloc(record) => record.0.is_empty(),
        }
    }
}

impl Container<&ArrayBody> {
    /// Iterates over the elements of the array.
    pub fn iter(&self) -> impl Iterator<Item = &NickelValue> {
        self.into_opt()
            .into_iter()
            .map(|array_body| array_body.array.iter())
            .flatten()
    }

    /// Iterates over the pending contracts.
    pub fn iter_pending_contracts(&self) -> impl Iterator<Item = &RuntimeContract> {
        self.into_opt()
            .into_iter()
            .map(|array_body| array_body.pending_contracts.iter())
            .flatten()
    }

    /// Returns the length of the underlying array.
    pub fn len(&self) -> usize {
        self.into_opt()
            .map_or(0, |array_body| array_body.array.len())
    }

    /// Checks if this record is [Self::Empty], or is [Self::Alloc] where the underlying array is
    /// empty (is such that [crate::bytecode::value::Array::is_empty] returns `true`).
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
    Number(&'a NumberBody),
    Array(Container<&'a ArrayBody>),
    Record(Container<&'a RecordBody>),
    String(&'a StringBody),
    Thunk(&'a ThunkBody),
    Term(&'a TermBody),
    Label(&'a LabelBody),
    EnumVariant(&'a EnumVariantBody),
    ForeignId(&'a ForeignIdBody),
    SealingKey(&'a SealingKeyBody),
    CustomContract(&'a CustomContractBody),
    Type(&'a TypeBody),
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
    /// part in [Self::Array] and [Self::Record]. They won't appear as [ValueContent::InlineValue].
    Null(&'a mut NickelValue),
    Bool(&'a mut NickelValue),
    Number(&'a mut NumberBody),
    Array(Container<&'a mut ArrayBody>),
    Record(Container<&'a mut RecordBody>),
    String(&'a mut StringBody),
    Thunk(&'a mut ThunkBody),
    Term(&'a mut TermBody),
    Label(&'a mut LabelBody),
    EnumVariant(&'a mut EnumVariantBody),
    ForeignId(&'a mut ForeignIdBody),
    SealingKey(&'a mut SealingKeyBody),
    CustomContract(&'a mut CustomContractBody),
    Type(&'a mut TypeBody),
}

/// A lazy handle to the owned content of a value block.
///
/// It's a common pattern to need to move the content out of a block (copying the content if it's
/// shared, but avoiding copy if it's unique, like [std::)  only if a value matches some patterns. Typically
/// during program transformations, where blocks should always be 1-reference counted, and where
/// one transformation only affects specific nodes.
///
/// This is the *lazy* part: while the type of the body is known, the content is hidden behind a
/// lazy handle, which can either be unwrapped further - consuming the original value irreversibly
/// and producing an owned version of the body - or reverted back to the original value.
pub enum ValueContent {
    /// It can seem useless to carry a lens here, since there's no real data to take out. However,
    /// it's important to keep the ability to restore a [Self] value to the original [NickelValue],
    /// which is precisely the role of the lens.
    Null(lens::ValueLens<()>),
    Bool(lens::ValueLens<bool>),
    Number(lens::ValueLens<NumberBody>),
    Array(lens::ValueLens<Container<ArrayBody>>),
    Record(lens::ValueLens<Container<RecordBody>>),
    String(lens::ValueLens<StringBody>),
    Thunk(lens::ValueLens<ThunkBody>),
    Term(lens::TermContent),
    Label(lens::ValueLens<LabelBody>),
    EnumVariant(lens::ValueLens<EnumVariantBody>),
    ForeignId(lens::ValueLens<ForeignIdBody>),
    SealingKey(lens::ValueLens<SealingKeyBody>),
    CustomContract(lens::ValueLens<CustomContractBody>),
    Type(lens::ValueLens<TypeBody>),
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

#[cfg(test)]
mod tests {
    use super::*;

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
            src_id: Files::new().add("<test>", String::from("empty")),
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
                .phys_eq(
                    &NickelValue::null().with_inline_pos_idx(pos_table.push(dummy_pos))
                )
        );
        assert!(
            inline_true
                .with_inline_pos_idx(pos_table.push(dummy_pos))
                .phys_eq(
                    &NickelValue::bool_true().with_inline_pos_idx(pos_table.push(dummy_pos))
                )
        );
        assert!(
            inline_false
                .with_inline_pos_idx(pos_table.push(dummy_pos))
                .phys_eq(
                    &NickelValue::bool_false()
                        .with_inline_pos_idx(pos_table.push(dummy_pos))
                )
        );
        assert!(
            inline_empty_array
                .with_inline_pos_idx(pos_table.push(dummy_pos))
                .phys_eq(
                    &NickelValue::empty_array()
                        .with_inline_pos_idx(pos_table.push(dummy_pos))
                )
        );
        assert!(
            inline_empty_record
                .with_inline_pos_idx(pos_table.push(dummy_pos))
                .phys_eq(
                    &NickelValue::empty_record()
                        .with_inline_pos_idx(pos_table.push(dummy_pos))
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

        assert_eq!(number_value.as_number().unwrap().0, Number::from(42));
        assert_eq!(
            string_value.as_string().unwrap().0,
            NickelString::from("Hello, World!")
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

        assert!(as_val.try_get_mut::<NumberBody>().unwrap().is_none());

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
        *cow.make_mut() = NumberBody(Number::from(0));

        // The original value wasn't 1-RCed, so it should be left unchanged and cow must be a
        // separate copy.
        assert_eq!(as_val.header().ref_count(), 2);
        assert_eq!(as_val.decode::<NumberBody>().0, Number::from(42));
        assert_eq!(cow.header().ref_count(), 1);
        assert_eq!(cow.decode::<NumberBody>().0, Number::from(0));

        unsafe { ManuallyDrop::drop(&mut block_copy) }

        *as_val.get_mut() = NumberBody(Number::from(100));
        assert_eq!(as_val.header().ref_count(), 1);
        assert_eq!(as_val.decode::<NumberBody>().0, Number::from(100));
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

        assert_eq!(record_value.decode::<RecordBody>().0.fields.len(), 1);
        assert_eq!(array_value.decode::<ArrayBody>().array.len(), 1);

        record_value
            .get_mut::<RecordBody>()
            .0
            .fields
            .insert(LocIdent::from("world"), Default::default());
        array_value
            .get_mut::<ArrayBody>()
            .array
            .push(NickelValue::null());

        let array_copy = array_value.clone();
        let record_copy = record_value.clone();

        assert_eq!(record_copy.decode::<RecordBody>().0.fields.len(), 2);
        assert_eq!(array_copy.decode::<ArrayBody>().array.len(), 2);
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

        assert_eq!(record.as_record().unwrap().unwrap_alloc().0.fields.len(), 1);
        assert_eq!(array.as_array().unwrap().unwrap_alloc().array.len(), 1);

        if let Some(ValueContentRefMut::Record(Container::Alloc(RecordBody(record)))) =
            record.content_mut()
        {
            record
                .fields
                .insert(LocIdent::from("world"), Default::default());
        } else {
            panic!("Expected RecordBody");
        }

        if let Some(ValueContentRefMut::Array(Container::Alloc(array_body))) = array.content_mut() {
            array_body.array.push(NickelValue::null());
        } else {
            panic!("Expected ArrayBody");
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

        assert_eq!(record.as_record().unwrap().unwrap_alloc().0.fields.len(), 1);
        assert_eq!(array.as_array().unwrap().unwrap_alloc().array.len(), 1);

        if let ValueContentRefMut::Record(Container::Alloc(RecordBody(record))) =
            record.content_make_mut()
        {
            record
                .fields
                .insert(LocIdent::from("world"), Default::default());
        } else {
            panic!("Expected RecordBody");
        }

        if let ValueContentRefMut::Array(Container::Alloc(array_body)) = array.content_make_mut() {
            array_body.array.push(NickelValue::null());
        } else {
            panic!("Expected ArrayBody");
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
