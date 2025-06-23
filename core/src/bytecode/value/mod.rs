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
    label::Label,
    term::{
        record::RecordData, string::NickelString, EnumVariantAttrs, ForeignIdPayload, Number,
        SealingKey,
    },
    typ::Type,
};
use nickel_lang_vector::Slice;
use std::{
    alloc::{alloc, dealloc, Layout},
    cmp::max,
    mem::{size_of, transmute, ManuallyDrop},
    ptr::{self, NonNull},
};

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
#[derive(Debug)]
pub struct NickelValue(usize);

impl NickelValue {
    /// The mask for the tag bits in a value pointer.
    const VALUE_TAG_MASK: usize = 0b11;

    /// Returns the tag bits of this value.
    pub fn tag(&self) -> ValueTag {
        (self.0 & Self::VALUE_TAG_MASK).try_into().unwrap()
    }

    /// Creates a new inline value.
    pub const fn inline(inline: InlineValue) -> Self {
        // Safety: inline values are "pre-tagged", so they already have their low bits set to the
        // Inline tag and are represented as `usize`
        unsafe { NickelValue(transmute::<InlineValue, usize>(inline)) }
    }

    /// Creates a new null value.
    pub const fn null() -> Self {
        Self::inline(InlineValue::Null)
    }

    /// Creates a new true value.
    pub const fn bool_true() -> Self {
        Self::inline(InlineValue::True)
    }

    /// Creates a new false value.
    pub const fn bool_false() -> Self {
        Self::inline(InlineValue::False)
    }

    /// Creates a new empty array.
    pub const fn empty_array() -> Self {
        Self::inline(InlineValue::EmptyArray)
    }

    /// Creates a new empty array.
    pub const fn empty_record() -> Self {
        Self::inline(InlineValue::EmptyRecord)
    }

    /// Allocates a new number value.
    pub fn number(value: Number) -> Self {
        ValueBlockRc::encode(NumberBody(value)).into_value()
    }

    /// Allocates a new string value.
    pub fn string(value: impl Into<NickelString>) -> Self {
        ValueBlockRc::encode(StringBody(value.into())).into_value()
    }

    /// Allocates a new array value. If the array is empty, it is automatically inlined as
    /// [InlineValue::EmptyArray].
    pub fn array(value: Array) -> Self {
        if value.is_empty() {
            Self::inline(InlineValue::EmptyArray)
        } else {
            ValueBlockRc::encode(ArrayBody(value)).into_value()
        }
    }

    /// Allocates a new record value. If the record is empty, it is automatically inlined as
    /// [InlineValue::EmptyRecord].
    pub fn record(value: RecordData) -> Self {
        if value.is_empty() {
            Self::inline(InlineValue::EmptyRecord)
        } else {
            ValueBlockRc::encode(RecordBody(value)).into_value()
        }
    }

    /// Allocates a new thunk value.
    pub fn thunk(value: CacheIndex) -> Self {
        ValueBlockRc::encode(ThunkBody(value)).into_value()
    }

    /// Allocates a new label value.
    pub fn label(value: Label) -> Self {
        ValueBlockRc::encode(LabelBody(value)).into_value()
    }

    /// Allocates a new enum variant value.
    pub fn enum_variant(tag: LocIdent, arg: Option<NickelValue>, attrs: EnumVariantAttrs) -> Self {
        ValueBlockRc::encode(EnumVariantBody { tag, arg, attrs }).into_value()
    }

    /// Returns the inner value of `self` if it's an inline value, or `None` otherwise.
    pub fn as_inline(&self) -> Option<InlineValue> {
        InlineValue::try_from(self).ok()
    }

    /// Converts the inner value of `self` to an inline value bypassing any safety checks.
    ///
    /// # Safety
    ///
    /// `self` must hold a valid inline value when seen as a `usize`.
    pub unsafe fn as_inline_unchecked(&self) -> InlineValue {
        // Safety: `InlineValue` is `#[repr(usize)]`, ensuring that it has the same layout as
        // `self.0`. The precondition of the function ensures that `self.0` is a valid value for
        // [InlineValue].
        transmute::<usize, InlineValue>(self.0)
    }

    /// Returns a reference to the inner number stored in this value if `self` is a value block
    /// with tag number, or `None` otherwise.
    pub fn as_number(&self) -> Option<&NumberBody> {
        self.as_value_body()
    }

    /// Returns a reference to the inner string stored in this value if `self` is a value block
    /// with tag number, or `None` otherwise.
    pub fn as_string(&self) -> Option<&StringBody> {
        self.as_value_body()
    }

    /// Returns a reference to the inner array stored in this value if `self` is a value block
    /// with tag number, or `None` otherwise.
    pub fn as_array(&self) -> Option<&ArrayBody> {
        self.as_value_body()
    }

    /// Returns a reference to the inner record stored in this value if `self` is a value block
    /// with tag number, or `None` otherwise.
    pub fn as_record(&self) -> Option<&RecordBody> {
        self.as_value_body()
    }

    /// Returns a reference to the inner thunk stored in this value if `self` is a value block with
    /// tag number, or `None` otherwise.
    pub fn as_thunk(&self) -> Option<&ThunkBody> {
        self.as_value_body()
    }

    /// Returns a reference to the inner label stored in this value if `self` is a value block with
    /// tag number, or `None` otherwise.
    pub fn as_label(&self) -> Option<&LabelBody> {
        self.as_value_body()
    }

    /// Returns a reference to the inner enum variant stored in this value if `self` is a value
    /// block with tag number, or `None` otherwise.
    pub fn as_enum_variant(&self) -> Option<&EnumVariantBody> {
        self.as_value_body()
    }

    /// Returns the value block pointed to by this Nickel value if it's a value block, or `Err`
    /// otherwise. Since this method must consume `self` in the case it's a value block, in order
    /// to keep the reference count correct, the caller can get `self` back in case of error.
    pub fn as_value_block(self) -> Result<ValueBlockRc, Self> {
        if self.tag() == ValueTag::Pointer {
            let this = ManuallyDrop::new(self);
            // Safety: if the `Pointer` tag is set, `self.0` must be a valid, non-null pointer
            // initialised from a `ValueBlock`.
            unsafe { Ok(ValueBlockRc(NonNull::new_unchecked(this.0 as *mut u8))) }
        } else {
            Err(self)
        }
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
            unsafe { ValueBlockRc::try_decode_from_raw(NonNull::new_unchecked(self.0 as *mut u8)) }
        } else {
            None
        }
    }

    /// Returns a typed reference to the content of the value, or the content of the inline value
    /// directly.
    pub fn content(&self) -> ValueContentRef<'_> {
        match self.tag() {
            // Safety: if `self.tag()` is `Pointer`, then the content of self must be a valid
            // non-null pointer to a value block.
            ValueTag::Pointer => unsafe {
                let as_ptr = NonNull::new_unchecked(self.0 as *mut u8);
                let tag = ValueBlockRc::header_from_raw(as_ptr).tag();

                // Safety: additionally, the lifetime of the return `ValueContentRef<'_>` is tied to
                // `&self`, so the former won't outlive the value block.
                match tag {
                    BodyTag::Number => {
                        ValueContentRef::Number(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    BodyTag::Array => {
                        ValueContentRef::Array(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    BodyTag::Record => {
                        ValueContentRef::Record(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    BodyTag::String => {
                        ValueContentRef::String(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
                    }
                    BodyTag::Thunk => {
                        ValueContentRef::Thunk(ValueBlockRc::decode_from_raw_unchecked(as_ptr))
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
            // Safety: `self.tag()` is `ValueTag::Inline`
            ValueTag::Inline => unsafe { ValueContentRef::Inline(self.as_inline_unchecked()) },
        }
    }

    /// Checks for physical equality of two Nickel values. This is a very fast check that is
    /// complete for inline values but partial otherwise (i.e. it only returns `true` for pointers
    /// if the values physically point to the same value block).
    pub fn phys_eq(&self, other: &Self) -> bool {
        self.0 == other.0
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
                // We need to prevent this value block to be dropped as this would decrement the
                // refcount, nullifying our increment.
                let block_ptr =
                    ManuallyDrop::new(ValueBlockRc::from_raw_unchecked(self.0 as *mut u8));
                block_ptr.incr_ref_count();
            }
        }

        NickelValue(self.0)
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
                std::mem::drop(ValueBlockRc::from_raw_unchecked(self.0 as *mut u8));
            }
        }
    }
}

impl From<InlineValue> for NickelValue {
    fn from(inline: InlineValue) -> Self {
        NickelValue::inline(inline)
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
    type Error = TagMismatchError;

    fn try_from(value: NickelValue) -> Result<Self, Self::Error> {
        if value.tag() == ValueTag::Pointer {
            Ok(unsafe {
                // We need to prevent value from being dropped, or this will decrease the refcount.
                let value = ManuallyDrop::new(value);
                // Safety: if `self.tag()` is `Pointer`, `self.0` must be a valid non-null pointer
                // to a value block
                ValueBlockRc::from_raw_unchecked(value.0 as *mut u8)
            })
        } else {
            Err(TagMismatchError)
        }
    }
}

impl From<ValueBlockRc> for NickelValue {
    fn from(block: ValueBlockRc) -> Self {
        block.into_value()
    }
}
/// Pointer tag used by [NickelValue] to discriminate between the pointer and non-pointer kind of Nickel values.
#[repr(usize)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
///////////
// CAUTION
///////////
// unsafe functions are relying on the precise values and range of `ValueTag`. If you add or remove
// tags, make sure to update all the corresponding code, in particular conversion functions from
// and to numeric types.
pub enum ValueTag {
    /// A heap-allocated value, meaning the tagged data is a valid pointer to [ValueBlockRc].
    Pointer = 0,
    /// The tag for an [InlineValue], which is not a pointer.
    Inline = 1,
}

impl From<ValueTag> for usize {
    fn from(tag: ValueTag) -> Self {
        // Safety: `#[repr(usize)]` on [ValueTag] guarantees that the enum is represented in memory
        // with the exact same layout as `usize`
        unsafe { transmute::<ValueTag, usize>(tag) }
    }
}

/// Out of bounds error when trying to convert a numeric type to a tag.
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct TagOutOfBoundsError;

impl TryFrom<usize> for ValueTag {
    type Error = TagOutOfBoundsError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        if value < 2 {
            // Safety: `#[repr(usize)]` on [ValueTag] guarantees that the enum is safe to transmute
            // to and from `usize`, as long as we are in the range of valid tags.
            Ok(unsafe { transmute::<usize, ValueTag>(value) })
        } else {
            Err(TagOutOfBoundsError)
        }
    }
}

/// Encode non-pointer data (currently an [InlineValue]) as a tagged pointer representation (a
/// [NickelValue]).
const fn encode_value(content: usize, tag: ValueTag) -> usize {
    match tag {
        ValueTag::Pointer => content,
        _ => (content << 2) | (tag as usize),
    }
}

/// Encode an inline value as a tagged pointer representation (a [NickelValue]).
/// `encode_inline(code)` is the same as `encode_value(code, ValueTag::Inline)`.
const fn encode_inline(code: usize) -> usize {
    encode_value(code, ValueTag::Inline)
}

/// Small Nickel values that can be inlined in the higher bits (excluding the tag) of the one-word
/// representation of a Nickel value. Their numeric value is directly encoded with the inline value
/// tag included, so that no bit shifting is needed at all for writing or reading them.
#[repr(usize)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InlineValue {
    Null = encode_inline(0),
    True = encode_inline(1),
    False = encode_inline(2),
    EmptyArray = encode_inline(3),
    EmptyRecord = encode_inline(4),
}

/// The discriminating tag for the different kinds of data that can be store in a value block.
///////////
// CAUTION
///////////
// unsafe functions are relying on the precise values and range of `ValueTag`. If you add or remove
// tags, make sure to update all the corresponding code, in particular conversion functions from
// and to numeric types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum BodyTag {
    Number = 0,
    Array = 1,
    Record = 2,
    String = 3,
    Thunk = 4,
    Label = 5,
    EnumVariant = 6,
    ForeignId = 7,
    SealingKey = 8,
    CustomContract = 9,
    Type = 10,
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
            BodyTag::Label => ValueBlockRc::padding::<LabelBody>(),
            BodyTag::EnumVariant => ValueBlockRc::padding::<EnumVariantBody>(),
            BodyTag::ForeignId => ValueBlockRc::padding::<ForeignIdBody>(),
            BodyTag::SealingKey => ValueBlockRc::padding::<SealingKeyBody>(),
            BodyTag::CustomContract => ValueBlockRc::padding::<CustomContractBody>(),
            BodyTag::Type => ValueBlockRc::padding::<TypeBody>(),
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
            BodyTag::Label => ValueBlockRc::block_layout::<LabelBody>(),
            BodyTag::EnumVariant => ValueBlockRc::block_layout::<EnumVariantBody>(),
            BodyTag::ForeignId => ValueBlockRc::block_layout::<ForeignIdBody>(),
            BodyTag::SealingKey => ValueBlockRc::block_layout::<SealingKeyBody>(),
            BodyTag::CustomContract => ValueBlockRc::block_layout::<CustomContractBody>(),
            BodyTag::Type => ValueBlockRc::block_layout::<TypeBody>(),
        }
    }
}

impl From<BodyTag> for u8 {
    fn from(tag: BodyTag) -> Self {
        tag as u8
    }
}

impl TryFrom<u8> for BodyTag {
    type Error = TagOutOfBoundsError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value <= 10 {
            // Safety: `#[repr(u8)]` on `BodyTag` guarantees that the enum is safe to transmute to
            // and from `u8`, as long as we are in the range of valid tags.
            Ok(unsafe { transmute::<u8, BodyTag>(value) })
        } else {
            Err(TagOutOfBoundsError)
        }
    }
}

/// A one-word header for a heap-allocated Nickel value. The layout is as follows (size is given
/// in bits):
///
/// ```text
/// +----------+-------------------------+
/// | Tag (8)  | (Strong) Ref Count (56) |
/// +----------+-------------------------+
/// ```
// We set a minimal alignment of `4` bytes, so that pointers to the content of a value block
// (which are aligned to the max of the alignement of `ValueBlockHeader` and the content) is
// guaranteed to have at least the last 4 bits free for tagging (although we currently only use two
// bits).
#[repr(Rust, align(4))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ValueBlockHeader(u64);

impl ValueBlockHeader {
    /// The mask for the reference count in the header.
    const REF_COUNT_MASK: u64 = 0x00_FF_FF_FF_FF_FF_FF_FF;

    /// Creates a new header for a value block with the given tag and a reference count of 1.
    pub fn new(tag: BodyTag) -> Self {
        // Encode the tag, and set the reference count to 1.
        Self(((tag as u64) << 56) | 1)
    }

    /// Returns the tag stored in this header.
    pub fn tag(&self) -> BodyTag {
        // The tag is stored in the 8 most significant bits of the header, hence needs to be
        // shifted of `64-8=56` bits to the right.
        ((self.0 >> 56) as u8).try_into().unwrap()
    }

    /// Returns the ref count of this value block.
    pub fn ref_count(&self) -> u64 {
        self.0 & Self::REF_COUNT_MASK
    }

    fn set_ref_count(&mut self, count: u64) {
        assert!(count <= Self::REF_COUNT_MASK, "reference count overflow");

        self.0 = (self.0 & !Self::REF_COUNT_MASK) | count;
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

#[derive(Clone, Debug)]
pub struct NumberBody(Number);

#[derive(Clone, Debug)]
pub struct StringBody(NickelString);

#[derive(Clone, Debug)]
pub struct ArrayBody(Array);

#[derive(Clone, Debug)]
pub struct RecordBody(RecordData);

#[derive(Clone, Debug)]
pub struct ThunkBody(CacheIndex);

#[derive(Clone, Debug)]
pub struct LabelBody(Label);

#[derive(Clone, Debug)]
pub struct EnumVariantBody {
    pub tag: LocIdent,
    pub arg: Option<NickelValue>,
    pub attrs: EnumVariantAttrs,
}

#[derive(Clone, Debug)]
pub struct ForeignIdBody(ForeignIdPayload);

#[derive(Clone, Debug)]
pub struct CustomContractBody(NickelValue);

#[derive(Clone, Debug)]
pub struct SealingKeyBody(SealingKey);

#[derive(Clone, Debug)]
pub struct TypeBody {
    /// The static type.
    typ: Type,
    /// The conversion of this type to a contract, that is, `typ.contract()?`. This field
    /// serves as a caching mechanism so we only run the contract generation code once per type
    /// written by the user.
    contract: NickelValue,
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

/// A pointer to a heap-allocated, reference-counted (included in the pointee) Nickel value of
/// variable size, although the size is a deterministic function of the tag stored in the header
/// (first word of the block).
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
/// isn't a multiple of the total alignement. Currently, for example, there is no padding on x86_64
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
    /// then dropped, this pointer becomes invalid and musn't be used anymore (and in particular be
    /// passed to this function).
    pub unsafe fn from_raw(ptr: *mut u8) -> Option<Self> {
        NonNull::new(ptr).map(ValueBlockRc)
    }

    /// Creates a new [Self] from a raw pointer without checking for null.
    ///
    /// # Safety
    ///
    /// Same conditions as for [ValueBlockRc::from_raw], plus `ptr` must not be null.
    pub unsafe fn from_raw_unchecked(ptr: *mut u8) -> Self {
        ValueBlockRc(NonNull::new_unchecked(ptr))
    }

    /// Converts a pointer to a value block to a raw pointer. See [Self::from_raw].
    pub fn into_raw(self) -> *mut u8 {
        // We must avoid dropping `Self` here, which would decrement the reference count.
        let this = ManuallyDrop::new(self);
        this.0.as_ptr()
    }

    /// Converts this value block to a [NickelValue] pointer. To keep the reference count
    /// consistent, this function consumes the value block without decrementing the reference
    /// count, which is left unchanged by the conversion.
    pub fn into_value(self) -> NickelValue {
        // We must avoid dropping `Self` here, which would decrement the reference count.
        let this = ManuallyDrop::new(self);
        NickelValue(this.0.as_ptr() as usize)
    }

    /// Returns the header of this value block.
    fn header(&self) -> ValueBlockHeader {
        unsafe { Self::header_from_raw(self.0) }
    }

    /// Returns the header of a value block pointed to by `ptr`.
    ///
    /// # Safety
    ///
    /// `ptr` must be a valid pointer into a value block.
    unsafe fn header_from_raw(ptr: NonNull<u8>) -> ValueBlockHeader {
        *ptr.cast::<ValueBlockHeader>().as_ref()
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
        // active mutable borrow to `self` and the reference is 1-counted, but this excluded here
        // (since we borrow `self` immutably).
        unsafe { (*self.header_mut()).incr_ref_count() }
    }

    /// Decrements the reference count of this value block.
    fn decr_ref_count(&self) {
        // Safety: we never hand over mutable references into a value block unless there is an
        // active mutable borrow to `self` and the reference is 1-counted, but this excluded here
        // (since we borrow `self` immutably).
        unsafe { (*self.header_mut()).decr_ref_count() }
    }

    /// Same as [std::rc::Rc::get_mut] but for a value block. Mutably borrows the value block.
    /// Returns an error if the tag doesn't match `T::Tag`.
    pub fn try_get_mut<T: ValueBlockBody>(&mut self) -> Result<Option<&mut T>, TagMismatchError> {
        if self.header().tag() != T::TAG {
            Err(TagMismatchError)
        } else if self.header().ref_count() != 1 {
            Ok(None)
        } else {
            // Safety: we know that the value block is unique, so we can safely decode the content
            // without any risk of aliasing.
            unsafe { Ok(Some(self.decode_mut_unchecked::<T>())) }
        }
    }

    /// Panicking variant of [Self::try_get_mut]. Equivalent to
    /// `self.try_get_mut().unwrap().unwrap()`.
    pub fn get_mut<T: ValueBlockBody>(&mut self) -> &mut T {
        self.try_get_mut().unwrap().unwrap()
    }

    /// Same as [std::rc::Rc::make_mut] but for a value block. Returns an error if the tag doesn't
    /// match `T::Tag`.
    pub fn try_make_mut<T: ValueBlockBody + Clone>(&mut self) -> Result<&mut T, TagMismatchError> {
        if self.header().tag() != T::TAG {
            Err(TagMismatchError)
        } else if self.header().ref_count() == 1 {
            // Safety: we know that the value block is unique, so we can safely decode the content
            // without any risk of aliasing.
            unsafe { Ok(self.decode_mut_unchecked::<T>()) }
        } else {
            let unique = ValueBlockRc::encode(self.decode::<T>().clone());
            *self = unique;
            unsafe { Ok(self.decode_mut_unchecked::<T>()) }
        }
    }

    /// Panicking variant of [Self::try_make_mut]. Equivalent to `self.try_make_mut().unwrap()`.
    pub fn make_mut<T: ValueBlockBody + Clone>(&mut self) -> &mut T {
        self.try_make_mut().unwrap()
    }

    /// Returns the required padding in bytes between the header and the body in [ValueBlockRc]
    /// depending on the tag. Calls to [ValueBlockRc::padding] under the hood instantiated with the
    /// right type.
    const fn padding<T: ValueBlockBody>() -> usize {
        let align = Self::block_align::<T>();
        let leftover = size_of::<ValueBlockHeader>() % align;

        (align - leftover) % align
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
        // above ensures that the nearest multiple of the alignment after size is less than
        // `isize::MAX`.
        unsafe { Layout::from_size_align_unchecked(size, Self::block_align::<T>()) }
    }

    /// Allocates a new value block with the given value `T` as content.
    fn encode<T: ValueBlockBody>(value: T) -> Self {
        unsafe {
            let start = alloc(Self::block_layout::<T>());

            if start.is_null() {
                panic!("out of memory: failed to allocate memory for Nickel value")
            }

            let header_ptr = start as *mut ValueBlockHeader;
            header_ptr.write(ValueBlockHeader::new(T::TAG));

            let body_ptr =
                start.add(size_of::<ValueBlockHeader>() + Self::padding::<T>()) as *mut T;
            body_ptr.write(value);

            Self(NonNull::new_unchecked(start))
        }
    }

    /// Tries to decode this value black as a reference to a value of type `T`. Returns `None` if
    /// the tag of this value block is not `T::TAG`.
    fn try_decode<T: ValueBlockBody>(&self) -> Option<&T> {
        (self.header().tag() == T::TAG).then(|| unsafe { self.decode_unchecked() })
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
        Self::decode_from_raw_unchecked(self.0)
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
        self.0
            .add(size_of::<ValueBlockHeader>() + Self::padding::<T>())
            .cast::<T>()
            .as_mut()
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
        ptr.add(size_of::<ValueBlockHeader>() + ValueBlockRc::padding::<T>())
            .cast::<T>()
            .as_ref()
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
        (Self::header_from_raw(ptr).tag() == T::TAG).then(|| Self::decode_from_raw_unchecked(ptr))
    }
}

impl Drop for ValueBlockRc {
    fn drop(&mut self) {
        if self.header().ref_count() == 1 {
            unsafe {
                let tag = self.header().tag();
                // Safety: the value block is guaranteed to have been allocated with a size of
                // `size_of::<ValueBlockHeader>()` + `tag.padding()` + `size_of::<T>()`.
                let body_ptr = self
                    .0
                    .as_ptr()
                    .add(size_of::<ValueBlockHeader>() + tag.padding());

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

/// An enum representation of a decoded Nickel value. This is useful to match on a general value in
/// a typed way.
///
/// Note that we only ever store references to the content of a value block here, or full inline
/// values, so this enum is compact and doesn't defeat the purpose of the whole value encoding
/// (beside the fact that [ValueContentRef] is only materialized temporarily by helpers and not a
/// standard value representation).
#[derive(Clone, Copy, Debug)]
pub enum ValueContentRef<'a> {
    Code,
    Inline(InlineValue),
    Number(&'a NumberBody),
    Array(&'a ArrayBody),
    Record(&'a RecordBody),
    String(&'a StringBody),
    Thunk(&'a ThunkBody),
    Label(&'a LabelBody),
    EnumVariant(&'a EnumVariantBody),
    ForeignId(&'a ForeignIdBody),
    SealingKey(&'a SealingKeyBody),
    CustomContract(&'a CustomContractBody),
    Type(&'a TypeBody),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inline_values() {
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

        assert!(inline_null.phys_eq(&NickelValue::inline(InlineValue::Null)));
        assert!(inline_true.phys_eq(&NickelValue::inline(InlineValue::True)));
        assert!(inline_false.phys_eq(&NickelValue::inline(InlineValue::False)));
        assert!(inline_empty_array.phys_eq(&NickelValue::inline(InlineValue::EmptyArray)));
        assert!(inline_empty_record.phys_eq(&NickelValue::inline(InlineValue::EmptyRecord)));

        assert!(!inline_null.phys_eq(&NickelValue::inline(InlineValue::True)));
    }

    #[test]
    fn basic_value_blocks() {
        let number_value = NickelValue::number(Number::from(42));
        let string_value = NickelValue::string("Hello, World!");

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
        let number_value = NickelValue::number(Number::from(42));

        let mut copies =
            std::array::from_fn::<_, 20, _>(|_| ManuallyDrop::new(number_value.clone()));

        let mut as_val = number_value.as_value_block().unwrap();
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
        array_data.push(NickelValue::inline(InlineValue::Null));

        let record = NickelValue::record(record_data);
        let array = NickelValue::array(array_data);

        let mut record_value = record.as_value_block().unwrap();
        let mut array_value = array.as_value_block().unwrap();

        assert_eq!(record_value.decode::<RecordBody>().0.fields.len(), 1);
        assert_eq!(array_value.decode::<ArrayBody>().0.len(), 1);

        record_value
            .get_mut::<RecordBody>()
            .0
            .fields
            .insert(LocIdent::from("world"), Default::default());
        array_value
            .get_mut::<ArrayBody>()
            .0
            .push(NickelValue::inline(InlineValue::Null));

        let array_copy = array_value.clone();
        let record_copy = record_value.clone();

        assert_eq!(record_copy.decode::<RecordBody>().0.fields.len(), 2);
        assert_eq!(array_copy.decode::<ArrayBody>().0.len(), 2);
    }

    #[test]
    fn empty_containers_are_inlined() {
        let empty_record = NickelValue::record(RecordData::default());
        let empty_array = NickelValue::array(Array::default());

        assert_eq!(empty_record.tag(), ValueTag::Inline);
        assert_eq!(empty_array.tag(), ValueTag::Inline);

        assert_eq!(empty_record.as_inline(), Some(InlineValue::EmptyRecord));
        assert_eq!(empty_array.as_inline(), Some(InlineValue::EmptyArray));
    }
}
