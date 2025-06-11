//! Runtime representation of Nickel values.

use crate::{
    eval::cache::CacheIndex,
    term::{record::RecordData, string::NickelString, ForeignIdPayload, EnumVariantAttrs},
    identifier::LocIdent,
};
use nickel_lang_vector::Slice;
use std::{num::NonZero, rc::Rc, ptr::NonNull};

/// A tagged pointer to a Nickel value. If the least significant bit is set, the value is an inline
/// value. If the second least significant bit is set, the value is a thunk, that is a pointer to
/// code. Finally, if the two least significant bits are clear, the value is a pointer to a
/// heap-allocated value.
pub struct NickelValue(usize);

// Since a `NickelValue` can be an reference counted pointer in disguise, we can't just copy it
// blindly. We need to go through `Rc::clone` to make sure the count is up to date.
impl Clone for NickelValue {
    fn clone(&self) -> Self {
        if self.tag() == ValueTag::Pointer {
            unsafe {
                //TODO: the following commented line doesn't work, as an `Rc` of a dynamically
                // sized type is a fat pointer and thus can't be reconstructed from a raw pointer.
                // This hints at the fact that we will probably need to abandon the `u8`
                // representation and just refer to untyped *u8 memory that is manually allocated
                // through alloc.
                // let rc = Rc::from_raw(self.0 as *const ContentData);
                let rc: ManagedContent = todo!();
                NickelValue(ManagedContent(Rc::into_raw(rc.clone()) as usize))
            }
        } else {
            NickelValue(self.0)
        }
    }
}

impl NickelValue {
    pub const fn tag(&self) -> ValueTag {
        self.0 & VALUE_TAG_MASK
    }
}

impl TryFrom<NickelValue> for InlineValue {
    type Error = ();

    fn try_from(value: NickelValue) -> Result<Self, Self::Error> {
        (value.tag() == ValueTag::Inline).then_some(value).ok_or(())
    }
}

impl TryFrom<NickelValue> for ManagedContent {
    type Error = ();

    fn try_from(value: NickelValue) -> Result<Self, Self::Error> {
        unsafe {
            (value.tag() == ValueTag::Pointer)
                //.then_some(Rc::from_raw(value.0 as *const ContentData))
                .then_some(Rc::from_raw(todo!()))
                .ok_or(())
        }
    }
}

/// We use the lower two bits of a pointer (or an inline value) as a tag. This module defines the
/// masks corresponding to each tag.
#[repr(usize)]
pub enum ValueTag {
    /// The tag for a general heap-allocated value. The underlying value is to be interpreted as a
    /// pointer.
    Pointer = 0,
    /// The tag for an [super::InlineValue].
    Inline = 1,
    /// The tag for a thunk, which is a pointer to code. Currently, this is to be interpreted as a
    /// pointer to the [old ast](crate::term::RichTerm), but will eventually be a code address.
    Code = 2,
}

const VALUE_TAG_MASK: usize = 0b11;

const fn encode_value(content: usize, tag: ValueTag) -> usize {
    match tag {
        ValueTag::Pointer => content,
        _ => (content << 2) | (tag as usize),
    }
}

const fn decode_value(value: usize) -> usize {
    match value & (VALUE_TAG_MASK as usize) {
        ValueTag::Pointer => value,
        _ => value >> 2,
    }
}

const fn encode_inline(code: usize) -> usize {
    encode_value(code, ValueTag::Inline)
}

/// Small values that can be inlined in the one-word representation of a Nickel value. Their numeric
/// value is directly encoded (tagged), so that no bit shifting is needed
/// at all for encoding and decoding them.
#[repr(usize)]
pub enum InlineValue {
    Null = encode_inline(0),
    True = encode_inline(1),
    False = encode_inline(2),
    EmptyArray = encode_inline(3),
    EmptyRecord = encode_inline(4),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ContentTag {
    Array,
    Record,
    String,
    Thunk,
    Label,
    EnumVariant,
    ForeignId,
    SealingKey,
    CustomContract,
    Type,
}

#[repr(align(8))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// A one-word header for a heap-allocated Nickel value. The layout is as follows (values are given
/// in bits):
///
/// ```text
/// +-----------------+-----------------+----------------------+
/// | Closurized (1)  | Tag (7)         | Strong Ref Count (56) |
/// +-----------------+-----------------+----------------------+
/// ```
pub struct ContentHeader(u64);

impl ContentHeader {
    pub fn tag(&self) -> ContentTag {
        // The tag is stored in the 7 least significant bits of the header.
        (((self.0 & 0x7F_FF_FF_FF_FF_FF_FF_FF) as u8) as ContentTag
    }

    pub fn closurized(&self) -> bool {
        // The closurized bit is the most significant bit of the header.
        (self.0.get & (1 << 63)) != 0
    }

    pub fn strong_ref_count(&self) -> u64 {
        self.0 & 0x00_FF_FF_FF_FF_FF_FF_FF
    }
}

//TODO:
impl ContentHeader {
    pub fn closurized(mut self) -> Self {
        self.closurized = true;
        self
    }

    pub fn with_tag(tag: ContentTag) -> Self {
        Self {
            tag,
            closurized: false,
            padding: [0; 6],
        }
    }
}

/// Marker trait for representable values.
pub trait ValueContent {
    const TAG: ContentTag;
}

pub type Array = Slice<NickelValue, 32>;

pub type Record = RecordData;

pub type String = NickelString;

pub type Thunk = CacheIndex;

pub type Label = crate::label::Label;

pub struct EnumVariant {
    pub tag: LocIdent,
    pub arg: NickelValue,
    pub attrs: EnumVariantAttrs,
}

pub type ForeignId = ForeignIdPayload;

pub type CustomContract = crate::term::CustomContract;

pub trait TaggedContent {
    fn tag(&self) -> ContentTag;
}

impl ValueContent for Array {
    const TAG: ContentTag = ContentTag::Array;
}

impl ValueContent for Record {
    const TAG: ContentTag = ContentTag::Record;
}

impl ValueContent for String {
    const TAG: ContentTag = ContentTag::String;
}

impl ValueContent for Thunk {
    const TAG: ContentTag = ContentTag::Thunk;
}

impl ValueContent for Label {
    const TAG: ContentTag = ContentTag::Label;
}

impl ValueContent for EnumVariant {
    const TAG: ContentTag = ContentTag::EnumVariant;
}

impl ValueContent for ForeignId {
    const TAG: ContentTag = ContentTag::ForeignId;
}

impl ValueContent for CustomContract {
    const TAG: ContentTag = ContentTag::CustomContract;
}

pub trait Decode<T: ValueContent>: TaggedContent {
    /// Decode a Nickel value from a tagged pointer.
    fn try_decode(self) -> Option<T> {
        (self.tag() == T::TAG).then(|| unsafe { self.decode_unchecked() })
    }

    /// Decode a Nickel value from a tagged pointer, panicking if the value is not of the expected
    /// type.
    fn decode(self) -> T {
        self.try_decode().unwrap()
    }
    /// Decode a Nickel value from a tagged pointer without performing any safety checks.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the value is of the expected type, that is that the tag of
    /// `self.tag()` matches `T::tag`, or undefined behavior will follow.
    unsafe fn decode_unchecked(self) -> T;

    /// Variant of [Self::try_decode] operating on a borrowed value.
    fn try_decode_ref(&self) -> Option<&T> {
        (self.tag() == T::TAG).then(|| unsafe { self.decode_ref_unchecked() })
    }
    /// Variant of [Self::decode] operating on a borrowed value.

    fn decode_ref(&self) -> &T {
        self.try_decode_ref().unwrap()
    }

    /// Variant of [Self::decode_unchecked] operating on a borrowed value.
    unsafe fn decode_ref_unchecked(&self) -> &T;

    /// Variant of [Self::try_decode] operating on a mutable borrowed value.
    fn try_decode_mut(&mut self) -> Option<&mut T> {
        (self.tag() == T::TAG).then(|| unsafe { self.decode_mut_unchecked() })
    }

    /// Variant of [Self::decode] operating on a mutable borrowed value.
    fn decode_mut(&mut self) -> &mut T {
        self.try_decode_mut().unwrap()
    }

    /// Variant of [Self::decode_unchecked] operating on a mutable borrowed value.
    unsafe fn decode_mut_unchecked(&mut self) -> &mut T;
}

pub trait Encode<T: ValueContent>: TaggedContent {
    /// Creates a [NickelValueData] from a [NickelValueContent].
    fn encode(value: T) -> Self;
}


/// A pointer to a heap-allocated, reference-counter (included in the pointee) Nickel value of
/// variable size (although the size is a deterministic function of the tag) with a custom drop
/// implementation that correctly calls the destructor of the corresponding value.
///
/// To maintain uniquness, [Self] can't be cloned and is always accessed through an `Rc`.
pub struct ValueBlockPtr(NonNull<u8>);

impl Drop for ValueBlockPtr {
    fn drop(&mut self) {
        todo!() 
    }
}

pub struct ManagedContent(Rc<ValueBlockPtr>);

/// The content of a heap-allocated Nickel value. The pointee of a `NickelValue` when the latter
/// isn't an inline value.
//TODO: we could be even more aggressive and represent this as untyped, unsized `u8*` where we just
//know that the first byte must be the tag, and then the length is a function of the tag. For now,
//we'll use `[u8]`, even if it means we waste one word for the length of the slice (which we don't
//need).
pub struct ContentData {
    header: ContentHeader,
    body: Bytes,
}

impl ContentData {
    /// Create a new `NickelValueData` with the given tag and body. Wrapper around
    /// [Encode::encode].
    pub fn new<T: ValueContent>(body: T) -> Self {
        Self::encode(body)
    }
}

impl TaggedContent for ContentData {
    fn tag(&self) -> ContentTag {
        self.header.tag
    }
}

impl<T: ValueContent> Decode<T> for ContentData {
    unsafe fn decode_unchecked(self) -> T {
        std::mem::transmute::<[u8], T>(self.body)
    }

    unsafe fn decode_ref_unchecked(&self) -> &T {
        std::mem::transmute::<&[u8], &T>(&self.body)
    }

    unsafe fn decode_mut_unchecked(&mut self) -> &mut T {
        std::mem::transmute::<&mut [u8], &mut T>(&mut self.body)
    }
}

impl<T: ValueContent> Encode<T> for ContentData {
    fn encode(value: T) -> Self {
        Self {
            header: ContentHeader::with_tag(T::TAG),
            body: unsafe { std::mem::transmute::<T, [u8]>(value) },
        }
    }
}
