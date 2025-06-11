//! Runtime representation of Nickel values.

use crate::{
    eval::cache::CacheIndex,
    identifier::LocIdent,
    term::{record::RecordData, string::NickelString, EnumVariantAttrs, ForeignIdPayload},
};
use nickel_lang_vector::Slice;
use std::ptr::NonNull;

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
                let block_ptr = ValueBlockRc::from_raw(self.0 as *mut u8);
                NickelValue(block_ptr.clone().into_raw() as usize)
            }
        } else {
            NickelValue(self.0)
        }
    }
}

impl NickelValue {
    const VALUE_TAG_MASK: usize = 0b11;

    pub fn tag(&self) -> ValueTag {
        (self.0 & Self::VALUE_TAG_MASK).try_into().unwrap()
    }
}

impl TryFrom<NickelValue> for InlineValue {
    type Error = ();

    fn try_from(value: NickelValue) -> Result<Self, Self::Error> {
        (value.tag() == ValueTag::Inline)
            .then_some(unsafe { std::mem::transmute::<usize, InlineValue>(value.0) })
            .ok_or(())
    }
}

impl TryFrom<NickelValue> for ValueBlockRc {
    type Error = ();

    fn try_from(value: NickelValue) -> Result<Self, Self::Error> {
        unsafe {
            (value.tag() == ValueTag::Pointer)
                .then_some(ValueBlockRc::from_raw(value.0 as *mut u8))
                .ok_or(())
        }
    }
}

/// We use the lower two bits of a pointer (or an inline value) as a tag. This module defines the
/// masks corresponding to each tag.
#[repr(usize)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl From<ValueTag> for usize {
    fn from(tag: ValueTag) -> Self {
        unsafe { std::mem::transmute::<ValueTag, usize>(tag) }
    }
}

impl TryFrom<usize> for ValueTag {
    type Error = ();

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(ValueTag::Pointer),
            1 => Ok(ValueTag::Inline),
            2 => Ok(ValueTag::Code),
            _ => Err(()),
        }
    }
}

const fn encode_value(content: usize, tag: ValueTag) -> usize {
    match tag {
        ValueTag::Pointer => content,
        _ => (content << 2) | (tag as usize),
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

impl From<ContentTag> for u8 {
    fn from(tag: ContentTag) -> Self {
        tag as u8
    }
}

impl TryFrom<u8> for ContentTag {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(ContentTag::Array),
            1 => Ok(ContentTag::Record),
            2 => Ok(ContentTag::String),
            3 => Ok(ContentTag::Thunk),
            4 => Ok(ContentTag::Label),
            5 => Ok(ContentTag::EnumVariant),
            6 => Ok(ContentTag::ForeignId),
            7 => Ok(ContentTag::SealingKey),
            8 => Ok(ContentTag::CustomContract),
            9 => Ok(ContentTag::Type),
            _ => Err(()),
        }
    }
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
    /// The mask for the strong reference count in the header.
    const STRONG_REF_COUNT_MASK: u64 = 0x00_FF_FF_FF_FF_FF_FF_FF;
    /// The mask for the closurized bit in the header.
    const CLOSURIZED_MASK: u64 = 1 << 63;
    /// The mask for the tag for a value stored on one byte (with the closurized bit). Note that
    /// this is NOT the mask for the tag in the full header, which would need to be shifter to the
    /// right by 56 bits.
    const TAG_BYTE_MASK: u64 = 0b01111111;

    pub fn new(tag: ContentTag) -> Self {
        debug_assert!(
            tag as u64 <= Self::TAG_BYTE_MASK,
            "ContentTag value must fit in 7 bits"
        );

        Self((tag as u64) << 56)
    }

    pub fn tag(&self) -> ContentTag {
        // The tag is stored in the 7 significant bits of the header.
        (((self.0 >> 56) & Self::TAG_BYTE_MASK) as u8)
            .try_into()
            .unwrap()
    }

    pub fn closurized(&self) -> bool {
        // The closurized bit is the most significant bit of the header.
        (self.0 & Self::CLOSURIZED_MASK) != 0
    }

    pub fn strong_ref_count(&self) -> u64 {
        self.0 & Self::STRONG_REF_COUNT_MASK
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

pub trait Decode<T: ValueContent>: TaggedContent
where
    Self: Sized,
{
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

/// A pointer to a heap-allocated, reference-counted (included in the pointee) Nickel value of
/// variable size, although the size is a deterministic function of the tag stored in the first
/// word of the value. Think of it as a `Rc<ContentData>`, where the `ContentData` is a DST.
pub struct ValueBlockRc(NonNull<u8>);

impl ValueBlockRc {
    /// Create a new `ValueBlockRc` from a raw pointer.
    pub unsafe fn from_raw(ptr: *mut u8) -> Self {
        todo!()
    }

    pub fn into_raw(self) -> *mut u8 {
        self.0.as_ptr()
    }

    /// Get the raw pointer to the content.
    pub fn as_ptr(&self) -> *mut u8 {
        self.0.as_ptr()
    }

    pub fn as_value(&self) -> NickelValue {
        NickelValue(self.0.as_ptr() as usize)
    }

    /// Get the tag of the content.
    pub fn tag(&self) -> ContentTag {
        unsafe { (*(self.as_ptr() as *const ContentHeader)).tag() }
    }
}

impl Drop for ValueBlockRc {
    fn drop(&mut self) {
        todo!()
    }
}

impl Clone for ValueBlockRc {
    fn clone(&self) -> Self {
        todo!()
    }
}

/// The content of a heap-allocated Nickel value. The pointee of a `NickelValue` when the latter
/// isn't an inline value.
//TODO: we could be even more aggressive and represent this as untyped, unsized `u8*` where we just
//know that the first byte must be the tag, and then the length is a function of the tag. For now,
//we'll use `[u8]`, even if it means we waste one word for the length of the slice (which we don't
//need).
// pub struct ContentData {
//     header: ContentHeader,
//     body: Bytes,
// }

// impl ContentData {
//     /// Create a new `NickelValueData` with the given tag and body. Wrapper around
//     /// [Encode::encode].
//     pub fn new<T: ValueContent>(body: T) -> Self {
//         Self::encode(body)
//     }
// }

impl TaggedContent for ValueBlockRc {
    fn tag(&self) -> ContentTag {
        todo!()
    }
}

impl<T: ValueContent> Decode<T> for ValueBlockRc {
    unsafe fn decode_unchecked(self) -> T {
        todo!()
    }

    unsafe fn decode_ref_unchecked(&self) -> &T {
        todo!()
    }

    unsafe fn decode_mut_unchecked(&mut self) -> &mut T {
        todo!()
    }
}

impl<T: ValueContent> Encode<T> for ValueBlockRc {
    fn encode(value: T) -> Self {
        todo!()
        // Self {
        //     header: ContentHeader::with_tag(T::TAG),
        //     body: unsafe { std::mem::transmute::<T, [u8]>(value) },
        // }
    }
}
