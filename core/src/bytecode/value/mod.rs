//! Runtime representation of Nickel values.

use crate::{
    eval::cache::CacheIndex,
    identifier::LocIdent,
    label::Label,
    term::{
        record::RecordData, string::NickelString, CustomContract, EnumVariantAttrs,
        ForeignIdPayload, SealingKey,
    },
    typ::Type,
};
use nickel_lang_vector::Slice;
use std::alloc::{alloc, Layout};
use std::ptr::{self, NonNull};

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
                let block_ptr = ValueBlockRc::from_raw_unchecked(self.0 as *mut u8);
                // We clone the `ValueBlockRc` to increment the strong reference count.
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
                .then_some(ValueBlockRc::from_raw_unchecked(value.0 as *mut u8))
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
        // Safety: `#[repr(usize)]` on [ValueTag] guarantees that the enum is represented in memory
        // with the exact same layout as `usize`
        unsafe { std::mem::transmute::<ValueTag, usize>(tag) }
    }
}

impl TryFrom<usize> for ValueTag {
    type Error = ();

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        // TODO: is there a faster way to do this? I'm not sure how the compiler optimizes this.
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
        //TODO: is there a faster way to do this? I'm not sure how the compiler optimizes this.
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// A one-word header for a heap-allocated Nickel value. The layout is as follows (values are given
/// in bits):
///
/// ```text
/// +-----------------+-----------------+----------------------+
/// | Closurized (1)  | Tag (7)         | Strong Ref Count (56) |
/// +-----------------+-----------------+----------------------+
/// ```
#[repr(packed(8))]
struct ContentHeader(u64);

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

    pub fn is_closurized(&self) -> bool {
        // The closurized bit is the most significant bit of the header.
        (self.0 & Self::CLOSURIZED_MASK) != 0
    }

    pub fn closurized(mut self) -> Self {
        // The closurized bit is the most significant bit of the header.
        self.0 |= Self::CLOSURIZED_MASK;
        self
    }

    pub fn strong_ref_count(&self) -> u64 {
        self.0 & Self::STRONG_REF_COUNT_MASK
    }

    fn set_strong_ref_count(&mut self, count: u64) {
        assert!(
            count <= Self::STRONG_REF_COUNT_MASK,
            "Strong reference count must fit in 56 bits"
        );
        self.0 = (self.0 & !Self::STRONG_REF_COUNT_MASK) | count;
    }

    fn increment_strong_ref_count(&mut self) {
        self.set_strong_ref_count(self.strong_ref_count() + 1);
    }

    fn decrement_strong_ref_count(&mut self) {
        self.set_strong_ref_count(self.strong_ref_count() - 1);
    }
}

/// Marker trait for representable values.
///
/// # Alignment of value content
///
/// It is of the utmost importance that all types implementing this trait are aligned on at most 8
/// bytes.
///
/// The reason is that when we allocate a value block, we need to put the header first (which is
/// also aligned to 8 bytes), and then the body of the value. We would need to precompute an
/// alignement for the initial address and a `n` that are optimal such that `alloced_addr` is
/// header-aligned and `alloced_addr+n` is body-aligned where `alloc_addr+1..alloc_addr+(n-1)`
/// would be padding. We would also need to recompute the potential padding each time, based on the
/// tag in the header, to skip padding.
///
/// To get rid of padding cheks and computations on every value dereference, we simply require that
/// both the header and the body are aligned to 8 bytes. The header is 8 bytes long and is
/// naturally aligned on 8-bytes boundaries (alhtough that might not be true of all platforms). On
/// 64bits, most non-trivial structs are also aligned to 8 bytes, and usually less so on 32bits
/// platforms, so we shouldn't actually override the default alignment of the types involved in
/// most cases. **However, a mis-alignment might lead to undefined behavior, so we make extra sure
/// that both the header and the types implementing this trait are aligned to exactly 8 bytes.**
pub trait ValueContent {
    const TAG: ContentTag;
}

#[repr(packed(8))]
pub struct ArrayContent(Slice<NickelValue, 32>);
#[repr(packed(8))]
pub struct RecordContent(RecordData);
#[repr(packed(8))]
pub struct StringContent(NickelString);
#[repr(packed(8))]
pub struct ThunkContent(CacheIndex);
#[repr(packed(8))]
pub struct LabelContent(Label);
#[repr(packed(8))]
pub struct EnumVariantContent {
    pub tag: LocIdent,
    pub arg: NickelValue,
    pub attrs: EnumVariantAttrs,
}
#[repr(packed(8))]
pub struct ForeignIdContent(ForeignIdPayload);
#[repr(packed(8))]
pub struct CustomContractContent(CustomContract);
#[repr(packed(8))]
pub struct SealingKeyContent(SealingKey);
#[repr(packed(8))]
pub struct TypeContent(Type);

pub trait TaggedContent {
    fn tag(&self) -> ContentTag;
}

impl ValueContent for ArrayContent {
    const TAG: ContentTag = ContentTag::Array;
}

impl ValueContent for RecordContent {
    const TAG: ContentTag = ContentTag::Record;
}

impl ValueContent for StringContent {
    const TAG: ContentTag = ContentTag::String;
}

impl ValueContent for ThunkContent {
    const TAG: ContentTag = ContentTag::Thunk;
}

impl ValueContent for LabelContent {
    const TAG: ContentTag = ContentTag::Label;
}

impl ValueContent for EnumVariantContent {
    const TAG: ContentTag = ContentTag::EnumVariant;
}

impl ValueContent for ForeignIdContent {
    const TAG: ContentTag = ContentTag::ForeignId;
}

impl ValueContent for CustomContractContent {
    const TAG: ContentTag = ContentTag::CustomContract;
}

pub trait Decode<T: ValueContent>: TaggedContent
where
    Self: Sized,
{
    //TODO: decoding can only work for unique (1-counted) values. Even then, maybe `make_mut` or
    //`try_into` would probably be a better API.
    //
    // /// Decode a Nickel value from a tagged pointer.
    // fn try_decode(self) -> Option<T> {
    //     (self.tag() == T::TAG).then(|| unsafe { self.decode_unchecked() })
    // }
    //
    // /// Decode a Nickel value from a tagged pointer, panicking if the value is not of the expected
    // /// type.
    // fn decode(self) -> T {
    //     self.try_decode().unwrap()
    // }
    //
    // /// Decode a Nickel value from a tagged pointer without performing any safety checks.
    // ///
    // /// # Safety
    // ///
    // /// The caller must ensure that the value is of the expected type, that is that the tag of
    // /// `self.tag()` matches `T::tag`, or undefined behavior will follow.
    // unsafe fn decode_unchecked(self) -> T;

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

    //TODO: same as for decode.
    // /// Variant of [Self::try_decode] operating on a mutable borrowed value.
    // fn try_decode_mut(&mut self) -> Option<&mut T> {
    //     (self.tag() == T::TAG).then(|| unsafe { self.decode_mut_unchecked() })
    // }
    //
    // /// Variant of [Self::decode] operating on a mutable borrowed value.
    // fn decode_mut(&mut self) -> &mut T {
    //     self.try_decode_mut().unwrap()
    // }
    //
    // /// Variant of [Self::decode_unchecked] operating on a mutable borrowed value.
    // unsafe fn decode_mut_unchecked(&mut self) -> &mut T;
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
    /// Creates a new `ValueBlockRc` from a raw pointer.
    ///
    /// # Safety
    ///
    /// `ptr` must have been obtained from a previous call to `ValueBlockRc::into_raw`, and the
    /// value block must not have been deallocated since then. This is typically the case if the
    /// pointer has been obtained from `ValueBlockRc::into_raw` and hasn't been converted back to
    /// `ValueBlockRc`. However, if the same pointer is converted back to a `ValueBlockRc` which is
    /// then dropped, this pointer becomes invalid and musn't be used anymore (and in particular be
    /// passed to this function).
    pub unsafe fn from_raw(ptr: *mut u8) -> Option<Self> {
        NonNull::new(ptr).map(ValueBlockRc)
    }

    /// Create a new `ValueBlockRc` from a raw pointer without checking for null.
    pub unsafe fn from_raw_unchecked(ptr: *mut u8) -> Self {
        ValueBlockRc(NonNull::new_unchecked(ptr))
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

    fn header(&self) -> ContentHeader {
        unsafe { *self.0.cast::<ContentHeader>().as_ref() }
    }

    //TODO: should we implement incr/decr directly here, to avoid leaking a dangerous mutable
    //reference to the header that could in theory be aliased by the caller?
    fn header_mut(&self) -> &mut ContentHeader {
        unsafe { self.0.cast::<ContentHeader>().as_mut() }
    }
}

impl Drop for ValueBlockRc {
    fn drop(&mut self) {
        if self.header().strong_ref_count() == 1 {
            unsafe {
                let tag = self.header().tag();
                let body_ptr = self.0.as_ptr().add(std::mem::size_of::<ContentHeader>());

                match tag {
                    ContentTag::Array => {
                        ptr::drop_in_place(body_ptr as *mut ArrayContent);
                    }
                    ContentTag::Record => {
                        ptr::drop_in_place(body_ptr as *mut RecordContent);
                    }
                    ContentTag::String => {
                        ptr::drop_in_place(body_ptr as *mut StringContent);
                    }
                    ContentTag::Thunk => {
                        ptr::drop_in_place(body_ptr as *mut ThunkContent);
                    }
                    ContentTag::Label => {
                        ptr::drop_in_place(body_ptr as *mut LabelContent);
                    }
                    ContentTag::EnumVariant => {
                        ptr::drop_in_place(body_ptr as *mut EnumVariantContent);
                    }
                    ContentTag::ForeignId => {
                        ptr::drop_in_place(body_ptr as *mut ForeignIdContent);
                    }
                    ContentTag::CustomContract => {
                        ptr::drop_in_place(body_ptr as *mut CustomContractContent);
                    }
                    ContentTag::SealingKey => {
                        ptr::drop_in_place(body_ptr as *mut SealingKeyContent);
                    }
                    ContentTag::Type => {
                        ptr::drop_in_place(body_ptr as *mut TypeContent);
                    }
                }
            }
        } else {
            self.header_mut().decrement_strong_ref_count();
        }
    }
}

impl Clone for ValueBlockRc {
    fn clone(&self) -> Self {
        self.header_mut().increment_strong_ref_count();
        Self(self.0)
    }
}

/// The content of a heap-allocated Nickel value. The pointee of a `NickelValue` when the latter
/// isn't an inline value.
impl TaggedContent for ValueBlockRc {
    fn tag(&self) -> ContentTag {
        self.header().tag()
    }
}

impl<T: ValueContent> Decode<T> for ValueBlockRc {
    unsafe fn decode_ref_unchecked(&self) -> &T {
        self.0
            .add(std::mem::size_of::<ContentHeader>())
            .cast::<T>()
            .as_ref()
    }
}

impl<T: ValueContent> Encode<T> for ValueBlockRc {
    fn encode(value: T) -> Self {
        unsafe {
            let header_layout = Layout::new::<ContentHeader>();
            let body_layout = Layout::new::<T>();

            assert!(
                header_layout.align() == 8 && body_layout.align() == 8 && header_layout.size() == 8,
            );

            let final_layout =
                Layout::from_size_align(header_layout.size() + body_layout.size(), 8).unwrap();

            let start = alloc(final_layout);
            assert!(
                !start.is_null(),
                "Out of memory: failed to allocate memory for Nickel value"
            );

            let header_ptr = start as *mut ContentHeader;
            header_ptr.write(ContentHeader::new(T::TAG));

            let body_ptr = start.add(header_layout.size()) as *mut T;
            body_ptr.write(value);

            Self(NonNull::new_unchecked(start))
        }
    }
}
