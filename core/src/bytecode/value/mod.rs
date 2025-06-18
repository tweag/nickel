//! Runtime representation of Nickel values.
//!
//! This modules implement a custom memory layout for a memory-efficient representation of Nickel
//! values. See (RFC007)[https://github.com/tweag/nickel/blob/master/rfcs/007-bytecode-interpreter.md] for
//! more details.
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
use std::alloc::{alloc, dealloc, Layout};
use std::mem::{size_of, transmute, ManuallyDrop};
use std::ptr::{self, NonNull};

/// A Nickel array.
pub type Array = Slice<NickelValue, 32>;

/// A mismatch between an expected tag and the found tag in a value decoding process.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TagMismatchError<T> {
    expected: T,
    found: T,
}

/// The unified representation of Nickel values.
///
/// A tagged pointer to a [reference-counted Nickel value block](ValueBlockRc), or an inline
/// numeric value. The two least significant bits of the pointer are used as the tag. See
/// [ValueTag] for more details.
pub struct NickelValue(usize);

impl NickelValue {
    /// The mask for the tag bits in a value pointer.
    const VALUE_TAG_MASK: usize = 0b11;

    /// Returns the tag bits of this value.
    pub fn tag(&self) -> ValueTag {
        (self.0 & Self::VALUE_TAG_MASK).try_into().unwrap()
    }

    /// Creates a new inline value.
    pub fn inline(inline: InlineValue) -> Self {
        // Safety: inline values are "pre-tagged", so they already have the tag INLINE set, and are
        // represented as `usize`
        unsafe { NickelValue(transmute::<InlineValue, usize>(inline)) }
    }

    /// Allocates a new number value.
    pub fn number(value: Number) -> Self {
        ValueBlockRc::encode(NumberBody(value)).to_value()
    }

    /// Allocates a new string value.
    pub fn string(value: impl Into<NickelString>) -> Self {
        ValueBlockRc::encode(StringBody(value.into())).to_value()
    }

    /// Allocates a new array value.
    ///
    /// Note that this function won't automatically convert the array to an inline value
    /// [InlineValue::EmptyArray] if the array is empty.
    pub fn array(value: Array) -> Self {
        ValueBlockRc::encode(ArrayBody(value)).to_value()
    }

    /// Allocate a new record value.
    ///
    /// Note that this function won't automatically convert the array to an inline value
    /// [InlineValue::EmptyArray] if the array is empty.
    pub fn record(value: RecordData) -> Self {
        ValueBlockRc::encode(RecordBody(value)).to_value()
    }

    /// Allocate a new thunk value.
    pub fn thunk(value: CacheIndex) -> Self {
        ValueBlockRc::encode(ThunkBody(value)).to_value()
    }

    /// Allocate a new label value.
    pub fn label(value: Label) -> Self {
        ValueBlockRc::encode(LabelBody(value)).to_value()
    }

    /// Allocate a new enum variant value.
    pub fn enum_variant(tag: LocIdent, arg: Option<NickelValue>, attrs: EnumVariantAttrs) -> Self {
        ValueBlockRc::encode(EnumVariantBody { tag, arg, attrs }).to_value()
    }

    /// Check for physical equality of two Nickel values. This is a very fast check that is
    /// complete for inline values but partial otherwise (i.e. it only returns `true` if the values
    /// physically point to the same memory location, although different allocation can be
    /// semantically equal).
    pub fn phys_eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

// Since a `NickelValue` can be a reference-counted pointer in disguise, we can't just copy it
// blindly. We need to go through `ValueBlockRc::clone` to make sure the reference count is
// incremented accordingly.
impl Clone for NickelValue {
    fn clone(&self) -> Self {
        if self.tag() == ValueTag::Pointer {
            unsafe {
                // We need to prevent this block to be dropped as this would decrement the refcount,
                // nullifying our increment.
                let block_ptr =
                    ManuallyDrop::new(ValueBlockRc::from_raw_unchecked(self.0 as *mut u8));
                block_ptr.incr_ref_count();
                NickelValue(self.0)
            }
        } else {
            NickelValue(self.0)
        }
    }
}

// Same for `Clone`: since we might be a reference-counted pointer in disguise, we need to properly
// decrement the underlying ref count.
impl Drop for NickelValue {
    fn drop(&mut self) {
        if self.tag() == ValueTag::Pointer {
            unsafe {
                let block_ptr = ValueBlockRc::from_raw_unchecked(self.0 as *mut u8);
                std::mem::drop(block_ptr);
            }
        }
    }
}

impl From<InlineValue> for NickelValue {
    fn from(inline: InlineValue) -> Self {
        NickelValue::inline(inline)
    }
}

impl TryFrom<NickelValue> for InlineValue {
    type Error = TagMismatchError<ValueTag>;

    fn try_from(value: NickelValue) -> Result<Self, Self::Error> {
        if value.tag() == ValueTag::Inline {
            // Safety: `InlineValue` is `#[repr(usize)]`, ensuring that it has the same layout.
            // Additionally, it's an invariant of `NickelValue` that if `tag` is
            // `ValueTag::Inline`, then the value must be a valid inline value (come from a
            // conversion from an `InlineValue`).
            Ok(unsafe { transmute::<usize, InlineValue>(value.0) })
        } else {
            Err(TagMismatchError {
                expected: ValueTag::Inline,
                found: value.tag(),
            })
        }
    }
}

impl TryFrom<NickelValue> for ValueBlockRc {
    type Error = TagMismatchError<ValueTag>;

    fn try_from(value: NickelValue) -> Result<Self, Self::Error> {
        if value.tag() == ValueTag::Pointer {
            Ok(unsafe {
                // We need to prevent value from being dropped, or this will decrease the refcount.
                let value = ManuallyDrop::new(value);
                ValueBlockRc::from_raw_unchecked(value.0 as *mut u8)
            })
        } else {
            Err(TagMismatchError {
                expected: ValueTag::Pointer,
                found: value.tag(),
            })
        }
    }
}

/// Pointer tag used by [NickelValue] to discriminate between the pointer and non-pointer kind of Nickel values.
#[repr(usize)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
// CAUTION: unsafe functions are relying on the precise values and range of `ValueTag`. If you add
// or remove tags, make sure to update all the corresponding code, in particular conversion
// functions from and to numeric types.
pub enum ValueTag {
    /// A heap-allocated value, meaning the tagged data is a valid pointer to [ValueBlockRc].
    Pointer = 0,
    /// The tag for an [InlineValue], which is not a pointer.
    Inline = 1,
    /// The tag for a thunk, which is a pointer to code. Currently, this is to be interpreted as a
    /// pointer to the [old ast](crate::term::RichTerm), but will eventually be a code address in
    /// the bytecode virtual machine.
    Code = 2,
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
pub struct TagOutOfBoundsError<T> {
    found: T,
    max: T,
}

impl TryFrom<usize> for ValueTag {
    type Error = TagOutOfBoundsError<usize>;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        if value <= 2 {
            // Safety: `#[repr(usize)]` on [ValueTag] guarantees that the enum is safe to transmute
            // to and from `usize`, as long as we are in the range of valid tags.
            Ok(unsafe { transmute::<usize, ValueTag>(value) })
        } else {
            Err(TagOutOfBoundsError {
                found: value,
                max: 2,
            })
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
/// tag included, so that no bit shifting is needed at all for creating them or reading them.
#[repr(usize)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InlineValue {
    Null = encode_inline(0),
    True = encode_inline(1),
    False = encode_inline(2),
    EmptyArray = encode_inline(3),
    EmptyRecord = encode_inline(4),
}

/// The discriminating tag for the different kinds of content that can be store in a value block.
// CAUTION: unsafe functions are relying on the precise values and range of `BodyTag`. If you
// add or remove tags, make sure to update all the corresponding code, in particular conversion
// functions from and to numeric types.
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
    /// Returns the layout to be used for de-allocation of a whole value block depending on the
    /// tag. Calls to [ValueBlockRc::block_layout] under the hood instantiated with the right type.
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
    type Error = TagOutOfBoundsError<u8>;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value <= 10 {
            // Safety: `#[repr(u8)]` on `BodyTag` guarantees that the enum is safe to transmute to
            // and from `u8`, as long as we are in the range of valid tags.
            Ok(unsafe { transmute::<u8, BodyTag>(value) })
        } else {
            Err(TagOutOfBoundsError {
                found: value,
                max: 10,
            })
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
#[repr(Rust, align(8))]
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

/// Marker trait for Nickel values that are stored in a value block.
///
/// # Alignment of value content
///
/// It is very important that all types implementing this trait are aligned on at most 8 bytes,
/// using `#[repr(packed(8))]`.
///
/// The reason is that when we allocate a value block, we need to put the header first and then the
/// body of the value. If the alignment of both the header and the body are arbitrary, we need to
/// precompute an alignement for the initial address to provide to `alloc` and a `n >=
/// size_of(header)` that are optimal such that the base address is header-aligned and
/// `base_address+n` is content-aligned (where `base_address+sizeof(header)..base_address+(n-1)` would
/// be uninitialised padding). We would also need to recompute the padding to skip each time we access
/// the content of a value based on the tag in the header.
///
/// To get rid of this complexity, we require that the header is at least 8-bytes aligned, and that
/// the content is at most 8-bytes aligned. Since the header is 8 exactly bytes (independently from
/// the platform), this ensures that if we allocate `base_address` with the alignment of header,
/// then `base_address + size_of(header)` is at least 8-bytes aligned, and thus that it is
/// content-aligned. Doing so, we need no computations, nor any padding.
///
/// On most platforms (including 64bits and 32bits), non-trivial structs are usually aligned to at most 8 bytes (as long as
/// we don't use large integer types). The header should be 8-bytes aligned on most 64bits
/// platform. All in all, we shouldn't actually override the default alignment of the types
/// involved in most cases. **However, since a misalignment will lead to undefined behavior,
/// always make extra sure that that those constraints (header is at least 8-bytes aligned, value
/// content is at most 8-bytes aligned) are always enforced!**
pub trait ValueBlockBody {
    const TAG: BodyTag;
}

#[repr(Rust, packed(8))]
pub struct NumberBody(Number);
#[repr(Rust, packed(8))]
pub struct StringBody(NickelString);
#[repr(Rust, packed(8))]
pub struct ArrayBody(Array);
#[repr(Rust, packed(8))]
pub struct RecordBody(RecordData);
#[repr(Rust, packed(8))]
pub struct ThunkBody(CacheIndex);
#[repr(Rust, packed(8))]
pub struct LabelBody(Label);
#[repr(Rust, packed(8))]
pub struct EnumVariantBody {
    pub tag: LocIdent,
    pub arg: Option<NickelValue>,
    pub attrs: EnumVariantAttrs,
}
#[repr(Rust, packed(8))]
pub struct ForeignIdBody(ForeignIdPayload);
/// A custom contract. The content must be a function (or function-like terms like a match
/// expression) of two arguments: a label and the value to be checked. In particular, it must
/// be a weak-head normal form, and this invariant may be relied upon elsewhere in the
/// codebase (although it's not the case at the time of writing, to the best of my knowledge).
///
/// Having a separate node for custom contracts lets us leverage the additional information for
/// example to implement a restricted `or` combinator on contracts, which needs to know which
/// contracts support booleans operations (predicates and validators), or for better error
/// messages in the future when parametric contracts aren't fully applied
/// ([#1460](https://github.com/tweag/nickel/issues/1460)). In the future, the custom contract
/// node might also include even more metadata.
///
/// # Immediate and delayed parts
///
/// Custom contracts usually have two parts, an immediate part and a delayed part.
///
/// The immediate part is similar to a predicate or a validator: this is a function that takes a
/// value and return either `'Ok` or `'Error {..}`. The immediate part gathers the checks that can
/// be done eagerly, without forcing the value (the immediate part can actually force the value,
/// but it's up to the implementer to decide - for builtin contracts, the immediate part never
/// forces values)
///
/// The delayed part is a partial identity which takes a label and the value and either blames or
/// return the value with potential delayed checks buried inside.
///
/// Note that this is a conceptual distinction. It did happen that we experimented with making
/// this distinction explicit, with custom contracts being represented by two different
/// functions, one for each part. But this proved to be cumbersome in many ways (both for us
/// language developers and for users). Instead, we decided to make custom contracts just one
/// function of type `Label -> Dyn -> [| 'Ok Dyn, 'Error {..} |]`, which gives enough
/// information to extract the immediate and the delayed part anyway. The delayed part, if any,
/// is embedded in the return value of the case `'Ok Dyn`, where the argument is the original
/// value with the delayed checks inside.
///
/// # Naked functions as custom contracts
///
/// Nowadays, using dedicated constructors is the only documented way of creating custom
/// contracts: `std.contract.custom`, `std.contract.from_validator`, etc. The requirement to
/// use those dedicated constructors is unfortunately a breaking change (prior to Nickel 1.8)
/// as custom contracts were written as naked functions before. Using naked functions is
/// discouraged and will be deprecated in the future, but `%contract/apply%` still supports
/// them.
#[repr(Rust, packed(8))]
pub struct CustomContractBody(NickelValue);
#[repr(Rust, packed(8))]
pub struct SealingKeyBody(SealingKey);
#[repr(Rust, packed(8))]
/// A type in term position, such as in `let my_contract = Number -> Number in ...`.
///
/// During evaluation, this will get turned into a contract.
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
#[repr(Rust, packed(8))]
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
    /// in between. Note that if the same pointer is converted back to a `ValueBlockRc` which is
    /// then dropped, this pointer becomes invalid and musn't be used anymore (and in particular be
    /// passed to this function).
    pub unsafe fn from_raw(ptr: *mut u8) -> Option<Self> {
        NonNull::new(ptr).map(ValueBlockRc)
    }

    /// Creates a new `ValueBlockRc` from a raw pointer without checking for null.
    ///
    /// # Safety
    ///
    /// Same conditions as for [ValueBlockRc::from_raw], plus `ptr` must not be null.
    pub unsafe fn from_raw_unchecked(ptr: *mut u8) -> Self {
        ValueBlockRc(NonNull::new_unchecked(ptr))
    }

    pub fn into_raw(self) -> *mut u8 {
        // We must avoid dropping `Self` here, which would decrement the reference count.
        let this = ManuallyDrop::new(self);
        (*this).0.as_ptr()
    }

    /// Gets the raw pointer to the content.
    fn as_ptr(&self) -> *mut u8 {
        self.0.as_ptr()
    }

    /// Converts this value block to a [NickelValue] pointer. To avoid duplicating raw pointers
    /// without properly incrementing the reference count, this function consumes the value block.
    pub fn to_value(self) -> NickelValue {
        // We must avoid dropping `Self` here, which would decrement the reference count.
        let this = ManuallyDrop::new(self);
        NickelValue((*this).0.as_ptr() as usize)
    }

    /// Returns the header of this value block.
    fn header(&self) -> ValueBlockHeader {
        unsafe { *self.0.cast::<ValueBlockHeader>().as_ref() }
    }

    /// Returns a mutable pointer to the header of this value block.
    fn header_mut(&self) -> *mut ValueBlockHeader {
        unsafe { self.0.cast::<ValueBlockHeader>().as_mut() }
    }

    /// Increments the reference count of this value block.
    fn incr_ref_count(&self) {
        unsafe { (*self.header_mut()).incr_ref_count() }
    }

    /// Decrements the reference count of this value block.
    fn decr_ref_count(&self) {
        unsafe { (*self.header_mut()).decr_ref_count() }
    }

    /// Same as [std::rc::Rc::try_unwrap] but for a value block. Mutably borrows the value block
    /// and returns `Some` if the value block is unique (i.e. has a reference count of 1), or
    /// returns `None` otherwise.
    pub fn get_mut<T: ValueBlockBody>(
        &mut self,
    ) -> Result<Option<&mut T>, TagMismatchError<BodyTag>> {
        if self.header().tag() != T::TAG {
            Err(TagMismatchError {
                expected: T::TAG,
                found: self.header().tag(),
            })
        } else if self.header().ref_count() != 1 {
            Ok(None)
        } else {
            // Safety: we know that the value block is unique, so we can safely decode the content
            // without any risk of aliasing.
            unsafe { Ok(Some(self.decode_mut_unchecked::<T>())) }
        }
    }

    /// Same as [std::rc::Rc::make_mut] but for a value block.
    pub fn make_mut<T: ValueBlockBody + Clone>(
        &mut self,
    ) -> Result<&mut T, TagMismatchError<BodyTag>> {
        if self.header().tag() != T::TAG {
            Err(TagMismatchError {
                expected: T::TAG,
                found: self.header().tag(),
            })
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

    /// Determines the layout for allocation a de-allocation of value blocks for a given value content type `T`.
    fn block_layout<T: ValueBlockBody>() -> Layout {
        let header_layout = Layout::new::<ValueBlockHeader>();
        let body_layout = Layout::new::<T>();

        assert!(
            header_layout.align() >= 8 && body_layout.align() <= 8 && header_layout.size() == 8
        );

        Layout::from_size_align(header_layout.size() + body_layout.size(), 8).unwrap()
    }

    fn encode<T: ValueBlockBody>(value: T) -> Self {
        unsafe {
            let start = alloc(Self::block_layout::<T>());

            if start.is_null() {
                panic!("out of memory: failed to allocate memory for Nickel value")
            }

            let header_ptr = start as *mut ValueBlockHeader;
            header_ptr.write(ValueBlockHeader::new(T::TAG));

            let body_ptr = start.add(size_of::<ValueBlockHeader>()) as *mut T;
            body_ptr.write(value);

            Self(NonNull::new_unchecked(start))
        }
    }

    /// Tries to decode this value black as a reference to a value of type `T`. Returns `None` if
    /// the tag of this value block is not `T::TAG`.
    fn try_decode<T: ValueBlockBody>(&self) -> Option<&T> {
        (self.header().tag() == T::TAG).then(|| unsafe { self.decode_unchecked() })
    }

    /// Panicking variant of [Self::try_decode_ref]. Same as `self.try_decode_ref().unwrap()`.
    #[track_caller]
    fn decode<T: ValueBlockBody>(&self) -> &T {
        self.try_decode().unwrap()
    }

    /// Unsafe variant of [Self::try_decode_ref]. Doesn't perform any tag check, and blindly try to
    /// decode the content of this block to a `&T`.
    ///
    /// # Safety
    ///
    /// The content of this value block must have been encoded from a value of type `T`, that is
    /// `self.tag() == T::TAG`.
    unsafe fn decode_unchecked<T: ValueBlockBody>(&self) -> &T {
        self.0
            .add(size_of::<ValueBlockHeader>())
            .cast::<T>()
            .as_ref()
    }

    /// Mutable variant of [Self::decode_unchecked].
    ///
    /// # Safety
    ///
    /// The content of this value block must have been encoded from a value of type `T`, that is
    /// `self.tag() == T::TAG`. You must ensure that there is no active mutable reference inside
    /// this value block as long as the returned mutable reference is alive. This is typically the
    /// case if the reference count of the value block is 1.
    unsafe fn decode_mut_unchecked<T: ValueBlockBody>(&mut self) -> &mut T {
        self.0
            .add(size_of::<ValueBlockHeader>())
            .cast::<T>()
            .as_mut()
    }
}

impl Drop for ValueBlockRc {
    fn drop(&mut self) {
        if self.header().ref_count() == 1 {
            unsafe {
                let tag = self.header().tag();
                let body_ptr = self.0.as_ptr().add(size_of::<ValueBlockHeader>());

                // Call the destructor of the body
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inline_values() {
        let inline_null = NickelValue::inline(InlineValue::Null);
        let inline_true = NickelValue::inline(InlineValue::True);
        let inline_false = NickelValue::inline(InlineValue::False);
        let inline_empty_array = NickelValue::inline(InlineValue::EmptyArray);
        let inline_empty_record = NickelValue::inline(InlineValue::EmptyRecord);

        assert_eq!(inline_null.tag(), ValueTag::Inline);
        assert_eq!(inline_true.tag(), ValueTag::Inline);
        assert_eq!(inline_false.tag(), ValueTag::Inline);
        assert_eq!(inline_empty_array.tag(), ValueTag::Inline);
        assert_eq!(inline_empty_record.tag(), ValueTag::Inline);

        assert_eq!(inline_null.clone().try_into(), Ok(InlineValue::Null));
        assert_eq!(inline_true.clone().try_into(), Ok(InlineValue::True));
        assert_eq!(inline_false.clone().try_into(), Ok(InlineValue::False));
        assert_eq!(
            inline_empty_array.clone().try_into(),
            Ok(InlineValue::EmptyArray)
        );
        assert_eq!(
            inline_empty_record.clone().try_into(),
            Ok(InlineValue::EmptyRecord)
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

        assert_eq!(
            ValueBlockRc::try_from(number_value.clone())
                .unwrap()
                .decode::<NumberBody>()
                .0,
            Number::from(42)
        );
        assert_eq!(
            ValueBlockRc::try_from(string_value.clone())
                .unwrap()
                .decode::<StringBody>()
                .0,
            NickelString::from("Hello, World!")
        );

        assert!(ValueBlockRc::try_from(number_value)
            .unwrap()
            .try_decode::<StringBody>()
            .is_none());
        assert!(ValueBlockRc::try_from(string_value)
            .unwrap()
            .try_decode::<NumberBody>()
            .is_none());
    }

    #[test]
    fn ref_counting() {
        let number_value = NickelValue::number(Number::from(42));

        let mut copies =
            std::array::from_fn::<_, 20, _>(|_| ManuallyDrop::new(number_value.clone()));

        let mut as_val = ValueBlockRc::try_from(number_value).unwrap();
        assert_eq!(as_val.header().ref_count(), 21);

        let mut block_copy = ManuallyDrop::new(as_val.clone());

        assert_eq!(as_val.header().ref_count(), 22);

        assert!(as_val.get_mut::<NumberBody>().unwrap().is_none());

        for i in 0..10 {
            unsafe {
                ManuallyDrop::drop(&mut copies[i]);
            }
        }

        assert_eq!(as_val.header().ref_count(), 12);

        for i in 10..20 {
            unsafe {
                ManuallyDrop::drop(&mut copies[i]);
            }
        }

        unsafe { ManuallyDrop::drop(&mut block_copy) }

        *as_val.get_mut().unwrap().unwrap() = NumberBody(Number::from(100));
        assert_eq!(as_val.header().ref_count(), 1);
        assert_eq!(as_val.decode::<NumberBody>().0, Number::from(100));
    }
}
