//! Lenses are a way to lazily and conditionally extract owned data from a Nickel value.
use super::{
    Container, InlineValue, NickelValue, RefCount, TermData, ValueBlockData, ValueBlockHeader,
    ValueBlockRc,
};

use crate::{
    error::{EvalErrorKind, ParseError},
    files::FileId,
    identifier::LocIdent,
    term::{
        AnnotatedData, AppData, FunData, FunPatternData, Import, LetData, LetPatternData,
        MatchData, Op1Data, Op2Data, OpNData, RecRecordData, SealedData, StrChunk, Term,
    },
    metrics::increment,
};

use std::{
    alloc::dealloc,
    marker::PhantomData,
    mem::ManuallyDrop,
    ptr::{self, NonNull},
};

/// A lazy handle to part or all of the content of a Nickel value, making it possible to
/// conditionally take owned data out. If the value is unique (1-ref counted), the data is directly
/// moved out and the corresponding block is consumed. Otherwise, the content is cloned, similarly
/// to [std::rc::Rc::unwrap_or_clone].
///
/// [Self] can either be consumed using `take()`, returning the owned content of the data, peeked
/// at by reference using `peek()` or `value()`, or reverted back to the original value using
/// `restore()`. When the structure of the lens isn't enough to check if the content should be
/// taken or not, [Self] also provides [Self::peek] which returns a reference to the inner value,
/// so that it can be examined arbitrarily.
///
/// See also [super::ValueContent].
pub struct ValueLens<T> {
    value: NickelValue,
    phantom: PhantomData<T>,
}

impl<T> ValueLens<T> {
    /// Do not access the content and restore the original value unchanged.
    pub fn restore(self) -> NickelValue {
        self.value
    }

    /// Peeks at the underlying value, without consuming the lens.
    pub fn value(&self) -> &NickelValue {
        &self.value
    }
}

impl<T: ValueBlockData + Clone> ValueLens<Container<T>> {
    /// Create a new lens extracting either an inlined empty container or an allocated container
    /// data from a value.
    ///
    /// # Safety
    ///
    /// `value` must either be an inlined empty container (matching `T`) or a value block with tag
    /// `T::TAG`.
    pub(super) unsafe fn container_lens(value: NickelValue) -> Self {
        ValueLens {
            value,
            phantom: PhantomData,
        }
    }

    pub fn take(self) -> Container<T> {
        // The precondition ensures that if `v` is inline, it is the corresponding empty
        // container.
        if self.value.is_inline() {
            Container::Empty
        } else {
            Container::Alloc(ValueLens::<T>::extract_or_clone(self.value))
        }
    }
}

impl<T: ValueBlockData + Clone> ValueLens<T> {
    /// Create a new lens extracting data of type `T` from a value.
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [T::TAG].
    pub(super) unsafe fn content_lens(value: NickelValue) -> Self {
        ValueLens {
            value,
            phantom: PhantomData,
        }
    }

    /// Custom extractor for a value block. If the reference is 1-counted, the content is extracted
    /// and the first closure is called with an owned value. Otherwise, the second closure is
    /// called with a reference to the content. This makes it possible to finer things than the
    /// blunt clone of [Self::extract_or_clone], such as avoiding cloning the outer `Box` wrapper
    /// of some `Term` variants when the data is shared.
    fn with_content<F, G, R>(value: NickelValue, on_owned: F, on_ref: G) -> R
    where
        F: FnOnce(T) -> R,
        G: FnOnce(&T) -> R,
    {
        // Safety: the fields of `ValueLens` are private, so it can only be constructed from within
        // this module. We maintain the invariant that if `with_content` is used as a lens for
        // a `ValueLens` object, then `T : ValueBlockData` and `self.value` is a value block whose
        // tag matches `T::TAG`, so `self.value.data` is a valid pointer to a `ValueBlockHeader`
        // followed by a `U` at the right offset.
        unsafe {
            let ptr = NonNull::new_unchecked(value.data as *mut u8);
            let ref_count = ptr.cast::<ValueBlockHeader>().as_ref().ref_count;
            let ptr_content = ptr.add(ValueBlockRc::data_offset::<T>()).cast::<T>();

            if ref_count == RefCount::ONE {
                increment!("lens::take: copy avoided");
                // Since we "move" the original content, we don't want to run the destructor (if
                // `T` owns e.g. a `HashMap`, it would otherwise be de-allocated when `value` goes
                // out of scope, and we would return a dangling value).
                let _ = ManuallyDrop::new(value);

                // Safety: the content of a NickelValue with `T::TAG` should always be valid for
                // `T`
                let content = ptr::read(ptr_content.as_ptr());

                // While we don't want the destructor to run, we do want to clean up the original
                // allocation.
                dealloc(ptr.as_ptr(), T::TAG.block_layout());
                on_owned(content)
            } else {
                increment!("lens::take: copy required");
                on_ref(ptr_content.as_ref())
            }
        }
    }

    /// Standard extractor for a value block.
    fn extract_or_clone(value: NickelValue) -> T {
        Self::with_content(value, |v| v, |data| data.clone())
    }

    pub fn take(self) -> T {
        Self::extract_or_clone(self.value)
    }
}

impl ValueLens<()> {
    /// Creates a new lens extracting a null from a value.
    pub(super) fn null_lens(value: NickelValue) -> Self {
        Self {
            value,
            phantom: PhantomData,
        }
    }

    pub fn take(self) {}
}

impl ValueLens<bool> {
    /// Creates a new lens extracting a bool from a value.
    ///
    /// # Safety
    ///
    /// `value.tag()` must be [super::ValueTag::Inline]
    ///
    /// # Panic
    ///
    /// Extraction through [ValueLens::take] will panics if the inline value is neither
    /// [super::InlineValue::True] nor [super::InlineValue::False].
    pub(super) unsafe fn bool_lens(value: NickelValue) -> Self {
        Self {
            value,
            phantom: PhantomData,
        }
    }

    pub fn take(self) -> bool {
        // Safety: we maintain the invariant throughout this module that if `T = InlineValue`, then
        // `self.value` must be an inline value.
        match unsafe { self.value.as_inline_unchecked() } {
            InlineValue::True => true,
            InlineValue::False => false,
            _ => panic!("unexpected non-boolean inline value in the extractor of ValueLens<bool>"),
        }
    }
}

/// Counterpart of [super::ValueContent] for [crate::term::Term] representing the inner term stored in a block
/// value. This makes it possible to conditionally take data out depending not only on the type of
/// the block, but on the specific constructor of the term inside that block as well.
///
/// This is the same type as [crate::term::Term] but where all the enum variant arguments have been
/// wrapped in a lens.
pub enum TermContent {
    StrChunks(ValueLens<Vec<StrChunk<NickelValue>>>),
    Fun(ValueLens<FunData>),
    FunPattern(ValueLens<Box<FunPatternData>>),
    Let(ValueLens<Box<LetData>>),
    LetPattern(ValueLens<Box<LetPatternData>>),
    App(ValueLens<AppData>),
    Var(ValueLens<LocIdent>),
    RecRecord(ValueLens<Box<RecRecordData>>),
    Closurize(ValueLens<NickelValue>),
    Match(ValueLens<MatchData>),
    Op1(ValueLens<Box<Op1Data>>),
    Op2(ValueLens<Box<Op2Data>>),
    OpN(ValueLens<OpNData>),
    Sealed(ValueLens<Box<SealedData>>),
    Annotated(ValueLens<Box<AnnotatedData>>),
    Import(ValueLens<Import>),
    ResolvedImport(ValueLens<FileId>),
    ParseError(ValueLens<Box<ParseError>>),
    RuntimeError(ValueLens<Box<EvalErrorKind>>),
}

impl TermContent {
    /// Do not access the content and restore the original value unchanged.
    pub fn restore(self) -> NickelValue {
        match self {
            TermContent::StrChunks(lens) => lens.restore(),
            TermContent::Fun(lens) => lens.restore(),
            TermContent::FunPattern(lens) => lens.restore(),
            TermContent::Let(lens) => lens.restore(),
            TermContent::LetPattern(lens) => lens.restore(),
            TermContent::App(lens) => lens.restore(),
            TermContent::Var(lens) => lens.restore(),
            TermContent::RecRecord(lens) => lens.restore(),
            TermContent::Closurize(lens) => lens.restore(),
            TermContent::Match(lens) => lens.restore(),
            TermContent::Op1(lens) => lens.restore(),
            TermContent::Op2(lens) => lens.restore(),
            TermContent::OpN(lens) => lens.restore(),
            TermContent::Sealed(lens) => lens.restore(),
            TermContent::Annotated(lens) => lens.restore(),
            TermContent::Import(lens) => lens.restore(),
            TermContent::ResolvedImport(lens) => lens.restore(),
            TermContent::ParseError(lens) => lens.restore(),
            TermContent::RuntimeError(lens) => lens.restore(),
        }
    }

    /// Returns a reference to the inner term. This can be useful to perform more elaborate pattern
    /// matching before deciding to take data out.
    pub fn term(&self) -> &Term {
        let value = match self {
            TermContent::StrChunks(lens) => &lens.value,
            TermContent::Fun(lens) => &lens.value,
            TermContent::FunPattern(lens) => &lens.value,
            TermContent::Let(lens) => &lens.value,
            TermContent::LetPattern(lens) => &lens.value,
            TermContent::App(lens) => &lens.value,
            TermContent::Var(lens) => &lens.value,
            TermContent::RecRecord(lens) => &lens.value,
            TermContent::Closurize(lens) => &lens.value,
            TermContent::Match(lens) => &lens.value,
            TermContent::Op1(lens) => &lens.value,
            TermContent::Op2(lens) => &lens.value,
            TermContent::OpN(lens) => &lens.value,
            TermContent::Sealed(lens) => &lens.value,
            TermContent::Annotated(lens) => &lens.value,
            TermContent::Import(lens) => &lens.value,
            TermContent::ResolvedImport(lens) => &lens.value,
            TermContent::ParseError(lens) => &lens.value,
            TermContent::RuntimeError(lens) => &lens.value,
        };

        // unwrap(): if the lens is a TermContent, then the underlying value must be a term block.
        value.as_term().unwrap()
    }

    /// Unconditionally take the inner `Term` out, ignoring the actual shape of the content.
    pub fn take(self) -> Term {
        let value = match self {
            TermContent::StrChunks(lens) => lens.value,
            TermContent::Fun(lens) => lens.value,
            TermContent::FunPattern(lens) => lens.value,
            TermContent::Let(lens) => lens.value,
            TermContent::LetPattern(lens) => lens.value,
            TermContent::App(lens) => lens.value,
            TermContent::Var(lens) => lens.value,
            TermContent::RecRecord(lens) => lens.value,
            TermContent::Closurize(lens) => lens.value,
            TermContent::Match(lens) => lens.value,
            TermContent::Op1(lens) => lens.value,
            TermContent::Op2(lens) => lens.value,
            TermContent::OpN(lens) => lens.value,
            TermContent::Sealed(lens) => lens.value,
            TermContent::Annotated(lens) => lens.value,
            TermContent::Import(lens) => lens.value,
            TermContent::ResolvedImport(lens) => lens.value,
            TermContent::ParseError(lens) => lens.value,
            TermContent::RuntimeError(lens) => lens.value,
        };

        // Safety: since the value was extracted from `TermContent`, it must be a value block with a
        // term inside.
        unsafe { ValueLens::<TermData>::content_lens(value).take() }
    }
}

/// Generates a lens implementation with a constructor and an extractor for a given [Term]
/// constructor and the corresponding type of its payload.
///
/// # Arguments
///
/// - `$lens_cons`: the name of the constructor
/// - `$lens_extrct`: the  name of the extractor
/// - `$term_cons`: the corresponding variant of [Term]
/// - `$type`: the type of the argument of `$term_cons`
///
/// # Safety
///
/// The generated lens constructor is unsafe. The safety rule for the constructor is specified in
/// the function documentation within the macro definition just below.
macro_rules! impl_term_lens {
    ( $lens_cons:ident, $lens_extrct:ident, $term_cons:ident, $type:ty ) => {
        impl ValueLens<$type> {
            // Creates a new lens extracting `Term::$term_cons`.
            //
            // # Safety
            //
            // `value` must be a value block with tag `DataTag::Term`, and the inner term must
            // match `Term::$term_cons`.
            pub(super) unsafe fn $lens_cons(value: NickelValue) -> Self {
                Self {
                    value,
                    phantom: PhantomData,
                }
            }

            // Extractor for `Term::$term_cons`.
            pub fn take(self) -> $type {
                if let Term::$term_cons(data) = ValueLens::<TermData>::extract_or_clone(self.value)
                {
                    data
                } else {
                    unreachable!()
                }
            }

            /// Peeks at the underlying term data without taking ownership.
            pub fn peek(&self) -> &$type {
                if let Some(Term::$term_cons(data)) = self.value.as_term() {
                    data
                } else {
                    unreachable!()
                }
            }
        }
    };
}

/// Same as `impl_term_lens!`, but specialized for terms with boxed payload, where the reference
/// and the extraction code are different. In particular, we try to avoid allocating a new [Box]
/// when copying non-1RC content (in practice, we always want to move the result on the stack).
macro_rules! impl_term_boxed_lens {
    ( $lens_cons:ident, $lens_extrct:ident, $term_cons:ident, $type:ty ) => {
        impl ValueLens<Box<$type>> {
            // Creates a new lens extracting `Term::$term_cons`.
            //
            // # Safety
            //
            // `value` must be a value block with tag `DataTag::Term`, and the inner term must
            // match `Term::$term_cons`.
            pub(super) unsafe fn $lens_cons(value: NickelValue) -> Self {
                Self {
                    value,
                    phantom: PhantomData,
                }
            }

            // Extractor for `Term::$term_cons`.
            pub fn take(self) -> Box<$type> {
                if let Term::$term_cons(data) = ValueLens::<TermData>::extract_or_clone(self.value)
                {
                    data
                } else {
                    unreachable!()
                }
            }

            /// Variant of `take()` for boxed term data that returns the data unboxed. When the
            /// value is shared, [Self::take_unboxed] avoids cloning the outer box and directly
            /// clones the inner data instead. If you're going to move out of the box anyway,
            /// prefer this variant, which should avoid an ephemeral heap-allocation.
            pub fn take_unboxed(self) -> $type {
                ValueLens::<TermData>::with_content(
                    self.value,
                    |owned| {
                        if let Term::$term_cons(data) = owned {
                            *data
                        } else {
                            unreachable!()
                        }
                    },
                    |as_ref| {
                        if let Term::$term_cons(data) = as_ref {
                            (**data).clone()
                        } else {
                            unreachable!()
                        }
                    },
                )
            }

            /// Returns a reference to the inner term data without taking ownership.
            pub fn peek(&self) -> &$type {
                if let Some(Term::$term_cons(data)) = self.value.as_term() {
                    &**data
                } else {
                    unreachable!()
                }
            }
        }
    };
}

impl_term_lens!(
    term_closurize_lens,
    term_closurize_extractor,
    Closurize,
    NickelValue
);

impl_term_lens!(
    term_str_chunks_lens,
    term_str_chunks_extractor,
    StrChunks,
    Vec<StrChunk<NickelValue>>
);

impl_term_lens!(term_fun_lens, term_fun_extractor, Fun, FunData);

impl_term_boxed_lens!(
    term_fun_pat_lens,
    term_fun_pat_extractor,
    FunPattern,
    FunPatternData
);

impl_term_boxed_lens!(term_let_lens, term_let_extractor, Let, LetData);

impl_term_boxed_lens!(
    term_let_pat_lens,
    term_let_pat_extractor,
    LetPattern,
    LetPatternData
);

impl_term_lens!(term_app_lens, term_app_extractor, App, AppData);

impl_term_lens!(term_var_lens, term_var_extractor, Var, LocIdent);

impl_term_boxed_lens!(
    term_rec_record_lens,
    term_rec_record_extractor,
    RecRecord,
    RecRecordData
);

impl_term_lens!(term_match_lens, term_match_extractor, Match, MatchData);

impl_term_boxed_lens!(term_op1_lens, term_op1_extractor, Op1, Op1Data);

impl_term_boxed_lens!(term_op2_lens, term_op2_extractor, Op2, Op2Data);

impl_term_lens!(term_opn_lens, term_opn_extractor, OpN, OpNData);

impl_term_boxed_lens!(term_sealed_lens, term_sealed_extractor, Sealed, SealedData);

impl_term_boxed_lens!(
    term_annotated_lens,
    term_annotated_extractor,
    Annotated,
    AnnotatedData
);

impl_term_lens!(term_import_lens, term_import_extractor, Import, Import);

impl_term_lens!(
    term_resolved_import_lens,
    term_resolved_import_extractor,
    ResolvedImport,
    FileId
);

impl_term_boxed_lens!(
    term_parse_error_lens,
    term_parse_error_extractor,
    ParseError,
    ParseError
);

impl_term_boxed_lens!(
    term_runtime_error_lens,
    term_runtime_error_extractor,
    RuntimeError,
    EvalErrorKind
);
