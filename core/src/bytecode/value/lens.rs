//! Lenses are a way to lazily and conditionally extract owned data from a Nickel value.

use super::{
    InlineValue, NickelValue, RefCount, TermBody, ValueBlockBody, ValueBlockHeader, ValueBlockRc,
};

use crate::{
    error::{EvalError, ParseError},
    files::FileId,
    identifier::LocIdent,
    label::Label,
    term::{
        pattern::Pattern,
        record::{Field, Include, RecordData, RecordDeps},
        BinaryOp, Import, LetAttrs, MatchData, NAryOp, SealingKey, StrChunk, Term, TypeAnnotation,
        UnaryOp,
    },
};

use smallvec::SmallVec;

use std::{
    alloc::{alloc, dealloc},
    ptr::{self, NonNull},
};

/// A lazy handle to part or all of the content of a Nickel value, making it possible to
/// conditionally take owned data out. If the value is unique (1-ref counted), the data is directly
/// moved out and the corresponding block is consumed. Otherwise, the body is cloned, similarly to
/// [std::rc::Rc::unwrap_or_clone].
///
/// [Self] can either be consumed using `take()`, returning the owned content of the body, or
/// reverted back to the original value using `restore()`.
///
/// See also [super::ValueContent].
pub struct ValueLens<T> {
    value: NickelValue,
    /// An extractor for the data `T` to take out from the value block.
    lens: fn(NickelValue) -> T,
}

impl<T> ValueLens<T> {
    /// Do not access the body and restore the original value unchanged.
    pub fn restore(self) -> NickelValue {
        self.value
    }

    /// Consumes the value and return the content of the body. If the block is unique, it is
    /// consumed, If the block is shared, the content is cloned. [Self::take] behaves very much
    /// like [std::rc::Rc::unwrap_or_clone].
    pub fn take(self) -> T {
        (self.lens)(self.value)
    }
}

impl<T: ValueBlockBody + Clone> ValueLens<T> {
    /// Create a new lens extracting a body of type `T` from a value.
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [T::TAG].
    pub(super) unsafe fn body_lens(value: NickelValue) -> Self {
        ValueLens {
            value,
            lens: Self::body_extractor,
        }
    }

    /// Extractor for a value block.
    fn body_extractor(value: NickelValue) -> T {
        // Safety: the fields of LazyValueLens are private, so it can only be constructed from
        // within this module. We maintain the invariant that if `body_extractor` is used as a lens
        // for a `LazyValueLens` object, then `T : ValueBlockBody` and `self.value` is a value
        // block whose tag matches `T::TAG`, so `self.value.data` is a valid pointer to a
        // `ValueBlockHeader` followed by a `U` at the right offset.
        unsafe {
            let ptr = NonNull::new_unchecked(value.data as *mut u8);
            let ref_count = ptr.cast::<ValueBlockHeader>().as_ref().ref_count;
            let ptr_content = ptr.add(ValueBlockRc::body_offset::<T>()).cast::<T>();

            if ref_count == RefCount::ONE {
                let content = ptr::read(ptr_content.as_ptr());
                dealloc(ptr.as_ptr(), T::TAG.block_layout());
                content
            } else {
                ptr_content.as_ref().clone()
            }
        }
    }
}

impl ValueLens<InlineValue> {
    /// Creates a new lens extracting an inline value from a value.
    ///
    /// # Safety
    ///
    /// `value.tag()` must be [super::ValueTag::Inline].
    pub(super) unsafe fn inline_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::inline_extractor,
        }
    }

    /// Extractor for an inline value.
    fn inline_extractor(value: NickelValue) -> InlineValue {
        // Safety: we maintain the invariant throughout this module that if `T = InlineValue`, then
        // `self.value` must be an inline value.
        unsafe { value.as_inline_unchecked() }
    }
}

/// Counterpart of [super::ValueContent] for [crate::term::Term] representing the inner term stored in a block
/// value. This makes it possible to conditionally take data out depending not only on the type of
/// the block, but on the specific constructor of the term inside that block as well.
///
/// This is the same type as [crate::term::Term] but where all the enum variant arguments have been
/// wrapped in a lens.
pub enum TermContent {
    Value(ValueLens<NickelValue>),
    StrChunks(ValueLens<Vec<StrChunk<NickelValue>>>),
    Fun(ValueLens<(LocIdent, NickelValue)>),
    FunPattern(ValueLens<(Pattern, NickelValue)>),
    Let(
        ValueLens<(
            SmallVec<[(LocIdent, NickelValue); 4]>,
            NickelValue,
            LetAttrs,
        )>,
    ),
    LetPattern(ValueLens<(SmallVec<[(Pattern, NickelValue); 1]>, NickelValue, LetAttrs)>),
    App(ValueLens<(NickelValue, NickelValue)>),
    Var(ValueLens<LocIdent>),
    RecRecord(
        ValueLens<(
            RecordData,
            Vec<Include>,
            Vec<(NickelValue, Field)>,
            Option<RecordDeps>,
        )>,
    ),
    Closurize(ValueLens<NickelValue>),
    Match(ValueLens<MatchData>),
    Op1(ValueLens<(UnaryOp, NickelValue)>),
    Op2(ValueLens<(BinaryOp, NickelValue, NickelValue)>),
    OpN(ValueLens<(NAryOp, Vec<NickelValue>)>),
    Sealed(ValueLens<(SealingKey, NickelValue, Label)>),
    Annotated(ValueLens<(TypeAnnotation, NickelValue)>),
    Import(ValueLens<Import>),
    ResolvedImport(ValueLens<FileId>),
    ParseError(ValueLens<ParseError>),
    RuntimeError(ValueLens<EvalError>),
}

impl TermContent {
    /// Do not access the content and restore the original value unchanged.
    pub fn restore(self) -> NickelValue {
        match self {
            TermContent::Value(lens) => lens.restore(),
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
}

impl ValueLens<NickelValue> {
    /// Creates a new lens extracting [crate::term::Term::Value].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::Value].
    pub(super) unsafe fn term_value_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_value_extractor,
        }
    }

    /// Creates a new lens extracting [crate::term::Term::Value].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::Closurize].
    pub(super) unsafe fn term_closurize_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_closurize_extractor,
        }
    }

    /// Extractor for [crate::term::Term::Value].
    fn term_value_extractor(value: NickelValue) -> NickelValue {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::Value(inner) = term.0 {
            inner
        } else {
            unreachable!()
        }
    }

    /// Extractor for [crate::term::Term::Closurize].
    fn term_closurize_extractor(value: NickelValue) -> NickelValue {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::Closurize(inner) = term.0 {
            inner
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<Vec<StrChunk<NickelValue>>> {
    /// Creates a new lens extracting [crate::term::Term::Value].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::StrChunks].
    pub(super) unsafe fn term_str_chunks_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_str_chunks_extractor,
        }
    }

    /// Extractor for [crate::term::Term::StrChunks].
    fn term_str_chunks_extractor(value: NickelValue) -> Vec<StrChunk<NickelValue>> {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::StrChunks(chunks) = term.0 {
            chunks
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<(LocIdent, NickelValue)> {
    /// Creates a new lens extracting [crate::term::Term::Fun].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::Fun].
    pub(super) unsafe fn term_fun_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_fun_extractor,
        }
    }

    /// Extractor for [crate::term::Term::Fun].
    fn term_fun_extractor(value: NickelValue) -> (LocIdent, NickelValue) {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::Fun(arg, body) = term.0 {
            (arg, body)
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<(Pattern, NickelValue)> {
    /// Creates a new lens extracting [crate::term::Term::FunPattern].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::FunPattern].
    pub(super) unsafe fn term_fun_pat_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_fun_pat_extractor,
        }
    }

    /// Extractor for [crate::term::Term::FunPattern].
    fn term_fun_pat_extractor(value: NickelValue) -> (Pattern, NickelValue) {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::FunPattern(pat, body) = term.0 {
            (pat, body)
        } else {
            unreachable!()
        }
    }
}

impl
    ValueLens<(
        SmallVec<[(LocIdent, NickelValue); 4]>,
        NickelValue,
        LetAttrs,
    )>
{
    /// Creates a new lens extracting [crate::term::Term::Let].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::Let].
    pub(super) unsafe fn term_let_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_let_extractor,
        }
    }

    /// Extractor for [crate::term::Term::Let].
    fn term_let_extractor(
        value: NickelValue,
    ) -> (
        SmallVec<[(LocIdent, NickelValue); 4]>,
        NickelValue,
        LetAttrs,
    ) {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::Let(bindings, body, attrs) = term.0 {
            (bindings, body, attrs)
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<(SmallVec<[(Pattern, NickelValue); 1]>, NickelValue, LetAttrs)> {
    /// Creates a new lens extracting [crate::term::Term::LetPattern].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::LetPattern].
    pub(super) unsafe fn term_let_pat_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_let_pat_extractor,
        }
    }

    /// Extractor for [crate::term::Term::LetPattern].
    fn term_let_pat_extractor(
        value: NickelValue,
    ) -> (SmallVec<[(Pattern, NickelValue); 1]>, NickelValue, LetAttrs) {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::LetPattern(bindings, body, attrs) = term.0 {
            (bindings, body, attrs)
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<(NickelValue, NickelValue)> {
    /// Creates a new lens extracting [crate::term::Term::App].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::App].
    pub(super) unsafe fn term_app_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_app_extractor,
        }
    }

    /// Extractor for [crate::term::Term::App].
    fn term_app_extractor(value: NickelValue) -> (NickelValue, NickelValue) {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::App(fun, arg) = term.0 {
            (fun, arg)
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<LocIdent> {
    /// Creates a new lens extracting [crate::term::Term::Var].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::Var].
    pub(super) unsafe fn term_var_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_var_extractor,
        }
    }

    /// Extractor for [crate::term::Term::Var].
    fn term_var_extractor(value: NickelValue) -> LocIdent {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::Var(ident) = term.0 {
            ident
        } else {
            unreachable!()
        }
    }
}

impl
    ValueLens<(
        RecordData,
        Vec<Include>,
        Vec<(NickelValue, Field)>,
        Option<RecordDeps>,
    )>
{
    /// Creates a new lens extracting [crate::term::Term::RecRecord].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::RecRecord].
    pub(super) unsafe fn term_rec_record_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_rec_record_extractor,
        }
    }

    /// Extractor for [crate::term::Term::RecRecord].
    fn term_rec_record_extractor(
        value: NickelValue,
    ) -> (
        RecordData,
        Vec<Include>,
        Vec<(NickelValue, Field)>,
        Option<RecordDeps>,
    ) {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::RecRecord(data, includes, fields, deps) = term.0 {
            (data, includes, fields, deps)
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<MatchData> {
    /// Creates a new lens extracting [crate::term::Term::Match].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::Match].
    pub(super) unsafe fn term_match_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_match_extractor,
        }
    }

    /// Extractor for [crate::term::Term::Match].
    fn term_match_extractor(value: NickelValue) -> MatchData {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::Match(data) = term.0 {
            data
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<(UnaryOp, NickelValue)> {
    /// Creates a new lens extracting [crate::term::Term::Op1].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::Op1].
    pub(super) unsafe fn term_op1_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_op1_extractor,
        }
    }

    /// Extractor for [crate::term::Term::Op1].
    fn term_op1_extractor(value: NickelValue) -> (UnaryOp, NickelValue) {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::Op1(op, arg) = term.0 {
            (op, arg)
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<(BinaryOp, NickelValue, NickelValue)> {
    /// Creates a new lens extracting [crate::term::Term::Op2].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::Op2].
    pub(super) unsafe fn term_op2_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_op2_extractor,
        }
    }

    /// Extractor for [crate::term::Term::Op2].
    fn term_op2_extractor(value: NickelValue) -> (BinaryOp, NickelValue, NickelValue) {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::Op2(op, left, right) = term.0 {
            (op, left, right)
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<(NAryOp, Vec<NickelValue>)> {
    /// Creates a new lens extracting [crate::term::Term::OpN].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::OpN].
    pub(super) unsafe fn term_opn_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_opn_extractor,
        }
    }

    /// Extractor for [crate::term::Term::OpN].
    fn term_opn_extractor(value: NickelValue) -> (NAryOp, Vec<NickelValue>) {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::OpN(op, args) = term.0 {
            (op, args)
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<(SealingKey, NickelValue, Label)> {
    /// Creates a new lens extracting [crate::term::Term::Sealed].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::Sealed].
    pub(super) unsafe fn term_sealed_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_sealed_extractor,
        }
    }

    /// Extractor for [crate::term::Term::Sealed].
    fn term_sealed_extractor(value: NickelValue) -> (SealingKey, NickelValue, Label) {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::Sealed(key, body, label) = term.0 {
            (key, body, label)
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<(TypeAnnotation, NickelValue)> {
    /// Creates a new lens extracting [crate::term::Term::Annotated].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::Annotated].
    pub(super) unsafe fn term_annotated_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_annotated_extractor,
        }
    }

    /// Extractor for [crate::term::Term::Annotated].
    fn term_annotated_extractor(value: NickelValue) -> (TypeAnnotation, NickelValue) {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::Annotated(annot, body) = term.0 {
            (annot, body)
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<Import> {
    /// Creates a new lens extracting [crate::term::Term::Import].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::Import].
    pub(super) unsafe fn term_import_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_import_extractor,
        }
    }

    /// Extractor for [crate::term::Term::Import].
    fn term_import_extractor(value: NickelValue) -> Import {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::Import(import) = term.0 {
            import
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<FileId> {
    /// Creates a new lens extracting [crate::term::Term::ResolvedImport].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::ResolvedImport].
    pub(super) unsafe fn term_resolved_import_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_resolved_import_extractor,
        }
    }

    /// Extractor for [crate::term::Term::ResolvedImport].
    fn term_resolved_import_extractor(value: NickelValue) -> FileId {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::ResolvedImport(file_id) = term.0 {
            file_id
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<ParseError> {
    /// Creates a new lens extracting [crate::term::Term::ParseError].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::ParseError].
    pub(super) unsafe fn term_parse_error_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_parse_error_extractor,
        }
    }

    /// Extractor for [crate::term::Term::ParseError].
    fn term_parse_error_extractor(value: NickelValue) -> ParseError {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::ParseError(err) = term.0 {
            err
        } else {
            unreachable!()
        }
    }
}

impl ValueLens<EvalError> {
    /// Creates a new lens extracting [crate::term::Term::RuntimeError].
    ///
    /// # Safety
    ///
    /// `value` must be a value block with tag [super::BodyTag::Term], and the inner term must
    /// match [crate::term::Term::RuntimeError].
    pub(super) unsafe fn term_runtime_error_lens(value: NickelValue) -> Self {
        Self {
            value,
            lens: Self::term_runtime_error_extractor,
        }
    }

    /// Extractor for [crate::term::Term::RuntimeError].
    fn term_runtime_error_extractor(value: NickelValue) -> EvalError {
        let term = ValueLens::<TermBody>::body_extractor(value);

        if let Term::RuntimeError(err) = term.0 {
            err
        } else {
            unreachable!()
        }
    }
}
