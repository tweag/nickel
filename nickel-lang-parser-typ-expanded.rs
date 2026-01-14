pub mod typ {
    //! Nickel static types.
    //!
    //! The type system of Nickel is comprised of primitive types, arrays, records, functions, and
    //! opaque types (contracts). This is a structural type system with row polymorphism for both
    //! records and enums.
    //!
    //! ## Record types (rows)
    //!
    //! A row type for a record is represented as a linked list of pairs `(id, type)` indicating the
    //! name and the type of each field. Row-polymorphism means that the tail of this list can be a
    //! type variable which can be abstracted over, leaving the row open for future extension. A simple
    //! illustration is record field access:
    //!
    //! ```nickel
    //! let f : forall a. { some_field : Number; a} -> Number =
    //!   fun record => record.some_field
    //! ```
    //!
    //! The type `{ some_field: Number; a }` indicates that an argument to this function must have at
    //! least the field `some_field` of type `Number`, but may contain other fields (or not).
    //!
    //! ## Dictionaries
    //!
    //! A dictionary type `{ _ : Type }` represents a record whose fields all have the type `Type`. The
    //! count and the name of the fields aren't constrained. Dictionaries can be mapped over, extended,
    //! shrinked and accessed in a type-safe manner.
    //!
    //! # Enum types
    //!
    //! An enum type is also a row type where each element is a tag, such as `[| 'foo, 'bar, 'baz |]`.
    //! This type represent values that can be either `'foo`, `'bar` or `'baz`. Enums support row
    //! polymorphism as well.
    //!
    //! # Contracts
    //!
    //! To each type corresponds a contract, which is equivalent to a Nickel function which checks at
    //! runtime that its argument is of the given type. Contract checks are introduced by a contract
    //! annotation or propagated via merging. They ensure sane interaction between typed and untyped
    //! parts.
    //!
    //! Conversely, any Nickel term seen as a contract corresponds to a type, which is opaque and can
    //! only be equated with itself.
    use crate::{
        error::{ParseError, ParseErrors},
        identifier::{Ident, LocIdent},
    };
    use std::{collections::HashSet, convert::Infallible};
    /// A record row, mapping an identifier to a type. A record type is a dictionary mapping
    /// identifiers to Nickel type. Record types are represented as sequences of `RecordRowF`, ending
    /// potentially with a type variable or `Dyn` in tail position.
    ///
    /// # Type parameters
    ///
    /// As other types with the `F` suffix, this type is parametrized by one or more recursive
    /// unfoldings (here, `Ty` for `TypeF`). See [`TypeF`] for more details.
    pub struct RecordRowF<Ty> {
        pub id: LocIdent,
        pub typ: Ty,
    }
    #[automatically_derived]
    impl<Ty: ::core::clone::Clone> ::core::clone::Clone for RecordRowF<Ty> {
        #[inline]
        fn clone(&self) -> RecordRowF<Ty> {
            RecordRowF {
                id: ::core::clone::Clone::clone(&self.id),
                typ: ::core::clone::Clone::clone(&self.typ),
            }
        }
    }
    #[automatically_derived]
    impl<Ty> ::core::marker::StructuralPartialEq for RecordRowF<Ty> {}
    #[automatically_derived]
    impl<Ty: ::core::cmp::PartialEq> ::core::cmp::PartialEq for RecordRowF<Ty> {
        #[inline]
        fn eq(&self, other: &RecordRowF<Ty>) -> bool {
            self.id == other.id && self.typ == other.typ
        }
    }
    #[automatically_derived]
    impl<Ty: ::core::cmp::Eq> ::core::cmp::Eq for RecordRowF<Ty> {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<LocIdent>;
            let _: ::core::cmp::AssertParamIsEq<Ty>;
        }
    }
    #[automatically_derived]
    impl<Ty: ::core::fmt::Debug> ::core::fmt::Debug for RecordRowF<Ty> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "RecordRowF",
                "id",
                &self.id,
                "typ",
                &&self.typ,
            )
        }
    }
    #[automatically_derived]
    ///An archived [`RecordRowF`]
    #[bytecheck(crate = ::rkyv::bytecheck)]
    #[repr(C)]
    pub struct ArchivedRecordRowF<Ty>
    where
        LocIdent: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
    {
        ///The archived counterpart of [`RecordRowF::id`]
        pub id: <LocIdent as ::rkyv::Archive>::Archived,
        ///The archived counterpart of [`RecordRowF::typ`]
        pub typ: <Ty as ::rkyv::Archive>::Archived,
    }
    #[automatically_derived]
    unsafe impl<
        Ty,
        __C: ::rkyv::bytecheck::rancor::Fallible + ?::core::marker::Sized,
    > ::rkyv::bytecheck::CheckBytes<__C> for ArchivedRecordRowF<Ty>
    where
        LocIdent: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        <__C as ::rkyv::bytecheck::rancor::Fallible>::Error: ::rkyv::bytecheck::rancor::Trace,
        <LocIdent as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
        <Ty as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
    {
        unsafe fn check_bytes(
            value: *const Self,
            context: &mut __C,
        ) -> ::core::result::Result<
            (),
            <__C as ::rkyv::bytecheck::rancor::Fallible>::Error,
        > {
            <<LocIdent as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                __C,
            >>::check_bytes(&raw const (*value).id, context)
                .map_err(|e| {
                    <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                        e,
                        ::rkyv::bytecheck::StructCheckContext {
                            struct_name: "ArchivedRecordRowF",
                            field_name: "id",
                        },
                    )
                })?;
            <<Ty as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                __C,
            >>::check_bytes(&raw const (*value).typ, context)
                .map_err(|e| {
                    <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                        e,
                        ::rkyv::bytecheck::StructCheckContext {
                            struct_name: "ArchivedRecordRowF",
                            field_name: "typ",
                        },
                    )
                })?;
            ::core::result::Result::Ok(())
        }
    }
    #[automatically_derived]
    ///The resolver for an archived [`RecordRowF`]
    pub struct RecordRowFResolver<Ty>
    where
        LocIdent: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
    {
        id: <LocIdent as ::rkyv::Archive>::Resolver,
        typ: <Ty as ::rkyv::Archive>::Resolver,
    }
    impl<Ty> ::rkyv::Archive for RecordRowF<Ty>
    where
        LocIdent: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
    {
        type Archived = ArchivedRecordRowF<Ty>;
        type Resolver = RecordRowFResolver<Ty>;
        #[allow(clippy::unit_arg)]
        fn resolve(&self, resolver: Self::Resolver, out: ::rkyv::Place<Self::Archived>) {
            let field_ptr = unsafe { &raw mut (*out.ptr()).id };
            let field_out = unsafe {
                ::rkyv::Place::from_field_unchecked(out, field_ptr)
            };
            <LocIdent as ::rkyv::Archive>::resolve(&self.id, resolver.id, field_out);
            let field_ptr = unsafe { &raw mut (*out.ptr()).typ };
            let field_out = unsafe {
                ::rkyv::Place::from_field_unchecked(out, field_ptr)
            };
            <Ty as ::rkyv::Archive>::resolve(&self.typ, resolver.typ, field_out);
        }
    }
    unsafe impl<Ty> ::rkyv::traits::Portable for ArchivedRecordRowF<Ty>
    where
        LocIdent: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        <LocIdent as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <Ty as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
    {}
    /// An enum row, mapping an identifier to an optional type. If the associated type is `None`, this
    /// represent a bare (unapplied) enum tag such as `'foo`. If the enum is applied (a variant), the
    /// type is store in `typ`. An enum type is a set of enum rows, represented as a sequence of
    /// `EnumRow`s, ending potentially with a type variable tail position.
    ///
    /// # Type parameters
    ///
    /// As other types with the `F` suffix, this type is parametrized by one or more recursive
    /// unfoldings (here, `Ty` for `TypeF`). See [`TypeF`] for more details.
    pub struct EnumRowF<Ty> {
        pub id: LocIdent,
        pub typ: Option<Ty>,
    }
    #[automatically_derived]
    impl<Ty: ::core::clone::Clone> ::core::clone::Clone for EnumRowF<Ty> {
        #[inline]
        fn clone(&self) -> EnumRowF<Ty> {
            EnumRowF {
                id: ::core::clone::Clone::clone(&self.id),
                typ: ::core::clone::Clone::clone(&self.typ),
            }
        }
    }
    #[automatically_derived]
    impl<Ty> ::core::marker::StructuralPartialEq for EnumRowF<Ty> {}
    #[automatically_derived]
    impl<Ty: ::core::cmp::PartialEq> ::core::cmp::PartialEq for EnumRowF<Ty> {
        #[inline]
        fn eq(&self, other: &EnumRowF<Ty>) -> bool {
            self.id == other.id && self.typ == other.typ
        }
    }
    #[automatically_derived]
    impl<Ty: ::core::cmp::Eq> ::core::cmp::Eq for EnumRowF<Ty> {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<LocIdent>;
            let _: ::core::cmp::AssertParamIsEq<Option<Ty>>;
        }
    }
    #[automatically_derived]
    impl<Ty: ::core::fmt::Debug> ::core::fmt::Debug for EnumRowF<Ty> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "EnumRowF",
                "id",
                &self.id,
                "typ",
                &&self.typ,
            )
        }
    }
    #[automatically_derived]
    ///An archived [`EnumRowF`]
    #[bytecheck(crate = ::rkyv::bytecheck)]
    #[repr(C)]
    pub struct ArchivedEnumRowF<Ty>
    where
        LocIdent: ::rkyv::Archive,
        Option<Ty>: ::rkyv::Archive,
    {
        ///The archived counterpart of [`EnumRowF::id`]
        pub id: <LocIdent as ::rkyv::Archive>::Archived,
        ///The archived counterpart of [`EnumRowF::typ`]
        pub typ: <Option<Ty> as ::rkyv::Archive>::Archived,
    }
    #[automatically_derived]
    unsafe impl<
        Ty,
        __C: ::rkyv::bytecheck::rancor::Fallible + ?::core::marker::Sized,
    > ::rkyv::bytecheck::CheckBytes<__C> for ArchivedEnumRowF<Ty>
    where
        LocIdent: ::rkyv::Archive,
        Option<Ty>: ::rkyv::Archive,
        <__C as ::rkyv::bytecheck::rancor::Fallible>::Error: ::rkyv::bytecheck::rancor::Trace,
        <LocIdent as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
        <Option<Ty> as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
    {
        unsafe fn check_bytes(
            value: *const Self,
            context: &mut __C,
        ) -> ::core::result::Result<
            (),
            <__C as ::rkyv::bytecheck::rancor::Fallible>::Error,
        > {
            <<LocIdent as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                __C,
            >>::check_bytes(&raw const (*value).id, context)
                .map_err(|e| {
                    <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                        e,
                        ::rkyv::bytecheck::StructCheckContext {
                            struct_name: "ArchivedEnumRowF",
                            field_name: "id",
                        },
                    )
                })?;
            <<Option<
                Ty,
            > as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                __C,
            >>::check_bytes(&raw const (*value).typ, context)
                .map_err(|e| {
                    <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                        e,
                        ::rkyv::bytecheck::StructCheckContext {
                            struct_name: "ArchivedEnumRowF",
                            field_name: "typ",
                        },
                    )
                })?;
            ::core::result::Result::Ok(())
        }
    }
    #[automatically_derived]
    ///The resolver for an archived [`EnumRowF`]
    pub struct EnumRowFResolver<Ty>
    where
        LocIdent: ::rkyv::Archive,
        Option<Ty>: ::rkyv::Archive,
    {
        id: <LocIdent as ::rkyv::Archive>::Resolver,
        typ: <Option<Ty> as ::rkyv::Archive>::Resolver,
    }
    impl<Ty> ::rkyv::Archive for EnumRowF<Ty>
    where
        LocIdent: ::rkyv::Archive,
        Option<Ty>: ::rkyv::Archive,
    {
        type Archived = ArchivedEnumRowF<Ty>;
        type Resolver = EnumRowFResolver<Ty>;
        #[allow(clippy::unit_arg)]
        fn resolve(&self, resolver: Self::Resolver, out: ::rkyv::Place<Self::Archived>) {
            let field_ptr = unsafe { &raw mut (*out.ptr()).id };
            let field_out = unsafe {
                ::rkyv::Place::from_field_unchecked(out, field_ptr)
            };
            <LocIdent as ::rkyv::Archive>::resolve(&self.id, resolver.id, field_out);
            let field_ptr = unsafe { &raw mut (*out.ptr()).typ };
            let field_out = unsafe {
                ::rkyv::Place::from_field_unchecked(out, field_ptr)
            };
            <Option<Ty> as ::rkyv::Archive>::resolve(&self.typ, resolver.typ, field_out);
        }
    }
    unsafe impl<Ty> ::rkyv::traits::Portable for ArchivedEnumRowF<Ty>
    where
        LocIdent: ::rkyv::Archive,
        Option<Ty>: ::rkyv::Archive,
        <LocIdent as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <Option<Ty> as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
    {}
    /// Generic sequence of record rows potentially with a type variable or `Dyn` in tail position.
    ///
    /// As other types with the `F` suffix, this type is parametrized by one or more recursive
    /// unfoldings. See [`TypeF`] for more details.
    ///
    /// # Type parameters
    ///
    /// - `Ty` is the recursive unfolding of a Nickel type stored inside one row. In practice, a
    ///   wrapper around an instantiation of `TypeF`.
    /// - `RRows` is the recursive unfolding of record rows (the tail of this row sequence). In
    ///   practice, a wrapper around an instantiation of `RecordRowsF`.
    pub enum RecordRowsF<Ty, RRows> {
        Empty,
        Extend { row: RecordRowF<Ty>, tail: RRows },
        TailVar(LocIdent),
        TailDyn,
    }
    #[automatically_derived]
    impl<Ty: ::core::clone::Clone, RRows: ::core::clone::Clone> ::core::clone::Clone
    for RecordRowsF<Ty, RRows> {
        #[inline]
        fn clone(&self) -> RecordRowsF<Ty, RRows> {
            match self {
                RecordRowsF::Empty => RecordRowsF::Empty,
                RecordRowsF::Extend { row: __self_0, tail: __self_1 } => {
                    RecordRowsF::Extend {
                        row: ::core::clone::Clone::clone(__self_0),
                        tail: ::core::clone::Clone::clone(__self_1),
                    }
                }
                RecordRowsF::TailVar(__self_0) => {
                    RecordRowsF::TailVar(::core::clone::Clone::clone(__self_0))
                }
                RecordRowsF::TailDyn => RecordRowsF::TailDyn,
            }
        }
    }
    #[automatically_derived]
    impl<Ty, RRows> ::core::marker::StructuralPartialEq for RecordRowsF<Ty, RRows> {}
    #[automatically_derived]
    impl<
        Ty: ::core::cmp::PartialEq,
        RRows: ::core::cmp::PartialEq,
    > ::core::cmp::PartialEq for RecordRowsF<Ty, RRows> {
        #[inline]
        fn eq(&self, other: &RecordRowsF<Ty, RRows>) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
                && match (self, other) {
                    (
                        RecordRowsF::Extend { row: __self_0, tail: __self_1 },
                        RecordRowsF::Extend { row: __arg1_0, tail: __arg1_1 },
                    ) => __self_0 == __arg1_0 && __self_1 == __arg1_1,
                    (RecordRowsF::TailVar(__self_0), RecordRowsF::TailVar(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    _ => true,
                }
        }
    }
    #[automatically_derived]
    impl<Ty: ::core::cmp::Eq, RRows: ::core::cmp::Eq> ::core::cmp::Eq
    for RecordRowsF<Ty, RRows> {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<RecordRowF<Ty>>;
            let _: ::core::cmp::AssertParamIsEq<RRows>;
            let _: ::core::cmp::AssertParamIsEq<LocIdent>;
        }
    }
    #[automatically_derived]
    impl<Ty: ::core::fmt::Debug, RRows: ::core::fmt::Debug> ::core::fmt::Debug
    for RecordRowsF<Ty, RRows> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                RecordRowsF::Empty => ::core::fmt::Formatter::write_str(f, "Empty"),
                RecordRowsF::Extend { row: __self_0, tail: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Extend",
                        "row",
                        __self_0,
                        "tail",
                        &__self_1,
                    )
                }
                RecordRowsF::TailVar(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "TailVar",
                        &__self_0,
                    )
                }
                RecordRowsF::TailDyn => ::core::fmt::Formatter::write_str(f, "TailDyn"),
            }
        }
    }
    #[automatically_derived]
    ///An archived [`RecordRowsF`]
    #[bytecheck(crate = ::rkyv::bytecheck)]
    #[repr(u8)]
    pub enum ArchivedRecordRowsF<Ty, RRows>
    where
        RecordRowF<Ty>: ::rkyv::Archive,
        RRows: ::rkyv::Archive,
        LocIdent: ::rkyv::Archive,
    {
        ///The archived counterpart of [`RecordRowsF::Empty`]
        #[allow(dead_code)]
        Empty,
        ///The archived counterpart of [`RecordRowsF::Extend`]
        #[allow(dead_code)]
        Extend {
            ///The archived counterpart of [`RecordRowsF::Extend::row`]
            row: <RecordRowF<Ty> as ::rkyv::Archive>::Archived,
            ///The archived counterpart of [`RecordRowsF::Extend::tail`]
            tail: <RRows as ::rkyv::Archive>::Archived,
        },
        ///The archived counterpart of [`RecordRowsF::TailVar`]
        #[allow(dead_code)]
        TailVar(
            ///The archived counterpart of [`RecordRowsF::TailVar::0`]
            <LocIdent as ::rkyv::Archive>::Archived,
        ),
        ///The archived counterpart of [`RecordRowsF::TailDyn`]
        #[allow(dead_code)]
        TailDyn,
    }
    const _: () = {
        #[repr(u8)]
        enum Tag {
            Empty,
            Extend,
            TailVar,
            TailDyn,
        }
        struct Discriminant;
        #[automatically_derived]
        impl Discriminant {
            #[allow(non_upper_case_globals)]
            const Empty: u8 = Tag::Empty as u8;
            #[allow(non_upper_case_globals)]
            const Extend: u8 = Tag::Extend as u8;
            #[allow(non_upper_case_globals)]
            const TailVar: u8 = Tag::TailVar as u8;
            #[allow(non_upper_case_globals)]
            const TailDyn: u8 = Tag::TailDyn as u8;
        }
        #[repr(C)]
        struct VariantExtend<Ty, RRows>
        where
            RecordRowF<Ty>: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
        {
            __tag: Tag,
            row: <RecordRowF<Ty> as ::rkyv::Archive>::Archived,
            tail: <RRows as ::rkyv::Archive>::Archived,
            __phantom: ::core::marker::PhantomData<ArchivedRecordRowsF<Ty, RRows>>,
        }
        #[repr(C)]
        struct VariantTailVar<Ty, RRows>(
            Tag,
            <LocIdent as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<ArchivedRecordRowsF<Ty, RRows>>,
        )
        where
            RecordRowF<Ty>: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive;
        #[automatically_derived]
        unsafe impl<
            Ty,
            RRows,
            __C: ::rkyv::bytecheck::rancor::Fallible + ?::core::marker::Sized,
        > ::rkyv::bytecheck::CheckBytes<__C> for ArchivedRecordRowsF<Ty, RRows>
        where
            RecordRowF<Ty>: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            <__C as ::rkyv::bytecheck::rancor::Fallible>::Error: ::rkyv::bytecheck::rancor::Source,
            <RecordRowF<
                Ty,
            > as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <RRows as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <LocIdent as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
        {
            unsafe fn check_bytes(
                value: *const Self,
                context: &mut __C,
            ) -> ::core::result::Result<
                (),
                <__C as ::rkyv::bytecheck::rancor::Fallible>::Error,
            > {
                let tag = *value.cast::<u8>();
                match tag {
                    Discriminant::Empty => {}
                    Discriminant::Extend => {
                        let value = value.cast::<VariantExtend<Ty, RRows>>();
                        <<RecordRowF<
                            Ty,
                        > as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).row, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::NamedEnumVariantCheckContext {
                                        enum_name: "ArchivedRecordRowsF",
                                        variant_name: "Extend",
                                        field_name: "row",
                                    },
                                )
                            })?;
                        <<RRows as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).tail, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::NamedEnumVariantCheckContext {
                                        enum_name: "ArchivedRecordRowsF",
                                        variant_name: "Extend",
                                        field_name: "tail",
                                    },
                                )
                            })?;
                    }
                    Discriminant::TailVar => {
                        let value = value.cast::<VariantTailVar<Ty, RRows>>();
                        <<LocIdent as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).1, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::UnnamedEnumVariantCheckContext {
                                        enum_name: "ArchivedRecordRowsF",
                                        variant_name: "TailVar",
                                        field_index: 1,
                                    },
                                )
                            })?;
                    }
                    Discriminant::TailDyn => {}
                    _ => {
                        return ::core::result::Result::Err(
                            <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Source>::new(::rkyv::bytecheck::InvalidEnumDiscriminantError {
                                enum_name: "ArchivedRecordRowsF",
                                invalid_discriminant: tag,
                            }),
                        );
                    }
                }
                ::core::result::Result::Ok(())
            }
        }
    };
    #[automatically_derived]
    ///The resolver for an archived [`RecordRowsF`]
    pub enum RecordRowsFResolver<Ty, RRows>
    where
        RecordRowF<Ty>: ::rkyv::Archive,
        RRows: ::rkyv::Archive,
        LocIdent: ::rkyv::Archive,
    {
        ///The resolver for [`RecordRowsF::Empty`]
        #[allow(dead_code)]
        Empty,
        ///The resolver for [`RecordRowsF::Extend`]
        #[allow(dead_code)]
        Extend {
            row: <RecordRowF<Ty> as ::rkyv::Archive>::Resolver,
            tail: <RRows as ::rkyv::Archive>::Resolver,
        },
        ///The resolver for [`RecordRowsF::TailVar`]
        #[allow(dead_code)]
        TailVar(<LocIdent as ::rkyv::Archive>::Resolver),
        ///The resolver for [`RecordRowsF::TailDyn`]
        #[allow(dead_code)]
        TailDyn,
    }
    const _: () = {
        #[repr(u8)]
        enum ArchivedTag {
            Empty,
            Extend,
            TailVar,
            TailDyn,
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for ArchivedTag {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for ArchivedTag {
            #[inline]
            fn eq(&self, other: &ArchivedTag) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
            }
        }
        #[automatically_derived]
        impl ::core::cmp::PartialOrd for ArchivedTag {
            #[inline]
            fn partial_cmp(
                &self,
                other: &ArchivedTag,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                ::core::cmp::PartialOrd::partial_cmp(&__self_discr, &__arg1_discr)
            }
        }
        #[repr(C)]
        struct ArchivedVariantExtend<Ty, RRows>
        where
            RecordRowF<Ty>: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
        {
            __tag: ArchivedTag,
            row: <RecordRowF<Ty> as ::rkyv::Archive>::Archived,
            tail: <RRows as ::rkyv::Archive>::Archived,
            __phantom: ::core::marker::PhantomData<RecordRowsF<Ty, RRows>>,
        }
        #[repr(C)]
        struct ArchivedVariantTailVar<Ty, RRows>(
            ArchivedTag,
            <LocIdent as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<RecordRowsF<Ty, RRows>>,
        )
        where
            RecordRowF<Ty>: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive;
        impl<Ty, RRows> ::rkyv::Archive for RecordRowsF<Ty, RRows>
        where
            RecordRowF<Ty>: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
        {
            type Archived = ArchivedRecordRowsF<Ty, RRows>;
            type Resolver = RecordRowsFResolver<Ty, RRows>;
            #[allow(clippy::unit_arg)]
            fn resolve(
                &self,
                resolver: Self::Resolver,
                out: ::rkyv::Place<Self::Archived>,
            ) {
                let __this = self;
                match resolver {
                    RecordRowsFResolver::Empty => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::Empty);
                        }
                    }
                    RecordRowsFResolver::Extend { row: resolver_0, tail: resolver_1 } => {
                        match __this {
                            RecordRowsF::Extend { row: self_0, tail: self_1, .. } => {
                                let out = unsafe {
                                    out.cast_unchecked::<ArchivedVariantExtend<Ty, RRows>>()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).__tag };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::Extend);
                                }
                                let field_ptr = unsafe { &raw mut (*out.ptr()).row };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <RecordRowF<
                                    Ty,
                                > as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                                let field_ptr = unsafe { &raw mut (*out.ptr()).tail };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <RRows as ::rkyv::Archive>::resolve(
                                    self_1,
                                    resolver_1,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                    RecordRowsFResolver::TailVar(resolver_0) => {
                        match __this {
                            RecordRowsF::TailVar(self_0, ..) => {
                                let out = unsafe {
                                    out.cast_unchecked::<ArchivedVariantTailVar<Ty, RRows>>()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).0 };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::TailVar);
                                }
                                let field_ptr = unsafe { &raw mut (*out.ptr()).1 };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <LocIdent as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                    RecordRowsFResolver::TailDyn => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::TailDyn);
                        }
                    }
                }
            }
        }
    };
    unsafe impl<Ty, RRows> ::rkyv::traits::Portable for ArchivedRecordRowsF<Ty, RRows>
    where
        RecordRowF<Ty>: ::rkyv::Archive,
        RRows: ::rkyv::Archive,
        LocIdent: ::rkyv::Archive,
        <RecordRowF<Ty> as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <RRows as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <LocIdent as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
    {}
    /// Generic sequence of enum rows potentially with a type variable in tail position.
    ///
    /// As other types with the `F` suffix, this type is parametrized by one or more recursive
    /// unfoldings. See [`TypeF`] for more details.
    ///
    /// # Type parameters
    ///
    /// - `ERows` is the recursive unfolding of enum rows (the tail of this row sequence). In practice,
    ///   a wrapper around `EnumRowsF`.
    pub enum EnumRowsF<Ty, ERows> {
        Empty,
        Extend { row: EnumRowF<Ty>, tail: ERows },
        TailVar(LocIdent),
    }
    #[automatically_derived]
    impl<Ty: ::core::clone::Clone, ERows: ::core::clone::Clone> ::core::clone::Clone
    for EnumRowsF<Ty, ERows> {
        #[inline]
        fn clone(&self) -> EnumRowsF<Ty, ERows> {
            match self {
                EnumRowsF::Empty => EnumRowsF::Empty,
                EnumRowsF::Extend { row: __self_0, tail: __self_1 } => {
                    EnumRowsF::Extend {
                        row: ::core::clone::Clone::clone(__self_0),
                        tail: ::core::clone::Clone::clone(__self_1),
                    }
                }
                EnumRowsF::TailVar(__self_0) => {
                    EnumRowsF::TailVar(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    #[automatically_derived]
    impl<Ty, ERows> ::core::marker::StructuralPartialEq for EnumRowsF<Ty, ERows> {}
    #[automatically_derived]
    impl<
        Ty: ::core::cmp::PartialEq,
        ERows: ::core::cmp::PartialEq,
    > ::core::cmp::PartialEq for EnumRowsF<Ty, ERows> {
        #[inline]
        fn eq(&self, other: &EnumRowsF<Ty, ERows>) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
                && match (self, other) {
                    (
                        EnumRowsF::Extend { row: __self_0, tail: __self_1 },
                        EnumRowsF::Extend { row: __arg1_0, tail: __arg1_1 },
                    ) => __self_0 == __arg1_0 && __self_1 == __arg1_1,
                    (EnumRowsF::TailVar(__self_0), EnumRowsF::TailVar(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    _ => true,
                }
        }
    }
    #[automatically_derived]
    impl<Ty: ::core::cmp::Eq, ERows: ::core::cmp::Eq> ::core::cmp::Eq
    for EnumRowsF<Ty, ERows> {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<EnumRowF<Ty>>;
            let _: ::core::cmp::AssertParamIsEq<ERows>;
            let _: ::core::cmp::AssertParamIsEq<LocIdent>;
        }
    }
    #[automatically_derived]
    impl<Ty: ::core::fmt::Debug, ERows: ::core::fmt::Debug> ::core::fmt::Debug
    for EnumRowsF<Ty, ERows> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                EnumRowsF::Empty => ::core::fmt::Formatter::write_str(f, "Empty"),
                EnumRowsF::Extend { row: __self_0, tail: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Extend",
                        "row",
                        __self_0,
                        "tail",
                        &__self_1,
                    )
                }
                EnumRowsF::TailVar(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "TailVar",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    ///An archived [`EnumRowsF`]
    #[bytecheck(crate = ::rkyv::bytecheck)]
    #[repr(u8)]
    pub enum ArchivedEnumRowsF<Ty, ERows>
    where
        EnumRowF<Ty>: ::rkyv::Archive,
        ERows: ::rkyv::Archive,
        LocIdent: ::rkyv::Archive,
    {
        ///The archived counterpart of [`EnumRowsF::Empty`]
        #[allow(dead_code)]
        Empty,
        ///The archived counterpart of [`EnumRowsF::Extend`]
        #[allow(dead_code)]
        Extend {
            ///The archived counterpart of [`EnumRowsF::Extend::row`]
            row: <EnumRowF<Ty> as ::rkyv::Archive>::Archived,
            ///The archived counterpart of [`EnumRowsF::Extend::tail`]
            tail: <ERows as ::rkyv::Archive>::Archived,
        },
        ///The archived counterpart of [`EnumRowsF::TailVar`]
        #[allow(dead_code)]
        TailVar(
            ///The archived counterpart of [`EnumRowsF::TailVar::0`]
            <LocIdent as ::rkyv::Archive>::Archived,
        ),
    }
    const _: () = {
        #[repr(u8)]
        enum Tag {
            Empty,
            Extend,
            TailVar,
        }
        struct Discriminant;
        #[automatically_derived]
        impl Discriminant {
            #[allow(non_upper_case_globals)]
            const Empty: u8 = Tag::Empty as u8;
            #[allow(non_upper_case_globals)]
            const Extend: u8 = Tag::Extend as u8;
            #[allow(non_upper_case_globals)]
            const TailVar: u8 = Tag::TailVar as u8;
        }
        #[repr(C)]
        struct VariantExtend<Ty, ERows>
        where
            EnumRowF<Ty>: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
        {
            __tag: Tag,
            row: <EnumRowF<Ty> as ::rkyv::Archive>::Archived,
            tail: <ERows as ::rkyv::Archive>::Archived,
            __phantom: ::core::marker::PhantomData<ArchivedEnumRowsF<Ty, ERows>>,
        }
        #[repr(C)]
        struct VariantTailVar<Ty, ERows>(
            Tag,
            <LocIdent as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<ArchivedEnumRowsF<Ty, ERows>>,
        )
        where
            EnumRowF<Ty>: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive;
        #[automatically_derived]
        unsafe impl<
            Ty,
            ERows,
            __C: ::rkyv::bytecheck::rancor::Fallible + ?::core::marker::Sized,
        > ::rkyv::bytecheck::CheckBytes<__C> for ArchivedEnumRowsF<Ty, ERows>
        where
            EnumRowF<Ty>: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            <__C as ::rkyv::bytecheck::rancor::Fallible>::Error: ::rkyv::bytecheck::rancor::Source,
            <EnumRowF<
                Ty,
            > as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <ERows as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <LocIdent as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
        {
            unsafe fn check_bytes(
                value: *const Self,
                context: &mut __C,
            ) -> ::core::result::Result<
                (),
                <__C as ::rkyv::bytecheck::rancor::Fallible>::Error,
            > {
                let tag = *value.cast::<u8>();
                match tag {
                    Discriminant::Empty => {}
                    Discriminant::Extend => {
                        let value = value.cast::<VariantExtend<Ty, ERows>>();
                        <<EnumRowF<
                            Ty,
                        > as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).row, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::NamedEnumVariantCheckContext {
                                        enum_name: "ArchivedEnumRowsF",
                                        variant_name: "Extend",
                                        field_name: "row",
                                    },
                                )
                            })?;
                        <<ERows as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).tail, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::NamedEnumVariantCheckContext {
                                        enum_name: "ArchivedEnumRowsF",
                                        variant_name: "Extend",
                                        field_name: "tail",
                                    },
                                )
                            })?;
                    }
                    Discriminant::TailVar => {
                        let value = value.cast::<VariantTailVar<Ty, ERows>>();
                        <<LocIdent as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).1, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::UnnamedEnumVariantCheckContext {
                                        enum_name: "ArchivedEnumRowsF",
                                        variant_name: "TailVar",
                                        field_index: 1,
                                    },
                                )
                            })?;
                    }
                    _ => {
                        return ::core::result::Result::Err(
                            <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Source>::new(::rkyv::bytecheck::InvalidEnumDiscriminantError {
                                enum_name: "ArchivedEnumRowsF",
                                invalid_discriminant: tag,
                            }),
                        );
                    }
                }
                ::core::result::Result::Ok(())
            }
        }
    };
    #[automatically_derived]
    ///The resolver for an archived [`EnumRowsF`]
    pub enum EnumRowsFResolver<Ty, ERows>
    where
        EnumRowF<Ty>: ::rkyv::Archive,
        ERows: ::rkyv::Archive,
        LocIdent: ::rkyv::Archive,
    {
        ///The resolver for [`EnumRowsF::Empty`]
        #[allow(dead_code)]
        Empty,
        ///The resolver for [`EnumRowsF::Extend`]
        #[allow(dead_code)]
        Extend {
            row: <EnumRowF<Ty> as ::rkyv::Archive>::Resolver,
            tail: <ERows as ::rkyv::Archive>::Resolver,
        },
        ///The resolver for [`EnumRowsF::TailVar`]
        #[allow(dead_code)]
        TailVar(<LocIdent as ::rkyv::Archive>::Resolver),
    }
    const _: () = {
        #[repr(u8)]
        enum ArchivedTag {
            Empty,
            Extend,
            TailVar,
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for ArchivedTag {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for ArchivedTag {
            #[inline]
            fn eq(&self, other: &ArchivedTag) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
            }
        }
        #[automatically_derived]
        impl ::core::cmp::PartialOrd for ArchivedTag {
            #[inline]
            fn partial_cmp(
                &self,
                other: &ArchivedTag,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                ::core::cmp::PartialOrd::partial_cmp(&__self_discr, &__arg1_discr)
            }
        }
        #[repr(C)]
        struct ArchivedVariantExtend<Ty, ERows>
        where
            EnumRowF<Ty>: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
        {
            __tag: ArchivedTag,
            row: <EnumRowF<Ty> as ::rkyv::Archive>::Archived,
            tail: <ERows as ::rkyv::Archive>::Archived,
            __phantom: ::core::marker::PhantomData<EnumRowsF<Ty, ERows>>,
        }
        #[repr(C)]
        struct ArchivedVariantTailVar<Ty, ERows>(
            ArchivedTag,
            <LocIdent as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<EnumRowsF<Ty, ERows>>,
        )
        where
            EnumRowF<Ty>: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive;
        impl<Ty, ERows> ::rkyv::Archive for EnumRowsF<Ty, ERows>
        where
            EnumRowF<Ty>: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
        {
            type Archived = ArchivedEnumRowsF<Ty, ERows>;
            type Resolver = EnumRowsFResolver<Ty, ERows>;
            #[allow(clippy::unit_arg)]
            fn resolve(
                &self,
                resolver: Self::Resolver,
                out: ::rkyv::Place<Self::Archived>,
            ) {
                let __this = self;
                match resolver {
                    EnumRowsFResolver::Empty => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::Empty);
                        }
                    }
                    EnumRowsFResolver::Extend { row: resolver_0, tail: resolver_1 } => {
                        match __this {
                            EnumRowsF::Extend { row: self_0, tail: self_1, .. } => {
                                let out = unsafe {
                                    out.cast_unchecked::<ArchivedVariantExtend<Ty, ERows>>()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).__tag };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::Extend);
                                }
                                let field_ptr = unsafe { &raw mut (*out.ptr()).row };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <EnumRowF<
                                    Ty,
                                > as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                                let field_ptr = unsafe { &raw mut (*out.ptr()).tail };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <ERows as ::rkyv::Archive>::resolve(
                                    self_1,
                                    resolver_1,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                    EnumRowsFResolver::TailVar(resolver_0) => {
                        match __this {
                            EnumRowsF::TailVar(self_0, ..) => {
                                let out = unsafe {
                                    out.cast_unchecked::<ArchivedVariantTailVar<Ty, ERows>>()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).0 };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::TailVar);
                                }
                                let field_ptr = unsafe { &raw mut (*out.ptr()).1 };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <LocIdent as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                }
            }
        }
    };
    unsafe impl<Ty, ERows> ::rkyv::traits::Portable for ArchivedEnumRowsF<Ty, ERows>
    where
        EnumRowF<Ty>: ::rkyv::Archive,
        ERows: ::rkyv::Archive,
        LocIdent: ::rkyv::Archive,
        <EnumRowF<Ty> as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <ERows as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <LocIdent as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
    {}
    /// The kind of a quantified type variable.
    ///
    /// Nickel uses several forms of polymorphism. A type variable can be substituted for a type, as in
    /// `id : forall a. a -> a`, for record rows as in `access_foo : forall a . {foo : Number; a} ->
    /// Number}`, or for enum rows. This information is implicit in the source syntax: we don't require
    /// users to write e.g. `forall a :: Type` or `forall a :: Rows`. But the kind of a variable is
    /// required for the typechecker. It is thus determined during parsing and stored as `VarKind` where
    /// type variables are introduced, that is, on forall quantifiers.
    pub enum VarKind {
        #[default]
        Type,
        /// `excluded` keeps track of which rows appear somewhere alongside the tail, and therefore
        /// cannot appear in the tail. For instance `forall r. { ; r } -> { x : Number ; r }` assumes
        EnumRows { excluded: HashSet<Ident> },
        /// Same as for [Self::EnumRows].
        RecordRows { excluded: HashSet<Ident> },
    }
    #[automatically_derived]
    impl ::core::clone::Clone for VarKind {
        #[inline]
        fn clone(&self) -> VarKind {
            match self {
                VarKind::Type => VarKind::Type,
                VarKind::EnumRows { excluded: __self_0 } => {
                    VarKind::EnumRows {
                        excluded: ::core::clone::Clone::clone(__self_0),
                    }
                }
                VarKind::RecordRows { excluded: __self_0 } => {
                    VarKind::RecordRows {
                        excluded: ::core::clone::Clone::clone(__self_0),
                    }
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for VarKind {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for VarKind {
        #[inline]
        fn eq(&self, other: &VarKind) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
                && match (self, other) {
                    (
                        VarKind::EnumRows { excluded: __self_0 },
                        VarKind::EnumRows { excluded: __arg1_0 },
                    ) => __self_0 == __arg1_0,
                    (
                        VarKind::RecordRows { excluded: __self_0 },
                        VarKind::RecordRows { excluded: __arg1_0 },
                    ) => __self_0 == __arg1_0,
                    _ => true,
                }
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for VarKind {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<HashSet<Ident>>;
            let _: ::core::cmp::AssertParamIsEq<HashSet<Ident>>;
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for VarKind {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                VarKind::Type => ::core::fmt::Formatter::write_str(f, "Type"),
                VarKind::EnumRows { excluded: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "EnumRows",
                        "excluded",
                        &__self_0,
                    )
                }
                VarKind::RecordRows { excluded: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "RecordRows",
                        "excluded",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::default::Default for VarKind {
        #[inline]
        fn default() -> VarKind {
            Self::Type
        }
    }
    #[automatically_derived]
    ///An archived [`VarKind`]
    #[bytecheck(crate = ::rkyv::bytecheck)]
    #[repr(u8)]
    pub enum ArchivedVarKind
    where
        HashSet<Ident>: ::rkyv::Archive,
        HashSet<Ident>: ::rkyv::Archive,
    {
        ///The archived counterpart of [`VarKind::Type`]
        #[allow(dead_code)]
        Type,
        ///The archived counterpart of [`VarKind::EnumRows`]
        #[allow(dead_code)]
        EnumRows {
            ///The archived counterpart of [`VarKind::EnumRows::excluded`]
            excluded: <HashSet<Ident> as ::rkyv::Archive>::Archived,
        },
        ///The archived counterpart of [`VarKind::RecordRows`]
        #[allow(dead_code)]
        RecordRows {
            ///The archived counterpart of [`VarKind::RecordRows::excluded`]
            excluded: <HashSet<Ident> as ::rkyv::Archive>::Archived,
        },
    }
    const _: () = {
        #[repr(u8)]
        enum Tag {
            Type,
            EnumRows,
            RecordRows,
        }
        struct Discriminant;
        #[automatically_derived]
        impl Discriminant {
            #[allow(non_upper_case_globals)]
            const Type: u8 = Tag::Type as u8;
            #[allow(non_upper_case_globals)]
            const EnumRows: u8 = Tag::EnumRows as u8;
            #[allow(non_upper_case_globals)]
            const RecordRows: u8 = Tag::RecordRows as u8;
        }
        #[repr(C)]
        struct VariantEnumRows
        where
            HashSet<Ident>: ::rkyv::Archive,
            HashSet<Ident>: ::rkyv::Archive,
        {
            __tag: Tag,
            excluded: <HashSet<Ident> as ::rkyv::Archive>::Archived,
            __phantom: ::core::marker::PhantomData<ArchivedVarKind>,
        }
        #[repr(C)]
        struct VariantRecordRows
        where
            HashSet<Ident>: ::rkyv::Archive,
            HashSet<Ident>: ::rkyv::Archive,
        {
            __tag: Tag,
            excluded: <HashSet<Ident> as ::rkyv::Archive>::Archived,
            __phantom: ::core::marker::PhantomData<ArchivedVarKind>,
        }
        #[automatically_derived]
        unsafe impl<
            __C: ::rkyv::bytecheck::rancor::Fallible + ?::core::marker::Sized,
        > ::rkyv::bytecheck::CheckBytes<__C> for ArchivedVarKind
        where
            HashSet<Ident>: ::rkyv::Archive,
            HashSet<Ident>: ::rkyv::Archive,
            <__C as ::rkyv::bytecheck::rancor::Fallible>::Error: ::rkyv::bytecheck::rancor::Source,
            <HashSet<
                Ident,
            > as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <HashSet<
                Ident,
            > as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
        {
            unsafe fn check_bytes(
                value: *const Self,
                context: &mut __C,
            ) -> ::core::result::Result<
                (),
                <__C as ::rkyv::bytecheck::rancor::Fallible>::Error,
            > {
                let tag = *value.cast::<u8>();
                match tag {
                    Discriminant::Type => {}
                    Discriminant::EnumRows => {
                        let value = value.cast::<VariantEnumRows>();
                        <<HashSet<
                            Ident,
                        > as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).excluded, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::NamedEnumVariantCheckContext {
                                        enum_name: "ArchivedVarKind",
                                        variant_name: "EnumRows",
                                        field_name: "excluded",
                                    },
                                )
                            })?;
                    }
                    Discriminant::RecordRows => {
                        let value = value.cast::<VariantRecordRows>();
                        <<HashSet<
                            Ident,
                        > as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).excluded, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::NamedEnumVariantCheckContext {
                                        enum_name: "ArchivedVarKind",
                                        variant_name: "RecordRows",
                                        field_name: "excluded",
                                    },
                                )
                            })?;
                    }
                    _ => {
                        return ::core::result::Result::Err(
                            <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Source>::new(::rkyv::bytecheck::InvalidEnumDiscriminantError {
                                enum_name: "ArchivedVarKind",
                                invalid_discriminant: tag,
                            }),
                        );
                    }
                }
                ::core::result::Result::Ok(())
            }
        }
    };
    #[automatically_derived]
    ///The resolver for an archived [`VarKind`]
    pub enum VarKindResolver
    where
        HashSet<Ident>: ::rkyv::Archive,
        HashSet<Ident>: ::rkyv::Archive,
    {
        ///The resolver for [`VarKind::Type`]
        #[allow(dead_code)]
        Type,
        ///The resolver for [`VarKind::EnumRows`]
        #[allow(dead_code)]
        EnumRows { excluded: <HashSet<Ident> as ::rkyv::Archive>::Resolver },
        ///The resolver for [`VarKind::RecordRows`]
        #[allow(dead_code)]
        RecordRows { excluded: <HashSet<Ident> as ::rkyv::Archive>::Resolver },
    }
    const _: () = {
        #[repr(u8)]
        enum ArchivedTag {
            Type,
            EnumRows,
            RecordRows,
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for ArchivedTag {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for ArchivedTag {
            #[inline]
            fn eq(&self, other: &ArchivedTag) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
            }
        }
        #[automatically_derived]
        impl ::core::cmp::PartialOrd for ArchivedTag {
            #[inline]
            fn partial_cmp(
                &self,
                other: &ArchivedTag,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                ::core::cmp::PartialOrd::partial_cmp(&__self_discr, &__arg1_discr)
            }
        }
        #[repr(C)]
        struct ArchivedVariantEnumRows
        where
            HashSet<Ident>: ::rkyv::Archive,
            HashSet<Ident>: ::rkyv::Archive,
        {
            __tag: ArchivedTag,
            excluded: <HashSet<Ident> as ::rkyv::Archive>::Archived,
            __phantom: ::core::marker::PhantomData<VarKind>,
        }
        #[repr(C)]
        struct ArchivedVariantRecordRows
        where
            HashSet<Ident>: ::rkyv::Archive,
            HashSet<Ident>: ::rkyv::Archive,
        {
            __tag: ArchivedTag,
            excluded: <HashSet<Ident> as ::rkyv::Archive>::Archived,
            __phantom: ::core::marker::PhantomData<VarKind>,
        }
        impl ::rkyv::Archive for VarKind
        where
            HashSet<Ident>: ::rkyv::Archive,
            HashSet<Ident>: ::rkyv::Archive,
        {
            type Archived = ArchivedVarKind;
            type Resolver = VarKindResolver;
            #[allow(clippy::unit_arg)]
            fn resolve(
                &self,
                resolver: Self::Resolver,
                out: ::rkyv::Place<Self::Archived>,
            ) {
                let __this = self;
                match resolver {
                    VarKindResolver::Type => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::Type);
                        }
                    }
                    VarKindResolver::EnumRows { excluded: resolver_0 } => {
                        match __this {
                            VarKind::EnumRows { excluded: self_0, .. } => {
                                let out = unsafe {
                                    out.cast_unchecked::<ArchivedVariantEnumRows>()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).__tag };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::EnumRows);
                                }
                                let field_ptr = unsafe { &raw mut (*out.ptr()).excluded };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <HashSet<
                                    Ident,
                                > as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                    VarKindResolver::RecordRows { excluded: resolver_0 } => {
                        match __this {
                            VarKind::RecordRows { excluded: self_0, .. } => {
                                let out = unsafe {
                                    out.cast_unchecked::<ArchivedVariantRecordRows>()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).__tag };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::RecordRows);
                                }
                                let field_ptr = unsafe { &raw mut (*out.ptr()).excluded };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <HashSet<
                                    Ident,
                                > as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                }
            }
        }
    };
    unsafe impl ::rkyv::traits::Portable for ArchivedVarKind
    where
        HashSet<Ident>: ::rkyv::Archive,
        HashSet<Ident>: ::rkyv::Archive,
        <HashSet<Ident> as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <HashSet<Ident> as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
    {}
    /// Equivalent to `std::mem::Discriminant<VarKind>`, but we can do things like match on it
    pub enum VarKindDiscriminant {
        Type,
        EnumRows,
        RecordRows,
    }
    #[automatically_derived]
    impl ::core::marker::Copy for VarKindDiscriminant {}
    #[automatically_derived]
    #[doc(hidden)]
    unsafe impl ::core::clone::TrivialClone for VarKindDiscriminant {}
    #[automatically_derived]
    impl ::core::clone::Clone for VarKindDiscriminant {
        #[inline]
        fn clone(&self) -> VarKindDiscriminant {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for VarKindDiscriminant {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for VarKindDiscriminant {
        #[inline]
        fn eq(&self, other: &VarKindDiscriminant) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for VarKindDiscriminant {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    #[automatically_derived]
    impl ::core::hash::Hash for VarKindDiscriminant {
        #[inline]
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            ::core::hash::Hash::hash(&__self_discr, state)
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for VarKindDiscriminant {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    VarKindDiscriminant::Type => "Type",
                    VarKindDiscriminant::EnumRows => "EnumRows",
                    VarKindDiscriminant::RecordRows => "RecordRows",
                },
            )
        }
    }
    #[automatically_derived]
    ///An archived [`VarKindDiscriminant`]
    #[bytecheck(crate = ::rkyv::bytecheck)]
    #[repr(u8)]
    pub enum ArchivedVarKindDiscriminant {
        ///The archived counterpart of [`VarKindDiscriminant::Type`]
        #[allow(dead_code)]
        Type,
        ///The archived counterpart of [`VarKindDiscriminant::EnumRows`]
        #[allow(dead_code)]
        EnumRows,
        ///The archived counterpart of [`VarKindDiscriminant::RecordRows`]
        #[allow(dead_code)]
        RecordRows,
    }
    const _: () = {
        #[repr(u8)]
        enum Tag {
            Type,
            EnumRows,
            RecordRows,
        }
        struct Discriminant;
        #[automatically_derived]
        impl Discriminant {
            #[allow(non_upper_case_globals)]
            const Type: u8 = Tag::Type as u8;
            #[allow(non_upper_case_globals)]
            const EnumRows: u8 = Tag::EnumRows as u8;
            #[allow(non_upper_case_globals)]
            const RecordRows: u8 = Tag::RecordRows as u8;
        }
        #[automatically_derived]
        unsafe impl<
            __C: ::rkyv::bytecheck::rancor::Fallible + ?::core::marker::Sized,
        > ::rkyv::bytecheck::CheckBytes<__C> for ArchivedVarKindDiscriminant
        where
            <__C as ::rkyv::bytecheck::rancor::Fallible>::Error: ::rkyv::bytecheck::rancor::Source,
        {
            unsafe fn check_bytes(
                value: *const Self,
                context: &mut __C,
            ) -> ::core::result::Result<
                (),
                <__C as ::rkyv::bytecheck::rancor::Fallible>::Error,
            > {
                let tag = *value.cast::<u8>();
                match tag {
                    Discriminant::Type => {}
                    Discriminant::EnumRows => {}
                    Discriminant::RecordRows => {}
                    _ => {
                        return ::core::result::Result::Err(
                            <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Source>::new(::rkyv::bytecheck::InvalidEnumDiscriminantError {
                                enum_name: "ArchivedVarKindDiscriminant",
                                invalid_discriminant: tag,
                            }),
                        );
                    }
                }
                ::core::result::Result::Ok(())
            }
        }
    };
    #[automatically_derived]
    ///The resolver for an archived [`VarKindDiscriminant`]
    pub enum VarKindDiscriminantResolver {
        ///The resolver for [`VarKindDiscriminant::Type`]
        #[allow(dead_code)]
        Type,
        ///The resolver for [`VarKindDiscriminant::EnumRows`]
        #[allow(dead_code)]
        EnumRows,
        ///The resolver for [`VarKindDiscriminant::RecordRows`]
        #[allow(dead_code)]
        RecordRows,
    }
    const _: () = {
        #[repr(u8)]
        enum ArchivedTag {
            Type,
            EnumRows,
            RecordRows,
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for ArchivedTag {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for ArchivedTag {
            #[inline]
            fn eq(&self, other: &ArchivedTag) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
            }
        }
        #[automatically_derived]
        impl ::core::cmp::PartialOrd for ArchivedTag {
            #[inline]
            fn partial_cmp(
                &self,
                other: &ArchivedTag,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                ::core::cmp::PartialOrd::partial_cmp(&__self_discr, &__arg1_discr)
            }
        }
        impl ::rkyv::Archive for VarKindDiscriminant {
            type Archived = ArchivedVarKindDiscriminant;
            type Resolver = VarKindDiscriminantResolver;
            #[allow(clippy::unit_arg)]
            fn resolve(
                &self,
                resolver: Self::Resolver,
                out: ::rkyv::Place<Self::Archived>,
            ) {
                let __this = self;
                match resolver {
                    VarKindDiscriminantResolver::Type => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::Type);
                        }
                    }
                    VarKindDiscriminantResolver::EnumRows => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::EnumRows);
                        }
                    }
                    VarKindDiscriminantResolver::RecordRows => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::RecordRows);
                        }
                    }
                }
            }
        }
    };
    unsafe impl ::rkyv::traits::Portable for ArchivedVarKindDiscriminant {}
    impl From<&VarKind> for VarKindDiscriminant {
        fn from(vk: &VarKind) -> Self {
            match vk {
                VarKind::Type => VarKindDiscriminant::Type,
                VarKind::EnumRows { .. } => VarKindDiscriminant::EnumRows,
                VarKind::RecordRows { .. } => VarKindDiscriminant::RecordRows,
            }
        }
    }
    /// Flavour of a dictionary type. There are currently two way of writing a dictionary type-ish
    /// object: as a dictionary contract `{_ | T}` or as a dictionary type `{_ : T}`. Ideally, the
    /// former wouldn't even be a type but mostly syntactic sugar for a builtin contract application,
    /// or maybe a proper AST node.
    ///
    /// However, the LSP needs to handle both dictionary types and contracts specifically in order to
    /// provide good completion. As we added dictionary contract just before 1.0 to fix a non trivial
    /// issue with respect to polymorphic contracts ([GitHub
    /// issue](https://github.com/tweag/nickel/issues/1228)), the solution to just tweak dictionary
    /// types to be able to hold both kinds - generating a different contract  - seemed to be the
    /// simplest to preserve the user experience (LSP, handling of dictionary when reporting a contract
    /// blame, etc.).
    ///
    /// Dictionary contracts might get a proper AST node later on.
    pub enum DictTypeFlavour {
        /// Dictionary type (`{_ : T}`)
        Type,
        /// Dictionary contract (`{_ | T}`)
        Contract,
    }
    #[automatically_derived]
    #[doc(hidden)]
    unsafe impl ::core::clone::TrivialClone for DictTypeFlavour {}
    #[automatically_derived]
    impl ::core::clone::Clone for DictTypeFlavour {
        #[inline]
        fn clone(&self) -> DictTypeFlavour {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for DictTypeFlavour {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    DictTypeFlavour::Type => "Type",
                    DictTypeFlavour::Contract => "Contract",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for DictTypeFlavour {}
    #[automatically_derived]
    impl ::core::cmp::Eq for DictTypeFlavour {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for DictTypeFlavour {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for DictTypeFlavour {
        #[inline]
        fn eq(&self, other: &DictTypeFlavour) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
        }
    }
    #[automatically_derived]
    ///An archived [`DictTypeFlavour`]
    #[bytecheck(crate = ::rkyv::bytecheck)]
    #[repr(u8)]
    pub enum ArchivedDictTypeFlavour {
        ///The archived counterpart of [`DictTypeFlavour::Type`]
        #[allow(dead_code)]
        Type,
        ///The archived counterpart of [`DictTypeFlavour::Contract`]
        #[allow(dead_code)]
        Contract,
    }
    const _: () = {
        #[repr(u8)]
        enum Tag {
            Type,
            Contract,
        }
        struct Discriminant;
        #[automatically_derived]
        impl Discriminant {
            #[allow(non_upper_case_globals)]
            const Type: u8 = Tag::Type as u8;
            #[allow(non_upper_case_globals)]
            const Contract: u8 = Tag::Contract as u8;
        }
        #[automatically_derived]
        unsafe impl<
            __C: ::rkyv::bytecheck::rancor::Fallible + ?::core::marker::Sized,
        > ::rkyv::bytecheck::CheckBytes<__C> for ArchivedDictTypeFlavour
        where
            <__C as ::rkyv::bytecheck::rancor::Fallible>::Error: ::rkyv::bytecheck::rancor::Source,
        {
            unsafe fn check_bytes(
                value: *const Self,
                context: &mut __C,
            ) -> ::core::result::Result<
                (),
                <__C as ::rkyv::bytecheck::rancor::Fallible>::Error,
            > {
                let tag = *value.cast::<u8>();
                match tag {
                    Discriminant::Type => {}
                    Discriminant::Contract => {}
                    _ => {
                        return ::core::result::Result::Err(
                            <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Source>::new(::rkyv::bytecheck::InvalidEnumDiscriminantError {
                                enum_name: "ArchivedDictTypeFlavour",
                                invalid_discriminant: tag,
                            }),
                        );
                    }
                }
                ::core::result::Result::Ok(())
            }
        }
    };
    #[automatically_derived]
    ///The resolver for an archived [`DictTypeFlavour`]
    pub enum DictTypeFlavourResolver {
        ///The resolver for [`DictTypeFlavour::Type`]
        #[allow(dead_code)]
        Type,
        ///The resolver for [`DictTypeFlavour::Contract`]
        #[allow(dead_code)]
        Contract,
    }
    const _: () = {
        #[repr(u8)]
        enum ArchivedTag {
            Type,
            Contract,
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for ArchivedTag {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for ArchivedTag {
            #[inline]
            fn eq(&self, other: &ArchivedTag) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
            }
        }
        #[automatically_derived]
        impl ::core::cmp::PartialOrd for ArchivedTag {
            #[inline]
            fn partial_cmp(
                &self,
                other: &ArchivedTag,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                ::core::cmp::PartialOrd::partial_cmp(&__self_discr, &__arg1_discr)
            }
        }
        impl ::rkyv::Archive for DictTypeFlavour {
            type Archived = ArchivedDictTypeFlavour;
            type Resolver = DictTypeFlavourResolver;
            #[allow(clippy::unit_arg)]
            fn resolve(
                &self,
                resolver: Self::Resolver,
                out: ::rkyv::Place<Self::Archived>,
            ) {
                let __this = self;
                match resolver {
                    DictTypeFlavourResolver::Type => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::Type);
                        }
                    }
                    DictTypeFlavourResolver::Contract => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::Contract);
                        }
                    }
                }
            }
        }
    };
    unsafe impl ::rkyv::traits::Portable for ArchivedDictTypeFlavour {}
    /// A Nickel type.
    ///
    /// # Generic representation (functor)
    ///
    /// A Nickel type is represented by a tree and is naturally defined in a recursive manner: for
    /// example, one would expect the constructor for function types (`Arrow`) to look like:
    ///
    /// ```rust
    /// pub enum Type {
    ///      Arrow(Box<Type>, Box<Type>),
    ///      // ...
    /// }
    /// ```
    ///
    /// However, `TypeF` is slightly different, in that it is parametrized by a generic type instead of
    /// using a concrete definition like `Box<Types>` (forget about rows for now):
    ///
    /// ```rust
    /// pub enum TypeF<Ty /* , .. */> {
    ///      Arrow(Ty, Ty),
    ///      // ...
    /// }
    /// ```
    ///
    /// `Ty` is also called a recursive unfolding throughout the documentation. By defining `struct
    /// Type(TypeF<Box<Types>>)`, we get back the original, natural definition.
    ///
    /// ## Motivation 1: variation on `Types`
    ///
    /// Having a generic definition makes it possible to easily create other types with the _same shape_
    /// as `Type` (seen as trees), but with enriched nodes. The typical use-case in Nickel is the
    /// variation on types used by the typechecker. During type inference, the typechecker operates on
    /// trees where each node can either be a concrete type, or a unification variable (a unknown type
    /// to be inferred). Instead of duplicating the whole definition of `Type` as well as all basic
    /// methods, we can simply have a different recursive definition:
    ///
    /// ```
    /// # // phony declarations to make this example pass the tests
    /// # type VarId = ();
    /// # type TypeF<T> = T;
    ///
    /// pub enum UnifType {
    ///    UnifVar(VarId),
    ///    Concrete(TypeF<Box<UnifType> /*, .. */>),
    ///    // ..
    ///  }
    /// ```
    ///
    /// We get something that looks like normal Nickel types, except that each node can also be a
    /// unification variable as well.
    ///
    /// ## Motivation 2: recursion schemes
    ///
    /// This definition is actually in the style of recursion schemes. Pedantically, `TypeF` (hence the
    /// `F` suffix) is a functor, but the formal details aren't so important: keep in mind that the `F`
    /// suffix means that the recursive occurrences of subtrees (and enum rows and record rows as well)
    /// are replaced by generic parameters.
    ///
    /// The usual motivation for recursion schemes is that they allow for elegant and simple definitions
    /// of recursive transformation over trees (here, `TypeF`, and more generally anything with an `F`
    /// suffix) in terms of simple appropriate chaining of `map` and folding/unfolding operations. A
    /// good example is the definition of [crate::ast::typ::Type::traverse]. Although [crate::ast::Node] isn't currently
    /// defined using functors per se, the way program transformations are written is in the same style
    /// as recursion schemes: we simply define the action of a transformation as a mapping on the
    /// current node, and let the traversal take care of the plumbing of recursion and reconstruction.
    ///
    /// ## Type parameters
    ///
    /// - `Ty`: the recursive unfolding of Nickel types
    /// - `RRows`: the recursive unfolding of record rows
    /// - `ERows`: the recursive unfolding of enum rows
    /// - `Te`: the type of a term (used to store contracts)
    pub enum TypeF<Ty, RRows, ERows, Te> {
        /// The dynamic type, or unitype. Assigned to values whose actual type is not statically known
        /// or checked.
        Dyn,
        /// A floating point number.
        Number,
        /// A boolean.
        Bool,
        /// A string literal.
        String,
        /// A symbol.
        ///
        /// See `Term::Sealed` in `nickel_lang_core`.
        Symbol,
        /// The type of `Term::ForeignId`.
        ForeignId,
        /// A type created from a user-defined contract.
        Contract(Te),
        /// A function.
        Arrow(Ty, Ty),
        /// A type variable.
        Var(Ident),
        /// A forall binder.
        Forall { var: LocIdent, var_kind: VarKind, body: Ty },
        /// An enum type, composed of a sequence of enum rows.
        Enum(ERows),
        /// A record type, composed of a sequence of record rows.
        Record(RRows),
        /// A dictionary type.
        Dict { type_fields: Ty, flavour: DictTypeFlavour },
        /// A parametrized array.
        Array(Ty),
        /// A type wildcard, wrapping an ID unique within a given file.
        Wildcard(usize),
    }
    #[automatically_derived]
    impl<
        Ty: ::core::clone::Clone,
        RRows: ::core::clone::Clone,
        ERows: ::core::clone::Clone,
        Te: ::core::clone::Clone,
    > ::core::clone::Clone for TypeF<Ty, RRows, ERows, Te> {
        #[inline]
        fn clone(&self) -> TypeF<Ty, RRows, ERows, Te> {
            match self {
                TypeF::Dyn => TypeF::Dyn,
                TypeF::Number => TypeF::Number,
                TypeF::Bool => TypeF::Bool,
                TypeF::String => TypeF::String,
                TypeF::Symbol => TypeF::Symbol,
                TypeF::ForeignId => TypeF::ForeignId,
                TypeF::Contract(__self_0) => {
                    TypeF::Contract(::core::clone::Clone::clone(__self_0))
                }
                TypeF::Arrow(__self_0, __self_1) => {
                    TypeF::Arrow(
                        ::core::clone::Clone::clone(__self_0),
                        ::core::clone::Clone::clone(__self_1),
                    )
                }
                TypeF::Var(__self_0) => TypeF::Var(::core::clone::Clone::clone(__self_0)),
                TypeF::Forall { var: __self_0, var_kind: __self_1, body: __self_2 } => {
                    TypeF::Forall {
                        var: ::core::clone::Clone::clone(__self_0),
                        var_kind: ::core::clone::Clone::clone(__self_1),
                        body: ::core::clone::Clone::clone(__self_2),
                    }
                }
                TypeF::Enum(__self_0) => {
                    TypeF::Enum(::core::clone::Clone::clone(__self_0))
                }
                TypeF::Record(__self_0) => {
                    TypeF::Record(::core::clone::Clone::clone(__self_0))
                }
                TypeF::Dict { type_fields: __self_0, flavour: __self_1 } => {
                    TypeF::Dict {
                        type_fields: ::core::clone::Clone::clone(__self_0),
                        flavour: ::core::clone::Clone::clone(__self_1),
                    }
                }
                TypeF::Array(__self_0) => {
                    TypeF::Array(::core::clone::Clone::clone(__self_0))
                }
                TypeF::Wildcard(__self_0) => {
                    TypeF::Wildcard(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    #[automatically_derived]
    impl<Ty, RRows, ERows, Te> ::core::marker::StructuralPartialEq
    for TypeF<Ty, RRows, ERows, Te> {}
    #[automatically_derived]
    impl<
        Ty: ::core::cmp::PartialEq,
        RRows: ::core::cmp::PartialEq,
        ERows: ::core::cmp::PartialEq,
        Te: ::core::cmp::PartialEq,
    > ::core::cmp::PartialEq for TypeF<Ty, RRows, ERows, Te> {
        #[inline]
        fn eq(&self, other: &TypeF<Ty, RRows, ERows, Te>) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
                && match (self, other) {
                    (TypeF::Contract(__self_0), TypeF::Contract(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (
                        TypeF::Arrow(__self_0, __self_1),
                        TypeF::Arrow(__arg1_0, __arg1_1),
                    ) => __self_0 == __arg1_0 && __self_1 == __arg1_1,
                    (TypeF::Var(__self_0), TypeF::Var(__arg1_0)) => __self_0 == __arg1_0,
                    (
                        TypeF::Forall {
                            var: __self_0,
                            var_kind: __self_1,
                            body: __self_2,
                        },
                        TypeF::Forall {
                            var: __arg1_0,
                            var_kind: __arg1_1,
                            body: __arg1_2,
                        },
                    ) => {
                        __self_0 == __arg1_0 && __self_1 == __arg1_1
                            && __self_2 == __arg1_2
                    }
                    (TypeF::Enum(__self_0), TypeF::Enum(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (TypeF::Record(__self_0), TypeF::Record(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (
                        TypeF::Dict { type_fields: __self_0, flavour: __self_1 },
                        TypeF::Dict { type_fields: __arg1_0, flavour: __arg1_1 },
                    ) => __self_0 == __arg1_0 && __self_1 == __arg1_1,
                    (TypeF::Array(__self_0), TypeF::Array(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (TypeF::Wildcard(__self_0), TypeF::Wildcard(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    _ => true,
                }
        }
    }
    #[automatically_derived]
    impl<
        Ty: ::core::cmp::Eq,
        RRows: ::core::cmp::Eq,
        ERows: ::core::cmp::Eq,
        Te: ::core::cmp::Eq,
    > ::core::cmp::Eq for TypeF<Ty, RRows, ERows, Te> {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<Te>;
            let _: ::core::cmp::AssertParamIsEq<Ty>;
            let _: ::core::cmp::AssertParamIsEq<Ident>;
            let _: ::core::cmp::AssertParamIsEq<LocIdent>;
            let _: ::core::cmp::AssertParamIsEq<VarKind>;
            let _: ::core::cmp::AssertParamIsEq<ERows>;
            let _: ::core::cmp::AssertParamIsEq<RRows>;
            let _: ::core::cmp::AssertParamIsEq<DictTypeFlavour>;
            let _: ::core::cmp::AssertParamIsEq<usize>;
        }
    }
    #[automatically_derived]
    impl<
        Ty: ::core::fmt::Debug,
        RRows: ::core::fmt::Debug,
        ERows: ::core::fmt::Debug,
        Te: ::core::fmt::Debug,
    > ::core::fmt::Debug for TypeF<Ty, RRows, ERows, Te> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                TypeF::Dyn => ::core::fmt::Formatter::write_str(f, "Dyn"),
                TypeF::Number => ::core::fmt::Formatter::write_str(f, "Number"),
                TypeF::Bool => ::core::fmt::Formatter::write_str(f, "Bool"),
                TypeF::String => ::core::fmt::Formatter::write_str(f, "String"),
                TypeF::Symbol => ::core::fmt::Formatter::write_str(f, "Symbol"),
                TypeF::ForeignId => ::core::fmt::Formatter::write_str(f, "ForeignId"),
                TypeF::Contract(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Contract",
                        &__self_0,
                    )
                }
                TypeF::Arrow(__self_0, __self_1) => {
                    ::core::fmt::Formatter::debug_tuple_field2_finish(
                        f,
                        "Arrow",
                        __self_0,
                        &__self_1,
                    )
                }
                TypeF::Var(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Var",
                        &__self_0,
                    )
                }
                TypeF::Forall { var: __self_0, var_kind: __self_1, body: __self_2 } => {
                    ::core::fmt::Formatter::debug_struct_field3_finish(
                        f,
                        "Forall",
                        "var",
                        __self_0,
                        "var_kind",
                        __self_1,
                        "body",
                        &__self_2,
                    )
                }
                TypeF::Enum(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Enum",
                        &__self_0,
                    )
                }
                TypeF::Record(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Record",
                        &__self_0,
                    )
                }
                TypeF::Dict { type_fields: __self_0, flavour: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Dict",
                        "type_fields",
                        __self_0,
                        "flavour",
                        &__self_1,
                    )
                }
                TypeF::Array(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Array",
                        &__self_0,
                    )
                }
                TypeF::Wildcard(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Wildcard",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    ///An archived [`TypeF`]
    #[bytecheck(crate = ::rkyv::bytecheck)]
    #[repr(u8)]
    pub enum ArchivedTypeF<Ty, RRows, ERows, Te>
    where
        Te: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        Ident: ::rkyv::Archive,
        LocIdent: ::rkyv::Archive,
        VarKind: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        ERows: ::rkyv::Archive,
        RRows: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        DictTypeFlavour: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        usize: ::rkyv::Archive,
    {
        ///The archived counterpart of [`TypeF::Dyn`]
        #[allow(dead_code)]
        Dyn,
        ///The archived counterpart of [`TypeF::Number`]
        #[allow(dead_code)]
        Number,
        ///The archived counterpart of [`TypeF::Bool`]
        #[allow(dead_code)]
        Bool,
        ///The archived counterpart of [`TypeF::String`]
        #[allow(dead_code)]
        String,
        ///The archived counterpart of [`TypeF::Symbol`]
        #[allow(dead_code)]
        Symbol,
        ///The archived counterpart of [`TypeF::ForeignId`]
        #[allow(dead_code)]
        ForeignId,
        ///The archived counterpart of [`TypeF::Contract`]
        #[allow(dead_code)]
        Contract(
            ///The archived counterpart of [`TypeF::Contract::0`]
            <Te as ::rkyv::Archive>::Archived,
        ),
        ///The archived counterpart of [`TypeF::Arrow`]
        #[allow(dead_code)]
        Arrow(
            ///The archived counterpart of [`TypeF::Arrow::0`]
            <Ty as ::rkyv::Archive>::Archived,
            ///The archived counterpart of [`TypeF::Arrow::1`]
            <Ty as ::rkyv::Archive>::Archived,
        ),
        ///The archived counterpart of [`TypeF::Var`]
        #[allow(dead_code)]
        Var(
            ///The archived counterpart of [`TypeF::Var::0`]
            <Ident as ::rkyv::Archive>::Archived,
        ),
        ///The archived counterpart of [`TypeF::Forall`]
        #[allow(dead_code)]
        Forall {
            ///The archived counterpart of [`TypeF::Forall::var`]
            var: <LocIdent as ::rkyv::Archive>::Archived,
            ///The archived counterpart of [`TypeF::Forall::var_kind`]
            var_kind: <VarKind as ::rkyv::Archive>::Archived,
            ///The archived counterpart of [`TypeF::Forall::body`]
            body: <Ty as ::rkyv::Archive>::Archived,
        },
        ///The archived counterpart of [`TypeF::Enum`]
        #[allow(dead_code)]
        Enum(
            ///The archived counterpart of [`TypeF::Enum::0`]
            <ERows as ::rkyv::Archive>::Archived,
        ),
        ///The archived counterpart of [`TypeF::Record`]
        #[allow(dead_code)]
        Record(
            ///The archived counterpart of [`TypeF::Record::0`]
            <RRows as ::rkyv::Archive>::Archived,
        ),
        ///The archived counterpart of [`TypeF::Dict`]
        #[allow(dead_code)]
        Dict {
            ///The archived counterpart of [`TypeF::Dict::type_fields`]
            type_fields: <Ty as ::rkyv::Archive>::Archived,
            ///The archived counterpart of [`TypeF::Dict::flavour`]
            flavour: <DictTypeFlavour as ::rkyv::Archive>::Archived,
        },
        ///The archived counterpart of [`TypeF::Array`]
        #[allow(dead_code)]
        Array(
            ///The archived counterpart of [`TypeF::Array::0`]
            <Ty as ::rkyv::Archive>::Archived,
        ),
        ///The archived counterpart of [`TypeF::Wildcard`]
        #[allow(dead_code)]
        Wildcard(
            ///The archived counterpart of [`TypeF::Wildcard::0`]
            <usize as ::rkyv::Archive>::Archived,
        ),
    }
    const _: () = {
        #[repr(u8)]
        enum Tag {
            Dyn,
            Number,
            Bool,
            String,
            Symbol,
            ForeignId,
            Contract,
            Arrow,
            Var,
            Forall,
            Enum,
            Record,
            Dict,
            Array,
            Wildcard,
        }
        struct Discriminant;
        #[automatically_derived]
        impl Discriminant {
            #[allow(non_upper_case_globals)]
            const Dyn: u8 = Tag::Dyn as u8;
            #[allow(non_upper_case_globals)]
            const Number: u8 = Tag::Number as u8;
            #[allow(non_upper_case_globals)]
            const Bool: u8 = Tag::Bool as u8;
            #[allow(non_upper_case_globals)]
            const String: u8 = Tag::String as u8;
            #[allow(non_upper_case_globals)]
            const Symbol: u8 = Tag::Symbol as u8;
            #[allow(non_upper_case_globals)]
            const ForeignId: u8 = Tag::ForeignId as u8;
            #[allow(non_upper_case_globals)]
            const Contract: u8 = Tag::Contract as u8;
            #[allow(non_upper_case_globals)]
            const Arrow: u8 = Tag::Arrow as u8;
            #[allow(non_upper_case_globals)]
            const Var: u8 = Tag::Var as u8;
            #[allow(non_upper_case_globals)]
            const Forall: u8 = Tag::Forall as u8;
            #[allow(non_upper_case_globals)]
            const Enum: u8 = Tag::Enum as u8;
            #[allow(non_upper_case_globals)]
            const Record: u8 = Tag::Record as u8;
            #[allow(non_upper_case_globals)]
            const Dict: u8 = Tag::Dict as u8;
            #[allow(non_upper_case_globals)]
            const Array: u8 = Tag::Array as u8;
            #[allow(non_upper_case_globals)]
            const Wildcard: u8 = Tag::Wildcard as u8;
        }
        #[repr(C)]
        struct VariantContract<Ty, RRows, ERows, Te>(
            Tag,
            <Te as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<ArchivedTypeF<Ty, RRows, ERows, Te>>,
        )
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive;
        #[repr(C)]
        struct VariantArrow<Ty, RRows, ERows, Te>(
            Tag,
            <Ty as ::rkyv::Archive>::Archived,
            <Ty as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<ArchivedTypeF<Ty, RRows, ERows, Te>>,
        )
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive;
        #[repr(C)]
        struct VariantVar<Ty, RRows, ERows, Te>(
            Tag,
            <Ident as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<ArchivedTypeF<Ty, RRows, ERows, Te>>,
        )
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive;
        #[repr(C)]
        struct VariantForall<Ty, RRows, ERows, Te>
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive,
        {
            __tag: Tag,
            var: <LocIdent as ::rkyv::Archive>::Archived,
            var_kind: <VarKind as ::rkyv::Archive>::Archived,
            body: <Ty as ::rkyv::Archive>::Archived,
            __phantom: ::core::marker::PhantomData<ArchivedTypeF<Ty, RRows, ERows, Te>>,
        }
        #[repr(C)]
        struct VariantEnum<Ty, RRows, ERows, Te>(
            Tag,
            <ERows as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<ArchivedTypeF<Ty, RRows, ERows, Te>>,
        )
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive;
        #[repr(C)]
        struct VariantRecord<Ty, RRows, ERows, Te>(
            Tag,
            <RRows as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<ArchivedTypeF<Ty, RRows, ERows, Te>>,
        )
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive;
        #[repr(C)]
        struct VariantDict<Ty, RRows, ERows, Te>
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive,
        {
            __tag: Tag,
            type_fields: <Ty as ::rkyv::Archive>::Archived,
            flavour: <DictTypeFlavour as ::rkyv::Archive>::Archived,
            __phantom: ::core::marker::PhantomData<ArchivedTypeF<Ty, RRows, ERows, Te>>,
        }
        #[repr(C)]
        struct VariantArray<Ty, RRows, ERows, Te>(
            Tag,
            <Ty as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<ArchivedTypeF<Ty, RRows, ERows, Te>>,
        )
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive;
        #[repr(C)]
        struct VariantWildcard<Ty, RRows, ERows, Te>(
            Tag,
            <usize as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<ArchivedTypeF<Ty, RRows, ERows, Te>>,
        )
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive;
        #[automatically_derived]
        unsafe impl<
            Ty,
            RRows,
            ERows,
            Te,
            __C: ::rkyv::bytecheck::rancor::Fallible + ?::core::marker::Sized,
        > ::rkyv::bytecheck::CheckBytes<__C> for ArchivedTypeF<Ty, RRows, ERows, Te>
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive,
            <__C as ::rkyv::bytecheck::rancor::Fallible>::Error: ::rkyv::bytecheck::rancor::Source,
            <Te as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <Ty as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <Ty as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <Ident as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <LocIdent as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <VarKind as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <Ty as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <ERows as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <RRows as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <Ty as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <DictTypeFlavour as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<
                __C,
            >,
            <Ty as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
            <usize as ::rkyv::Archive>::Archived: ::rkyv::bytecheck::CheckBytes<__C>,
        {
            unsafe fn check_bytes(
                value: *const Self,
                context: &mut __C,
            ) -> ::core::result::Result<
                (),
                <__C as ::rkyv::bytecheck::rancor::Fallible>::Error,
            > {
                let tag = *value.cast::<u8>();
                match tag {
                    Discriminant::Dyn => {}
                    Discriminant::Number => {}
                    Discriminant::Bool => {}
                    Discriminant::String => {}
                    Discriminant::Symbol => {}
                    Discriminant::ForeignId => {}
                    Discriminant::Contract => {
                        let value = value
                            .cast::<VariantContract<Ty, RRows, ERows, Te>>();
                        <<Te as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).1, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::UnnamedEnumVariantCheckContext {
                                        enum_name: "ArchivedTypeF",
                                        variant_name: "Contract",
                                        field_index: 1,
                                    },
                                )
                            })?;
                    }
                    Discriminant::Arrow => {
                        let value = value.cast::<VariantArrow<Ty, RRows, ERows, Te>>();
                        <<Ty as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).1, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::UnnamedEnumVariantCheckContext {
                                        enum_name: "ArchivedTypeF",
                                        variant_name: "Arrow",
                                        field_index: 1,
                                    },
                                )
                            })?;
                        <<Ty as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).2, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::UnnamedEnumVariantCheckContext {
                                        enum_name: "ArchivedTypeF",
                                        variant_name: "Arrow",
                                        field_index: 2,
                                    },
                                )
                            })?;
                    }
                    Discriminant::Var => {
                        let value = value.cast::<VariantVar<Ty, RRows, ERows, Te>>();
                        <<Ident as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).1, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::UnnamedEnumVariantCheckContext {
                                        enum_name: "ArchivedTypeF",
                                        variant_name: "Var",
                                        field_index: 1,
                                    },
                                )
                            })?;
                    }
                    Discriminant::Forall => {
                        let value = value.cast::<VariantForall<Ty, RRows, ERows, Te>>();
                        <<LocIdent as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).var, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::NamedEnumVariantCheckContext {
                                        enum_name: "ArchivedTypeF",
                                        variant_name: "Forall",
                                        field_name: "var",
                                    },
                                )
                            })?;
                        <<VarKind as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).var_kind, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::NamedEnumVariantCheckContext {
                                        enum_name: "ArchivedTypeF",
                                        variant_name: "Forall",
                                        field_name: "var_kind",
                                    },
                                )
                            })?;
                        <<Ty as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).body, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::NamedEnumVariantCheckContext {
                                        enum_name: "ArchivedTypeF",
                                        variant_name: "Forall",
                                        field_name: "body",
                                    },
                                )
                            })?;
                    }
                    Discriminant::Enum => {
                        let value = value.cast::<VariantEnum<Ty, RRows, ERows, Te>>();
                        <<ERows as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).1, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::UnnamedEnumVariantCheckContext {
                                        enum_name: "ArchivedTypeF",
                                        variant_name: "Enum",
                                        field_index: 1,
                                    },
                                )
                            })?;
                    }
                    Discriminant::Record => {
                        let value = value.cast::<VariantRecord<Ty, RRows, ERows, Te>>();
                        <<RRows as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).1, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::UnnamedEnumVariantCheckContext {
                                        enum_name: "ArchivedTypeF",
                                        variant_name: "Record",
                                        field_index: 1,
                                    },
                                )
                            })?;
                    }
                    Discriminant::Dict => {
                        let value = value.cast::<VariantDict<Ty, RRows, ERows, Te>>();
                        <<Ty as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).type_fields, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::NamedEnumVariantCheckContext {
                                        enum_name: "ArchivedTypeF",
                                        variant_name: "Dict",
                                        field_name: "type_fields",
                                    },
                                )
                            })?;
                        <<DictTypeFlavour as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).flavour, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::NamedEnumVariantCheckContext {
                                        enum_name: "ArchivedTypeF",
                                        variant_name: "Dict",
                                        field_name: "flavour",
                                    },
                                )
                            })?;
                    }
                    Discriminant::Array => {
                        let value = value.cast::<VariantArray<Ty, RRows, ERows, Te>>();
                        <<Ty as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).1, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::UnnamedEnumVariantCheckContext {
                                        enum_name: "ArchivedTypeF",
                                        variant_name: "Array",
                                        field_index: 1,
                                    },
                                )
                            })?;
                    }
                    Discriminant::Wildcard => {
                        let value = value
                            .cast::<VariantWildcard<Ty, RRows, ERows, Te>>();
                        <<usize as ::rkyv::Archive>::Archived as ::rkyv::bytecheck::CheckBytes<
                            __C,
                        >>::check_bytes(&raw const (*value).1, context)
                            .map_err(|e| {
                                <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Trace>::trace(
                                    e,
                                    ::rkyv::bytecheck::UnnamedEnumVariantCheckContext {
                                        enum_name: "ArchivedTypeF",
                                        variant_name: "Wildcard",
                                        field_index: 1,
                                    },
                                )
                            })?;
                    }
                    _ => {
                        return ::core::result::Result::Err(
                            <<__C as ::rkyv::bytecheck::rancor::Fallible>::Error as ::rkyv::bytecheck::rancor::Source>::new(::rkyv::bytecheck::InvalidEnumDiscriminantError {
                                enum_name: "ArchivedTypeF",
                                invalid_discriminant: tag,
                            }),
                        );
                    }
                }
                ::core::result::Result::Ok(())
            }
        }
    };
    #[automatically_derived]
    ///The resolver for an archived [`TypeF`]
    pub enum TypeFResolver<Ty, RRows, ERows, Te>
    where
        Te: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        Ident: ::rkyv::Archive,
        LocIdent: ::rkyv::Archive,
        VarKind: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        ERows: ::rkyv::Archive,
        RRows: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        DictTypeFlavour: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        usize: ::rkyv::Archive,
    {
        ///The resolver for [`TypeF::Dyn`]
        #[allow(dead_code)]
        Dyn,
        ///The resolver for [`TypeF::Number`]
        #[allow(dead_code)]
        Number,
        ///The resolver for [`TypeF::Bool`]
        #[allow(dead_code)]
        Bool,
        ///The resolver for [`TypeF::String`]
        #[allow(dead_code)]
        String,
        ///The resolver for [`TypeF::Symbol`]
        #[allow(dead_code)]
        Symbol,
        ///The resolver for [`TypeF::ForeignId`]
        #[allow(dead_code)]
        ForeignId,
        ///The resolver for [`TypeF::Contract`]
        #[allow(dead_code)]
        Contract(<Te as ::rkyv::Archive>::Resolver),
        ///The resolver for [`TypeF::Arrow`]
        #[allow(dead_code)]
        Arrow(<Ty as ::rkyv::Archive>::Resolver, <Ty as ::rkyv::Archive>::Resolver),
        ///The resolver for [`TypeF::Var`]
        #[allow(dead_code)]
        Var(<Ident as ::rkyv::Archive>::Resolver),
        ///The resolver for [`TypeF::Forall`]
        #[allow(dead_code)]
        Forall {
            var: <LocIdent as ::rkyv::Archive>::Resolver,
            var_kind: <VarKind as ::rkyv::Archive>::Resolver,
            body: <Ty as ::rkyv::Archive>::Resolver,
        },
        ///The resolver for [`TypeF::Enum`]
        #[allow(dead_code)]
        Enum(<ERows as ::rkyv::Archive>::Resolver),
        ///The resolver for [`TypeF::Record`]
        #[allow(dead_code)]
        Record(<RRows as ::rkyv::Archive>::Resolver),
        ///The resolver for [`TypeF::Dict`]
        #[allow(dead_code)]
        Dict {
            type_fields: <Ty as ::rkyv::Archive>::Resolver,
            flavour: <DictTypeFlavour as ::rkyv::Archive>::Resolver,
        },
        ///The resolver for [`TypeF::Array`]
        #[allow(dead_code)]
        Array(<Ty as ::rkyv::Archive>::Resolver),
        ///The resolver for [`TypeF::Wildcard`]
        #[allow(dead_code)]
        Wildcard(<usize as ::rkyv::Archive>::Resolver),
    }
    const _: () = {
        #[repr(u8)]
        enum ArchivedTag {
            Dyn,
            Number,
            Bool,
            String,
            Symbol,
            ForeignId,
            Contract,
            Arrow,
            Var,
            Forall,
            Enum,
            Record,
            Dict,
            Array,
            Wildcard,
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for ArchivedTag {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for ArchivedTag {
            #[inline]
            fn eq(&self, other: &ArchivedTag) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
            }
        }
        #[automatically_derived]
        impl ::core::cmp::PartialOrd for ArchivedTag {
            #[inline]
            fn partial_cmp(
                &self,
                other: &ArchivedTag,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                ::core::cmp::PartialOrd::partial_cmp(&__self_discr, &__arg1_discr)
            }
        }
        #[repr(C)]
        struct ArchivedVariantContract<Ty, RRows, ERows, Te>(
            ArchivedTag,
            <Te as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<TypeF<Ty, RRows, ERows, Te>>,
        )
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive;
        #[repr(C)]
        struct ArchivedVariantArrow<Ty, RRows, ERows, Te>(
            ArchivedTag,
            <Ty as ::rkyv::Archive>::Archived,
            <Ty as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<TypeF<Ty, RRows, ERows, Te>>,
        )
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive;
        #[repr(C)]
        struct ArchivedVariantVar<Ty, RRows, ERows, Te>(
            ArchivedTag,
            <Ident as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<TypeF<Ty, RRows, ERows, Te>>,
        )
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive;
        #[repr(C)]
        struct ArchivedVariantForall<Ty, RRows, ERows, Te>
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive,
        {
            __tag: ArchivedTag,
            var: <LocIdent as ::rkyv::Archive>::Archived,
            var_kind: <VarKind as ::rkyv::Archive>::Archived,
            body: <Ty as ::rkyv::Archive>::Archived,
            __phantom: ::core::marker::PhantomData<TypeF<Ty, RRows, ERows, Te>>,
        }
        #[repr(C)]
        struct ArchivedVariantEnum<Ty, RRows, ERows, Te>(
            ArchivedTag,
            <ERows as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<TypeF<Ty, RRows, ERows, Te>>,
        )
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive;
        #[repr(C)]
        struct ArchivedVariantRecord<Ty, RRows, ERows, Te>(
            ArchivedTag,
            <RRows as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<TypeF<Ty, RRows, ERows, Te>>,
        )
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive;
        #[repr(C)]
        struct ArchivedVariantDict<Ty, RRows, ERows, Te>
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive,
        {
            __tag: ArchivedTag,
            type_fields: <Ty as ::rkyv::Archive>::Archived,
            flavour: <DictTypeFlavour as ::rkyv::Archive>::Archived,
            __phantom: ::core::marker::PhantomData<TypeF<Ty, RRows, ERows, Te>>,
        }
        #[repr(C)]
        struct ArchivedVariantArray<Ty, RRows, ERows, Te>(
            ArchivedTag,
            <Ty as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<TypeF<Ty, RRows, ERows, Te>>,
        )
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive;
        #[repr(C)]
        struct ArchivedVariantWildcard<Ty, RRows, ERows, Te>(
            ArchivedTag,
            <usize as ::rkyv::Archive>::Archived,
            ::core::marker::PhantomData<TypeF<Ty, RRows, ERows, Te>>,
        )
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive;
        impl<Ty, RRows, ERows, Te> ::rkyv::Archive for TypeF<Ty, RRows, ERows, Te>
        where
            Te: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            Ident: ::rkyv::Archive,
            LocIdent: ::rkyv::Archive,
            VarKind: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            ERows: ::rkyv::Archive,
            RRows: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            DictTypeFlavour: ::rkyv::Archive,
            Ty: ::rkyv::Archive,
            usize: ::rkyv::Archive,
        {
            type Archived = ArchivedTypeF<Ty, RRows, ERows, Te>;
            type Resolver = TypeFResolver<Ty, RRows, ERows, Te>;
            #[allow(clippy::unit_arg)]
            fn resolve(
                &self,
                resolver: Self::Resolver,
                out: ::rkyv::Place<Self::Archived>,
            ) {
                let __this = self;
                match resolver {
                    TypeFResolver::Dyn => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::Dyn);
                        }
                    }
                    TypeFResolver::Number => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::Number);
                        }
                    }
                    TypeFResolver::Bool => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::Bool);
                        }
                    }
                    TypeFResolver::String => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::String);
                        }
                    }
                    TypeFResolver::Symbol => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::Symbol);
                        }
                    }
                    TypeFResolver::ForeignId => {
                        let out = unsafe { out.cast_unchecked::<ArchivedTag>() };
                        unsafe {
                            out.write_unchecked(ArchivedTag::ForeignId);
                        }
                    }
                    TypeFResolver::Contract(resolver_0) => {
                        match __this {
                            TypeF::Contract(self_0, ..) => {
                                let out = unsafe {
                                    out.cast_unchecked::<
                                            ArchivedVariantContract<Ty, RRows, ERows, Te>,
                                        >()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).0 };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::Contract);
                                }
                                let field_ptr = unsafe { &raw mut (*out.ptr()).1 };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <Te as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                    TypeFResolver::Arrow(resolver_0, resolver_1) => {
                        match __this {
                            TypeF::Arrow(self_0, self_1, ..) => {
                                let out = unsafe {
                                    out.cast_unchecked::<
                                            ArchivedVariantArrow<Ty, RRows, ERows, Te>,
                                        >()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).0 };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::Arrow);
                                }
                                let field_ptr = unsafe { &raw mut (*out.ptr()).1 };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <Ty as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                                let field_ptr = unsafe { &raw mut (*out.ptr()).2 };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <Ty as ::rkyv::Archive>::resolve(
                                    self_1,
                                    resolver_1,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                    TypeFResolver::Var(resolver_0) => {
                        match __this {
                            TypeF::Var(self_0, ..) => {
                                let out = unsafe {
                                    out.cast_unchecked::<
                                            ArchivedVariantVar<Ty, RRows, ERows, Te>,
                                        >()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).0 };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::Var);
                                }
                                let field_ptr = unsafe { &raw mut (*out.ptr()).1 };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <Ident as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                    TypeFResolver::Forall {
                        var: resolver_0,
                        var_kind: resolver_1,
                        body: resolver_2,
                    } => {
                        match __this {
                            TypeF::Forall {
                                var: self_0,
                                var_kind: self_1,
                                body: self_2,
                                ..
                            } => {
                                let out = unsafe {
                                    out.cast_unchecked::<
                                            ArchivedVariantForall<Ty, RRows, ERows, Te>,
                                        >()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).__tag };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::Forall);
                                }
                                let field_ptr = unsafe { &raw mut (*out.ptr()).var };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <LocIdent as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                                let field_ptr = unsafe { &raw mut (*out.ptr()).var_kind };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <VarKind as ::rkyv::Archive>::resolve(
                                    self_1,
                                    resolver_1,
                                    out_field,
                                );
                                let field_ptr = unsafe { &raw mut (*out.ptr()).body };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <Ty as ::rkyv::Archive>::resolve(
                                    self_2,
                                    resolver_2,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                    TypeFResolver::Enum(resolver_0) => {
                        match __this {
                            TypeF::Enum(self_0, ..) => {
                                let out = unsafe {
                                    out.cast_unchecked::<
                                            ArchivedVariantEnum<Ty, RRows, ERows, Te>,
                                        >()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).0 };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::Enum);
                                }
                                let field_ptr = unsafe { &raw mut (*out.ptr()).1 };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <ERows as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                    TypeFResolver::Record(resolver_0) => {
                        match __this {
                            TypeF::Record(self_0, ..) => {
                                let out = unsafe {
                                    out.cast_unchecked::<
                                            ArchivedVariantRecord<Ty, RRows, ERows, Te>,
                                        >()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).0 };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::Record);
                                }
                                let field_ptr = unsafe { &raw mut (*out.ptr()).1 };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <RRows as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                    TypeFResolver::Dict {
                        type_fields: resolver_0,
                        flavour: resolver_1,
                    } => {
                        match __this {
                            TypeF::Dict { type_fields: self_0, flavour: self_1, .. } => {
                                let out = unsafe {
                                    out.cast_unchecked::<
                                            ArchivedVariantDict<Ty, RRows, ERows, Te>,
                                        >()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).__tag };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::Dict);
                                }
                                let field_ptr = unsafe {
                                    &raw mut (*out.ptr()).type_fields
                                };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <Ty as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                                let field_ptr = unsafe { &raw mut (*out.ptr()).flavour };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <DictTypeFlavour as ::rkyv::Archive>::resolve(
                                    self_1,
                                    resolver_1,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                    TypeFResolver::Array(resolver_0) => {
                        match __this {
                            TypeF::Array(self_0, ..) => {
                                let out = unsafe {
                                    out.cast_unchecked::<
                                            ArchivedVariantArray<Ty, RRows, ERows, Te>,
                                        >()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).0 };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::Array);
                                }
                                let field_ptr = unsafe { &raw mut (*out.ptr()).1 };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <Ty as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                    TypeFResolver::Wildcard(resolver_0) => {
                        match __this {
                            TypeF::Wildcard(self_0, ..) => {
                                let out = unsafe {
                                    out.cast_unchecked::<
                                            ArchivedVariantWildcard<Ty, RRows, ERows, Te>,
                                        >()
                                };
                                let tag_ptr = unsafe { &raw mut (*out.ptr()).0 };
                                unsafe {
                                    tag_ptr.write(ArchivedTag::Wildcard);
                                }
                                let field_ptr = unsafe { &raw mut (*out.ptr()).1 };
                                let out_field = unsafe {
                                    ::rkyv::Place::from_field_unchecked(out, field_ptr)
                                };
                                <usize as ::rkyv::Archive>::resolve(
                                    self_0,
                                    resolver_0,
                                    out_field,
                                );
                            }
                            #[allow(unreachable_patterns)]
                            _ => unsafe { ::core::hint::unreachable_unchecked() }
                        }
                    }
                }
            }
        }
    };
    unsafe impl<Ty, RRows, ERows, Te> ::rkyv::traits::Portable
    for ArchivedTypeF<Ty, RRows, ERows, Te>
    where
        Te: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        Ident: ::rkyv::Archive,
        LocIdent: ::rkyv::Archive,
        VarKind: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        ERows: ::rkyv::Archive,
        RRows: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        DictTypeFlavour: ::rkyv::Archive,
        Ty: ::rkyv::Archive,
        usize: ::rkyv::Archive,
        <Te as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <Ty as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <Ty as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <Ident as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <LocIdent as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <VarKind as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <Ty as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <ERows as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <RRows as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <Ty as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <DictTypeFlavour as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <Ty as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
        <usize as ::rkyv::Archive>::Archived: ::rkyv::traits::Portable,
    {}
    impl<Ty, RRows> RecordRowsF<Ty, RRows> {
        /// Map functions over the children nodes of record rows, when seen as a tree. The mutable
        /// state ( `S`) is threaded through the calls to the mapped functions. Functions are fallible
        /// and may return an error `E`, which causes `try_map_state` to return early with the same
        /// error.
        ///
        /// If we put aside the state and the error (see [RecordRowsF::map), this function makes
        /// `RecordRowsF` a functor (of arity 2). As hinted by the type signature, this function just
        /// maps on "one-level" of recursion, so to speak. Take the instantiated version `RecordRows`,
        /// and record rows of the form `{foo : T, bar: U, baz: V}`. Then, calling `try_map_state(f_ty,
        /// f_rrows, state)` on these rows will map `f_ty` onto `T` and `f_rrows` onto `{bar : U, baz:
        /// V}`.
        ///
        /// Note that `f_ty` isn't mapped onto `U` and `V` recursively: map isn't a recursive
        /// operation. It's however a building block to express recursive operations: as an example,
        /// see [crate::ast::typ::RecordRows::traverse].
        pub fn try_map_state<TyO, RRowsO, FTy, FRRows, S, E>(
            self,
            mut f_ty: FTy,
            mut f_rrows: FRRows,
            state: &mut S,
        ) -> Result<RecordRowsF<TyO, RRowsO>, E>
        where
            FTy: FnMut(Ty, &mut S) -> Result<TyO, E>,
            FRRows: FnMut(RRows, &mut S) -> Result<RRowsO, E>,
        {
            match self {
                RecordRowsF::Empty => Ok(RecordRowsF::Empty),
                RecordRowsF::Extend { row: RecordRowF { id, typ }, tail } => {
                    Ok(RecordRowsF::Extend {
                        row: RecordRowF {
                            id,
                            typ: f_ty(typ, state)?,
                        },
                        tail: f_rrows(tail, state)?,
                    })
                }
                RecordRowsF::TailDyn => Ok(RecordRowsF::TailDyn),
                RecordRowsF::TailVar(id) => Ok(RecordRowsF::TailVar(id)),
            }
        }
        /// Variant of `try_map_state` without threaded state.
        pub fn try_map<TyO, RRowsO, FTy, FRRows, E>(
            self,
            mut f_ty: FTy,
            mut f_rrows: FRRows,
        ) -> Result<RecordRowsF<TyO, RRowsO>, E>
        where
            FTy: FnMut(Ty) -> Result<TyO, E>,
            FRRows: FnMut(RRows) -> Result<RRowsO, E>,
        {
            let f_ty_lifted = |rrow: Ty, _: &mut ()| -> Result<TyO, E> { f_ty(rrow) };
            let f_rrows_lifted = |rrows: RRows, _: &mut ()| -> Result<RRowsO, E> {
                f_rrows(rrows)
            };
            self.try_map_state(f_ty_lifted, f_rrows_lifted, &mut ())
        }
        /// Variant of `try_map_state` with infallible functions.
        pub fn map_state<TyO, RRowsO, FTy, FRRows, S>(
            self,
            mut f_ty: FTy,
            mut f_rrows: FRRows,
            state: &mut S,
        ) -> RecordRowsF<TyO, RRowsO>
        where
            FTy: FnMut(Ty, &mut S) -> TyO,
            FRRows: FnMut(RRows, &mut S) -> RRowsO,
        {
            let f_ty_lifted = |rrow: Ty, state: &mut S| -> Result<TyO, ()> {
                Ok(f_ty(rrow, state))
            };
            let f_rrows_lifted = |rrows: RRows, state: &mut S| -> Result<RRowsO, ()> {
                Ok(f_rrows(rrows, state))
            };
            self.try_map_state(f_ty_lifted, f_rrows_lifted, state).unwrap()
        }
        /// Variant of `try_map_state` without threaded state and with infallible functions.
        pub fn map<TyO, RRowsO, FTy, FRRows>(
            self,
            mut f_ty: FTy,
            mut f_rrows: FRRows,
        ) -> RecordRowsF<TyO, RRowsO>
        where
            FTy: FnMut(Ty) -> TyO,
            FRRows: FnMut(RRows) -> RRowsO,
        {
            let f_ty_lifted = |rrow: Ty| -> Result<TyO, Infallible> { Ok(f_ty(rrow)) };
            let f_rrows_lifted = |rrows: RRows| -> Result<RRowsO, Infallible> {
                Ok(f_rrows(rrows))
            };
            self.try_map(f_ty_lifted, f_rrows_lifted).unwrap()
        }
    }
    impl<Ty, ERows> EnumRowsF<Ty, ERows> {
        /// Map functions over the tail of enum rows. The mutable state ( `S`) is threaded through the
        /// calls to the mapped function. The function is fallible and may return an error `E`, which
        /// causes `try_map_state` to return early with the same error.
        ///
        /// If we put aside the state and the error (see [EnumRowsF::map), this function makes
        /// `EnumRowsF` a functor. As hinted by the type signature, this function just maps on
        /// "one-level" of recursion, so to speak. Take the instantiated version `EnumRows`, and
        /// enum rows of the form ``[| 'foo, 'bar, 'baz |]``. Then, calling `try_map_state(f_erows,
        /// state)` on these rows will map `f_erows` onto ``[| 'bar, 'baz |]``.
        ///
        /// Note that `f_erows` is just mapped once. Map isn't a recursive operation. It's however a
        /// building block to express recursive operations: as an example, see
        /// [crate::ast::typ::RecordRows::traverse].
        pub fn try_map_state<TyO, ERowsO, FTy, FERows, S, E>(
            self,
            mut f_ty: FTy,
            f_erows: FERows,
            state: &mut S,
        ) -> Result<EnumRowsF<TyO, ERowsO>, E>
        where
            FTy: FnMut(Ty, &mut S) -> Result<TyO, E>,
            FERows: FnOnce(ERows, &mut S) -> Result<ERowsO, E>,
        {
            match self {
                EnumRowsF::Empty => Ok(EnumRowsF::Empty),
                EnumRowsF::Extend { row: EnumRowF { id, typ }, tail } => {
                    Ok(EnumRowsF::Extend {
                        row: EnumRowF {
                            id,
                            typ: typ.map(|ty| f_ty(ty, state)).transpose()?,
                        },
                        tail: f_erows(tail, state)?,
                    })
                }
                EnumRowsF::TailVar(id) => Ok(EnumRowsF::TailVar(id)),
            }
        }
        /// Variant of `try_map_state` without threaded state.
        pub fn try_map<TyO, ERowsO, FTy, FERows, E>(
            self,
            mut f_ty: FTy,
            mut f_erows: FERows,
        ) -> Result<EnumRowsF<TyO, ERowsO>, E>
        where
            FTy: FnMut(Ty) -> Result<TyO, E>,
            FERows: FnMut(ERows) -> Result<ERowsO, E>,
        {
            let f_ty_lifted = |erow: Ty, _: &mut ()| -> Result<TyO, E> { f_ty(erow) };
            let f_erows_lifted = |erows: ERows, _: &mut ()| -> Result<ERowsO, E> {
                f_erows(erows)
            };
            self.try_map_state(f_ty_lifted, f_erows_lifted, &mut ())
        }
        /// Variant of `try_map_state` with infallible functions.
        pub fn map_state<TyO, ERowsO, FTy, FERows, S>(
            self,
            mut f_ty: FTy,
            mut f_erows: FERows,
            state: &mut S,
        ) -> EnumRowsF<TyO, ERowsO>
        where
            FTy: FnMut(Ty, &mut S) -> TyO,
            FERows: FnMut(ERows, &mut S) -> ERowsO,
        {
            let f_ty_lifted = |erow: Ty, state: &mut S| -> Result<TyO, ()> {
                Ok(f_ty(erow, state))
            };
            let f_erows_lifted = |erows: ERows, state: &mut S| -> Result<ERowsO, ()> {
                Ok(f_erows(erows, state))
            };
            self.try_map_state(f_ty_lifted, f_erows_lifted, state).unwrap()
        }
        /// Variant of `try_map_state` without threaded state and with infallible functions.
        pub fn map<TyO, ERowsO, FTy, FERows>(
            self,
            mut f_ty: FTy,
            mut f_erows: FERows,
        ) -> EnumRowsF<TyO, ERowsO>
        where
            FTy: FnMut(Ty) -> TyO,
            FERows: FnMut(ERows) -> ERowsO,
        {
            let f_ty_lifted = |erow: Ty| -> Result<TyO, Infallible> { Ok(f_ty(erow)) };
            let f_erows_lifted = |erows: ERows| -> Result<ERowsO, Infallible> {
                Ok(f_erows(erows))
            };
            self.try_map(f_ty_lifted, f_erows_lifted).unwrap()
        }
    }
    impl<Ty, RRows, ERows, Te> TypeF<Ty, RRows, ERows, Te> {
        /// Map functions over the children nodes of a type, when seen as a tree. The mutable state (
        /// `S`) is threaded through the calls to the mapped functions. Functions are fallible and may
        /// return an error `E`, which causes `try_map_state` to return early with the same error.
        ///
        /// If we put aside the state and the error (see [RecordRowsF::map), this function makes
        /// `TypeF` a functor (of arity 3). As hinted by the type signature, this function just
        /// maps on "one-level" of recursion, so to speak.
        ///
        /// Take the instantiated version `Type`, and a type of the form `(Dyn -> Dyn) -> (Number ->
        /// Dyn)`. Then, calling `try_map_state(f_ty, ..)` on this type rows will map `f_ty` onto `(Dyn
        /// -> Dyn)` and `Number -> Dyn` because they are direct children of the root `Arrow` node.
        ///
        /// Note that `f_ty` isn't mapped onto `Dyn` and `Number` recursively: map isn't a recursive
        /// operation. It's however a building block to express recursive operations: as an example,
        /// see [crate::ast::typ::RecordRows::traverse].
        ///
        /// Since `TypeF` may contain record rows and enum rows as well, `f_rrows` and `f_erows` are
        /// required to know how to map on record and enum types respectively.
        pub fn try_map_state<TyO, RRowsO, ERowsO, TeO, FTy, FRRows, FERows, FTe, S, E>(
            self,
            mut f: FTy,
            mut f_rrows: FRRows,
            mut f_erows: FERows,
            mut f_ctr: FTe,
            state: &mut S,
        ) -> Result<TypeF<TyO, RRowsO, ERowsO, TeO>, E>
        where
            FTy: FnMut(Ty, &mut S) -> Result<TyO, E>,
            FRRows: FnMut(RRows, &mut S) -> Result<RRowsO, E>,
            FERows: FnMut(ERows, &mut S) -> Result<ERowsO, E>,
            FTe: FnMut(Te, &mut S) -> Result<TeO, E>,
        {
            match self {
                TypeF::Dyn => Ok(TypeF::Dyn),
                TypeF::Number => Ok(TypeF::Number),
                TypeF::Bool => Ok(TypeF::Bool),
                TypeF::String => Ok(TypeF::String),
                TypeF::ForeignId => Ok(TypeF::ForeignId),
                TypeF::Symbol => Ok(TypeF::Symbol),
                TypeF::Contract(t) => Ok(TypeF::Contract(f_ctr(t, state)?)),
                TypeF::Arrow(dom, codom) => {
                    Ok(TypeF::Arrow(f(dom, state)?, f(codom, state)?))
                }
                TypeF::Var(i) => Ok(TypeF::Var(i)),
                TypeF::Forall { var, var_kind, body } => {
                    Ok(TypeF::Forall {
                        var,
                        var_kind,
                        body: f(body, state)?,
                    })
                }
                TypeF::Enum(erows) => Ok(TypeF::Enum(f_erows(erows, state)?)),
                TypeF::Record(rrows) => Ok(TypeF::Record(f_rrows(rrows, state)?)),
                TypeF::Dict { type_fields, flavour: attrs } => {
                    Ok(TypeF::Dict {
                        type_fields: f(type_fields, state)?,
                        flavour: attrs,
                    })
                }
                TypeF::Array(t) => Ok(TypeF::Array(f(t, state)?)),
                TypeF::Wildcard(i) => Ok(TypeF::Wildcard(i)),
            }
        }
        /// Variant of `try_map_state` without threaded state.
        pub fn try_map<TyO, RRowsO, ERowsO, TeO, FTy, FRRows, FERows, FTe, E>(
            self,
            mut f: FTy,
            mut f_rrows: FRRows,
            mut f_erows: FERows,
            mut f_ctr: FTe,
        ) -> Result<TypeF<TyO, RRowsO, ERowsO, TeO>, E>
        where
            FTy: FnMut(Ty) -> Result<TyO, E>,
            FRRows: FnMut(RRows) -> Result<RRowsO, E>,
            FERows: FnMut(ERows) -> Result<ERowsO, E>,
            FTe: FnMut(Te) -> Result<TeO, E>,
        {
            let f_lifted = |ty: Ty, _: &mut ()| -> Result<TyO, E> { f(ty) };
            let f_rrows_lifted = |rrows: RRows, _: &mut ()| -> Result<RRowsO, E> {
                f_rrows(rrows)
            };
            let f_erows_lifted = |erows: ERows, _: &mut ()| -> Result<ERowsO, E> {
                f_erows(erows)
            };
            let f_ctr_lifted = |ctr: Te, _: &mut ()| -> Result<TeO, E> { f_ctr(ctr) };
            self.try_map_state(
                f_lifted,
                f_rrows_lifted,
                f_erows_lifted,
                f_ctr_lifted,
                &mut (),
            )
        }
        /// Variant of `try_map_state` with infallible functions.
        pub fn map_state<TyO, RRowsO, ERowsO, TeO, FTy, FRRows, FERows, FTe, S>(
            self,
            mut f: FTy,
            mut f_rrows: FRRows,
            mut f_erows: FERows,
            mut f_ctr: FTe,
            state: &mut S,
        ) -> TypeF<TyO, RRowsO, ERowsO, TeO>
        where
            FTy: FnMut(Ty, &mut S) -> TyO,
            FRRows: FnMut(RRows, &mut S) -> RRowsO,
            FERows: FnMut(ERows, &mut S) -> ERowsO,
            FTe: FnMut(Te, &mut S) -> TeO,
        {
            let f_lifted = |ty: Ty, state: &mut S| -> Result<TyO, Infallible> {
                Ok(f(ty, state))
            };
            let f_rrows_lifted = |
                rrows: RRows,
                state: &mut S,
            | -> Result<RRowsO, Infallible> { Ok(f_rrows(rrows, state)) };
            let f_erows_lifted = |
                erows: ERows,
                state: &mut S,
            | -> Result<ERowsO, Infallible> { Ok(f_erows(erows, state)) };
            let f_ctr_lifted = |ctr: Te, state: &mut S| -> Result<TeO, Infallible> {
                Ok(f_ctr(ctr, state))
            };
            self.try_map_state(
                    f_lifted,
                    f_rrows_lifted,
                    f_erows_lifted,
                    f_ctr_lifted,
                    state,
                )
                .unwrap()
        }
        /// Variant of `try_map_state` without threaded state and with infallible functions.
        pub fn map<TyO, RRowsO, ERowsO, TeO, FTy, FRRows, FERows, FTe>(
            self,
            mut f: FTy,
            mut f_rrows: FRRows,
            mut f_erows: FERows,
            mut f_ctr: FTe,
        ) -> TypeF<TyO, RRowsO, ERowsO, TeO>
        where
            FTy: FnMut(Ty) -> TyO,
            FRRows: FnMut(RRows) -> RRowsO,
            FERows: FnMut(ERows) -> ERowsO,
            FTe: FnMut(Te) -> TeO,
        {
            let f_lifted = |ty: Ty, _: &mut ()| -> TyO { f(ty) };
            let f_rrows_lifted = |rrows: RRows, _: &mut ()| -> RRowsO { f_rrows(rrows) };
            let f_erows_lifted = |erows: ERows, _: &mut ()| -> ERowsO { f_erows(erows) };
            let f_ctr_lifted = |ctr: Te, _: &mut ()| -> TeO { f_ctr(ctr) };
            self.map_state(
                f_lifted,
                f_rrows_lifted,
                f_erows_lifted,
                f_ctr_lifted,
                &mut (),
            )
        }
        pub fn is_wildcard(&self) -> bool {
            #[allow(non_exhaustive_omitted_patterns)]
            match self {
                TypeF::Wildcard(_) => true,
                _ => false,
            }
        }
        pub fn is_contract(&self) -> bool {
            #[allow(non_exhaustive_omitted_patterns)]
            match self {
                TypeF::Contract(_) => true,
                _ => false,
            }
        }
    }
    pub struct UnboundTypeVariableError(pub LocIdent);
    #[automatically_derived]
    impl ::core::clone::Clone for UnboundTypeVariableError {
        #[inline]
        fn clone(&self) -> UnboundTypeVariableError {
            UnboundTypeVariableError(::core::clone::Clone::clone(&self.0))
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for UnboundTypeVariableError {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "UnboundTypeVariableError",
                &&self.0,
            )
        }
    }
    impl From<UnboundTypeVariableError> for ParseError {
        fn from(err: UnboundTypeVariableError) -> Self {
            ParseError::UnboundTypeVariables(
                <[_]>::into_vec(::alloc::boxed::box_new([err.0])),
            )
        }
    }
    impl From<UnboundTypeVariableError> for ParseErrors {
        fn from(err: UnboundTypeVariableError) -> Self {
            ParseErrors::from(ParseError::from(err))
        }
    }
}
