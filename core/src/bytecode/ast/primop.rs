//! Primitive operations of the source language (there exists additional primops that are used at
//! runtime but can't be expressed in Nickel code).

use std::fmt;

use crate::{identifier::LocIdent, label::MergeKind};

pub use crate::term::RecordOpKind;

/// Nickel primitive operations.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PrimOp {
    // Unary operators or operator that are eager only in their first argument.
    /// Return an enum tag representing the type of the term.
    ///
    /// # Arguments
    ///
    /// 1. The term to get the type of.
    Typeof,

    /// Return an enum whose tag represents the type of the term and whose content
    /// carries a statically typed value.
    ///
    /// # Arguments
    ///
    /// 1. The term to get the type of.
    Cast,

    /// Boolean AND operator.
    ///
    /// # Arguments
    ///
    /// 1. The left condition.
    /// 2. (Lazy) The right condition.
    BoolAnd,

    /// Boolean OR operator.
    ///
    /// # Arguments
    ///
    /// 1. The left condition.
    /// 2. (Lazy) The right condition.
    BoolOr,

    /// Boolean NOT operator.
    ///
    /// # Arguments
    ///
    /// 1. The left condition.
    /// 2. (Lazy) The right condition.
    BoolNot,

    /// Raise a blame, which stops the execution and prints an error according to the label
    /// argument.
    ///
    /// # Arguments
    ///
    /// 1. The label to blame.
    Blame,

    /// Typecast an enum to a larger enum type.
    ///
    /// [Self::EnumEmbed] is used to upcast enums. For example, if a value `x` has enum type `[|
    /// 'a, 'b |]`, then `%enum/embed% c 'x` will have enum type `[| 'a, 'b, 'c |]`. It only
    /// affects typechecking as at runtime `%enum/embed% x` acts like the identity function.
    ///
    /// # Arguments
    ///
    /// 1. The enum to embed in a larger type.
    EnumEmbed(LocIdent),

    /// Static record access.
    ///
    /// Static means that the field identifier is a statically known string inside the source. This
    /// is how `record.field` is represented in the AST.
    ///
    /// # Arguments
    ///
    /// 1. The record to access.
    RecordStatAccess(LocIdent),

    /// Map a function on each element of an array.
    ///
    /// # Arguments
    ///
    /// 1. The array to map on.
    /// 2. (Lazy) The function to map.
    ArrayMap,

    /// Map a function on a record.
    ///
    /// The mapped function must take two arguments, the name of the field as a string, and the
    /// content of the field. [Self::RecordMap] then replaces the content of each field by the
    /// result of the function: i.e., `%record/map% { a = 2} f` evaluates to `{ a = f "a" 2 }`.
    ///
    /// # Arguments
    ///
    /// 1. The record to map on.
    /// 2. (Lazy) The function to map.
    RecordMap,

    /// Inverse the polarity of a label.
    ///
    /// # Arguments
    ///
    /// 1. The label.
    LabelFlipPol,

    /// Get the polarity of a label.
    ///
    /// # Arguments
    ///
    /// 1. The label.
    LabelPol,

    /// Go to the domain in the type path of a label.
    ///
    /// If the argument is a label with a [type path][crate::label::TyPath) representing some
    /// subtype of the type of the original contract, as in:
    ///
    /// ```text
    /// (Num -> Num) -> Num
    ///  ^^^^^^^^^^ type path
    /// ------------------- original type
    /// ```
    ///
    /// Then `GoDom` evaluates to a copy of this label, where the path has gone forward into the
    /// domain:
    ///
    /// ```text
    /// (Num -> Num) -> Num
    ///  ^^^ new type path
    /// ------------------- original type
    /// ```
    ///
    /// # Arguments
    ///
    /// 1. The label.
    LabelGoDom,

    /// Go to the codomain in the type path of a label.
    ///
    /// See `GoDom`.
    ///
    /// # Arguments
    ///
    /// 1. The label.
    LabelGoCodom,

    /// Go to the array in the type path of a label.
    ///
    /// See `GoDom`.
    ///
    /// # Arguments
    ///
    /// 1. The label.
    LabelGoArray,

    /// Go to the type ascribed to every field in a dictionary.
    ///
    /// See `GoDom`.
    ///
    /// # Arguments
    ///
    /// 1. The label.
    LabelGoDict,

    /// Force the evaluation of its argument and proceed with the second.
    ///
    /// # Arguments
    ///
    /// 1. The term to force.
    /// 2. (Lazy) The term to proceed with (the continuation).
    Seq,

    /// Recursively force the evaluation of its first argument then returns the second.
    ///
    /// Recursive here means that the evaluation does not stop at a WHNF, but the content of arrays
    /// and records is also recursively forced.
    ///
    /// # Arguments
    ///
    /// 1. The term to force.
    /// 2. (Lazy) The term to proceed with (the continuation).
    DeepSeq,

    /// Return the length of an array.
    ///
    /// # Arguments
    ///
    /// 1. The array to get the length of.
    ArrayLength,

    /// Generate an array of a given length by mapping a `Num -> Num` function onto `[1,..,n]`.
    ///
    /// # Arguments
    ///
    /// 1. The length of the array.
    /// 2. (Lazy) The generating function.
    ArrayGen,

    /// Return the names of the fields of a record as a string array.
    ///
    /// # Arguments
    ///
    /// 1. The record to get the fields of.
    RecordFields(RecordOpKind),

    /// Return the values of the fields of a record as an array.
    ///
    /// # Arguments
    ///
    /// 1. The record to get the values of the fields of.
    RecordValues,

    /// Remove heading and trailing spaces from a string.
    ///
    /// # Arguments
    ///
    /// 1. The string to trim.
    StringTrim,

    /// Return the array of characters of a string.
    ///
    /// # Arguments
    ///
    /// 1. The string to get the characters of.
    StringChars,

    /// Transform a string to uppercase.
    ///
    /// # Arguments
    ///
    /// 1. The string to transform.
    StringUppercase,

    /// Transform a string to lowercase.
    ///
    /// # Arguments
    ///
    /// 1. The string to transform.
    StringLowercase,

    /// Return the length of a string.
    ///
    /// # Arguments
    ///
    /// 1. The string to get the length of.
    StringLength,

    /// Transform a data to a string.
    ///
    /// # Arguments
    ///
    /// 1. The data to transform.
    ToString,

    /// Transform a string to a number.
    ///
    /// # Arguments
    ///
    /// 1. The string to transform.
    NumberFromString,

    /// Transform a string to an enum.
    ///
    /// # Arguments
    ///
    /// 1. The string to transform.
    EnumFromString,

    /// Test if a regex matches a string.
    ///
    /// Like [Self::StringFind], this is a unary operator because we would like a way to share the
    /// same "compiled regex" for many matching calls. This is done by returning a closure which
    /// store the compiled regex and can then be applied to many arguments with recompilation.
    ///
    /// # Arguments
    ///
    /// 1. The regex to match.
    /// 2. (Lazy) The string to match.
    StringIsMatch,

    /// Match a regex on a string, and returns the captured groups together, the index of the
    /// match, etc.
    ///
    /// # Arguments
    ///
    /// 1. The regex to match.
    /// 2. (Lazy) The string to match.
    StringFind,

    /// Returns all matches of a regex on a string, as an array of matches. Each
    /// match contains the match groups, the starting index of the match and the
    /// matched string.
    ///
    /// # Arguments
    ///
    /// 1. The regex to match.
    /// 2. (Lazy) The string to match.
    StringFindAll,

    /// Force full evaluation of a term and return it.
    ///
    /// This primop has been added in the context of lazy array contracts, to make serialization
    /// work with lazy array contracts in general.
    ///
    /// # `Force` vs. `DeepSeq`
    ///
    /// [Self::Force] updates at the indices containing arrays with a new version where the lazy
    /// contracts have all been applied, whereas [Self::DeepSeq] evaluates the same expressions,
    /// but it never updates at the index of an array with lazy contracts with an array where those
    /// contracts have been applied. In a way, the result of lazy contract application in arrays is
    /// "lost" in [Self::DeepSeq], while it's returned in [Self::Force].
    ///
    /// This means we can observe different results between `deep_seq x x` and `force x`, in some
    /// cases.
    ///
    /// It's also worth noting that [Self::DeepSeq] should be, in principle, more efficient that
    /// [Self::Force] as it does less cloning.
    ///
    /// # About `ignore_not_exported`
    ///
    /// When exporting a Nickel term, we first apply `Force` to the term to evaluate it. If there
    /// are record fields that have been marked `not_exported`, they would still be evaluated
    /// ordinarily, see [#1230](https://github.com/tweag/nickel/issues/1230). To stop this from
    /// happening, we introduce the `for_export` parameter here. When `for_export` is `true`, the
    /// evaluation of `Force` will skip fields that are marked as `not_exported`. When `for_export`
    /// is `false`, these fields are evaluated.
    ///
    /// # Arguments
    ///
    /// 1. The term to force.
    Force { ignore_not_exported: bool },

    /// Creates an "empty" record with a sealed tail copied from its record argument.
    ///
    /// Used in the `$record` contract implementation to ensure that we can define a `field_diff`
    /// function that preserves the sealed polymorphic tail of its argument.
    ///
    /// # Arguments
    ///
    /// 1. The model record to take the tail from.
    RecordEmptyWithTail,

    /// Freezes a recursive record to make it a static dictionary. Apply all pending lazy contracts
    /// (and flush them), and remove all dependency information, so that the value of the fields is
    /// fixed in time and subsequent overrides will only impact the overridden field.
    ///
    /// # Arguments
    ///
    /// 1. The record to freeze.
    RecordFreeze,

    /// Print a message when encountered during evaluation and proceed with the evaluation of the
    /// argument on the top of the stack. Operationally the same as the identity function
    ///
    /// # Arguments
    ///
    /// 1. The term to trace.
    /// 2. (Lazy) The term to proceed with (the continuation).
    Trace,

    /// Push a new, fresh diagnostic on the diagnostic stack of a contract label. This has the
    /// effect of saving the current diagnostic, as following calls to primops that modify the
    /// label's current diagnostic will modify the fresh one, instead of the one being stacked.
    /// This primop shouldn't be used directly by user a priori, but is used internally during e.g.
    /// contract application.
    ///
    /// # Arguments
    ///
    /// 1. The label to push the diagnostic on.
    LabelPushDiag,

    /// Evaluate a string of nix code into a resulting nickel value. Currently completely
    /// (strictly) evaluates the nix code, and must result in a value serializable into JSON.
    ///
    /// # Arguments
    ///
    /// 1. The nix code to evaluate.
    #[cfg(feature = "nix-experimental")]
    EvalNix,

    /// Retrieve the argument from an enum variant: `%enum/get_arg% ('Foo t) := t`
    ///
    /// # Arguments
    ///
    /// 1. The enum variant to get the argument from.
    EnumGetArg,
    /// Create an enum variant from a tag and an argument.
    ///
    /// # Arguments
    ///
    /// 1. The tag of the variant.
    /// 2. (Lazy) The argument of the variant.
    EnumMakeVariant,
    /// Return true if the given parameter is an enum variant.
    ///
    /// # Arguments
    ///
    /// 1. The enum to check.
    EnumIsVariant,
    /// Extract the tag from an enum tag or an enum variant.
    ///
    /// # Arguments
    ///
    /// 1. The enum to get the tag from.
    EnumGetTag,

    /// Wrap a contract implementation as a custom contract. You can think of this primop as a type
    /// constructor for custom contracts.
    ///
    /// # Arguments
    ///
    /// 1. The contract implementation (a function).
    ContractCustom,

    /// The cosinus function.
    ///
    /// # Arguments
    ///
    /// 1. The numeral argument.
    NumberArcCos,

    /// The sinus function.
    ///
    /// # Arguments
    ///
    /// 1. The numeral argument.
    NumberArcSin,

    /// The tangent function.
    ///
    /// # Arguments
    ///
    /// 1. The numeral argument.
    NumberArcTan,

    /// The cosinus function.
    ///
    /// # Arguments
    ///
    /// 1. The numeral argument.
    NumberCos,

    /// The sinus function.
    ///
    /// # Arguments
    ///
    /// 1. The numeral argument.
    NumberSin,

    /// The tangent function.
    ///
    /// # Arguments
    ///
    /// 1. The numeral argument.
    NumberTan,

    // Binary operators or multi-ary operators that are eager in their two first arguments.
    /// Addition of numerals.
    ///
    /// # Arguments
    ///
    /// 1. The left numeral.
    /// 2. The right numeral.
    Plus,

    /// Subtraction of numerals.
    ///
    /// # Arguments
    ///
    /// 1. The left numeral.
    /// 2. The right numeral.
    Sub,

    /// Multiplication of numerals.
    ///
    /// # Arguments
    ///
    /// 1. The left numeral.
    /// 2. The right numeral.
    Mult,

    /// Rational division of numerals.
    ///
    /// # Arguments
    ///
    /// 1. The dividend.
    /// 2. The divisor.
    Div,

    /// Modulo of numerals.
    ///
    /// # Arguments
    ///
    /// 1. The left numeral.
    /// 2. The right numeral.
    Modulo,

    /// Give the four quadrant arctangent of y and x.
    ///
    /// # Arguments
    ///
    /// 1. The first numeral.
    /// 2. The second numeral.
    NumberArcTan2,

    /// Give the logarithm of a number.
    ///
    /// # Arguments
    ///
    /// 1. The first numeral.
    /// 2. The second numeral.
    NumberLog,

    /// Raise a number to a power.
    ///
    /// # Arguments
    ///
    /// 1. The first numeral.
    /// 2. The second numeral.
    Pow,

    /// Concatenation of strings.
    ///
    /// # Arguments
    ///
    /// 1. The left string.
    /// 2. The right string.
    StringConcat,

    /// Polymorphic equality.
    ///
    /// # Arguments
    ///
    /// 1. The left value.
    /// 2. The right value.
    Eq,

    /// Strictly less than comparison operator.
    ///
    /// # Arguments
    ///
    /// 1. The left numeral.
    /// 2. The right numeral.
    LessThan,

    /// Less than or equal comparison operator.
    ///
    /// # Arguments
    ///
    /// 1. The left numeral.
    /// 2. The right numeral.
    LessOrEq,

    /// Strictly greater than comparison operator.
    ///
    /// # Arguments
    ///
    /// 1. The left numeral.
    /// 2. The right numeral.
    GreaterThan,

    /// Greater than or equal comparison operator.
    ///
    /// # Arguments
    ///
    /// 1. The left numeral.
    /// 2. The right numeral.
    GreaterOrEq,

    /// Apply a contract to a label and a value. The value is is stored on the stack unevaluated,
    /// while the contract and the label are the strict arguments to this operator.
    /// [Self::ContractApply] also accepts contracts as records, which are translated to a function
    /// that merge said contract with its argument. Finally, this operator marks the location of
    /// the contract argument on the stack for better error reporting.
    ///
    /// Either the contract raises a blame error, or the primop evaluates to the value returned by
    /// the contract that can be used in place of the original value.
    ///
    /// # Arguments
    ///
    /// 1. The contract to apply.
    /// 2. The label.
    /// 3. (Lazy) The argument to the contract.
    ContractApply,

    /// Variant of [Self::ContractApply] which also applies an arbitrary contract to a label and a
    /// value, but instead of either blaming or returning the value, it has the same return value
    /// as contract built via [Self::ContractCustom], that is `[| 'Ok Dyn, 'Error {..}|]`. The
    /// value returned through `'Ok` can still have lazy blame expressions inside, of course.
    ///
    /// Put differently, `%contract/check% (%contract/custom% custom) label value` is equivalent to
    /// `custom label value`, modulo argument tracking. [Self::ContractCheck] doesn't only work on
    /// custom contracts, but on builtin contracts as well.
    ///
    /// This operation is useful for contract composition, that is when calling a contract from
    /// another contract. In theory, one could use [Self::ContractApply], but the caller then needs
    /// to wrap the result in `'Ok`, and much more importantly, `%contract/apply%` converts all
    /// immediate errors returned as `'Error` into blame errors. This is not desirable, as blame
    /// errors can't be caught, which artificially makes the called contract entirely delayed. This
    /// typically wouldn't play very well with boolean combinators. On the other hand,
    /// [Self::ContractCheck] preserves the immediate/delayed part of the called contract.
    ///
    /// # Arguments
    ///
    /// 1. The contract to apply.
    /// 2. The label.
    /// 3. (Lazy) The argument to the contract.
    ContractCheck,

    /// Take a record of type `{message | String | optional, notes | String | optional}`, a label,
    /// and return the label with the diagnostic set accordingly.
    ///
    /// # Arguments
    ///
    /// 1. The error diagnostic as a record.
    /// 2. The label.
    LabelWithErrorData,

    /// Go to a specific field in the type path of a label.
    ///
    /// See [Self::LabelGoDom].
    ///
    /// # Arguments
    ///
    /// 1. The field (as a string)
    /// 2. The label.
    LabelGoField,

    /// Insert a new field into a record. Fail if the field already exists.
    ///
    /// # Arguments
    ///
    /// 1. The field name.
    /// 2. The record.
    /// 3. (Lazy) The value to insert.
    RecordInsert(RecordOpKind),

    /// Remove a field from a record. The field name is given as an argument.
    ///
    /// # Arguments
    ///
    /// 1. The field name.
    /// 2. The record.
    RecordRemove(RecordOpKind),

    /// Dynamically access a field of record. The field name is given as an argument which should
    /// evaluate to a string. This is how `record."%{field}"` is represented in the AST.
    ///
    /// # Arguments
    ///
    /// 1. The field name.
    /// 2. The record.
    RecordGet,

    /// Test if a record has a specific field.
    ///
    /// # Arguments
    ///
    /// 1. The field name.
    /// 2. The record.
    RecordHasField(RecordOpKind),

    /// Test if the field of a record exists and has a definition.
    ///
    /// # Arguments
    ///
    /// 1. The field name.
    /// 2. The record.
    RecordFieldIsDefined(RecordOpKind),

    /// Take a pair of records and split them into four separate records:
    ///
    /// - `left_only`: fields of the left argument but not in the right
    /// - `left_center`: fields of the left argument that happens to also be in the right (but the
    ///   value and the metadata are taken from the left)
    /// - `right_center`: fields of the right argument that happens to also be in the left (but the
    ///   value and the metadata are taken from the right)
    /// - `right_only`: fields of the right argument but not in the left
    ///
    /// As opposed to an equivalent user-defined implementation, this primop has better performance
    /// and is able to preserve field metadata.
    ///
    /// If `left` (resp. `right`) is open or has a sealed tail, then `left_only` (resp.
    /// `right_only`) will inherit the same properties. `left_center` (resp. `right_center`) is
    /// always closed and without a sealed tail.
    ///
    /// # Arguments
    ///
    /// 1. The left record.
    /// 2. The right record.
    RecordSplitPair,

    /// Take a pair of disjoint records (i.e. records with no common field) and combine them into
    /// one. It's a form of merging, but based on the assumption that the records are disjoint and
    /// thus non-conflicting, it's simpler and more efficient than a general merge.
    ///
    /// As for merge, this raises a blame error if one of the arguments has a sealed tail.
    ///
    /// # Arguments
    ///
    /// 1. The left record.
    /// 2. The right record.
    RecordDisjointMerge,

    /// Concatenate two arrays.
    ///
    /// # Arguments
    ///
    /// 1. The left array.
    /// 2. The right array.
    ArrayConcat,

    /// Access the n-th element of an array.
    ///
    /// Warning: the arguments are swapped compared to [crate::term::BinaryOp::ArrayAt].
    ///
    /// # Arguments
    ///
    /// 1. The index.
    /// 2. The array.
    ArrayAt,

    /// The merge operator (see [crate::eval::merge]). `Merge` is parametrized by a
    /// [crate::label::MergeKind] flavour, which carries additional information for error-reporting
    /// purpose.
    ///
    /// # Arguments
    ///
    /// 1. The first argument.
    /// 2. The second argument.
    Merge(MergeKind),

    /// Hash a string.
    ///
    /// # Arguments
    ///
    /// 1. An enum representing the hash function to use (`'Md5, `Sha256`, etc.). See Nickel's
    ///    stdlib documentation for `std.hash`.
    /// 2. The string to hash.
    Hash,

    /// Serialize a value to a string.
    ///
    /// # Arguments
    ///
    /// 1. An enum representing the serialization format (`'Json`, `'Yaml`, etc.). See Nickel's
    ///    stdlib documentation for `std.serialize`.
    /// 2. The value to serialize.
    Serialize,

    /// Deserialize a string to a value.
    ///
    /// # Arguments
    ///
    /// 1. An enum representing the serialization format (`'Json`, `'Yaml`, etc.). See Nickel's
    ///    stdlib documentation for `std.deserialize`.
    /// 2. The string to deserialize.
    Deserialize,

    /// Split a string into an array.
    ///
    /// # Arguments
    ///
    /// 1. The string to split.
    /// 2. The separator.
    StringSplit,

    /// Determine if a string is a substring of another one.
    ///
    /// Warning: the arguments are swapped compared to [crate::term::BinaryOp::StringContains].
    ///
    /// # Arguments
    ///
    /// 1. The substring.
    /// 2. The string.
    StringContains,

    /// Compare two strings lexicographically.
    ///
    /// # Arguments
    ///
    /// 1. The left string.
    /// 2. The right string.
    StringCompare,

    /// Seal a term with a sealing key (used by the implementation of polymorphic contracts).
    ///
    /// # Arguments
    ///
    /// 1. The sealing key.
    /// 2. The term to seal.
    Seal,

    /// Unseal a term given the right sealing key. See [Self::Seal].
    ///
    /// # Arguments
    ///
    /// 1. The sealing key.
    /// 2. The term to unseal.
    Unseal,

    /// Lazily apply a contract to an Array.
    ///
    /// This simply inserts a contract into the array attributes.
    ///
    /// # Arguments
    ///
    /// 1. The label.
    /// 2. The array.
    /// 3. (Lazy) The contract to apply.
    ContractArrayLazyApp,

    /// Lazily map contracts over a record. The arguments are a label and a function which takes
    /// the name of the field as a parameter and returns the corresponding contract.
    ///
    /// # Arguments
    ///
    /// 1. The label.
    /// 2. The record.
    /// 3. (Lazy) The contract parametrized by field names.
    ContractRecordLazyApp,

    /// Set the message of the current diagnostic of a label.
    ///
    /// # Arguments
    ///
    /// 1. The message.
    /// 2. The label.
    LabelWithMessage,

    /// Set the notes of the current diagnostic of a label.
    ///
    /// # Arguments
    ///
    /// 1. The notes (an array of evaluated string). Important: the notes must be forced before
    ///    calling to this primop.
    /// 2. The label.
    LabelWithNotes,

    /// Append a note to the current diagnostic of a label.
    ///
    /// # Arguments
    ///
    /// 1. The note (a string).
    /// 2. The label.
    LabelAppendNote,

    /// Look up the [`crate::label::TypeVarData`] associated with a sealing key in the type
    /// environment of a [label](crate::label::Label).
    ///
    /// # Arguments
    ///
    /// 1. The sealing key.
    /// 2. The label.
    LabelLookupTypeVar,

    // N-ary primops for `n > 2`.
    /// Replace a substring by another one in a string.
    ///
    /// # Arguments
    ///
    /// 1. String to replace in.
    /// 2. The string to replace.
    /// 3. The replacement.
    StringReplace,

    /// Replace all substrings matching a pattern by a given string.
    ///
    /// # Arguments
    ///
    /// 1. String to replace in.
    /// 2. The pattern to replace.
    /// 3. The replacement.
    StringReplaceRegex,

    /// Return a substring of an original string.
    ///
    /// Warning: the argument order is different than [crate::term::NAryOp::StringSubstr].
    ///
    /// # Arguments
    ///
    /// 1. Start index of the substring (included).
    /// 2. End index of the substring (excluded).
    /// 3. The string to slice.
    StringSubstr,

    /// The merge operator in contract mode (see [crate::eval::merge]).
    ///
    /// # Arguments
    ///
    /// 1. The label.
    /// 2. The record to check against the contract.
    /// 3. The record contract.
    MergeContract,

    /// Seals one record into the tail of another. Used to ensure that functions using polymorphic
    /// record contracts do not violate parametricity.
    ///
    /// # Arguments
    ///
    /// 1. A sealing key, which must be provided later to unseal the tail.
    /// 2. A label, which will be used to assign blame correctly tail access
    ///    is attempted.
    /// 3. A record, which is the record we wish to seal the tail into.
    /// 4. The record that we wish to seal.
    RecordSealTail,

    /// Unseals a term from the tail of a record and returns it.
    ///
    /// # Arguments
    ///
    /// 1. The sealing key, which was used to seal the tail.
    /// 2. A label which will be used to assign blame correctly if
    ///    something goes wrong while unsealing.
    /// 3. The record whose tail we wish to unseal.
    RecordUnsealTail,

    /// Insert type variable data into the `type_environment` of a [`crate::label::Label`]
    ///
    /// # Arguments
    ///
    /// 1. The sealing key assigned to the type variable.
    /// 2. The introduction polarity](crate::label::Polarity) of the type variable
    /// 3. The [kind](crate::typ::VarKind) of the type variable
    /// 4. A label on which to operate.
    LabelInsertTypeVar,

    /// Return a sub-array corresponding to a range. Given that Nickel uses array slices under the
    /// hood, as long as the array isn't modified later, this operation is constant in time and
    /// memory. However, it will currently retain the original in memory until the slice becomes
    /// dead.
    ///
    /// # Arguments
    ///
    /// 1. The start index of the slice (included).
    /// 2. The end index of the slice (excluded).
    /// 3. The array to slice.
    ArraySlice,
}

/// Syntactic positioning of a primitive operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OpPos {
    Infix,
    Postfix,
    Prefix,
}

impl fmt::Display for PrimOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PrimOp::*;

        match self {
            Typeof => write!(f, "typeof"),
            Cast => write!(f, "cast"),
            BoolAnd => write!(f, "(&&)"),
            BoolOr => write!(f, "(||)"),
            BoolNot => write!(f, "bool/not"),
            Blame => write!(f, "blame"),
            EnumEmbed(_) => write!(f, "enum/embed"),
            RecordStatAccess(_) => write!(f, "record/access"),
            ArrayMap => write!(f, "array/map"),
            RecordMap => write!(f, "record/map"),
            LabelFlipPol => write!(f, "label/flip_polarity"),
            LabelPol => write!(f, "label/polarity"),
            LabelGoDom => write!(f, "label/go_dom"),
            LabelGoCodom => write!(f, "label/go_codom"),
            LabelGoArray => write!(f, "label/go_array"),
            LabelGoDict => write!(f, "label/go_dict"),
            Seq => write!(f, "seq"),
            DeepSeq => write!(f, "deep_seq"),
            ArrayLength => write!(f, "array/length"),
            ArrayGen => write!(f, "array/generate"),
            RecordFields(RecordOpKind::IgnoreEmptyOpt) => write!(f, "record/fields"),
            RecordFields(RecordOpKind::ConsiderAllFields) => write!(f, "record/fields_with_opts"),
            RecordValues => write!(f, "record/values"),
            StringTrim => write!(f, "string/trim"),
            StringChars => write!(f, "string/chars"),
            StringUppercase => write!(f, "string/uppercase"),
            StringLowercase => write!(f, "string/lowercase"),
            StringLength => write!(f, "string/length"),
            ToString => write!(f, "to_string"),
            NumberFromString => write!(f, "number/from_string"),
            EnumFromString => write!(f, "enum/from_string"),
            StringIsMatch => write!(f, "string/is_match"),
            StringFind => write!(f, "string/find"),
            StringFindAll => write!(f, "string/find_all"),
            Force { .. } => write!(f, "force"),
            RecordEmptyWithTail => write!(f, "record/empty_with_tail"),
            RecordFreeze => write!(f, "record/freeze"),
            Trace => write!(f, "trace"),
            LabelPushDiag => write!(f, "label/push_diag"),

            #[cfg(feature = "nix-experimental")]
            EvalNix => write!(f, "eval_nix"),

            EnumGetArg => write!(f, "enum/get_arg"),
            EnumMakeVariant => write!(f, "enum/make_variant"),
            EnumIsVariant => write!(f, "enum/is_variant"),
            EnumGetTag => write!(f, "enum/get_tag"),

            ContractCustom => write!(f, "contract/custom"),

            NumberArcCos => write!(f, "number/arccos"),
            NumberArcSin => write!(f, "number/arcsin"),
            NumberArcTan => write!(f, "number/arctan"),
            NumberCos => write!(f, "number/cos"),
            NumberSin => write!(f, "number/sin"),
            NumberTan => write!(f, "number/tan"),

            Plus => write!(f, "(+)"),
            Sub => write!(f, "(-)"),
            Mult => write!(f, "(*)"),
            Div => write!(f, "(/)"),
            Modulo => write!(f, "(%)"),
            NumberArcTan2 => write!(f, "number/arctan2"),
            NumberLog => write!(f, "number/log"),
            Pow => write!(f, "pow"),
            StringConcat => write!(f, "string/concat"),
            Eq => write!(f, "(==)"),
            LessThan => write!(f, "(<)"),
            LessOrEq => write!(f, "(<=)"),
            GreaterThan => write!(f, "(>)"),
            GreaterOrEq => write!(f, "(>=)"),
            ContractApply => write!(f, "contract/apply"),
            ContractCheck => write!(f, "contract/check"),
            LabelWithErrorData => write!(f, "label/with_error_data"),
            LabelGoField => write!(f, "label/go_field"),
            RecordInsert(RecordOpKind::IgnoreEmptyOpt) => write!(f, "record/insert"),
            RecordInsert(RecordOpKind::ConsiderAllFields) => write!(f, "record/insert_with_opts"),
            RecordRemove(RecordOpKind::IgnoreEmptyOpt) => write!(f, "record/remove"),
            RecordRemove(RecordOpKind::ConsiderAllFields) => write!(f, "record/remove_with_opts"),
            RecordGet => write!(f, "record/get"),
            RecordHasField(RecordOpKind::IgnoreEmptyOpt) => write!(f, "record/has_field"),
            RecordHasField(RecordOpKind::ConsiderAllFields) => {
                write!(f, "record/has_field_with_opts")
            }
            RecordFieldIsDefined(RecordOpKind::IgnoreEmptyOpt) => {
                write!(f, "record/field_is_defined")
            }
            RecordFieldIsDefined(RecordOpKind::ConsiderAllFields) => {
                write!(f, "record/field_is_defined_with_opts")
            }
            Self::RecordSplitPair => write!(f, "record/split_pair"),
            Self::RecordDisjointMerge => write!(f, "record/disjoint_merge"),
            ArrayConcat => write!(f, "(@)"),
            ArrayAt => write!(f, "array/at"),
            Merge(_) => write!(f, "(&)"),
            Hash => write!(f, "hash"),
            Serialize => write!(f, "serialize"),
            Deserialize => write!(f, "deserialize"),
            StringSplit => write!(f, "string/split"),
            StringContains => write!(f, "string/contains"),
            StringCompare => write!(f, "string/compare"),
            Seal => write!(f, "seal"),
            Unseal => write!(f, "unseal"),
            ContractArrayLazyApp => write!(f, "contract/array_lazy_apply"),
            ContractRecordLazyApp => write!(f, "contract/record_lazy_apply"),
            LabelWithMessage => write!(f, "label/with_message"),
            LabelWithNotes => write!(f, "label/with_notes"),
            LabelAppendNote => write!(f, "label/append_note"),
            LabelLookupTypeVar => write!(f, "label/lookup_type_variable"),

            StringReplace => write!(f, "string/replace"),
            StringReplaceRegex => write!(f, "string/replace_regex"),
            StringSubstr => write!(f, "string/substr"),
            MergeContract => write!(f, "record/merge_contract"),
            RecordSealTail => write!(f, "record/seal_tail"),
            RecordUnsealTail => write!(f, "record/unseal_tail"),
            LabelInsertTypeVar => write!(f, "label/insert_type_variable"),
            ArraySlice => write!(f, "array/slice"),
        }
    }
}

impl PrimOp {
    /// Returns the arity of the primop, i.e. the number of forced arguments it takes as a primop
    /// application. Additional lazy arguments aren't counted here, and would be part of an outer
    /// standard function application.
    pub fn arity(&self) -> usize {
        use PrimOp::*;

        match self {
            Typeof
            | Cast
            | BoolAnd
            | BoolOr
            | BoolNot
            | Blame
            | EnumEmbed(_)
            | RecordStatAccess(_)
            | ArrayMap
            | RecordMap
            | LabelFlipPol
            | LabelPol
            | LabelGoDom
            | LabelGoCodom
            | LabelGoArray
            | LabelGoDict
            | Seq
            | DeepSeq
            | ArrayLength
            | ArrayGen
            | RecordFields(_)
            | RecordValues
            | StringTrim
            | StringChars
            | StringUppercase
            | StringLowercase
            | StringLength
            | ToString
            | NumberFromString
            | EnumFromString
            | StringIsMatch
            | StringFind
            | StringFindAll
            | Force { .. }
            | RecordEmptyWithTail
            | RecordFreeze
            | Trace
            | LabelPushDiag
            | EnumGetArg
            | EnumMakeVariant
            | EnumIsVariant
            | EnumGetTag
            | ContractCustom
            | NumberArcCos
            | NumberArcSin
            | NumberArcTan
            | NumberCos
            | NumberSin
            | NumberTan => 1,
            #[cfg(feature = "nix-experimental")]
            EvalNix => 1,

            Plus
            | Sub
            | Mult
            | Div
            | Modulo
            | NumberArcTan2
            | NumberLog
            | Pow
            | StringConcat
            | Eq
            | LessThan
            | LessOrEq
            | GreaterThan
            | GreaterOrEq
            | ContractApply
            | ContractCheck
            | LabelWithErrorData
            | LabelGoField
            | RecordInsert(_)
            | RecordRemove(_)
            | RecordGet
            | RecordHasField(_)
            | RecordFieldIsDefined(_)
            | Self::RecordSplitPair
            | Self::RecordDisjointMerge
            | ArrayConcat
            | ArrayAt
            | Merge(_)
            | Hash
            | Serialize
            | Deserialize
            | StringSplit
            | StringContains
            | StringCompare
            | Seal
            | Unseal
            | ContractArrayLazyApp
            | ContractRecordLazyApp
            | LabelWithMessage
            | LabelWithNotes
            | LabelAppendNote
            | LabelLookupTypeVar => 2,

            StringReplace | StringReplaceRegex | StringSubstr | MergeContract
            | RecordUnsealTail | ArraySlice => 3,

            RecordSealTail | LabelInsertTypeVar => 4,
        }
    }

    /// Returns the syntactic positioning of this operator.
    pub fn positioning(&self) -> OpPos {
        use PrimOp::*;

        match self {
            BoolAnd | BoolOr | RecordStatAccess(_) => OpPos::Postfix,
            Plus | Sub | Mult | Div | Modulo | StringConcat | Eq | LessThan | LessOrEq
            | GreaterThan | GreaterOrEq | ArrayConcat | Merge(_) | RecordGet => OpPos::Infix,
            _ => OpPos::Prefix,
        }
    }
}
