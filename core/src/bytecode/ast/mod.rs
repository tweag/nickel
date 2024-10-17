//! The Nickel AST, as ingested by the bytecode compiler.
//!
//! Since the AST is built once for each Nickel expression and is then compiled away to bytecode,
//! the number nodes ever allocated should be reasonably bounded by the input program size. Thus,
//! for performance reasons, we allocate notes using an arena and keep them alive until the end of
//! compilation. In return, we get fast allocation and de-allocation, and we can easily reference
//! other nodes and data structures using native references.
//!
//! The corresponding lifetime of all the nodes - and thus of the arena as well - is consistently
//! called `'ast`.

use std::{ffi::OsString, rc, fmt};

use pattern::Pattern;
use record::Record;

use crate::{
    cache::InputFormat,
    error::ParseError,
    identifier::LocIdent,
    position::TermPos,
    term::{self, Number, StrChunk},
    typ::Type,
};

use bumpalo::Bump;

pub mod pattern;
pub mod record;

use pattern::*;

#[derive(Clone, Copy, Debug, PartialEq)]
/// A Nickel AST. Contains a root node and a span. Both are references so that `Ast` is cheap to
/// clone - it's in fact `Copy` - and to store.
pub struct Ast<'ast> {
    node: &'ast Node<'ast>,
    pos: &'ast TermPos,
}

/// A node of the Nickel AST.
///
/// Nodes are built by the parser and then mostly traversed immutably. Thus, they are immutable and
/// optimized for sharing and fast cloning. In particular, the data aren't owned and are mostly
/// references that are in practice pointing to one global arena.
///
/// Using an arena has another advantage: the data is allocated in the same order as the AST is
/// built. This means that even if there are reference indirections, the children of a node are
/// most likely close to the node itself in memory, which should be good for cache locality.
#[derive(Clone, Debug, PartialEq, Default)]
pub enum Node<'ast> {
    /// The null value.
    #[default]
    Null,

    /// A boolean value.
    Bool(bool),

    /// A number.
    ///
    /// A number is an arbitrary-precision rational in Nickel. It's not small and thus we put it
    /// behind a reference to avoid size bloat.
    Number(&'ast Number),

    /// A string literal.
    String(&'ast str),

    /// A string containing interpolated expressions, represented as a list of either literals or
    /// expressions.
    ///
    /// As opposed to [crate::term::Term::StrChunks], the chunks are stored in the original order:
    /// `"hello%{var}"` will give `["hello", var]`.
    StrChunks(&'ast [StrChunk<Ast<'ast>>]),

    /// A function.
    Fun(&'ast Pattern<'ast>, Ast<'ast>),

    /// A let-binding.
    Let {
        bindings: &'ast [(Pattern<'ast>, Ast<'ast>)],
        body: Ast<'ast>,
        rec: bool,
    },

    /// An application.
    App { fun: Ast<'ast>, arg: Ast<'ast> },

    /// A variable.
    Var(LocIdent),

    /// An enum variant (an algebraic datatype). Variants have at most once argument: variants with
    /// no arguments are often called simply enum tags. Note that one can just use a record as an
    /// argument to emulate variants with multiple arguments.
    EnumVariant {
        tag: LocIdent,
        arg: Option<Ast<'ast>>,
    },

    /// A record.
    Record(&'ast Record<'ast>),

    /// An if-then-else expression.
    IfThenElse {
        cond: Ast<'ast>,
        then_branch: Ast<'ast>,
        else_branch: Ast<'ast>,
    },

    /// A match expression. This expression is still to be applied to an argument to match on.
    Match(Match<'ast>),

    /// An array.
    Array(&'ast [Ast<'ast>]),

    /// A primitive operator application.
    PrimOp { op: PrimOp, args: &'ast [Ast<'ast>] },

    /// A term with a type and/or contract annotation.
    Annotated {
        annot: Annotation<'ast>,
        inner: Ast<'ast>,
    },

    /// An import.
    Import { path: OsString, format: InputFormat },

    /// A type in term position, such as in `let my_contract = Number -> Number in ...`.
    ///
    /// During evaluation, this will get turned into a contract.
    Type(&'ast Type),

    /// A term that couldn't be parsed properly. Used by the LSP to handle partially valid
    /// programs.
    ParseError(&'ast ParseError),
}

/// A flavor for record operations. By design, we want empty optional values to be transparent for
/// record operations, because they would otherwise make many operations fail spuriously (e.g.
/// trying to map over such an empty value). So they are most of the time silently ignored.
///
/// However, it's sometimes useful and even necessary to take them into account. This behavior is
/// controlled by [RecordOpKind].
#[derive(Clone, Debug, PartialEq, Eq, Copy, Default)]
pub enum RecordOpKind {
    #[default]
    IgnoreEmptyOpt,
    ConsiderAllFields,
}

/// The kind of a dynamic record extension. Kind indicates if a definition is expected for the
/// field being inserted, or if the inserted field doesn't have a definition.
#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub enum RecordExtKind {
    WithValue,
    WithoutValue,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PrimOp {
    /// Unary operators or operator that are eager only in their first argument.

    /// Return an enum tag representing the type of the term.
    Typeof,

    // Boolean AND and OR operator are encoded as unary operators so that they can be lazy in their
    // second argument.
    /// Boolean AND operator.
    BoolAnd,

    /// Boolean OR operator.
    BoolOr,

    /// Boolean NOT operator.
    BoolNot,

    /// Raise a blame, which stops the execution and prints an error according to the label
    /// argument.
    Blame,

    /// Typecast an enum to a larger enum type.
    ///
    /// `EnumEmbed` is used to upcast enums. For example, if a value `x` has enum type `a | b`,
    /// then `%enum/embed% c x` will have enum type `a | b | c`. It only affects typechecking as at
    /// runtime `%enum/embed% someId` acts like the identity function.
    EnumEmbed(LocIdent),

    /// Static record access.
    ///
    /// Static means that the field identifier is a statically known string inside the source.
    RecordGet(LocIdent),

    /// Map a function on each element of an array.
    ArrayMap,

    /// Map a function on a record.
    ///
    /// The mapped function must take two arguments, the name of the field as a string, and the
    /// content of the field. `RecordMap` then replaces the content of each field by the result of
    /// the function: i.e., `%record/map% f {a=2;}` evaluates to `{a=(f "a" 2);}`.
    RecordMap,

    /// Inverse the polarity of a label.
    LabelFlipPol,

    /// Get the polarity of a label.
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
    LabelGoDom,

    /// Go to the codomain in the type path of a label.
    ///
    /// See `GoDom`.
    LabelGoCodom,

    /// Go to the array in the type path of a label.
    ///
    /// See `GoDom`.
    LabelGoArray,

    /// Go to the type ascribed to every field in a dictionary.
    ///
    /// See `GoDom`.
    LabelGoDict,

    /// Force the evaluation of its argument and proceed with the second.
    Seq,

    /// Recursively force the evaluation of its first argument then returns the second.
    ///
    /// Recursive here means that the evaluation does not stop at a WHNF, but the content of arrays
    /// and records is also recursively forced.
    DeepSeq,

    /// Return the length of an array.
    ArrayLength,

    /// Generate an array of a given length by mapping a `Num -> Num` function onto `[1,..,n]`.
    ArrayGen,

    /// Generated by the evaluation of a string with interpolated expressions. `ChunksConcat`
    /// applied to the current chunk to evaluate. As additional state, it uses a string
    /// accumulator, the indentation of the chunk being evaluated, and the remaining chunks to be
    /// evaluated, all stored on the stack.
    ChunksConcat,

    /// Return the names of the fields of a record as a string array.
    RecordFields(RecordOpKind),

    /// Return the values of the fields of a record as an array.
    RecordValues,

    /// Remove heading and trailing spaces from a string.
    StringTrim,

    /// Return the array of characters of a string.
    StringChars,

    /// Transform a string to uppercase.
    StringUppercase,

    /// Transform a string to lowercase.
    StringLowercase,

    /// Return the length of a string.
    StringLength,

    /// Transform a data to a string.
    ToString,

    /// Transform a string to a number.
    NumberFromString,

    /// Transform a string to an enum.
    EnumFromString,

    /// Test if a regex matches a string.
    /// Like [`UnaryOp::StringFind`], this is a unary operator because we would like a way to share
    /// the same "compiled regex" for many matching calls. This is done by returning functions
    /// wrapping [`UnaryOp::StringIsMatchCompiled`] and [`UnaryOp::StringFindCompiled`]
    StringIsMatch,

    /// Match a regex on a string, and returns the captured groups together, the index of the
    /// match, etc.
    StringFind,

    /// Returns all matches of a regex on a string, as an array of matches. Each
    /// match contains the match groups, the starting index of the match and the
    /// matched string.
    StringFindAll,

    /// Force full evaluation of a term and return it.
    ///
    /// This was added in the context of [`BinaryOp::ContractArrayLazyApp`], in particular to make
    /// serialization work with lazy array contracts.
    ///
    /// # `Force` vs. `DeepSeq`
    ///
    /// [`UnaryOp::Force`] updates at the indices containing arrays with a new version where the
    /// lazy contracts have all been applied, whereas [`UnaryOp::DeepSeq`] evaluates the same
    /// expressions, but it never updates at the index of an array with lazy contracts with an array
    /// where those contracts have been applied. In a way, the result of lazy contract application
    /// in arrays is "lost" in [`UnaryOp::DeepSeq`], while it's returned in [`UnaryOp::Force`].
    ///
    /// This means we can observe different results between `deep_seq x x` and `force x`, in some
    /// cases.
    ///
    /// It's also worth noting that [`UnaryOp::DeepSeq`] should be, in principle, more efficient
    /// that [`UnaryOp::Force`] as it does less cloning.
    ///
    /// # About `ignore_not_exported`
    ///
    /// When exporting a Nickel term, we first apply `Force` to the term to evaluate it. If there
    /// are record fields that have been marked `not_exported`, they would still be evaluated
    /// ordinarily, see [#1230](https://github.com/tweag/nickel/issues/1230). To stop this from
    /// happening, we introduce the `for_export` parameter here. When `for_export` is `true`, the
    /// evaluation of `Force` will skip fields that are marked as `not_exported`. When `for_export`
    /// is `false`, these fields are evaluated.
    Force { ignore_not_exported: bool },

    /// Creates an "empty" record with the sealed tail of its [`Term::Record`] argument.
    ///
    /// Used in the `$record` contract implementation to ensure that we can define a `field_diff`
    /// function that preserves the sealed polymorphic tail of its argument.
    RecordEmptyWithTail,

    /// Print a message when encountered during evaluation and proceed with the evaluation of the
    /// argument on the top of the stack. Operationally the same as the identity function
    Trace,

    /// Push a new, fresh diagnostic on the diagnostic stack of a contract label. This has the
    /// effect of saving the current diagnostic, as following calls to primop that modifies the
    /// label's current diagnostic will modify the fresh one, istead of the one being stacked.
    /// This primop shouldn't be used directly by user a priori, but is used internally during e.g.
    /// contract application.
    LabelPushDiag,

    /// Evaluate a string of nix code into a resulting nickel value. Currently completely
    /// (strictly) evaluates the nix code, and must result in a value serializable into JSON.
    #[cfg(feature = "nix-experimental")]
    EvalNix,

    /// Retrive the argument from an enum variant: `%enum/get_arg% ('Foo t) := t`
    EnumGetArg,
    /// Create an enum variant from a tag and an argument. This operator is strict in tag and
    /// return a function that can be further applied to an argument.
    EnumMakeVariant,
    /// Return true if the given parameter is an enum variant.
    EnumIsVariant,
    /// Extract the tag from an enum tag or an enum variant.
    EnumGetTag,

    /// Wrap a contract implementation as a [CustomContract]. You can think of this primop as a
    /// type constructor for custom contracts.
    ContractCustom,

    /// The cosinus function.
    NumberArcCos,

    /// The sinus function.
    NumberArcSin,

    /// The tangent function.
    NumberArcTan,

    /// The cosinus function.
    NumberCos,

    /// The sinus function.
    NumberSin,

    /// The tangent function.
    NumberTan,

    /// Binary operators or multi-ary operators that are eager in their two first arguments.

    /// Addition of numerals.
    Plus,

    /// Subtraction of numerals.
    Sub,

    /// Multiplication of numerals.
    Mult,

    /// Rational division of numerals.
    Div,

    /// Modulo of numerals.
    Modulo,

    /// Give the four quadrant arctangent of y and x.
    NumberArcTan2,

    /// Give the logarithm of a number.
    NumberLog,

    /// Raise a number to a power.
    Pow,

    /// Concatenation of strings.
    StringConcat,

    /// Polymorphic equality.
    Eq,

    /// Strictly less than comparison operator.
    LessThan,

    /// Less than or equal comparison operator.
    LessOrEq,

    /// Strictly greater than comparison operator.
    GreaterThan,

    /// Greater than or equal comparison operator.
    GreaterOrEq,

    /// Apply a contract to a label and a value. The value is is stored on the stack unevaluated,
    /// while the contract and the label are the strict arguments to this operator. `ApplyContract`
    /// also accepts contracts as records, which are translated to a function that merge said
    /// contract with its argument. Finally, this operator marks the location of the contract
    /// argument on the stack for better error reporting.
    ///
    /// Either the contract raises a blame error, or the primop evaluates to the value returned by
    /// the contract that can be used in place of the original value.
    ContractApply,

    /// Variant of [Self::ContractApply] which also applies an arbitrary contract to a label and a
    /// value, but instead of either blaming or returning the value, it has the same return value
    /// as contract built via [UnaryOp::ContractCustom], that is `[| 'Ok Dyn, 'Error {..}|]`. The
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
    ContractCheck,

    /// Take a record of type `{message | String | optional, notes | String | optional}`.
    LabelWithErrorData,

    /// Unseal a sealed term.
    ///
    /// See [`BinaryOp::Seal`].
    Unseal,

    /// Go to a specific field in the type path of a label.
    ///
    /// See `LabelGoDom`.
    LabelGoField,

    /// Extend a record with a dynamic field.
    ///
    /// Dynamic means that the field name may be an expression instead of a statically known
    /// string. `RecordExtend` tries to evaluate this name to a string, and in case of success, add
    /// a field with this name to the given record with the expression on top of the stack as
    /// content.
    ///
    /// The field may have been defined with attached metadata, pending contracts and may or may
    /// not have a defined value. We can't store those information as a term argument (metadata
    /// aren't first class values, at least at the time of writing), so for now we attach it
    /// directly to the extend primop. This isn't ideal, and in the future we may want to have a
    /// more principled primop.
    RecordInsert {
        metadata: FieldMetadata,
        pending_contracts: Vec<RuntimeContract>,
        ext_kind: RecordExtKind,
        op_kind: RecordOpKind,
    },

    /// Remove a field from a record. The field name is given as an argument.
    RecordRemove(RecordOpKind),

    /// Dynamically access a field of record. The field name is given as an argument which should
    /// evaluate to a string.
    RecordGet,

    /// Test if a record has a specific field.
    RecordHasField(RecordOpKind),

    /// Test if the field of a record exists and has a definition.
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
    /// `right_only`) will inherit the same properties. `left_center` (resp. `right_center`) are
    /// always closed and without a sealed tail.
    RecordSplitPair,

    /// Take a pair of disjoint records (i.e. records with no common field) and combine them into
    /// one. It's a form of merging, but based on the assumption that the records are disjoint and
    /// thus non-conflicting, it's simpler and more efficient than a general merge.
    ///
    /// As for merge, this raises a blame error if one of the arguments has a sealed tail.
    RecordDisjointMerge,

    /// Concatenate two arrays.
    ArrayConcat,

    /// Access the n-th element of an array.
    ArrayAt,

    /// The merge operator (see [crate::eval::merge]). `Merge` is parametrized by a
    /// [crate::label::MergeLabel], which carries additional information for error-reporting
    /// purpose.
    Merge(MergeLabel),

    /// Hash a string.
    Hash,

    /// Serialize a value to a string.
    Serialize,

    /// Deserialize a string to a value.
    Deserialize,

    /// Split a string into an array.
    StringSplit,

    /// Determine if a string is a substring of another one.
    StringContains,

    /// Compare two strings lexicographically.
    StringCompare,

    /// Seal a term with a sealing key (see [`Term::Sealed`]).
    Seal,

    /// Lazily apply a contract to an Array.
    /// This simply inserts a contract into the array attributes.
    ContractArrayLazyApp,

    /// Lazily map contracts over a record. The arguments are a label and a function which takes
    /// the name of the field as a parameter and returns the corresponding contract.
    ContractRecordLazyApp,

    /// Set the message of the current diagnostic of a label.
    LabelWithMessage,

    /// Set the notes of the current diagnostic of a label.
    LabelWithNotes,

    /// Append a note to the current diagnostic of a label.
    LabelAppendNote,

    /// Look up the [`crate::label::TypeVarData`] associated with a [`SealingKey`] in the type
    /// environment of a [label](Term::Lbl)
    LabelLookupTypeVar,

}

impl fmt::Display for PrimOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PrimOp::*;

        match self {
            Typeof => write!(f, "typeof"),
            BoolAnd => write!(f, "(&&)"),
            BoolOr => write!(f, "(||)"),
            BoolNot => write!(f, "bool/not"),
            Blame => write!(f, "blame"),
            EnumEmbed(_) => write!(f, "enum/embed"),
            RecordGet(_) => write!(f, "record/access"),
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
            ChunksConcat => write!(f, "chunks_concat"),
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
        }
    }
}

impl PrimOp {
    pub fn arity(&self) -> usize {
        match self {
            _ => todo!(),
        }
    }
}

impl From<&term::UnaryOp> for PrimOp {
    fn from(op: &term::UnaryOp) -> Self {
        todo!()
    }
}

impl From<&term::BinaryOp> for PrimOp {
    fn from(op: &term::BinaryOp) -> Self {
        todo!()
    }
}

impl From<&term::NAryOp> for PrimOp {
    fn from(op: &term::NAryOp) -> Self {
        todo!()
    }
}

/// A branch of a match expression.
#[derive(Debug, PartialEq, Clone)]
pub struct MatchBranch<'ast> {
    /// The pattern on the left hand side of `=>`.
    pub pattern: Pattern<'ast>,
    /// A potential guard, which is an additional side-condition defined as `if cond`. The value
    /// stored in this field is the boolean condition itself.
    pub guard: Option<Ast<'ast>>,
    /// The body of the branch, on the right hand side of `=>`.
    pub body: Ast<'ast>,
}

/// Content of a match expression.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Match<'ast> {
    /// Branches of the match expression, where the first component is the pattern on the left hand
    /// side of `=>` and the second component is the body of the branch.
    pub branches: &'ast [MatchBranch<'ast>],
}

/// A type and/or contract annotation.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Annotation<'ast> {
    /// The type annotation (using `:`).
    pub typ: Option<Type>,

    /// The contracts annotation (using `|`).
    pub contracts: &'ast [Type],
}

impl<'ast> Annotation<'ast> {
    /// Return the main annotation, which is either the type annotation if any, or the first
    /// contract annotation.
    pub fn first(&'ast self) -> Option<&'ast Type> {
        self.typ.as_ref().or(self.contracts.iter().next())
    }

    /// Iterate over the annotations, starting by the type and followed by the contracts.
    pub fn iter(&'ast self) -> impl Iterator<Item = &'ast Type> {
        self.typ.iter().chain(self.contracts.iter())
    }

    /// Return a string representation of the contracts (without the static type annotation) as a
    /// comma-separated list.
    pub fn contracts_to_string(&self) -> Option<String> {
        (!self.contracts.is_empty()).then(|| {
            self.contracts
                .iter()
                .map(|typ| format!("{typ}"))
                .collect::<Vec<_>>()
                .join(",")
        })
    }

    /// Return `true` if this annotation is empty, i.e. hold neither a type annotation nor
    /// contracts annotations.
    pub fn is_empty(&self) -> bool {
        self.typ.is_none() && self.contracts.is_empty()
    }
}

/// Own the arenas required to allocate new AST nodes and provide builder methods to create them.
///
/// # Drop and arena allocation
///
/// The most popular choice for arena is the `bumpalo` crate, which is a fast bump allocator that
/// can handle heterogeneous data. However, it doesn't support destructors, which is a problem
/// because some of the nodes in the AST owns heap allocated data and needs to be de-allocated
/// (`Number`, `Type` and errors for now).
///
/// Another choice is `typed-arena` and derivatives, which does run destructors, but can only store
/// one type of values. As the number of types that need to be dropped is relatively small, we use
/// a general `bumpalo` arena by default, and specialized typed arenas for stuff that need to be
/// dropped.
pub struct AstBuilder {
    generic_arena: Bump,
    type_arena: typed_arena::Arena<Type>,
    number_arena: typed_arena::Arena<Number>,
    error_arena: typed_arena::Arena<ParseError>,
}

impl AstBuilder {
    /// Create a new `AstBuilder`.
    pub fn new() -> Self {
        Self {
            generic_arena: Bump::new(),
            type_arena: typed_arena::Arena::new(),
            number_arena: typed_arena::Arena::new(),
            error_arena: typed_arena::Arena::new(),
        }
    }

    pub fn null(&self) -> &Node<'_> {
        self.generic_arena.alloc(Node::Null)
    }

    pub fn bool(&self, value: bool) -> &Node<'_> {
        self.generic_arena.alloc(Node::Bool(value))
    }

    pub fn number(&self, number: Number) -> &Node<'_> {
        let number = self.number_arena.alloc(number);
        self.generic_arena.alloc(Node::Number(number))
    }

    pub fn string<'ast, 'b>(&'ast self, s: &'b str) -> &'ast Node<'ast> {
        let s = self.generic_arena.alloc_str(s);
        self.generic_arena.alloc(Node::String(s))
    }

    pub fn str_chunks_iter<'ast, I>(&'ast self, chunks: I) -> &'ast Node<'ast>
    where
        I: IntoIterator<Item = StrChunk<Ast<'ast>>>,
        I::IntoIter: ExactSizeIterator,
    {
        let chunks = self.generic_arena.alloc_slice_fill_iter(chunks);
        self.generic_arena.alloc(Node::StrChunks(chunks))
    }

    pub fn fun<'ast>(&'ast self, pat: &'ast Pattern<'ast>, body: Ast<'ast>) -> &'ast Node<'ast> {
        self.generic_arena.alloc(Node::Fun(pat, body))
    }

    pub fn let_binding<'ast, I>(
        &'ast self,
        bindings: I,
        body: Ast<'ast>,
        rec: bool,
    ) -> &'ast Node<'ast>
    where
        I: IntoIterator<Item = (Pattern<'ast>, Ast<'ast>)>,
        I::IntoIter: ExactSizeIterator,
    {
        let bindings = self.generic_arena.alloc_slice_fill_iter(bindings);
        self.generic_arena.alloc(Node::Let {
            bindings,
            body,
            rec,
        })
    }

    pub fn app<'ast>(&'ast self, fun: Ast<'ast>, arg: Ast<'ast>) -> &'ast Node<'ast> {
        self.generic_arena.alloc(Node::App { fun, arg })
    }

    pub fn var<'ast>(&'ast self, ident: LocIdent) -> &'ast Node<'ast> {
        self.generic_arena.alloc(Node::Var(ident))
    }

    pub fn enum_variant<'ast>(
        &'ast self,
        tag: LocIdent,
        arg: Option<Ast<'ast>>,
    ) -> &'ast Node<'ast> {
        self.generic_arena.alloc(Node::EnumVariant { tag, arg })
    }

    pub fn record<'ast>(&'ast self, record: Record<'ast>) -> &'ast Node<'ast> {
        let record = self.generic_arena.alloc(record);
        self.generic_arena.alloc(Node::Record(record))
    }

    pub fn record_data<'ast, Ss, Ds>(
        &'ast self,
        stat_fields: Ss,
        dyn_fields: Ds,
        open: bool,
    ) -> &'ast Record<'ast>
    where
        Ss: IntoIterator<Item = (LocIdent, record::Field<'ast>)>,
        Ds: IntoIterator<Item = (Ast<'ast>, record::Field<'ast>)>,
        Ss::IntoIter: ExactSizeIterator,
        Ds::IntoIter: ExactSizeIterator,
    {
        let stat_fields = self.generic_arena.alloc_slice_fill_iter(stat_fields);
        let dyn_fields = self.generic_arena.alloc_slice_fill_iter(dyn_fields);
        self.generic_arena.alloc(Record {
            stat_fields,
            dyn_fields,
            open,
        })
    }

    pub fn if_then_else<'ast>(
        &'ast self,
        cond: Ast<'ast>,
        then_branch: Ast<'ast>,
        else_branch: Ast<'ast>,
    ) -> &'ast Node<'ast> {
        self.generic_arena.alloc(Node::IfThenElse {
            cond,
            then_branch,
            else_branch,
        })
    }

    pub fn match_expr<'ast, I>(&'ast self, branches: I) -> &'ast Node<'ast>
    where
        I: IntoIterator<Item = MatchBranch<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let branches = self.generic_arena.alloc_slice_fill_iter(branches);
        self.generic_arena.alloc(Node::Match(Match { branches }))
    }

    pub fn array<'ast, I>(&'ast self, elts: I) -> &'ast Node<'ast>
    where
        I: IntoIterator<Item = Ast<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let elts = self.generic_arena.alloc_slice_fill_iter(elts);
        self.generic_arena.alloc(Node::Array(elts))
    }

    pub fn prim_op<'ast, I>(&'ast self, op: PrimOp, args: I) -> &'ast Node<'ast>
    where
        I: IntoIterator<Item = Ast<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let args = self.generic_arena.alloc_slice_fill_iter(args);
        self.generic_arena.alloc(Node::PrimOp { op, args })
    }

    pub fn annotated<'ast>(
        &'ast self,
        annot: Annotation<'ast>,
        inner: Ast<'ast>,
    ) -> &'ast Node<'ast> {
        self.generic_arena.alloc(Node::Annotated { annot, inner })
    }

    pub fn import(&self, path: OsString, format: InputFormat) -> &Node<'_> {
        self.generic_arena.alloc(Node::Import { path, format })
    }

    pub fn typ<'ast>(&'ast self, typ: Type) -> &'ast Node<'ast> {
        let typ = self.type_arena.alloc(typ);
        self.generic_arena.alloc(Node::Type(typ))
    }

    pub fn parse_error<'ast>(&'ast self, error: ParseError) -> &'ast Node<'ast> {
        let error = self.error_arena.alloc(error);
        self.generic_arena.alloc(Node::ParseError(error))
    }

    pub fn ast<'ast>(&'ast self, node: &'ast Node<'ast>, pos: TermPos) -> Ast<'ast> {
        let pos = self.generic_arena.alloc(pos);
        Ast { node, pos }
    }

    pub fn pattern<'ast>(
        &'ast self,
        data: PatternData<'ast>,
        alias: Option<LocIdent>,
        pos: TermPos,
    ) -> &'ast Pattern<'ast> {
        self.generic_arena.alloc(Pattern { data, alias, pos })
    }

    pub fn enum_pattern<'ast>(
        &'ast self,
        tag: LocIdent,
        pattern: Option<Pattern<'ast>>,
        pos: TermPos,
    ) -> &'ast EnumPattern<'ast> {
        self.generic_arena.alloc(EnumPattern { tag, pattern, pos })
    }

    pub fn field_pattern<'ast>(
        &'ast self,
        matched_id: LocIdent,
        annotation: Annotation<'ast>,
        default: Option<Ast<'ast>>,
        pattern: Pattern<'ast>,
        pos: TermPos,
    ) -> &'ast FieldPattern<'ast> {
        self.generic_arena.alloc(FieldPattern {
            matched_id,
            annotation,
            default,
            pattern,
            pos,
        })
    }

    pub fn record_pattern<'ast, I>(
        &'ast self,
        patterns: I,
        tail: TailPattern,
        pos: TermPos,
    ) -> &'ast RecordPattern<'ast>
    where
        I: IntoIterator<Item = FieldPattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let patterns = self.generic_arena.alloc_slice_fill_iter(patterns);

        self.generic_arena.alloc(RecordPattern {
            patterns,
            tail,
            pos,
        })
    }

    pub fn array_pattern<'ast, I>(
        &'ast self,
        patterns: I,
        tail: TailPattern,
        pos: TermPos,
    ) -> &'ast ArrayPattern<'ast>
    where
        I: IntoIterator<Item = Pattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let patterns = self.generic_arena.alloc_slice_fill_iter(patterns);

        self.generic_arena.alloc(ArrayPattern {
            patterns,
            tail,
            pos,
        })
    }

    pub fn constant_pattern<'ast>(
        &'ast self,
        data: ConstantPatternData<'ast>,
        pos: TermPos,
    ) -> &'ast ConstantPattern<'ast> {
        self.generic_arena.alloc(ConstantPattern { data, pos })
    }

    pub fn or_pattern<'ast, I>(&'ast self, patterns: I, pos: TermPos) -> &'ast OrPattern<'ast>
    where
        I: IntoIterator<Item = Pattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let patterns = self.generic_arena.alloc_slice_fill_iter(patterns);

        self.generic_arena.alloc(OrPattern { patterns, pos })
    }

    pub fn from_pattern<'ast>(&'ast self, pattern: &term::pattern::Pattern) -> &'ast Pattern<'ast> {
        self.generic_arena.alloc(self.from_pattern_owned(pattern))
    }

    pub fn from_pattern_owned<'ast>(&'ast self, pattern: &term::pattern::Pattern) -> Pattern<'ast> {
        Pattern {
            data: self.from_pattern_data(&pattern.data),
            alias: pattern.alias,
            pos: pattern.pos,
        }
    }

    pub fn from_pattern_data<'ast>(
        &'ast self,
        data: &term::pattern::PatternData,
    ) -> PatternData<'ast> {
        match data {
            term::pattern::PatternData::Wildcard => PatternData::Wildcard,
            term::pattern::PatternData::Any(id) => PatternData::Any(*id),
            term::pattern::PatternData::Record(record_pattern) => {
                self.from_record_pattern(record_pattern)
            }
            term::pattern::PatternData::Array(array_pattern) => {
                self.from_array_pattern(array_pattern)
            }
            term::pattern::PatternData::Enum(enum_pattern) => self.from_enum_pattern(enum_pattern),
            term::pattern::PatternData::Constant(constant_pattern) => {
                self.from_constant_pattern(constant_pattern)
            }
            term::pattern::PatternData::Or(or_pattern) => self.from_or_pattern(or_pattern),
        }
    }

    pub fn from_record_pattern<'ast>(
        &'ast self,
        pattern: &term::pattern::RecordPattern,
    ) -> PatternData<'ast> {
        let patterns = pattern
            .patterns
            .iter()
            .map(|field_pattern| self.from_field_pattern(field_pattern));

        let tail = match pattern.tail {
            term::pattern::TailPattern::Empty => TailPattern::Empty,
            term::pattern::TailPattern::Open => TailPattern::Open,
            term::pattern::TailPattern::Capture(id) => TailPattern::Capture(id),
        };

        PatternData::Record(self.record_pattern(patterns, tail, pattern.pos))
    }

    pub fn from_field_pattern<'ast>(
        &'ast self,
        field_pat: &term::pattern::FieldPattern,
    ) -> FieldPattern<'ast> {
        let pattern = self.from_pattern_owned(&field_pat.pattern);

        let default = field_pat
            .default
            .as_ref()
            .map(|term| self.from_rich_term(term));
        let annotation = self.from_annotation(&field_pat.annotation);

        FieldPattern {
            matched_id: field_pat.matched_id,
            annotation,
            default,
            pattern,
            pos: field_pat.pos,
        }
    }

    pub fn from_array_pattern<'ast>(
        &'ast self,
        array_pat: &term::pattern::ArrayPattern,
    ) -> PatternData<'ast> {
        let patterns = array_pat
            .patterns
            .iter()
            .map(|pat| self.from_pattern_owned(pat));

        let tail = match array_pat.tail {
            term::pattern::TailPattern::Empty => TailPattern::Empty,
            term::pattern::TailPattern::Open => TailPattern::Open,
            term::pattern::TailPattern::Capture(id) => TailPattern::Capture(id),
        };

        PatternData::Array(self.array_pattern(patterns, tail, array_pat.pos))
    }

    pub fn from_enum_pattern<'ast>(
        &'ast self,
        enum_pat: &term::pattern::EnumPattern,
    ) -> PatternData<'ast> {
        let pattern = enum_pat
            .pattern
            .as_ref()
            .map(|pat| self.from_pattern_owned(pat));
        PatternData::Enum(self.enum_pattern(enum_pat.tag, pattern, enum_pat.pos))
    }

    pub fn from_constant_pattern<'ast>(
        &'ast self,
        pattern: &term::pattern::ConstantPattern,
    ) -> PatternData<'ast> {
        let data = match &pattern.data {
            term::pattern::ConstantPatternData::Bool(b) => ConstantPatternData::Bool(*b),
            term::pattern::ConstantPatternData::Number(n) => {
                ConstantPatternData::Number(self.generic_arena.alloc(n.clone()))
            }
            term::pattern::ConstantPatternData::String(s) => {
                ConstantPatternData::String(self.generic_arena.alloc_str(s))
            }
            term::pattern::ConstantPatternData::Null => ConstantPatternData::Null,
        };

        PatternData::Constant(self.constant_pattern(data, pattern.pos))
    }

    pub fn from_or_pattern<'ast>(
        &'ast self,
        pattern: &term::pattern::OrPattern,
    ) -> PatternData<'ast> {
        let patterns = pattern
            .patterns
            .iter()
            .map(|pat| self.from_pattern_owned(pat))
            .collect::<Vec<_>>();

        PatternData::Or(self.or_pattern(patterns, pattern.pos))
    }

    pub fn from_term<'ast>(&'ast self, term: &term::Term) -> &'ast Node<'ast> {
        use term::Term;

        match term {
            Term::Null => self.null(),
            Term::Bool(b) => self.bool(*b),
            Term::Num(n) => self.number(n.clone()),
            Term::Str(s) => self.string(s),
            Term::StrChunks(chunks) => {
                self.str_chunks_iter(chunks.iter().map(|chunk| match chunk {
                    term::StrChunk::Literal(s) => StrChunk::Literal(s.clone()),
                    term::StrChunk::Expr(e, indent) => {
                        StrChunk::Expr(self.from_rich_term(e), *indent)
                    }
                }))
            }
            Term::Fun(id, body) => self.fun(
                self.generic_arena.alloc(Pattern::any(*id)),
                self.from_rich_term(body),
            ),
            Term::FunPattern(pat, body) => {
                self.fun(self.from_pattern(pat), self.from_rich_term(body))
            }
            Term::Let(bindings, body, attrs) => self.let_binding(
                bindings
                    .iter()
                    .map(|(id, term)| (Pattern::any(*id), self.from_rich_term(term))),
                self.from_rich_term(body),
                attrs.rec,
            ),
            Term::LetPattern(bindings, body, attrs) => self.let_binding(
                bindings
                    .iter()
                    .map(|(pat, term)| (self.from_pattern_owned(pat), self.from_rich_term(term))),
                self.from_rich_term(body),
                attrs.rec,
            ),
            Term::App(fun, arg) => match fun.as_ref() {
                // We have to special-case if-then-else, which is encoded as a primop application
                // of the unary operator if-then-else to the condition, followed by two normal
                // applications for the if and else branches, which is a bit painful to match.
                Term::App(fun_inner, arg_inner)
                    if matches!(fun_inner.as_ref(), Term::Op1(term::UnaryOp::IfThenElse, _)) =>
                {
                    if let Term::Op1(term::UnaryOp::IfThenElse, cond) = fun_inner.as_ref() {
                        self.if_then_else(
                            self.from_rich_term(cond),
                            self.from_rich_term(arg_inner),
                            self.from_rich_term(arg),
                        )
                    } else {
                        self.app(self.from_rich_term(fun), self.from_rich_term(arg))
                    }
                }
                _ => self.app(self.from_rich_term(fun), self.from_rich_term(arg)),
            },
            Term::Var(id) => self.var(*id),
            Term::Enum(id) => self.enum_variant(*id, None),
            Term::EnumVariant { tag, arg, attrs: _ } => {
                self.enum_variant(*tag, Some(self.from_rich_term(arg)))
            }
            Term::RecRecord(data, dyn_fields, _deps) => {
                let stat_fields = self.generic_arena.alloc_slice_fill_iter(
                    data.fields
                        .iter()
                        .map(|(id, field)| (*id, self.from_field(field))),
                );

                let dyn_fields = self.generic_arena.alloc_slice_fill_iter(
                    dyn_fields
                        .iter()
                        .map(|(expr, field)| (self.from_rich_term(expr), self.from_field(field))),
                );

                let open = data.attrs.open;

                self.record(Record {
                    stat_fields,
                    dyn_fields,
                    open,
                })
            }
            Term::Record(data) => {
                let stat_fields = self.generic_arena.alloc_slice_fill_iter(
                    data.fields
                        .iter()
                        .map(|(id, field)| (*id, self.from_field(field))),
                );

                let open = data.attrs.open;

                self.record(Record {
                    stat_fields,
                    dyn_fields: self.generic_arena.alloc_slice_fill_iter(std::iter::empty()),
                    open,
                })
            }
            Term::Match(data) => {
                let branches = data.branches.iter().map(|branch| MatchBranch {
                    pattern: self.from_pattern_owned(&branch.pattern),
                    guard: branch.guard.as_ref().map(|term| self.from_rich_term(term)),
                    body: self.from_rich_term(&branch.body),
                });

                self.match_expr(branches)
            }
            Term::Array(data, _attrs) => {
                // We should probably make array's iterator an ExactSizeIterator. But for now, we
                // don't care about the translation's performance so it's simpler to just collect
                // them in a vec locally.
                let elts = data
                    .iter()
                    .map(|term| self.from_rich_term(term))
                    .collect::<Vec<_>>();
                self.array(elts)
            }
            Term::Op1(op, arg) => {
                self.prim_op(PrimOp::from(op), std::iter::once(self.from_rich_term(arg)))
            }
            Term::Op2(op, arg1, arg2) => self.prim_op(
                PrimOp::from(op),
                [arg1, arg2].iter().map(|arg| self.from_rich_term(arg)),
            ),
            Term::OpN(op, args) => self.prim_op(
                PrimOp::from(op),
                args.iter().map(|arg| self.from_rich_term(arg)),
            ),
            Term::SealingKey(_) => panic!("didn't expect a sealing key at the first stage"),
            Term::Sealed(..) => panic!("didn't expect a sealed term at the first stage"),
            Term::Annotated(annot, term) => {
                self.annotated(self.from_annotation(annot), self.from_rich_term(term))
            }
            Term::Import { path, format } => self.import(path.clone(), *format),
            Term::ResolvedImport(_) => panic!("didn't expect a resolved import at parsing stage"),
            Term::Type { typ, .. } => self.typ(typ.clone()),
            Term::CustomContract(_) => panic!("didn't expect a custom contract at parsing stage"),
            Term::ParseError(error) => self.parse_error(error.clone()),
            Term::RuntimeError(_) => panic!("didn't expect a runtime error at parsing stage"),
            Term::Closure(_) => panic!("didn't expect a closure at parsing stage"),
            Term::ForeignId(_) => panic!("didn't expect a foreign id at parsing stage"),
            _ => unimplemented!(),
        }
    }

    pub fn from_rich_term<'ast>(&'ast self, rterm: &term::RichTerm) -> Ast<'ast> {
        self.ast(self.from_term(rterm.as_ref()), rterm.pos)
    }

    pub fn from_annotation<'ast>(&'ast self, annot: &term::TypeAnnotation) -> Annotation<'ast> {
        let typ = annot.typ.as_ref().map(|typ| typ.typ.clone());

        let contracts = self
            .type_arena
            .alloc_extend(annot.contracts.iter().map(|contract| contract.typ.clone()));

        Annotation { typ, contracts }
    }

    pub fn from_field<'ast>(&'ast self, field: &term::record::Field) -> record::Field<'ast> {
        record::Field {
            value: field.value.as_ref().map(|term| self.from_rich_term(term)),
            metadata: self.from_metadata(&field.metadata),
        }
    }

    pub fn from_metadata<'ast>(
        &'ast self,
        metadata: &term::record::FieldMetadata,
    ) -> record::FieldMetadata<'ast> {
        let doc = metadata.doc.as_ref().map(|doc| rc::Rc::from(doc.as_str()));

        record::FieldMetadata {
            doc,
            annotation: self.from_annotation(&metadata.annotation),
            opt: metadata.opt,
            not_exported: metadata.not_exported,
            priority: metadata.priority.clone(),
        }
    }
}
