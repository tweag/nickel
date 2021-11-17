//! AST of a Nickel expression.
//!
//! # Core language
//!
//! At its core, Nickel is a lazy JSON with higher-order functions. It includes:
//! - Basic values: booleans, numerals, string
//! - Data structures: lists and records
//! - Binders: functions and let bindings
//!
//! It also features type annotations (promise and assume), and other typechecking related
//! constructs (label, symbols, etc.).
//!
//! # Enriched values
//!
//! Enriched values are special terms used to represent metadata about record fields: types or
//! contracts, default values, documentation, etc. They bring such usually external object down to
//! the term level, and together with [merge](../merge/index.html), they allow for flexible and
//! modular definitions of contracts, record and metadata all together.
use crate::destruct::Destruct;
use crate::identifier::Ident;
use crate::label::Label;
use crate::position::TermPos;
use crate::types::{AbsType, Types};
use codespan::FileId;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::ffi::OsString;
use std::fmt;

/// The AST of a Nickel expression.
///
/// Parsed terms also need to store their position in the source for error reporting.  This is why
/// this type is nested with [`RichTerm`](type.RichTerm.html).
///
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Term {
    /// The null value.
    Null,
    /// A boolean value.
    Bool(bool),
    /// A floating-point value.
    #[serde(serialize_with = "crate::serialize::serialize_num")]
    Num(f64),
    /// A literal string.
    Str(String),
    /// A string containing interpolated expressions, represented as a list of either literals or
    /// expressions.
    ///
    /// /|\ CHUNKS ARE STORED IN REVERSE ORDER. As they will be only popped one by one from the
    /// head of the list during evaluation, doing so on a `Vec` is costly, and using a more complex
    /// data structure is not really necessary, as once created, no other than popping is ever
    /// done.  In consequence, we just reverse the vector at parsing time, so that we can then pop
    /// efficiently from the back of it.
    #[serde(skip)]
    StrChunks(Vec<StrChunk<RichTerm>>),
    /// A function.
    #[serde(skip)]
    Fun(Ident, RichTerm),
    /// A blame label.
    #[serde(skip)]
    Lbl(Label),

    /// A let binding.
    #[serde(skip)]
    Let(Ident, RichTerm, RichTerm),
    #[serde(skip)]
    LetPattern(Option<Ident>, Destruct, RichTerm, RichTerm),
    /// An application.
    #[serde(skip)]
    App(RichTerm, RichTerm),
    /// A variable.
    #[serde(skip)]
    Var(Ident),

    /// An enum variant.
    Enum(Ident),

    /// A record, mapping identifiers to terms.
    #[serde(serialize_with = "crate::serialize::serialize_record")]
    #[serde(deserialize_with = "crate::serialize::deserialize_record")]
    Record(HashMap<Ident, RichTerm>, RecordAttrs),
    /// A recursive record, where the fields can reference each others.
    #[serde(skip)]
    RecRecord(
        HashMap<Ident, RichTerm>,
        Vec<(RichTerm, RichTerm)>, /* field whose name is defined by interpolation */
        RecordAttrs,
    ),
    /// A switch construct. The evaluation is done by the corresponding unary operator, but we
    /// still need this one for typechecking.
    Switch(
        RichTerm,                 /* tested expression */
        HashMap<Ident, RichTerm>, /* cases */
        Option<RichTerm>,         /* default */
    ),

    /// A list.
    List(Vec<RichTerm>),

    /// A primitive unary operator.
    #[serde(skip)]
    Op1(UnaryOp, RichTerm),
    /// A primitive binary operator.
    #[serde(skip)]
    Op2(BinaryOp, RichTerm, RichTerm),
    /// An primitive n-ary operator.
    #[serde(skip)]
    OpN(NAryOp, Vec<RichTerm>),

    /// A promise.
    ///
    /// Represent a subterm which is to be statically typechecked.
    #[serde(skip)]
    Promise(Types, Label, RichTerm),

    /// A symbol.
    ///
    /// A unique tag corresponding to a type variable. See `Wrapped` below.
    #[serde(skip)]
    Sym(i32),

    /// A wrapped term.
    ///
    /// Wrapped terms are introduced by contracts on polymorphic types. Take the following example:
    ///
    /// ```text
    /// let f = Assume(forall a. forall b. a -> b -> a, fun x y => y) in
    /// f true "a"
    /// ```
    ///
    /// This function is ill-typed. To check that, a polymorphic contract will:
    /// - Assign a unique identifier to each type variable: say `a => 1`, `b => 2`
    /// - For each cast on a negative occurrence of a type variable `a` or `b` (corresponding to an
    /// argument position), tag the argument with the associated identifier. In our example, `f
    /// true "a"` will push `Wrapped(1, true)` then `Wrapped(2, "a")` on the stack.
    /// - For each cast on a positive occurrence of a type variable, this contract check that the
    /// term is of the form `Wrapped(id, term)` where `id` corresponds to the identifier of the
    /// type variable. In our example, the last cast to `a` finds `Wrapped(2, "a")`, while it
    /// expected `Wrapped(1, _)`, hence it raises a positive blame.
    #[serde(skip)]
    Wrapped(i32, RichTerm),

    #[serde(serialize_with = "crate::serialize::serialize_meta_value")]
    #[serde(skip_deserializing)]
    MetaValue(MetaValue),

    /// An unresolved import.
    #[serde(skip)]
    Import(OsString),
    /// A resolved import (which has already been loaded and parsed).
    #[serde(skip)]
    ResolvedImport(FileId),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct RecordAttrs {
    pub open: bool,
}

impl Default for RecordAttrs {
    fn default() -> Self {
        RecordAttrs { open: false }
    }
}

impl RecordAttrs {
    pub fn merge(attrs1: RecordAttrs, attrs2: RecordAttrs) -> RecordAttrs {
        RecordAttrs {
            open: attrs1.open || attrs2.open,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Copy, Clone)]
pub enum MergePriority {
    Default,
    Normal,
}

impl Default for MergePriority {
    fn default() -> Self {
        Self::Normal
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Contract {
    pub types: Types,
    pub label: Label,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AbsMetaValue<V> {
    pub doc: Option<String>,
    pub types: Option<Contract>,
    pub contracts: Vec<Contract>,
    pub priority: MergePriority,
    pub value: V,
}

pub type MetaValue = AbsMetaValue<Option<RichTerm>>;

impl From<RichTerm> for MetaValue {
    fn from(rt: RichTerm) -> Self {
        MetaValue {
            doc: None,
            types: None,
            contracts: Vec::new(),
            priority: Default::default(),
            value: Some(rt),
        }
    }
}

impl MetaValue {
    pub fn new() -> Self {
        MetaValue {
            doc: None,
            types: None,
            contracts: Vec::new(),
            priority: Default::default(),
            value: None,
        }
    }

    /// Flatten two nested metavalues into one, combining their metadata. If data that can't be
    /// combined (typically, the documentation or the type annotation) are set by both metavalues,
    /// outer's one are kept.
    ///
    /// Note that no environment management such as closurization takes place, because this
    /// function is expected to be used on the AST before the evaluation (in the parser or during
    /// program transformation).
    ///
    /// #Preconditions
    /// - `outer.value` is assumed to be `inner`. While `flatten` may still work fine if this
    ///   condition is not fullfilled, the value of the final metavalue is set to be `inner`'s one,
    ///   and `outer`'s one is dropped.
    pub fn flatten(outer: MetaValue, mut inner: MetaValue) -> MetaValue {
        // Keep the outermost value for non-mergeable information, such as documentation, type annotation,
        // and so on, which is the one that is accessible from the outside anyway (by queries, by the typechecker, and
        // so on).
        // Keep the inner value
        let MetaValue {
            doc,
            types,
            mut contracts,
            priority,
            value: _,
        } = outer;

        // If both have type annotations, the result will have the outer one as a type annotation.
        // However we still need to enforce the corresponding contract to preserve the operational
        // semantics. Thus, the inner type annotation is derelicted to a contract.
        match inner.types.take() {
            Some(ctr) if types.is_some() => contracts.push(ctr),
            _ => (),
        };

        contracts.extend(inner.contracts.into_iter());

        MetaValue {
            doc: doc.or(inner.doc),
            types: types.or(inner.types),
            contracts,
            priority: std::cmp::min(priority, inner.priority),
            value: inner.value,
        }
    }
}

/// A chunk of a string with interpolated expressions inside. Same as `Either<String,
/// RichTerm>` but with explicit constructor names.
#[derive(Debug, PartialEq, Clone)]
pub enum StrChunk<E> {
    /// A string literal.
    Literal(String),
    /// An interpolated expression.
    Expr(
        E,     /* the expression */
        usize, /* the indentation level (see parser::utils::strip_indent) */
    ),
}

#[cfg(test)]
impl<E> StrChunk<E> {
    pub fn expr(e: E) -> Self {
        StrChunk::Expr(e, 0)
    }
}

impl Term {
    #[cfg(test)]
    /// Recursively apply a function to all `Term`s contained in a `RichTerm`.
    pub fn apply_to_rich_terms<F>(&mut self, func: F)
    where
        F: Fn(&mut RichTerm),
    {
        use self::Term::*;
        match self {
            Null => (),
            Switch(ref mut t, ref mut cases, ref mut def) => {
                cases.iter_mut().for_each(|c| {
                    let (_, t) = c;
                    func(t);
                });
                func(t);
                if let Some(def) = def {
                    func(def)
                }
            }
            Record(ref mut static_map, _) => {
                static_map.iter_mut().for_each(|(_, t)| func(t));
            }
            RecRecord(ref mut static_map, ref mut dyn_fields, _) => {
                static_map.iter_mut().for_each(|(_, t)| func(t));
                dyn_fields.iter_mut().for_each(|(t1, t2)| {
                    func(t1);
                    func(t2);
                });
            }
            Bool(_) | Num(_) | Str(_) | Lbl(_) | Var(_) | Sym(_) | Enum(_) | Import(_)
            | ResolvedImport(_) => {}
            Fun(_, ref mut t)
            | Op1(_, ref mut t)
            | Promise(_, _, ref mut t)
            | Wrapped(_, ref mut t) => {
                func(t);
            }
            MetaValue(ref mut meta) => {
                meta.contracts
                    .iter_mut()
                    .for_each(|Contract { types, .. }| match types.0 {
                        AbsType::Flat(ref mut rt) => func(rt),
                        _ => (),
                    });
                meta.value.iter_mut().for_each(func);
            }
            Let(_, ref mut t1, ref mut t2)
            | LetPattern(_, _, ref mut t1, ref mut t2)
            | App(ref mut t1, ref mut t2)
            | Op2(_, ref mut t1, ref mut t2) => {
                func(t1);
                func(t2);
            }
            OpN(_, ref mut terms) | List(ref mut terms) => terms.iter_mut().for_each(|t| {
                func(t);
            }),
            StrChunks(chunks) => chunks.iter_mut().for_each(|chunk| match chunk {
                StrChunk::Literal(_) => (),
                StrChunk::Expr(e, _) => func(e),
            }),
        }
    }

    /// Return the class of an expression in WHNF.
    ///
    /// The class of an expression is an approximation of its type used in error reporting. Class
    /// and type coincide for constants (numbers, strings and booleans) and lists. Otherwise the
    /// class is less precise than the type and indicates the general shape of the term: `"Record"`
    /// for records, `"Fun`" for functions, etc. If the term is not a WHNF, `None` is returned.
    pub fn type_of(&self) -> Option<String> {
        match self {
            Term::Null => Some("Null"),
            Term::Bool(_) => Some("Bool"),
            Term::Num(_) => Some("Num"),
            Term::Str(_) => Some("Str"),
            Term::Fun(_, _) => Some("Fun"),
            Term::Lbl(_) => Some("Label"),
            Term::Enum(_) => Some("Enum"),
            Term::Record(..) | Term::RecRecord(..) => Some("Record"),
            Term::List(_) => Some("List"),
            Term::Sym(_) => Some("Sym"),
            Term::Wrapped(_, _) => Some("Wrapped"),
            Term::MetaValue(_) => Some("Metavalue"),
            Term::Let(_, _, _)
            | Term::LetPattern(_, _, _, _)
            | Term::App(_, _)
            | Term::Var(_)
            | Term::Switch(..)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::OpN(..)
            | Term::Promise(_, _, _)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_) => None,
        }
        .map(String::from)
    }

    /// Return a shallow string representation of a term, used for error reporting.
    pub fn shallow_repr(&self) -> String {
        match self {
            Term::Null => String::from("null"),
            Term::Bool(true) => String::from("true"),
            Term::Bool(false) => String::from("false"),
            Term::Num(n) => format!("{}", n),
            Term::Str(s) => format!("\"{}\"", s),
            Term::StrChunks(chunks) => {
                let chunks_str: Vec<String> = chunks
                    .iter()
                    .map(|chunk| match chunk {
                        StrChunk::Literal(s) => s,
                        StrChunk::Expr(..) => "${ ... }",
                    })
                    .map(String::from)
                    .collect();

                format!("\"{}\"", chunks_str.join(""))
            }
            Term::Fun(_, _) => String::from("<func>"),
            Term::Lbl(_) => String::from("<label>"),
            Term::Enum(Ident(s)) => format!("`{}", s),
            Term::Record(..) | Term::RecRecord(..) => String::from("{ ... }"),
            Term::List(_) => String::from("[ ... ]"),
            Term::Sym(_) => String::from("<sym>"),
            Term::Wrapped(_, _) => String::from("<wrapped>"),
            Term::MetaValue(ref meta) => {
                let mut content = String::new();

                if meta.doc.is_some() {
                    content.push_str("doc,");
                }
                if !meta.contracts.is_empty() {
                    content.push_str("contract,");
                }

                let value_label = if meta.priority == MergePriority::Default {
                    "default"
                } else {
                    "value"
                };
                let value = if let Some(t) = &meta.value {
                    t.as_ref().shallow_repr()
                } else {
                    String::from("none")
                };

                format!("<{}{}={}>", content, value_label, value)
            }
            Term::Var(Ident(id)) => id.clone(),
            Term::Let(_, _, _)
            | Term::LetPattern(_, _, _, _)
            | Term::App(_, _)
            | Term::Switch(..)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::OpN(..)
            | Term::Promise(_, _, _)
            | Term::Import(_)
            | Term::ResolvedImport(_) => String::from("<unevaluated>"),
        }
    }

    /// Return a deep string representation of a term, used for printing in the REPL
    pub fn deep_repr(&self) -> String {
        match self {
            Term::Record(fields, _) | Term::RecRecord(fields, _, _) => {
                let fields_str: Vec<String> = fields
                    .iter()
                    .map(|(ident, term)| format!("{} = {}", ident, term.as_ref().deep_repr()))
                    .collect();

                let suffix = match self {
                    Term::RecRecord(_, dyn_fields, _) if !dyn_fields.is_empty() => ", ..",
                    _ => "",
                };

                format!("{{ {}{}}}", fields_str.join(", "), suffix)
            }
            Term::List(elements) => {
                let elements_str: Vec<String> = elements
                    .iter()
                    .map(|term| term.as_ref().deep_repr())
                    .collect();
                format!("[ {} ]", elements_str.join(", "))
            }
            _ => self.shallow_repr(),
        }
    }

    /// Determine if a term is in evaluated from, called weak head normal form (WHNF).
    pub fn is_whnf(&self) -> bool {
        match self {
            Term::Null
            | Term::Bool(_)
            | Term::Num(_)
            | Term::Str(_)
            | Term::Fun(_, _)
            | Term::Lbl(_)
            | Term::Enum(_)
            | Term::Record(..)
            | Term::List(_)
            | Term::Sym(_) => true,
            Term::Let(_, _, _)
            | Term::LetPattern(_, _, _, _)
            | Term::App(_, _)
            | Term::Var(_)
            | Term::Switch(..)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::OpN(..)
            | Term::Promise(_, _, _)
            | Term::Wrapped(_, _)
            | Term::MetaValue(_)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_)
            | Term::RecRecord(..) => false,
        }
    }

    /// Determine if a term is a metavalue.
    pub fn is_metavalue(&self) -> bool {
        matches!(self, Term::MetaValue(..))
    }

    /// Determine if a term is a constant.
    ///
    /// In this context, a constant is an atomic literal of the language: null, a boolean, a number, a
    /// string, a label, an enum tag or a symbol.
    pub fn is_constant(&self) -> bool {
        match self {
            Term::Null
            | Term::Bool(_)
            | Term::Num(_)
            | Term::Str(_)
            | Term::Lbl(_)
            | Term::Enum(_)
            | Term::Sym(_) => true,
            Term::Let(_, _, _)
            | Term::LetPattern(_, _, _, _)
            | Term::Record(..)
            | Term::List(_)
            | Term::Fun(_, _)
            | Term::App(_, _)
            | Term::Switch(..)
            | Term::Var(_)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::OpN(..)
            | Term::Promise(_, _, _)
            | Term::Wrapped(_, _)
            | Term::MetaValue(_)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_)
            | Term::RecRecord(..) => false,
        }
    }
}

/// Primitive unary operators.
///
/// Some operators, such as if-then-else or `seq`, actually take several arguments but are only
/// strict in one (the tested boolean for example, in the case of if-then-else). They are encoded
/// as unary operators of this argument: indeed, in an expression `if-then-else boolean thenBlock
/// elseBlock`, `if-then-else` can be seen as a unary operator taking a `Bool` argument and
/// evaluating to either the first projection `fun x y => x` or the second projection `fun x y =>
/// y`.
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    /// If-then-else.
    Ite(),

    /// Test if a term is a numeral.
    IsNum(),
    /// Test if a term is a boolean.
    IsBool(),
    /// Test if a term is string literal.
    IsStr(),
    /// Test if a term is a function.
    IsFun(),
    /// Test if a term is a list.
    IsList(),
    /// Test if a term is a record.
    IsRecord(),

    // Boolean AND and OR operator are encoded as unary operators so that they can be lazy in their
    // second argument.
    /// Boolean AND operator.
    BoolAnd(),
    /// Boolean OR operator.
    BoolOr(),
    /// Boolean NOT operator.
    BoolNot(),

    /// Raise a blame, which stops the execution and prints an error according to the label argument.
    Blame(),

    /// Typecast an enum to a larger enum type.
    ///
    /// `Embed` is used to upcast enums. For example, if a value `x` has enum type `a | b`, then
    /// `embed c x` will have enum type `a | b | c`. It only affects typechecking as at runtime
    /// `embed someId` act like the identity.
    Embed(Ident),
    /// A switch block. Used to match on a enumeration.
    Switch(bool /* presence of a default case */), //HashMap<Ident, CapturedTerm>, Option<CapturedTerm>),

    /// Static access to a record field.
    ///
    /// Static means that the field identifier is a statically known string inside the source.
    StaticAccess(Ident),

    /// Map a function on each element of a list.
    ListMap(),
    /// Map a function on a record.
    ///
    /// The mapped function must take two arguments, the name of the field as a string, and the
    /// content of the field. `RecordMap` then replaces the content of each field by the result of the
    /// function: i.e., `recordMap f {a=2;}` evaluates to `{a=(f "a" 2);}`.
    RecordMap(),

    /// Inverse the polarity of a label.
    ChangePolarity(),

    /// Get the polarity of a label.
    Pol(),
    /// Go to the domain in the type path of a label.
    ///
    /// If the argument is a label with a [type path](../label/enum.TyPath.html) representing some
    /// subtype of the type of the original contract, as in:
    ///
    /// ```text
    /// (Num -> Num) -> Num
    ///  ^^^^^^^^^^ type path
    /// ------------------- original type
    /// ```
    ///
    /// Then `GoDom` evaluates to a copy of this label, where the path has gone forward into the domain:
    ///
    /// ```text
    /// (Num -> Num) -> Num
    ///  ^^^ new type path
    /// ------------------- original type
    /// ```
    GoDom(),
    /// Go to the codomain in the type path of a label.
    ///
    /// See `GoDom`.
    GoCodom(),
    /// Go to the list in the type path of a label.
    ///
    /// See `GoDom`.
    GoList(),

    /// Wrap a term with a type tag (see `Wrapped` in [`Term`](enum.Term.html)).
    Wrap(),

    /// Force the evaluation of its argument and proceed with the second.
    Seq(),
    /// Recursively force the evaluation of its first argument then returns the second.
    ///
    /// Recursive here means that the evaluation does not stop at a WHNF, but the content of lists
    /// and records is also recursively forced.
    DeepSeq(),

    /// Return the head of a list.
    ListHead(),
    /// Return the tail of a list.
    ListTail(),
    /// Return the length of a list.
    ListLength(),
    /// Generate a list of a given length by mapping a `Num -> Num` function onto `[1,..,n]`.
    ListGen(),

    /// Generated by the evaluation of a string with interpolated expressions. `ChunksConcat`
    /// applied to the current chunk to evaluate. As additional state, it uses a string
    /// accumulator, the indentation of the chunk being evaluated, and the remaining chunks to be
    /// evaluated, all stored on the stack.
    ChunksConcat(),

    /// Return the names of the fields of a record as a string list.
    FieldsOf(),
    /// Return the values of the fields of a record as a list.
    ValuesOf(),

    /// Remove heading and trailing spaces from a string.
    StrTrim(),
    /// Return the list of characters of a string.
    StrChars(),
    /// Return the code of a character (givne as a string of length 1).
    CharCode(),
    /// Return the character corresponding to a code.
    CharFromCode(),
    /// Transform a string to uppercase.
    StrUppercase(),
    /// Transform a string to lowercase.
    StrLowercase(),
    /// Return the length of a string.
    StrLength(),
    /// Transform a data to a string.
    ToStr(),
    /// Transform a string to a number.
    NumFromStr(),
    /// Transform a string to an enum.
    EnumFromStr(),
}

/// Primitive binary operators
#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    /// Addition of numerals.
    Plus(),
    /// Substraction of numerals.
    Sub(),
    /// Multiplication of numerals.
    Mult(),
    /// Floating-point division of numerals.
    Div(),
    /// Modulo of numerals.
    Modulo(),
    /// Raise a number to a power.
    Pow(),
    /// Concatenation of strings.
    StrConcat(),
    /// Polymorphic equality.
    Eq(),
    /// Stricty less than comparison operator.
    LessThan(),
    /// Less than or equal comparison operator.
    LessOrEq(),
    /// Stricty greater than comparison operator.
    GreaterThan(),
    /// Greater than or equal comparison operator.
    GreaterOrEq(),
    /// An assume.
    ///
    /// Apply a contract to a label and a value. The value is is stored on the stack unevaluated,
    /// while the contract and the label are the strict arguments to this operator. Assume also
    /// accepts contracts as records, that are translates to a function that performs a merge
    /// operation with its argument. Finally, this operator marks the location of the contract
    /// argument for better error reporting.
    Assume(),
    /// Unwrap a tagged term.
    ///
    /// See `Wrap` in [`UnaryOp`](enum.UnaryOp.html).
    Unwrap(),
    /// Go to a specific field in the type path of a label.
    ///
    /// See `GoDom`.
    GoField(),
    /// Set the tag text of a blame label.
    Tag(),
    /// Extend a record with a dynamic field.
    ///
    /// Dynamic means that the field name may be an expression instead of a statically known
    /// string. `DynExtend` tries to evaluate this name to a string, and in case of success, add a
    /// field with this name to the given record with the expression on top of the stack as
    /// content.
    DynExtend(),
    /// Remove a field from a record. The field name is given as an arbitrary Nickel expression.
    DynRemove(),
    /// Access the field of record. The field name is given as an arbitrary Nickel expression.
    DynAccess(),
    /// Test if a record has a specific field.
    HasField(),
    /// Concatenate two lists.
    ListConcat(),
    /// Access the n-th element of a list.
    ListElemAt(),
    /// The merge operator (see the [merge module](../merge/index.html)).
    Merge(),

    /// Hash a string.
    Hash(),
    /// Serialize a value to a string.
    Serialize(),
    /// Deserialize a string to a value.
    Deserialize(),

    /// Split a string into a list.
    StrSplit(),
    /// Determine if a string is a substring of another one.
    StrContains(),
    /// Test if a regex matches a string.
    StrIsMatch(),
    /// Match a regex on a string, and returns the captured groups together, the index of the
    /// match, etc.
    StrMatch(),
}

impl BinaryOp {
    pub fn is_strict(&self) -> bool {
        match self {
            BinaryOp::Merge() => false,
            _ => true,
        }
    }
}

/// Primitive n-ary operators. Unary and binary operator make up for most of operators and are
/// hence special cased. `NAryOp` handles strict operations of arity greater than 2.
#[derive(Clone, Debug, PartialEq)]
pub enum NAryOp {
    /// Replace a substring by another one in a string.
    StrReplace(),
    /// Same as [`StrReplace()`], but the pattern is interpreted as a regular expression.
    ///
    /// [`StrReplace()`]: NAryOp::StrReplace
    StrReplaceRegex(),
    /// Return a substring of an original string.
    StrSubstr(),
    /// The merge operator in contract mode (see the [merge module](../merge/index.html)). The
    /// arguments are in order the contract's label, the value to check, and the contract as a
    /// record.
    MergeContract(),
}

impl NAryOp {
    pub fn arity(&self) -> usize {
        match self {
            NAryOp::StrReplace()
            | NAryOp::StrReplaceRegex()
            | NAryOp::StrSubstr()
            | NAryOp::MergeContract() => 3,
        }
    }

    pub fn is_strict(&self) -> bool {
        match self {
            _ => true,
        }
    }
}

impl fmt::Display for NAryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NAryOp::StrReplace() => write!(f, "strReplace"),
            NAryOp::StrReplaceRegex() => write!(f, "strReplaceRegex"),
            NAryOp::StrSubstr() => write!(f, "substring"),
            NAryOp::MergeContract() => write!(f, "mergeContract"),
        }
    }
}

/// Wrap [terms](type.Term.html) with positional information.
#[derive(Debug, PartialEq, Clone)]
pub struct RichTerm {
    pub term: Box<Term>,
    pub pos: TermPos,
}

impl RichTerm {
    /// Create a new value from a term and an optional position.
    pub fn new(t: Term, pos: TermPos) -> Self {
        RichTerm {
            term: Box::new(t),
            pos,
        }
    }

    /// Erase recursively the positional information.
    ///
    /// It allows to use rust `Eq` trait to compare the values of the underlying terms.
    #[cfg(test)]
    pub fn clean_pos(&mut self) {
        self.pos = TermPos::None;
        self.term
            .apply_to_rich_terms(|rt: &mut Self| rt.clean_pos());
    }

    /// Set the position and return the term updated.
    pub fn with_pos(mut self, pos: TermPos) -> Self {
        self.pos = pos;
        self
    }

    /// Apply a transformation on a whole term by mapping a function `f` on each node in a
    /// bottom-up manner. `f` may return a generic error `E` and use the state `S` which is
    /// passed around.
    pub fn traverse<F, S, E>(self, f: &mut F, state: &mut S) -> Result<RichTerm, E>
    where
        F: FnMut(RichTerm, &mut S) -> Result<RichTerm, E>,
    {
        let RichTerm { term, pos } = self;
        match *term {
            v @ Term::Null
            | v @ Term::Bool(_)
            | v @ Term::Num(_)
            | v @ Term::Str(_)
            | v @ Term::Lbl(_)
            | v @ Term::Sym(_)
            | v @ Term::Var(_)
            | v @ Term::Enum(_)
            | v @ Term::Import(_)
            | v @ Term::ResolvedImport(_) => f(
                RichTerm {
                    term: Box::new(v),
                    pos,
                },
                state,
            ),
            Term::Fun(id, t) => {
                let t = t.traverse(f, state)?;
                f(
                    RichTerm {
                        term: Box::new(Term::Fun(id, t)),
                        pos,
                    },
                    state,
                )
            }
            Term::Let(id, t1, t2) => {
                let t1 = t1.traverse(f, state)?;
                let t2 = t2.traverse(f, state)?;
                f(
                    RichTerm {
                        term: Box::new(Term::Let(id, t1, t2)),
                        pos,
                    },
                    state,
                )
            }
            Term::LetPattern(id, pat, t1, t2) => {
                let t1 = t1.traverse(f, state)?;
                let t2 = t2.traverse(f, state)?;
                f(
                    RichTerm {
                        term: Box::new(Term::LetPattern(id, pat, t1, t2)),
                        pos,
                    },
                    state,
                )
            }
            Term::App(t1, t2) => {
                let t1 = t1.traverse(f, state)?;
                let t2 = t2.traverse(f, state)?;
                f(
                    RichTerm {
                        term: Box::new(Term::App(t1, t2)),
                        pos,
                    },
                    state,
                )
            }
            Term::Switch(t, cases, default) => {
                // The annotation on `map_res` use Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let cases_res: Result<HashMap<Ident, RichTerm>, E> = cases
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,RichTerm), E>
                    .map(|(id, t)| t.traverse(f, state).map(|t_ok| (id.clone(), t_ok)))
                    .collect();

                let default = default
                    .map(|t| t.traverse(f, state))
                    // Transpose from Option<Result> to Result<Option>. There is a `transpose`
                    // method in Rust, but it has currently not made it to the stable version yet
                    .map_or(Ok(None), |res| res.map(Some))?;

                let t = t.traverse(f, state)?;

                f(
                    RichTerm::new(Term::Switch(t, cases_res?, default), pos),
                    state,
                )
            }
            Term::Op1(op, t) => {
                let t = t.traverse(f, state)?;
                f(
                    RichTerm {
                        term: Box::new(Term::Op1(op, t)),
                        pos,
                    },
                    state,
                )
            }
            Term::Op2(op, t1, t2) => {
                let t1 = t1.traverse(f, state)?;
                let t2 = t2.traverse(f, state)?;
                f(
                    RichTerm {
                        term: Box::new(Term::Op2(op, t1, t2)),
                        pos,
                    },
                    state,
                )
            }
            Term::OpN(op, ts) => {
                let ts_res: Result<Vec<RichTerm>, E> =
                    ts.into_iter().map(|t| t.traverse(f, state)).collect();
                f(
                    RichTerm {
                        term: Box::new(Term::OpN(op, ts_res?)),
                        pos,
                    },
                    state,
                )
            }
            Term::Promise(ty, l, t) => {
                let t = t.traverse(f, state)?;
                f(
                    RichTerm {
                        term: Box::new(Term::Promise(ty, l, t)),
                        pos,
                    },
                    state,
                )
            }
            Term::Wrapped(i, t) => {
                let t = t.traverse(f, state)?;
                f(
                    RichTerm {
                        term: Box::new(Term::Wrapped(i, t)),
                        pos,
                    },
                    state,
                )
            }
            Term::Record(map, attrs) => {
                // The annotation on `map_res` uses Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let map_res: Result<HashMap<Ident, RichTerm>, E> = map
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,RichTerm), E>
                    .map(|(id, t)| t.traverse(f, state).map(|t_ok| (id.clone(), t_ok)))
                    .collect();
                f(
                    RichTerm {
                        term: Box::new(Term::Record(map_res?, attrs)),
                        pos,
                    },
                    state,
                )
            }
            Term::RecRecord(map, dyn_fields, attrs) => {
                // The annotation on `map_res` uses Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let map_res: Result<HashMap<Ident, RichTerm>, E> = map
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,RichTerm), E>
                    .map(|(id, t)| Ok((id, t.traverse(f, state)?)))
                    .collect();
                let dyn_fields_res: Result<Vec<(RichTerm, RichTerm)>, E> = dyn_fields
                    .into_iter()
                    .map(|(id_t, t)| Ok((id_t.traverse(f, state)?, t.traverse(f, state)?)))
                    .collect();
                f(
                    RichTerm {
                        term: Box::new(Term::RecRecord(map_res?, dyn_fields_res?, attrs)),
                        pos,
                    },
                    state,
                )
            }
            Term::List(ts) => {
                let ts_res: Result<Vec<RichTerm>, E> =
                    ts.into_iter().map(|t| t.traverse(f, state)).collect();

                f(
                    RichTerm {
                        term: Box::new(Term::List(ts_res?)),
                        pos,
                    },
                    state,
                )
            }
            Term::StrChunks(chunks) => {
                let chunks_res: Result<Vec<StrChunk<RichTerm>>, E> = chunks
                    .into_iter()
                    .map(|chunk| match chunk {
                        chunk @ StrChunk::Literal(_) => Ok(chunk),
                        StrChunk::Expr(t, indent) => {
                            Ok(StrChunk::Expr(t.traverse(f, state)?, indent))
                        }
                    })
                    .collect();

                f(
                    RichTerm {
                        term: Box::new(Term::StrChunks(chunks_res?)),
                        pos,
                    },
                    state,
                )
            }
            Term::MetaValue(meta) => {
                let contracts: Result<Vec<Contract>, _> = meta
                    .contracts
                    .into_iter()
                    .map(|ctr| {
                        let types = match ctr.types {
                            Types(AbsType::Flat(t)) => Types(AbsType::Flat(t.traverse(f, state)?)),
                            ty => ty,
                        };
                        Ok(Contract { types, ..ctr })
                    })
                    .collect();
                let contracts = contracts?;

                let types = meta
                    .types
                    .map(|ctr| {
                        let types = match ctr.types {
                            Types(AbsType::Flat(t)) => Types(AbsType::Flat(t.traverse(f, state)?)),
                            ty => ty,
                        };
                        Ok(Contract { types, ..ctr })
                    })
                    // Transpose from Option<Result> to Result<Option>. There is a `transpose`
                    // method in Rust, but it has currently not made it to the stable version yet
                    .map_or(Ok(None), |res| res.map(Some))?;

                let value = meta
                    .value
                    .map(|t| t.traverse(f, state))
                    .map_or(Ok(None), |res| res.map(Some))?;

                let meta = MetaValue {
                    doc: meta.doc,
                    types,
                    contracts,
                    priority: meta.priority,
                    value,
                };

                f(
                    RichTerm {
                        term: Box::new(Term::MetaValue(meta)),
                        pos,
                    },
                    state,
                )
            }
        }
    }
}

impl From<RichTerm> for Term {
    fn from(rt: RichTerm) -> Self {
        *rt.term
    }
}

impl AsRef<Term> for RichTerm {
    fn as_ref(&self) -> &Term {
        &self.term
    }
}

impl From<Term> for RichTerm {
    fn from(t: Term) -> Self {
        RichTerm {
            term: Box::new(t),
            pos: TermPos::None,
        }
    }
}

#[macro_use]
/// Helpers to build `RichTerm` objects.
pub mod make {
    use super::*;

    /// Multi-ary application for types implementing `Into<RichTerm>`.
    #[macro_export]
    macro_rules! mk_app {
        ( $f:expr, $arg:expr) => {
            $crate::term::RichTerm::from($crate::term::Term::App($crate::term::RichTerm::from($f), $crate::term::RichTerm::from($arg)))
        };
        ( $f:expr, $fst:expr , $( $args:expr ),+ ) => {
            mk_app!(mk_app!($f, $fst), $( $args ),+)
        };
    }

    /// Multi-ary application for types implementing `Into<RichTerm>`.
    #[macro_export]
    macro_rules! mk_opn {
        ( $op:expr, $( $args:expr ),+) => {
            {
                let args = vec![$( RichTerm::from($args) ),+];
                $crate::term::RichTerm::from($crate::term::Term::OpN($op, args))
            }
        };
    }

    /// Multi argument function for types implementing `Into<Ident>` (for the identifiers), and
    /// `Into<RichTerm>` for the body.
    #[macro_export]
    macro_rules! mk_fun {
        ( $id:expr, $body:expr ) => {
            $crate::term::RichTerm::from($crate::term::Term::Fun($crate::identifier::Ident::from($id), $crate::term::RichTerm::from($body).into()))
        };
        ( $id1:expr, $id2:expr , $( $rest:expr ),+ ) => {
            mk_fun!($crate::identifier::Ident::from($id1), mk_fun!($id2, $( $rest ),+))
        };
    }

    /// Multi field record for types implementing `Into<Ident>` (for the identifiers), and
    /// `Into<RichTerm>` for the fields. Identifiers and corresponding content are specified as a
    /// tuple: `mk_record!(("field1", t1), ("field2", t2))` corresponds to the record `{ field1 =
    /// t1; field2 = t2 }`.
    #[macro_export]
    macro_rules! mk_record {
        ( $( ($id:expr, $body:expr) ),* ) => {
            {
                let mut map = std::collections::HashMap::new();
                $(
                    map.insert($id.into(), $body.into());
                )*
                $crate::term::RichTerm::from($crate::term::Term::Record(map, Default::default()))
            }
        };
    }

    /// Multi field record for types implementing `Into<Ident>` (for the identifiers), and
    /// `Into<RichTerm>` for the fields. Identifiers and corresponding content are specified as a
    /// tuple: `mk_record!(("field1", t1), ("field2", t2))` corresponds to the record `{ field1 =
    /// t1; field2 = t2 }`.
    #[macro_export]
    #[cfg(test)]
    macro_rules! mk_switch {
        ( $exp:expr, $( ($id:expr, $body:expr) ),* ; $default:expr ) => {
            {
                let mut map = std::collections::HashMap::new();
                $(
                    map.insert($id.into(), $body.into());
                )*
                $crate::term::RichTerm::from($crate::term::Term::Switch($crate::term::RichTerm::from($exp), map, Some($crate::term::RichTerm::from($default))))
            }
        };
        ( $exp:expr, $( ($id:expr, $body:expr) ),*) => {
                let mut map = std::collections::HashMap::new();
                $(
                    map.insert($id.into(), $body.into());
                )*
                $crate::term::RichTerm::from($crate::term::Term::Switch($crate::term::RichTerm::from($exp), map, None))
        };
    }

    pub fn var<I>(v: I) -> RichTerm
    where
        I: Into<Ident>,
    {
        Term::Var(v.into()).into()
    }

    pub fn let_in<I, T1, T2>(id: I, t1: T1, t2: T2) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
        I: Into<Ident>,
    {
        Term::Let(id.into(), t1.into(), t2.into()).into()
    }

    pub fn let_pat<I, D, T1, T2>(id: Option<I>, pat: D, t1: T1, t2: T2) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
        D: Into<Destruct>,
        I: Into<Ident> + fmt::Debug,
    {
        println!("");
        match pat.into() {
            d @ (Destruct::Record(_, _, _) | Destruct::List(_)) => {
                println!("pattern: {:?}", d);
                Term::LetPattern(id.map(|i| i.into()), d.into(), t1.into(), t2.into()).into()
            }
            Destruct::Empty => {
                if let Some(id) = id {
                    println!("{:?} is not a pattern", id);
                    let_in(id, t1, t2)
                } else {
                    Term::Null.into()
                }
            }
        }
    }

    #[cfg(test)]
    pub fn if_then_else<T1, T2, T3>(cond: T1, t1: T2, t2: T3) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
        T3: Into<RichTerm>,
    {
        mk_app!(Term::Op1(UnaryOp::Ite(), cond.into()), t1.into(), t2.into())
    }

    pub fn op1<T>(op: UnaryOp, t: T) -> RichTerm
    where
        T: Into<RichTerm>,
    {
        Term::Op1(op, t.into()).into()
    }

    pub fn op2<T1, T2>(op: BinaryOp, t1: T1, t2: T2) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
    {
        Term::Op2(op, t1.into(), t2.into()).into()
    }

    pub fn assume<T>(types: Types, l: Label, t: T) -> RichTerm
    where
        T: Into<RichTerm>,
    {
        mk_app!(
            op2(BinaryOp::Assume(), types.contract(), Term::Lbl(l)),
            t.into()
        )
    }

    pub fn string<S>(s: S) -> RichTerm
    where
        S: Into<String>,
    {
        Term::Str(s.into()).into()
    }

    pub fn id() -> RichTerm {
        mk_fun!("x", var("x"))
    }

    pub fn import<S>(path: S) -> RichTerm
    where
        S: Into<OsString>,
    {
        Term::Import(path.into()).into()
    }
}
