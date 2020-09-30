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
use crate::identifier::Ident;
use crate::label::Label;
use crate::position::RawSpan;
use crate::types::{AbsType, Types};
use codespan::FileId;
use std::collections::HashMap;

/// The AST of a Nickel expression.
///
/// Parsed terms also need to store their position in the source for error reporting.  This is why
/// this type is nested with [`RichTerm`](type.RichTerm.html).
///
#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    /// A boolean value.
    Bool(bool),
    /// A floating-point value.
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
    StrChunks(Vec<StrChunk<RichTerm>>),
    /// A function.
    Fun(Ident, RichTerm),
    /// A blame label.
    Lbl(Label),

    /// A let binding.
    Let(Ident, RichTerm, RichTerm),
    /// An application.
    App(RichTerm, RichTerm),
    /// A variable.
    Var(Ident),

    /// An enum variant.
    Enum(Ident),

    /// A record, mapping identifiers to terms.
    Record(HashMap<Ident, RichTerm>),
    /// A recursive record, where the fields can reference each others.
    RecRecord(HashMap<Ident, RichTerm>),

    /// A list.
    List(Vec<RichTerm>),

    /// A primitive unary operator.
    Op1(UnaryOp<RichTerm>, RichTerm),
    /// A primitive binary operator.
    Op2(BinaryOp<RichTerm>, RichTerm, RichTerm),

    /// A promise.
    ///
    /// Represent a subterm which is to be statically typechecked.
    Promise(Types, Label, RichTerm),

    /// An assume.
    ///
    /// Represent a subterm which is to be dynamically typechecked (dynamic types are also called.
    /// It ensures at runtime that the term satisfies the contract corresponding to the type, or it
    /// will blame the label instead.
    Assume(Types, Label, RichTerm),

    /// A symbol.
    ///
    /// A unique tag corresponding to a type variable. See `Wrapped` below.
    Sym(i32),

    /// A wrapped term.
    ///
    /// Wrapped terms are introduced by contracts on polymorphic types. Take the following example:
    ///
    /// ```
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
    Wrapped(i32, RichTerm),

    /// A contract. Enriched value.
    ///
    /// A contract at the term level. This contract is enforced when merged with a value.
    Contract(Types, Label),

    /// A default value. Enriched value.
    ///
    /// An enriched term representing a default value. It is dropped as soon as it is merged with a
    /// concrete value. Otherwise, if it lives long enough to be accessed, it evaluates to the
    /// underlying term.
    DefaultValue(RichTerm),

    /// A contract with combined with default value. Enriched value.
    ///
    /// This is a combination generated during evaluation, when merging a contract and a default
    /// value, as both need to be remembered.
    ContractWithDefault(Types, Label, RichTerm),

    /// A term together with its documentation string. Enriched value.
    Docstring(String, RichTerm),

    /// An unresolved import.
    Import(String),
    /// A resolved import (which has already been loaded and parsed).
    ResolvedImport(FileId),
}

/// A chunk of a string with interpolated expressions inside. Same as `Either<String,
/// RichTerm>` but with explicit constructor names.
#[derive(Debug, PartialEq, Clone)]
pub enum StrChunk<E> {
    Literal(String),
    Expr(E),
}

impl Term {
    /// Recursively apply a function to all `Term`s contained in a `RichTerm`.
    pub fn apply_to_rich_terms<F>(&mut self, func: F)
    where
        F: Fn(&mut RichTerm),
    {
        use self::Term::*;
        match self {
            Op1(UnaryOp::Switch(ref mut map, ref mut def), ref mut t) => {
                map.iter_mut().for_each(|e| {
                    let (_, t) = e;
                    func(t);
                });
                func(t);
                if let Some(def) = def {
                    func(def)
                }
            }
            Record(ref mut static_map) | RecRecord(ref mut static_map) => {
                static_map.iter_mut().for_each(|e| {
                    let (_, t) = e;
                    func(t);
                });
            }
            Op2(BinaryOp::DynExtend(ref mut t), ref mut t1, ref mut t2) => {
                func(t);
                func(t1);
                func(t2)
            }

            Bool(_)
            | Num(_)
            | Str(_)
            | Lbl(_)
            | Var(_)
            | Sym(_)
            | Enum(_)
            | Contract(_, _)
            | Import(_)
            | ResolvedImport(_) => {}
            Fun(_, ref mut t)
            | Op1(_, ref mut t)
            | Promise(_, _, ref mut t)
            | Assume(_, _, ref mut t)
            | Wrapped(_, ref mut t)
            | DefaultValue(ref mut t)
            | Docstring(_, ref mut t)
            | ContractWithDefault(_, _, ref mut t) => {
                func(t);
            }
            Let(_, ref mut t1, ref mut t2)
            | App(ref mut t1, ref mut t2)
            | Op2(_, ref mut t1, ref mut t2) => {
                func(t1);
                func(t2);
            }
            List(ref mut terms) => terms.iter_mut().for_each(|t| {
                func(t);
            }),
            StrChunks(chunks) => chunks.iter_mut().for_each(|chunk| match chunk {
                StrChunk::Literal(_) => (),
                StrChunk::Expr(e) => func(e),
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
            Term::Bool(_) => Some("Bool"),
            Term::Num(_) => Some("Num"),
            Term::Str(_) => Some("Str"),
            Term::Fun(_, _) => Some("Fun"),
            Term::Lbl(_) => Some("Label"),
            Term::Enum(_) => Some("Enum"),
            Term::Record(_) | Term::RecRecord(_) => Some("Record"),
            Term::List(_) => Some("List"),
            Term::Sym(_) => Some("Sym"),
            Term::Wrapped(_, _) => Some("Wrapped"),
            Term::Contract(_, _)
            | Term::ContractWithDefault(_, _, _)
            | Term::Docstring(_, _)
            | Term::DefaultValue(_) => Some("EnrichedValue"),
            Term::Let(_, _, _)
            | Term::App(_, _)
            | Term::Var(_)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::Promise(_, _, _)
            | Term::Assume(_, _, _)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_) => None,
        }
        .map(|s| String::from(s))
    }

    /// Return a shallow string representation of a term, used for error reporting.
    pub fn shallow_repr(&self) -> String {
        match self {
            Term::Bool(true) => String::from("true"),
            Term::Bool(false) => String::from("false"),
            Term::Num(n) => format!("{}", n),
            Term::Str(s) => format!("\"{}\"", s),
            Term::StrChunks(chunks) => {
                let chunks_str: Vec<String> = chunks
                    .into_iter()
                    .map(|chunk| match chunk {
                        StrChunk::Literal(s) => s,
                        StrChunk::Expr(_) => "${ ... }",
                    })
                    .map(String::from)
                    .collect();

                format!("\"{}\"", chunks_str.join(""))
            }
            Term::Fun(_, _) => String::from("<func>"),
            Term::Lbl(_) => String::from("<label>"),
            Term::Enum(Ident(s)) => format!("`{}", s),
            Term::Record(_) | Term::RecRecord(_) => String::from("{ ... }"),
            Term::List(_) => String::from("[ ... ]"),
            Term::Sym(_) => String::from("<sym>"),
            Term::Wrapped(_, _) => String::from("<wrapped>"),
            Term::Contract(_, _) => String::from("<enriched:contract>"),
            Term::ContractWithDefault(_, _, ref t) => {
                format!("<enriched:contract,default={}>", (*t.term).shallow_repr())
            }
            Term::Docstring(_, ref t) => {
                format!("<enriched:doc,term={}>", (*t.term).shallow_repr())
            }
            Term::DefaultValue(ref t) => format!("<enriched:default={}", (*t.term).shallow_repr()),
            Term::Let(_, _, _)
            | Term::App(_, _)
            | Term::Var(_)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::Promise(_, _, _)
            | Term::Assume(_, _, _)
            | Term::Import(_)
            | Term::ResolvedImport(_) => String::from("<unevaluated>"),
        }
    }

    /// Determine if a term is in evaluated from, called weak head normal form (WHNF).
    pub fn is_whnf(&self) -> bool {
        match self {
            Term::Bool(_)
            | Term::Num(_)
            | Term::Str(_)
            | Term::Fun(_, _)
            | Term::Lbl(_)
            | Term::Enum(_)
            | Term::Record(_)
            | Term::List(_)
            | Term::Sym(_) => true,
            Term::Let(_, _, _)
            | Term::App(_, _)
            | Term::Var(_)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::Promise(_, _, _)
            | Term::Assume(_, _, _)
            | Term::Wrapped(_, _)
            | Term::Contract(_, _)
            | Term::DefaultValue(_)
            | Term::ContractWithDefault(_, _, _)
            | Term::Docstring(_, _)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_)
            | Term::RecRecord(_) => false,
        }
    }

    /// Determine if a term is an enriched value.
    pub fn is_enriched(&self) -> bool {
        match self {
            Term::Contract(_, _)
            | Term::DefaultValue(_)
            | Term::ContractWithDefault(_, _, _)
            | Term::Docstring(_, _) => true,
            Term::Bool(_)
            | Term::Num(_)
            | Term::Str(_)
            | Term::StrChunks(_)
            | Term::Fun(_, _)
            | Term::Lbl(_)
            | Term::Enum(_)
            | Term::Record(_)
            | Term::RecRecord(_)
            | Term::List(_)
            | Term::Sym(_)
            | Term::Wrapped(_, _)
            | Term::Let(_, _, _)
            | Term::App(_, _)
            | Term::Var(_)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::Promise(_, _, _)
            | Term::Assume(_, _, _)
            | Term::Import(_)
            | Term::ResolvedImport(_) => false,
        }
    }

    /// Determine if a term is a constant.
    ///
    /// In this context, a constant is an atomic literal of the language: a boolean, a number, a
    /// string, a label, an enum tag or a symbol.
    pub fn is_constant(&self) -> bool {
        match self {
            Term::Bool(_)
            | Term::Num(_)
            | Term::Str(_)
            | Term::Lbl(_)
            | Term::Enum(_)
            | Term::Sym(_) => true,
            Term::Let(_, _, _)
            | Term::Record(_)
            | Term::List(_)
            | Term::Fun(_, _)
            | Term::App(_, _)
            | Term::Var(_)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::Promise(_, _, _)
            | Term::Assume(_, _, _)
            | Term::Wrapped(_, _)
            | Term::Contract(_, _)
            | Term::DefaultValue(_)
            | Term::ContractWithDefault(_, _, _)
            | Term::Docstring(_, _)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_)
            | Term::RecRecord(_) => false,
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
pub enum UnaryOp<CapturedTerm> {
    /// If-then-else.
    Ite(),

    /// Test if a number is zero.
    ///
    /// Will be removed once there is a reasonable equality.
    IsZero(),

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

    /// Raise a blame, which stops the execution and prints an error according to the label argument.
    Blame(),

    /// Typecast an enum to a larger enum type.
    ///
    /// `Embed` is used to upcast enums. For example, if a value `x` has enum type `a | b`, then
    /// `embed c x` will have enum type `a | b | c`. It only affects typechecking as at runtime
    /// `embed someId` act like the identity.
    Embed(Ident),
    // This is a hacky way to deal with this for now.
    //
    // Ideally it should change to eliminate the dependency with RichTerm
    // in the future.
    /// A switch block. Used to match on a enumeration.
    Switch(HashMap<Ident, CapturedTerm>, Option<CapturedTerm>),

    /// Static access to a record field.
    ///
    /// Static means that the field identifier is a statically known string inside the source.
    StaticAccess(Ident),

    /// Map a function on a record.
    ///
    /// The mapped function must take two arguments, the name of the field as a string, and the
    /// content of the field. `MapRec` then replaces the content of each field by the result of the
    /// function: i.e., `mapRec f {a=2;}` evaluates to `{a=(f "a" 2);}`.
    MapRec(CapturedTerm),

    /// Inverse the polarity of a label.
    ChangePolarity(),

    /// Get the polarity of a label.
    Pol(),
    /// Go to the domain in the type path of a label.
    ///
    /// If the argument is a label with a [type path](../label/enum.TyPath.html) representing some
    /// subtype of the type of the original contract, as in:
    ///
    /// ```
    /// (Num -> Num) -> Num
    ///  ^^^^^^^^^^ type path
    /// ------------------- original type
    /// ```
    ///
    /// Then `GoDom` evaluates to a copy of this label, where the path has gone forward into the domain:
    ///
    /// ```
    /// (Num -> Num) -> Num
    ///  ^^^ new type path
    /// ------------------- original type
    /// ```
    GoDom(),
    /// Go to the codomain in the type path of a label.
    ///
    /// See `GoDom`.
    GoCodom(),
    /// Append text to the tag of a label.
    Tag(String),

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

    /// Only generated during the evaluation of a string with interpolated expressions. It holds a
    /// string accumulator, the remaining chunks to be evaluated, and is applied to the current
    /// chunk being evaluated.
    ChunksConcat(String, Vec<StrChunk<CapturedTerm>>),
}

impl<Ty> UnaryOp<Ty> {
    pub fn map<To, F: Fn(Ty) -> To>(self, f: F) -> UnaryOp<To> {
        use UnaryOp::*;

        match self {
            Switch(m, op) => Switch(
                m.into_iter()
                    .map(|e| {
                        let (id, t) = e;
                        (id, f(t))
                    })
                    .collect(),
                op.map(f),
            ),
            MapRec(t) => MapRec(f(t)),

            Ite() => Ite(),

            IsZero() => IsZero(),

            IsNum() => IsNum(),
            IsBool() => IsBool(),
            IsStr() => IsStr(),
            IsFun() => IsFun(),
            IsList() => IsList(),

            Blame() => Blame(),

            Embed(id) => Embed(id),

            StaticAccess(id) => StaticAccess(id),

            ChangePolarity() => ChangePolarity(),
            Pol() => Pol(),
            GoDom() => GoDom(),
            GoCodom() => GoCodom(),
            Tag(s) => Tag(s),

            Wrap() => Wrap(),

            Seq() => Seq(),
            DeepSeq() => DeepSeq(),

            ListHead() => ListHead(),
            ListTail() => ListTail(),
            ListLength() => ListLength(),

            ChunksConcat(s, chunks) => ChunksConcat(
                s,
                chunks
                    .into_iter()
                    .map(|chunk| match chunk {
                        StrChunk::Literal(s) => StrChunk::Literal(s),
                        StrChunk::Expr(e) => StrChunk::Expr(f(e)),
                    })
                    .collect(),
            ),
        }
    }
}

/// Primitive binary operators
#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp<CapturedTerm> {
    /// Addition of numerals.
    Plus(),
    /// Concatenation of strings.
    PlusStr(),
    /// Unwrap a tagged term.
    ///
    /// See `Wrap` in [`UnaryOp`](enum.UnaryOp.html).
    Unwrap(),
    /// Equality on booleans.
    EqBool(),
    /// Extend a record with a dynamic field.
    ///
    /// Dynamic means that the field name may be an expression and not a statically known string.
    /// `DynExtend` tries to evaluate this name to a string, and in case of success, add a field
    /// with this name to the given record with the `CapturedTerm` as content.
    DynExtend(CapturedTerm),
    /// Remove a field from a record. The field name is given as an arbitrary Nickel expression.
    DynRemove(),
    /// Access the field of record. The field name is given as an arbitrary Nickel expression.
    DynAccess(),
    /// Test if a record has a specific field.
    HasField(),
    /// Concatenate two lists.
    ListConcat(),
    /// Map a function on each element of a list.
    ListMap(),
    /// Access the n-th element of a list.
    ListElemAt(),
    /// The merge operator (see the [merge module](../merge/index.html)).
    Merge(),
}

impl<Ty> BinaryOp<Ty> {
    pub fn map<To, F: Fn(Ty) -> To>(self, f: F) -> BinaryOp<To> {
        use BinaryOp::*;

        match self {
            DynExtend(t) => DynExtend(f(t)),
            Plus() => Plus(),
            PlusStr() => PlusStr(),
            Unwrap() => Unwrap(),
            EqBool() => EqBool(),
            DynRemove() => DynRemove(),
            DynAccess() => DynAccess(),
            HasField() => HasField(),
            ListConcat() => ListConcat(),
            ListMap() => ListMap(),
            ListElemAt() => ListElemAt(),
            Merge() => Merge(),
        }
    }

    pub fn is_strict(&self) -> bool {
        match self {
            BinaryOp::Merge() => false,
            _ => true,
        }
    }
}

/// Wrap [terms](type.Term.html) with positional information.
#[derive(Debug, PartialEq, Clone)]
pub struct RichTerm {
    pub term: Box<Term>,
    pub pos: Option<RawSpan>,
}

impl RichTerm {
    pub fn new(t: Term) -> RichTerm {
        RichTerm {
            term: Box::new(t),
            pos: None,
        }
    }

    /// Erase recursively the positional information.
    ///
    /// It allows to use rust `Eq` trait to compare the values of the underlying terms.
    pub fn clean_pos(&mut self) {
        self.pos = None;
        self.term
            .apply_to_rich_terms(|rt: &mut Self| rt.clean_pos());
    }

    pub fn app(rt1: RichTerm, rt2: RichTerm) -> RichTerm {
        Term::App(rt1, rt2).into()
    }

    pub fn var(s: String) -> RichTerm {
        Term::Var(Ident(s)).into()
    }

    pub fn fun(s: String, rt: RichTerm) -> RichTerm {
        Term::Fun(Ident(s), rt).into()
    }

    pub fn let_in(id: &str, e: RichTerm, t: RichTerm) -> RichTerm {
        Term::Let(Ident(id.to_string()), e, t).into()
    }

    pub fn ite(c: RichTerm, t: RichTerm, e: RichTerm) -> RichTerm {
        RichTerm::app(RichTerm::app(Term::Op1(UnaryOp::Ite(), c).into(), t), e)
    }

    pub fn plus(t0: RichTerm, t1: RichTerm) -> RichTerm {
        Term::Op2(BinaryOp::Plus(), t0, t1).into()
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
            v @ Term::Bool(_)
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
            Term::Op1(UnaryOp::Switch(cases, default), t) => {
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
                    // method in Rust, but it has currently not make it to the stable version yet
                    .map_or(Ok(None), |res| res.map(Some))?;

                let t = t.traverse(f, state)?;

                f(
                    RichTerm {
                        term: Box::new(Term::Op1(UnaryOp::Switch(cases_res?, default), t)),
                        pos,
                    },
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
            Term::Assume(ty, l, t) => {
                let t = t.traverse(f, state)?;
                f(
                    RichTerm {
                        term: Box::new(Term::Assume(ty, l, t)),
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
            Term::Record(map) => {
                // The annotation on `map_res` uses Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let map_res: Result<HashMap<Ident, RichTerm>, E> = map
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,RichTerm), E>
                    .map(|(id, t)| t.traverse(f, state).map(|t_ok| (id.clone(), t_ok)))
                    .collect();
                f(
                    RichTerm {
                        term: Box::new(Term::Record(map_res?)),
                        pos,
                    },
                    state,
                )
            }
            Term::RecRecord(map) => {
                // The annotation on `map_res` uses Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let map_res: Result<HashMap<Ident, RichTerm>, E> = map
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,RichTerm), E>
                    .map(|(id, t)| t.traverse(f, state).map(|t_ok| (id.clone(), t_ok)))
                    .collect();
                f(
                    RichTerm {
                        term: Box::new(Term::RecRecord(map_res?)),
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
                        StrChunk::Expr(t) => Ok(StrChunk::Expr(t.traverse(f, state)?)),
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
            Term::Contract(ty, label) => {
                let ty = match ty {
                    Types(AbsType::Flat(t)) => Types(AbsType::Flat(t.traverse(f, state)?)),
                    ty => ty,
                };

                Ok(RichTerm {
                    term: Box::new(Term::Contract(ty, label)),
                    pos,
                })
            }
            Term::DefaultValue(t) => {
                let t = t.traverse(f, state)?;
                f(
                    RichTerm {
                        term: Box::new(Term::DefaultValue(t)),
                        pos,
                    },
                    state,
                )
            }
            Term::ContractWithDefault(ty, lbl, t) => {
                let ty = match ty {
                    Types(AbsType::Flat(t)) => Types(AbsType::Flat(t.traverse(f, state)?)),
                    ty => ty,
                };

                let t = t.traverse(f, state)?;
                f(
                    RichTerm {
                        term: Box::new(Term::ContractWithDefault(ty, lbl, t)),
                        pos,
                    },
                    state,
                )
            }
            Term::Docstring(s, t) => {
                let t = t.traverse(f, state)?;
                f(
                    RichTerm {
                        term: Box::new(Term::Docstring(s, t)),
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
        Self::new(t)
    }
}
