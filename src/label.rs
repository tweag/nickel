//! Define the type of labels.
//!
//! A label is a value holding metadata relative to contract checking. It gives the user useful
//! information about the context of a contract failure.
use crate::position::RawSpan;
use crate::types::Types;
use std::fmt;

/// A type path.
///
/// Checking higher-order contracts can involve a good share of intermediate contract checking.
/// Take the following example:
/// ```
/// Assume((Num -> Num) -> Num) -> Num -> Num, fun ev => fun cst => ev (fun x => cst))
/// ```
/// Once called, various checks will be performed on the arguments of functions and their return
/// values:
/// 1. Check that `ev` provides a `Num` to `(fun x => cst)`
/// 2. Check that `(fun x => cst)` returns a `Num`
/// 3. Check that `ev (fun x => cst)` return a `Num`
/// 4. etc.
///
/// Each check can be linked to a base type occurrence (here, a `Num`) in the original type:
/// ```
/// (Num -> Num) -> Num) -> Num -> Num
///  ^^^1   ^^^2    ^^^3    etc.
/// ```
///
/// This is the information encoded by a type path: what part of the original type is currently
/// being checked by this label. It is then reported to the user in case of a blame.
#[derive(Debug, Clone, PartialEq)]
pub enum TyPath {
    Nil(),
    Domain(Box<TyPath>),
    Codomain(Box<TyPath>),
}

/// The construct from where a label originates.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ContractKind {
    Assume,
    Promise,
    Contract,
}

/// A blame label.
///
/// A label is associated to a contract check (an assume, a promise or a contract as an enriched
/// value) and contains information to report to the user when a contract fails and a blame occurs.
/// It includes in particular a [type path](enum.TyPath.html) and a **polarity**.
///
/// # Polarity
///
/// One crucial aspect of first class contracts is to be able to check higher-order types, which
/// are types with arrows in it. Consider the simplest example:
///
/// ```
/// Assume(Num -> Num, f)
/// ```
///
/// This does not entail that `f` returns a `Num` in *every* situation. The identity function `id
/// = fun x => x` can certainly be given the type `Num -> Num`, but `id "a" = "a"` is not a `Num`.
///
/// To satisfy the contract `Num -> Num` for `f` is to satisfy the predicate "if you give me a
/// `Num` as an argument, I give you a `Num` as a result". There is an additional contract to be
/// checked, which is not the responsibility of `f`, but the caller's (or context)
/// one.
///
/// `Assume(Num -> Num, f)` should thus be evaluated as `Assume(Num, fun arg
/// => f (Assume(Num, arg)))`, but we want to report the failures of the two introduced
/// subcontracts in a different way:
///  - The inner one (on the argument) says that `f` has been misused: it has been applied to
///  something that is not a `Num`.
///  - The outer one says that `f` failed to satisfy its contract, as it has been provided with a
///  `Num` (otherwise the inner contracts would have failed before) but failed to deliver a `Num`.
///
/// This duality caller/callee or function/context is indicated by the polarity: the outer
/// corresponds to a *positive* polarity (the contract is on the term), while the inner corresponds
/// to a *negative* one (the contact is on the context). The polarity always starts as `true` in
/// user-written contracts, but is toggled in the argument contract when the interpreter decomposes
/// an higher order-contract. This also generalizes to higher types such as `((Num -> Num) -> Num)
/// -> Num` where the polarity alternates each time.
#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    /// The type checked by the original contract.
    pub types: Types,
    /// The construct which introduced the orignal contract.
    pub kind: ContractKind,
    /// A string tag to be printed together with the error message.
    pub tag: String,
    /// The position of the original contract.
    pub span: RawSpan,
    /// The polarity, used for higher-order contracts, that specifies if the current contract is
    /// on the environment (ex, the argument of a function) or on the term.
    pub polarity: bool,
    /// The path of the type being currently checked in the original type.
    pub path: TyPath,
}

impl fmt::Display for ContractKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ContractKind::Assume => write!(f, "Assume"),
            ContractKind::Promise => write!(f, "Promise"),
            ContractKind::Contract => write!(f, "Contract"),
        }
    }
}

#[cfg(test)]
use crate::types::AbsType;
#[cfg(test)]
use codespan::Files;

#[cfg(test)]
impl Label {
    /// Generate a dummy label for testing purpose.
    pub fn dummy() -> Label {
        Label {
            types: Types(AbsType::Num()),
            kind: ContractKind::Contract,
            tag: "testing".to_string(),
            span: RawSpan {
                src_id: Files::new().add("<test>", String::from("empty")),
                start: 0.into(),
                end: 1.into(),
            },
            polarity: false,
            path: TyPath::Nil(),
        }
    }
}
