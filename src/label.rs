//! Define the type of labels
//!
//! A label is a value holding meta-data relative to contract checking. It is used to give the user
//! useful information to fix a contract failure.
use crate::position::RawSpan;

/// A type path
///
/// Checking higher-order contracts can involve a good share of intermediate contract checking.
/// Take the following example:
/// ```
/// Assume((Num -> Num) -> Num) -> Num -> Num, fun ev => fun cst => ev (fun x => cst))
/// ```
/// Once called, various checks will be performed on the arguments of functions and their return
/// values:
/// 1. check that `ev` provides an Num to (fun x => cst)
/// 2. check that (fun x => cst) returns a Num
/// 3. check that `ev (fun x => cst)` return a num
/// 4. etc.
///
/// Each check can be linked to a basic type occurrence (here, `Num`) in the original type:
/// (Num -> Num) -> Num) -> Num -> Num
///  ^^^1   ^^^2    ^^^3    etc.
///
/// This is the information encoded by a type path: what occurrence is currently being checked by this label, so that it can be reported to the user in case of the
/// failure of an higher order contract.
#[derive(Debug, Clone, PartialEq)]
pub enum TyPath {
    Nil(),
    Domain(Box<TyPath>),
    Codomain(Box<TyPath>),
}

/// A blame label
///
/// A label is associated to a contract check (an assume, a promise or a contract as an enriched
/// value) and contains informations to report to the user in case a contract fails and a blame
/// happens.
#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    /// A string tag to be printed together with the error message
    pub tag: String,
    /// The position of the original contract to report
    pub span: RawSpan,
    /// The polarity, used for higher-order contracts, that specify if the current contract is
    /// constraining the environment (ex, the argument of a function) or a term.
    pub polarity: bool,
    /// The path of the type being currently checked in the original type
    pub path: TyPath,
}
