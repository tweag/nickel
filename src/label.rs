//! Define the type of labels.
//!
//! A label is a value holding metadata relative to contract checking. It gives the user useful
//! information about the context of a contract failure.
use crate::position::RawSpan;
use crate::types::{AbsType, Types};

pub mod ty_path {
    //! Type paths.
    //!
    //! Checking higher-order contracts can involve a good share of intermediate contract checking.
    //! Take the following example:
    //! ```
    //! Assume((Num -> Num) -> Num) -> Num -> Num, fun ev => fun cst => ev (fun x => cst))
    //! ```
    //! Once called, various checks will be performed on the arguments of functions and their return
    //! values:
    //! 1. Check that `ev` provides a `Num` to `(fun x => cst)`
    //! 2. Check that `(fun x => cst)` returns a `Num`
    //! 3. Check that `ev (fun x => cst)` return a `Num`
    //! 4. etc.
    //!
    //! Each check can be linked to a base type occurrence (here, a `Num`) in the original type:
    //! ```
    //! (Num -> Num) -> Num) -> Num -> Num
    //!  ^^^1   ^^^2    ^^^3    etc.
    //! ```
    //!
    //! This is the information encoded by a type path: what part of the original type is currently
    //! being checked by this label. It is then reported to the user in case of a blame.
    //!
    //! Paths are encoded as lists of elements, specifying if the next step is either to go to the **domain**
    //! or to the **codomain**.

    use super::{AbsType, Types};
    use crate::identifier::Ident;

    /// An element of a path type.
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum Elem {
        Domain,
        Codomain,
    }

    pub type Path = Vec<Elem>;

    /// Determine if the path has only `Codomain` components.
    pub fn is_only_codom(p: &Path) -> bool {
        p.iter().all(|elt| *elt == Elem::Codomain)
    }

    /// Return the position span encoded by a type path in the string representation of the
    /// corresponding type.
    ///
    /// Used in the error reporting of blame errors (see
    /// [report_ty_paht](../error/fn.report_ty_path.html)).
    ///
    /// # Example
    ///
    /// - Type path: `Codomain(Domain(Nil()))`
    /// - Type : `Num -> Num -> Num`
    /// - Return: `(7, 10)`, which corresponds to the second `Num` occurrence.
    pub fn span<'a, I>(mut path_it: std::iter::Peekable<I>, mut ty: &Types) -> (usize, usize)
    where
        I: Iterator<Item = &'a Elem>,
        I: std::clone::Clone,
    {
        // peek() returns a reference, and hence keeps a mutable borrow of `path_it` which forbids
        // to call to next() in the same region. This is why we need to split the match in two
        // different block.
        let forall_offset = match (&ty.0, path_it.peek()) {
            (_, None) => {
                let repr = format!("{}", ty);
                return (0, repr.len());
            }
            (AbsType::Forall(_, _), Some(_)) => {
                // The length of "forall" plus the final separating dot and whitespace ". "
                let mut result = 8;
                while let AbsType::Forall(Ident(id), body) = &ty.0 {
                    // The length of the identifier plus the preceding whitespace
                    result += id.len() + 1;
                    ty = body.as_ref();
                }

                result
            }
            _ => 0,
        };

        match (&ty.0, path_it.next()) {
            (AbsType::Arrow(dom, codom), Some(next)) => {
                // The potential shift of the start position of the domain introduced by the couple
                // of parentheses around the domain. Parentheses are added when printing a function
                // type whose domain is itself a function.
                // For example, `Arrow(Arrow(Num, Num), Num)` is rendered as "(Num -> Num) -> Num".
                // In this case, the position of the sub-type "Num -> Num" starts at 1 instead of
                // 0.
                let paren_offset = match dom.0 {
                    AbsType::Arrow(_, _) => 1,
                    _ => 0,
                };

                match next {
                    Elem::Domain => {
                        let (dom_start, dom_end) = span(path_it, dom.as_ref());
                        (
                            dom_start + paren_offset + forall_offset,
                            dom_end + paren_offset + forall_offset,
                        )
                    }
                    Elem::Codomain => {
                        let (_, dom_end) = span(Vec::new().iter().peekable(), dom.as_ref());
                        let (codom_start, codom_end) = span(path_it, codom.as_ref());
                        // At this point, paren_offset is:
                        // (a) `1` if there is a couple of parentheses around the domain
                        // (b) `0` otherwise
                        // In case (a), we need to shift the beginning of the codomain by two,
                        // to also take into account the closing ')' character, whence the `offset*2`.
                        // The `4` corresponds to the arrow " -> ".
                        let offset = (paren_offset * 2) + 4 + dom_end + forall_offset;
                        (codom_start + offset, codom_end + offset)
                    }
                }
            }
            _ => panic!(),
        }
    }
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
    /// A string tag to be printed together with the error message.
    pub tag: String,
    /// The position of the original contract.
    pub span: RawSpan,
    /// The polarity, used for higher-order contracts, that specifies if the current contract is
    /// on the environment (ex, the argument of a function) or on the term.
    pub polarity: bool,
    /// The path of the type being currently checked in the original type.
    pub path: ty_path::Path,
}

#[cfg(test)]
use codespan::Files;

#[cfg(test)]
impl Label {
    /// Generate a dummy label for testing purpose.
    pub fn dummy() -> Label {
        Label {
            types: Types(AbsType::Num()),
            tag: "testing".to_string(),
            span: RawSpan {
                src_id: Files::new().add("<test>", String::from("empty")),
                start: 0.into(),
                end: 1.into(),
            },
            polarity: false,
            path: Vec::new(),
        }
    }
}
