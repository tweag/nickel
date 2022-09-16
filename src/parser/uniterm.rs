//! Additional AST nodes for the common UniTerm syntax (see RFC002 for more details).
use super::*;
use error::ParseError;
use utils::{build_record, elaborate_field_path, FieldPath, FieldPathElem};

use crate::{
    position::{RawSpan, TermPos},
    term::{Contract, MergePriority, MetaValue, RecordAttrs, RichTerm, SharedTerm, Term},
    types::{AbsType, Types, UnboundTypeVariableError},
};

use std::{borrow::Cow, collections::HashSet, convert::TryFrom};

/// A node of the uniterm AST. We only define new variants for those constructs that are common to
/// types and terms. Otherwise, we piggyback on the existing ASTs to avoid duplicating methods and
/// type definitions.
///
/// During parsing, some constructs are common to both terms and types, such as variables or record
/// literals ([`UniRecord`]s). They may be the source of ambiguities in the grammar, if we add two
/// different derivation for them. This happens inside rules that accepts both a term and and a
/// type, say the operands of an arrow `lhs -> rhs`. Take for example `lhs` to be a variable `a`.
/// It's hard not to end up having two possible derivation for `a`, one going first through term
/// rules and then types rules (a custom contract `#a` in the old syntax), and vice-versa (a type
/// variable `a` in the old syntax).
///
/// To avoid the issue, we parse the source as a `UniTermNode`s, and give only one possible
/// derivation - we will continue with the variable example in the following - through the parsing
/// rules. As long as we can't decide yet how to see this variable, we keep it as a
/// `UniTermNode::Var`.
///
/// As soon as this variable is used in a compound expression, the top-level rule tells us how to
/// translate it. For example, if we see an arrow `a -> Num`, then we will convert it to a type
/// variable, and return `UniTermNode::Types(AbsType::Arrow(..))` (there is actually a subtelty:
/// see [`fix_type_vars`], but let's ignore it here). If, on the other hand, we enter the rule for
/// an infix operator as in `a + 1`, `a` will be converted to a `Term::Var` and the resulting
/// uniterm will be `UniTermNode::Term(Term::Op2(..))`.
pub enum UniTermNode {
    /// A variable. Can refer both to a term variable or a type variable.
    Var(Ident),
    /// A record. Can refer both to a record literal or a record type.
    Record(UniRecord),
    /// A uniterm that has been determined to be a term.
    Term(RichTerm),
    /// A uniterm that has been determined to be a type.
    Types(Types),
}

/// A uniterm with positional information.
pub struct UniTerm {
    node: UniTermNode,
    pos: TermPos,
}

impl From<UniTermNode> for UniTerm {
    fn from(node: UniTermNode) -> Self {
        UniTerm {
            node,
            pos: TermPos::None,
        }
    }
}

impl UniTerm {
    pub fn with_pos(mut self, pos: TermPos) -> Self {
        self.pos = pos;
        self
    }
}

impl TryFrom<UniTerm> for Types {
    type Error = ParseError;

    fn try_from(ut: UniTerm) -> Result<Self, ParseError> {
        match ut.node {
            UniTermNode::Var(id) => Ok(Types(AbsType::Var(id))),
            UniTermNode::Record(r) => Types::try_from(r),
            UniTermNode::Types(ty) => Ok(ty),
            UniTermNode::Term(rt) => Ok(Types(AbsType::Flat(rt))),
        }
    }
}

impl TryFrom<UniTerm> for RichTerm {
    type Error = ParseError;

    fn try_from(ut: UniTerm) -> Result<Self, ParseError> {
        let UniTerm { node, pos } = ut;
        let rt = match node {
            UniTermNode::Var(id) => RichTerm::new(Term::Var(id), pos),
            UniTermNode::Record(r) => RichTerm::try_from(r)?,
            UniTermNode::Types(ty) => {
                ty.contract().map_err(|UnboundTypeVariableError(id)| {
                    // We unwrap the position of the identifier, which must be set at this stage of parsing
                    let pos = id.pos();
                    ParseError::UnboundTypeVariables(vec![id], pos.unwrap())
                })?
            }
            UniTermNode::Term(rt) => rt,
        };

        Ok(rt.with_pos(pos))
    }
}

impl From<RichTerm> for UniTerm {
    fn from(rt: RichTerm) -> Self {
        let pos = rt.pos;

        UniTerm {
            node: UniTermNode::Term(rt),
            pos,
        }
    }
}

impl From<Term> for UniTerm {
    fn from(t: Term) -> Self {
        Self::from(RichTerm::from(t))
    }
}

impl From<Types> for UniTerm {
    fn from(ty: Types) -> Self {
        UniTerm {
            node: UniTermNode::Types(ty),
            pos: TermPos::None,
        }
    }
}

impl From<UniRecord> for UniTerm {
    fn from(ur: UniRecord) -> Self {
        let pos = ur.pos;

        UniTerm {
            node: UniTermNode::Record(ur),
            pos,
        }
    }
}

/// A record in the `UniTerm` syntax.
#[derive(Clone)]
pub struct UniRecord {
    pub fields: Vec<(FieldPath, RichTerm)>,
    pub tail: Option<(Types, TermPos)>,
    pub attrs: RecordAttrs,
    pub pos: TermPos,
    /// The position of the final ellipsis `..`, if any. Used for error reporting. `pos_ellipsis`
    /// must be different from `TermPos::None` if and only if `attrs.open` is `true`.
    pub pos_ellipsis: TermPos,
}

/// Error indicating that a construct is not allowed when trying to interpret an `UniRecord` as a
/// record type in a strict way. See [`UniRecord::into_type_strict`]. Hold the position of the
/// illegal construct.
pub struct InvalidRecordTypeError(pub TermPos);

impl UniRecord {
    /// Try to convert a `UniRecord` to a type. The strict part means that the `UniRecord` must be
    /// a plain record type, uniquely containing fields of the form `fields: Type`. Currently, it
    /// doesn't support the field path syntax: `{foo.bar.baz : Type}.into_type_strict()` returns an
    /// `Err`.
    pub fn into_type_strict(self) -> Result<Types, InvalidRecordTypeError> {
        // An open record (with an ellipsis `..` at the end) can't be translated to a record type.
        // `pos_ellipsis` should be set iff `attrs.open` is true.
        debug_assert!((self.pos_ellipsis == TermPos::None) != self.attrs.open);

        if let Some(raw_span) = self.pos_ellipsis.into_opt() {
            return Err(InvalidRecordTypeError(TermPos::Original(raw_span)));
        }

        let ty = self
            .fields
            .into_iter()
            // Because we build row types as a linked list by folding on the original iterator, the
            // order of identifiers is reversed. This not a big deal but it's less confusing to the
            // user to print them in the original order for error reporting.
            .rev()
            .try_fold(
                self.tail
                    .map(|(tail, _)| tail)
                    .unwrap_or(Types(AbsType::RowEmpty())),
                |acc, (mut path, rt)| {
                    // We don't support compound paths for types, yet.
                    if path.len() > 1 {
                        let span = path
                            .into_iter()
                            .map(|path_elem| match path_elem {
                                FieldPathElem::Ident(id) => id.pos().into_opt(),
                                FieldPathElem::Expr(rt) => rt.pos.into_opt(),
                            })
                            .reduce(|acc, pos| {
                                acc.zip(pos)
                                    .and_then(|(acc, span)| RawSpan::fuse(acc, span))
                            })
                            .flatten();

                        Err(InvalidRecordTypeError(
                            span.map_or(TermPos::None, TermPos::Original),
                        ))
                    } else {
                        let elem = path.pop().unwrap();
                        match elem {
                            FieldPathElem::Ident(id) => {
                                // At parsing stage, all `Rc`s must be 1-counted. We can thus call
                                // `into_owned()` without risking to actually clone anything.
                                match rt.term.into_owned() {
                                    Term::MetaValue(MetaValue {
                                        doc: None,
                                        types: Some(ctrt),
                                        contracts,
                                        opt: false,
                                        priority: MergePriority::Normal,
                                        value: None,
                                    }) if contracts.is_empty() => Ok(Types(AbsType::RowExtend(
                                        id,
                                        Some(Box::new(ctrt.types)),
                                        Box::new(acc),
                                    ))),
                                    _ => {
                                        // Position of identifiers must always be set at this stage
                                        // (parsing)
                                        let span_id = id.pos().unwrap();
                                        let term_pos = rt.pos.into_opt().unwrap_or(span_id);
                                        Err(InvalidRecordTypeError(TermPos::Original(
                                            RawSpan::fuse(span_id, term_pos).unwrap(),
                                        )))
                                    }
                                }
                            }
                            FieldPathElem::Expr(rt) => Err(InvalidRecordTypeError(rt.pos)),
                        }
                    }
                },
            )?;
        Ok(Types(AbsType::StaticRecord(Box::new(ty))))
    }

    pub fn with_pos(mut self, pos: TermPos) -> Self {
        self.pos = pos;
        self
    }
}

impl TryFrom<UniRecord> for RichTerm {
    type Error = ParseError;

    /// Convert a `UniRecord` to a term. If the `UniRecord` has a tail, it is first interpreted as
    /// a type and then converted to a contract. Otherwise it is interpreted as a record directly.
    /// Fail if the `UniRecord` has a tail but isn't syntactically a record type either. Elaborate
    /// field paths `foo.bar = value` to the expanded form `{foo = {bar = value}}`.
    ///
    /// We also fix the type variables of the type appearing inside annotations (see
    /// [`fix_type_vars`]).
    fn try_from(ur: UniRecord) -> Result<Self, ParseError> {
        let pos = ur.pos;

        let result = if let Some((_, tail_pos)) = ur.tail {
            ur.into_type_strict()
                // We unwrap all positions: at this stage of the parsing, they must all be set
                .map_err(|InvalidRecordTypeError(pos)| {
                    ParseError::InvalidUniRecord(pos.unwrap(), tail_pos.unwrap(), pos.unwrap())
                })
                .and_then(|mut ty| {
                    fix_type_vars(&mut ty);
                    ty.contract().map_err(|UnboundTypeVariableError(id)| {
                        ParseError::UnboundTypeVariables(vec![id], pos.unwrap())
                    })
                })
        } else {
            let UniRecord { fields, attrs, .. } = ur;
            let elaborated = fields.into_iter().map(|(path, mut rt)| {
                fix_field_types(&mut rt);
                elaborate_field_path(path, rt)
            });

            Ok(RichTerm::from(build_record(elaborated, attrs)))
        };

        result.map(|rt| rt.with_pos(pos))
    }
}

impl TryFrom<UniRecord> for Types {
    type Error = ParseError;

    /// Convert a `UniRecord` to a type. If the `UniRecord` has a tail, it is interpreted strictly
    /// as a type and fail if it isn't a plain record type. Otherwise, we first try to interpret it
    /// as a plain record type, and if that doesn't work, we interpret it as a term and wrap it
    /// back as a user-defined contract.
    fn try_from(ur: UniRecord) -> Result<Self, ParseError> {
        let pos = ur.pos;

        if let Some((_, tail_pos)) = ur.tail {
            ur.into_type_strict()
                .map_err(|InvalidRecordTypeError(illegal_pos)| {
                    ParseError::InvalidUniRecord(
                        illegal_pos.unwrap(),
                        tail_pos.unwrap(),
                        pos.unwrap(),
                    )
                })
        } else {
            ur.clone()
                .into_type_strict()
                .or_else(|_| RichTerm::try_from(ur).map(|rt| Types(AbsType::Flat(rt))))
        }
    }
}

/// Post-process a type at the right hand side of an annotation by replacing each unbound type
/// variable by a term variable with the same identifier seen as a custom contract
/// (`AbsType::Var(id)` to `AbsType::Flat(Term::Var(id))`).
///
/// Since parsing is done bottom-up, and given the specification of the uniterm syntax for
/// variables occurring in types, we often can't know right away if a such a variable occurrence
/// will eventually be a type variable or a term variable seen as a custom contract.
///
/// Take for example `a -> b`. At this stage, `a` and `b` could be both variables referring to a
/// contract (e.g. in `x | a -> b`) or a type variable (e.g. in `x | forall a b. a -> b`),
/// depending on enclosing `forall`s. To handle both cases, we initially parse all variables inside
/// types as type variables. When reaching the right-hand side of an annotation, because `forall`s
/// can only bind locally in a type, we can then decide the actual nature of each occurrence. We
/// thus recurse into the newly constructed type to change those type variables that are not
/// actually bound by a `forall` to be term variables. This is the role of `fix_type_vars()`.
///
/// Once again because `forall`s only bind variables locally, and don't bind inside contracts, we
/// don't have to recurse into contracts and this pass will only visit each node of the AST at most
/// once in total.
///
/// There is one subtlety with unirecords, though. A unirecord can still be in interpreted as a
/// record type later. Take the following example:
///
/// ```nickel
/// let mk_pair : forall a b. a -> b -> {fst: a, snd: b} = <exp>
/// ```
///
/// Since this unirecord will eventually be interpreted as a record type, we can't know yet when
/// parsing `fst: a` if `a` will be a type variable or a term variable (while, for all other
/// constructs, an annotation is a boundary that `forall` binders can't cross). In this example,
/// there is indeed an enclosing forall binding `a`. With unirecords, before fixing type variables,
/// we have to wait until we eventually convert the unirecord to a term (in which case we fix all the
/// top-level annotations) or a type (in which case we do nothing: the enclosing type will trigger
/// the fix once it's fully constructed). Fixing a unirecord prior to a conversion to a term is
/// done by [`fix_field_types`].
pub fn fix_type_vars(ty: &mut Types) {
    fn fix_type_vars_aux(ty: &mut Types, mut bound_vars: Cow<HashSet<Ident>>) {
        match ty.0 {
            AbsType::Dyn()
            | AbsType::Num()
            | AbsType::Bool()
            | AbsType::Str()
            | AbsType::Sym()
            | AbsType::Flat(_)
            | AbsType::RowEmpty()
            | AbsType::Wildcard(_) => (),
            AbsType::Arrow(ref mut s, ref mut t) => {
                fix_type_vars_aux(s.as_mut(), Cow::Borrowed(bound_vars.as_ref()));
                fix_type_vars_aux(t.as_mut(), bound_vars);
            }
            AbsType::Var(ref mut id) => {
                if !bound_vars.contains(id) {
                    let id = std::mem::take(id);
                    let pos = id.pos();
                    ty.0 = AbsType::Flat(RichTerm::new(Term::Var(id), pos));
                }
            }
            AbsType::Forall(ref id, ref mut ty) => {
                bound_vars.to_mut().insert(id.clone());
                fix_type_vars_aux(&mut *ty, bound_vars);
            }
            AbsType::RowExtend(_, ref mut ty_opt, ref mut tail) => {
                if let Some(ref mut ty) = *ty_opt {
                    fix_type_vars_aux(ty.as_mut(), Cow::Borrowed(bound_vars.as_ref()));
                }

                // We don't touch a row tail that is a type variable, because the typechecker
                // relies on row types being well-formed, which the parser must ensure.
                // Well-formedness requires that only type variables and `Dyn` may appear in a row
                // tail position, so we don't turn such variables into term variables (having a
                // contract in tail position doesn't make sense, in the current model at least,
                // both for typechecking and at evaluation).
                if !matches!((**tail).0, AbsType::Var(_)) {
                    fix_type_vars_aux(tail.as_mut(), bound_vars);
                }
            }
            AbsType::DynRecord(ref mut ty)
            | AbsType::Array(ref mut ty)
            | AbsType::Enum(ref mut ty)
            | AbsType::StaticRecord(ref mut ty) => fix_type_vars_aux(ty.as_mut(), bound_vars),
        }
    }

    fix_type_vars_aux(ty, Cow::Owned(HashSet::new()))
}

/// Fix the type variables of types appearing as annotations of record fields. See
/// [`fix_type_vars`].
pub fn fix_field_types(rt: &mut RichTerm) {
    if let Term::MetaValue(ref mut m) = SharedTerm::make_mut(&mut rt.term) {
        if let Some(Contract { ref mut types, .. }) = m.types {
            fix_type_vars(types);
        }

        for ctr in m.contracts.iter_mut() {
            fix_type_vars(&mut ctr.types);
        }
    }
}
