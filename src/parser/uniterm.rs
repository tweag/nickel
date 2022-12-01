//! Additional AST nodes for the common UniTerm syntax (see RFC002 for more details).
use super::*;
use error::ParseError;
use utils::{build_record, elaborate_field_path, FieldPath, FieldPathElem};

use crate::{
    environment::Environment,
    position::{RawSpan, TermPos},
    term::{record::RecordAttrs, Contract, MergePriority, MetaValue, RichTerm, SharedTerm, Term},
    types::{
        EnumRows, EnumRowsIteratorItem, RecordRow, RecordRows, RecordRowsF, TypeF, Types,
        UnboundTypeVariableError, VarKind,
    },
};

use std::{cell::RefCell, convert::TryFrom};

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
/// variable, and return `UniTermNode::Types(TypeF::Arrow(..))` (there is actually a subtlety:
/// see [`FixTypeVars::fix_type_vars`], but let's ignore it here). If, on the other hand, we enter the rule for
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
            UniTermNode::Var(id) => Ok(Types(TypeF::Var(id))),
            UniTermNode::Record(r) => Types::try_from(r),
            UniTermNode::Types(ty) => Ok(ty),
            UniTermNode::Term(rt) => Ok(Types(TypeF::Flat(rt))),
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
            UniTermNode::Types(mut ty) => {
                ty.fix_type_vars(pos.unwrap())?;
                ty.contract().map_err(|UnboundTypeVariableError(id)| {
                    // We unwrap the position of the identifier, which must be set at this stage of parsing
                    let pos = id.pos;
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
    pub tail: Option<(RecordRows, TermPos)>,
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

        let rrows = self
            .fields
            .into_iter()
            // Because we build row types as a linked list by folding on the original iterator, the
            // order of identifiers is reversed. This not a big deal but it's less confusing to the
            // user to print them in the original order for error reporting.
            .rev()
            .try_fold(
                self.tail
                    .map(|(tail, _)| tail)
                    .unwrap_or(RecordRows(RecordRowsF::Empty)),
                |acc: RecordRows, (mut path, rt)| {
                    // We don't support compound paths for types, yet.
                    if path.len() > 1 {
                        let span = path
                            .into_iter()
                            .map(|path_elem| match path_elem {
                                FieldPathElem::Ident(id) => id.pos.into_opt(),
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
                                        priority: MergePriority::Neutral,
                                        value: None,
                                    }) if contracts.is_empty() => {
                                        Ok(RecordRows(RecordRowsF::Extend {
                                            row: RecordRow {
                                                id,
                                                types: Box::new(ctrt.types),
                                            },
                                            tail: Box::new(acc),
                                        }))
                                    }
                                    _ => {
                                        // Position of identifiers must always be set at this stage
                                        // (parsing)
                                        let span_id = id.pos.unwrap();
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
        Ok(Types(TypeF::Record(rrows)))
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
    /// [`FixTypeVars::fix_type_vars`]).
    fn try_from(ur: UniRecord) -> Result<Self, ParseError> {
        let pos = ur.pos;

        let result = if let Some((_, tail_pos)) = ur.tail {
            ur.into_type_strict()
                // We unwrap all positions: at this stage of the parsing, they must all be set
                .map_err(|InvalidRecordTypeError(pos)| {
                    ParseError::InvalidUniRecord(pos.unwrap(), tail_pos.unwrap(), pos.unwrap())
                })
                .and_then(|mut ty| {
                    ty.fix_type_vars(pos.unwrap())?;
                    ty.contract().map_err(|UnboundTypeVariableError(id)| {
                        ParseError::UnboundTypeVariables(vec![id], pos.unwrap())
                    })
                })
        } else {
            let UniRecord { fields, attrs, .. } = ur;
            let elaborated = fields
                .into_iter()
                .map(|(path, mut rt)| {
                    fix_field_types(&mut rt)?;
                    Ok(elaborate_field_path(path, rt))
                })
                .collect::<Result<Vec<_>, _>>()?;

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
                .or_else(|_| RichTerm::try_from(ur).map(|rt| Types(TypeF::Flat(rt))))
        }
    }
}

#[derive(PartialEq, Eq, Copy, Clone)]
pub(super) enum VarKindCellState {
    /// The variable kind is yet to be determined.
    Unset,
    /// The variable kind that has been determined by an usage of the bound variable.
    Set,
}

/// Data stored in a `VarKindCell`.
///
/// We could have a simpler implementation using `std::cell::once_cell` (which [`VarKindCell`] is
/// somehow emulating), but the latter is not stabilized yet. Used during the
/// [FixTypeVars::fix_type_vars] phase.
#[derive(PartialEq, Eq)]
pub(super) struct VarKindCellData {
    var_kind: VarKind,
    state: VarKindCellState,
}

/// Cell providing shared mutable access to a var_kind. This is used to decide the kind of a
/// variable associated to a forall in the `fix_type_vars` phase.
///
/// This cell provides interior mutability for [`VarKindCellData`]. It makes it possible to mutate
/// the inner data when put in an environment, which only provides immutable references to its
/// values.
#[derive(PartialEq, Eq)]
pub(super) struct VarKindCell(RefCell<VarKindCellData>);

/// Error raised by [`VarKindCell`] when trying to set a variable kind which is different from the
/// one already set.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub(super) struct VarKindMismatch;

/// Environment maintained during the `fix_type_vars` phase. Used both to determine if a variable
/// is bound by an enclosing forall (if `env.get(var_id).is_some()`), and to provide a shared
/// mutable variable kind that can be modified depending on the location of type variable
/// occurrences.
pub(super) type BoundVarEnv = Environment<Ident, VarKindCell>;

impl VarKindCell {
    /// Create a new `VarKindCell` with the `Unset` state. The kind is set to `VarKind::Type`,
    /// meaning that unused type variables are given this kind by default.
    pub(super) fn new() -> Self {
        VarKindCell(RefCell::new(VarKindCellData {
            var_kind: VarKind::Type,
            state: VarKindCellState::Unset,
        }))
    }

    /// Set the variable kind of the inner mutable reference if not set yet. If the variable kind
    /// is already set, check that the variable kind of the cell and the one provided as an
    /// argument are equals, or return `Err(_)` otherwise.
    pub(super) fn set_or_check_equal(&self, var_kind: VarKind) -> Result<(), VarKindMismatch> {
        match &mut *self.0.borrow_mut() {
            VarKindCellData {
                var_kind: data,
                ref mut state,
            } if *state == VarKindCellState::Unset => {
                *data = var_kind;
                *state = VarKindCellState::Set;
                Ok(())
            }
            VarKindCellData {
                var_kind: data,
                state: VarKindCellState::Set,
            } if *data == var_kind => Ok(()),
            _ => Err(VarKindMismatch),
        }
    }

    /// Return the current var_kind.
    pub fn var_kind(&self) -> VarKind {
        self.0.borrow().var_kind
    }
}

pub(super) trait FixTypeVars {
    /// Post-process a type at the right hand side of an annotation by replacing each unbound type
    /// variable by a term variable with the same identifier seen as a custom contract
    /// (`TypeF::Var(id)` to `TypeF::Flat(Term::Var(id))`).
    ///
    /// Additionally, this passes determine the kind of a variable introduced by a forall binder.
    ///
    /// # Type variable fixing
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
    ///
    /// # Variable kind
    ///
    /// Type variables can have different kind (cf [crate::types::VarKind]). The kind determines if
    /// the variable is meant to be substituted for a type, record rows, or enum rows.
    ///
    /// During parsing, the kind is determined syntactically, depending on where the corresponding
    /// bound occurrences occur:
    ///
    /// ```nickel
    /// # occurs in type position
    /// forall a. Num -> {_: Bar -> a}
    /// # occurs in record rows position
    /// forall a. Num -> {foo: Str ; a} -> {bar: Str ; a}
    /// # occurs both in record rows and enum rows position
    /// # this is inconsistent and will raise a parse error
    /// forall a. [| `foo, `bar; a |] -> {foo : Str, bar: Str; a}
    /// ```
    fn fix_type_vars(&mut self, span: RawSpan) -> Result<(), ParseError> {
        self.fix_type_vars_env(BoundVarEnv::new(), span)
    }

    /// Fix type vars in a given environment of variables bound by foralls enclosing this type. The
    /// environment maps bound variables to a reference to the variable kind of the corresponding
    /// forall.
    fn fix_type_vars_env(
        &mut self,
        bound_vars: BoundVarEnv,
        span: RawSpan,
    ) -> Result<(), ParseError>;
}

impl FixTypeVars for Types {
    fn fix_type_vars_env(
        &mut self,
        mut bound_vars: BoundVarEnv,
        span: RawSpan,
    ) -> Result<(), ParseError> {
        match self.0 {
            TypeF::Dyn
            | TypeF::Num
            | TypeF::Bool
            | TypeF::Str
            | TypeF::Sym
            | TypeF::Flat(_)
            | TypeF::Wildcard(_) => Ok(()),
            TypeF::Arrow(ref mut s, ref mut t) => {
                (*s).fix_type_vars_env(bound_vars.clone(), span)?;
                (*t).fix_type_vars_env(bound_vars, span)?;
                Ok(())
            }
            TypeF::Var(ref mut id) => {
                if let Some(cell) = bound_vars.get(id) {
                    cell.set_or_check_equal(VarKind::Type)
                        .map_err(|_| ParseError::TypeVariableKindMismatch { ty_var: *id, span })?;
                } else {
                    let id = *id;
                    let pos = id.pos;
                    self.0 = TypeF::Flat(RichTerm::new(Term::Var(id), pos));
                }
                Ok(())
            }
            TypeF::Forall {
                ref var,
                // TODO: pass a mutable ref to var_kind, and set the var kind while fixing type
                // vars, depending on the position of type vars. Raise an error if the same type var
                // appears in position with incompatible kinds (as a record rows and a type, for
                // example).
                ref mut var_kind,
                ref mut body,
            } => {
                // We span a new VarKindCell and put it in the environment. The recursive calls to
                // fix_type_vars will fill this cell with the correct kind, which we get afterwards
                // to set the right value for `var_kind`.
                bound_vars.insert(*var, VarKindCell::new());
                (*body).fix_type_vars_env(bound_vars.clone(), span)?;
                // unwrap(): we just inseted a value for `var` above, and environment can never
                // delete values.
                *var_kind = bound_vars.get(var).unwrap().var_kind();

                Ok(())
            }
            TypeF::Dict(ref mut ty) | TypeF::Array(ref mut ty) => {
                (*ty).fix_type_vars_env(bound_vars, span)
            }
            TypeF::Enum(ref mut erows) => erows.fix_type_vars_env(bound_vars, span),
            TypeF::Record(ref mut rrows) => rrows.fix_type_vars_env(bound_vars, span),
        }
    }
}

impl FixTypeVars for RecordRows {
    fn fix_type_vars_env(
        &mut self,
        bound_vars: BoundVarEnv,
        span: RawSpan,
    ) -> Result<(), ParseError> {
        match self.0 {
            RecordRowsF::Empty => Ok(()),
            RecordRowsF::TailDyn => Ok(()),
            // We can't have a contract in tail position, so we don't fix `TailVar`. However, we
            // have to set the correct kind for the corresponding forall binder.
            RecordRowsF::TailVar(ref id) => {
                if let Some(cell) = bound_vars.get(id) {
                    cell.set_or_check_equal(VarKind::RecordRows)
                        .map_err(|_| ParseError::TypeVariableKindMismatch { ty_var: *id, span })?;
                }
                Ok(())
            }
            RecordRowsF::Extend {
                ref mut row,
                ref mut tail,
            } => {
                row.types.fix_type_vars_env(bound_vars.clone(), span)?;
                tail.fix_type_vars_env(bound_vars, span)
            }
        }
    }
}

impl FixTypeVars for EnumRows {
    fn fix_type_vars_env(
        &mut self,
        bound_vars: BoundVarEnv,
        span: RawSpan,
    ) -> Result<(), ParseError> {
        // An enum row doesn't contain any subtypes (beside other enum rows). No term variable can
        // appear in it, so we don't have to traverse for fixing type variables properly.
        //
        // However, the second task of the fix_type_vars phase is to determine the variable kind of
        // forall binders: here, we do need to check if the fail of this enum is an enum row type
        // variable.
        let mut iter = self
            .iter()
            .skip_while(|item| matches!(item, EnumRowsIteratorItem::Row(_)));
        match iter.next() {
            Some(EnumRowsIteratorItem::TailVar(id)) => {
                if let Some(cell) = bound_vars.get(id) {
                    cell.set_or_check_equal(VarKind::EnumRows)
                        .map_err(|_| ParseError::TypeVariableKindMismatch { ty_var: *id, span })?;
                }
                Ok(())
            }
            // unreachable(): we consumed all the rows item via the `take_while()` call above
            Some(EnumRowsIteratorItem::Row(_)) => unreachable!(),
            None => Ok(()),
        }
    }
}

/// Fix the type variables of types appearing as annotations of record fields. See
/// [`Types::fix_type_vars`].
pub fn fix_field_types(rt: &mut RichTerm) -> Result<(), ParseError> {
    if let Term::MetaValue(ref mut m) = SharedTerm::make_mut(&mut rt.term) {
        if let Some(Contract { ref mut types, .. }) = m.types {
            types.fix_type_vars(rt.pos.unwrap())?;
        }

        for ctr in m.contracts.iter_mut() {
            ctr.types.fix_type_vars(rt.pos.unwrap())?;
        }
    }

    Ok(())
}
