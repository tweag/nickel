//! Additional AST nodes for the common UniTerm syntax (see RFC002 for more details).
use super::{error::InvalidRecordTypeError, *};
use error::ParseError;
use indexmap::{map::Entry, IndexMap};

use crate::{
    bytecode::ast::{
        self,
        record::{FieldDef, FieldMetadata, FieldPathElem},
        typ::{EnumRow, EnumRows, RecordRow, RecordRows, Type},
        *,
    },
    environment::Environment,
    identifier::Ident,
    position::{RawSpan, TermPos},
    typ::{DictTypeFlavour, EnumRowsF, RecordRowsF, TypeF, VarKind},
};

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

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
/// variable, and return `UniTermNode::Type(TypeF::Arrow(..))` (there is actually a subtlety: see
/// the in-code documentation of the private symbol `FixTypeVars::fix_type_vars`, but let's ignore
/// it here). If, on the other hand, we enter the rule for an infix operator as in `a + 1`, `a` will
/// be converted to a `Term::Var` and the resulting uniterm will be
/// `UniTermNode::Term(Term::Op2(..))`.
pub enum UniTermNode<'ast> {
    /// A variable. Can refer both to a term variable or a type variable.
    Var(LocIdent),
    /// A record. Can refer both to a record literal or a record type.
    Record(UniRecord<'ast>),
    /// A uniterm that has been determined to be a term.
    Term(Ast<'ast>),
    /// A uniterm that has been determined to be a type.
    Type(Type<'ast>),
}

/// A uniterm with positional information.
pub struct UniTerm<'ast> {
    node: UniTermNode<'ast>,
    pos: TermPos,
}

impl<'ast> From<UniTermNode<'ast>> for UniTerm<'ast> {
    fn from(node: UniTermNode<'ast>) -> Self {
        UniTerm {
            node,
            pos: TermPos::None,
        }
    }
}

impl UniTerm<'_> {
    pub fn with_pos(mut self, pos: TermPos) -> Self {
        self.pos = pos;
        self
    }
}

// For nodes such as `Type` or `Record`, the following implementation has to choose between two
// positions to use: the one of the wrapping `UniTerm`, and the one stored inside the `RichTerm` or
// the `Type`. This implementation assumes that the latest set is the one of `UniTerm`, which is
// the single source of truth. In fact, it happens that only the outermost uniterm position is set
// while the innermost is still `TermPos::None`.
impl<'ast> TryConvert<'ast, UniTerm<'ast>> for Type<'ast> {
    type Error = ParseError;

    fn try_convert(alloc: &'ast AstAlloc, ut: UniTerm<'ast>) -> Result<Self, ParseError> {
        let pos = ut.pos;

        let typ = match ut.node {
            UniTermNode::Var(id) => TypeF::Var(id.ident()),
            UniTermNode::Record(r) => Type::try_convert(alloc, r)?.typ,
            UniTermNode::Type(ty) => ty.typ,
            UniTermNode::Term(ast) => {
                if matches!(
                    ast.node,
                    Node::Null
                        | Node::Bool(_)
                        | Node::Number(_)
                        | Node::String(_)
                        | Node::Array(_)
                        | Node::EnumVariant { .. }
                        | Node::StringChunks(_)
                ) {
                    //unwrap(): uniterms are supposed to come from the parser, and thus have a
                    //well-defined position
                    return Err(ParseError::InvalidContract(ut.pos.unwrap()));
                }

                TypeF::Contract(alloc.alloc(Ast {
                    node: ast.node,
                    pos,
                }))
            }
        };

        Ok(Type { typ, pos })
    }
}

impl<'ast> TryConvert<'ast, UniTerm<'ast>> for Ast<'ast> {
    type Error = ParseError;

    fn try_convert(alloc: &'ast AstAlloc, ut: UniTerm<'ast>) -> Result<Self, ParseError> {
        let UniTerm { node, pos } = ut;

        let node = match node {
            UniTermNode::Var(id) => Node::Var(id),
            UniTermNode::Record(r) => Ast::try_convert(alloc, r)?.node,
            UniTermNode::Type(typ) => {
                let typ = typ.fix_type_vars(alloc, pos.unwrap())?;

                if let TypeF::Contract(ctr) = typ.typ {
                    ctr.node.clone()
                } else {
                    alloc.typ(typ)
                }
            }
            UniTermNode::Term(ast) => ast.node,
        };

        Ok(Ast { node, pos })
    }
}

impl<'ast> From<Ast<'ast>> for UniTerm<'ast> {
    fn from(ast: Ast<'ast>) -> Self {
        let pos = ast.pos;

        UniTerm {
            node: UniTermNode::Term(ast),
            pos,
        }
    }
}

impl<'ast> From<Node<'ast>> for UniTerm<'ast> {
    fn from(node: Node<'ast>) -> Self {
        UniTerm {
            node: UniTermNode::Term(node.into()),
            pos: TermPos::None,
        }
    }
}

impl<'ast> From<Type<'ast>> for UniTerm<'ast> {
    fn from(ty: Type<'ast>) -> Self {
        let pos = ty.pos;
        UniTerm {
            node: UniTermNode::Type(ty),
            pos,
        }
    }
}

impl<'ast> From<UniRecord<'ast>> for UniTerm<'ast> {
    fn from(ur: UniRecord<'ast>) -> Self {
        let pos = ur.pos;

        UniTerm {
            node: UniTermNode::Record(ur),
            pos,
        }
    }
}

impl<T, U> TryConvert<'_, T> for U
where
    U: TryFrom<T>,
{
    type Error = U::Error;

    fn try_convert(_: &AstAlloc, from: T) -> Result<Self, Self::Error> {
        U::try_from(from)
    }
}

/// A record in the `UniTerm` syntax.
#[derive(Clone)]
pub struct UniRecord<'ast> {
    pub fields: Vec<FieldDef<'ast>>,
    pub tail: Option<(RecordRows<'ast>, TermPos)>,
    pub open: bool,
    pub pos: TermPos,
    /// The position of the final ellipsis `..`, if any. Used for error reporting. `pos_ellipsis`
    /// must be different from `TermPos::None` if and only if `attrs.open` is `true`.
    pub pos_ellipsis: TermPos,
}

impl<'ast> UniRecord<'ast> {
    /// Check if a field definition has a type annotation but no definition. This is currently
    /// forbidden for record literals that aren't record types. In that case, raise the
    /// corresponding parse error.
    pub fn check_typed_field_without_def(&self) -> Result<(), ParseError> {
        enum FieldState {
            // A field with a type annotation but without a definition was encountered. Still, we
            // might find a definition later, because of piecewise definitions
            Candidate((RawSpan, RawSpan)),
            // Marker to indicate that a field has been defined before, and can no longer raise an
            // error.
            Defined,
        }

        // We have to be a bit careful because of piecewise definitions. That is, we still want to
        // accept record literals such as:
        //
        // ```
        // {
        //   map : forall a b. (a -> b) -> Array a -> Array b,
        //   map = fun f array => ...
        // }
        // ```
        //
        // On the other hand, it's a bit too complex to handle the case of piecewise definitions:
        //
        // ```
        // {
        //    foo.bar.baz : Num,
        //    foo.bar.baz = 1,
        // }
        // ```
        //
        // This is arguably much less common and useful. In this case, we are more restrictive and
        // reject such an example, although it would theoretically be acceptable as it's elaborated
        // as a record literal that is accepted:
        //
        // ```
        // { foo = { bar = {baz : Num = 1 } } }
        // ```
        // We're using an index map because this map impacts the determinism of error reporting.
        let mut candidate_fields = IndexMap::new();

        let first_without_def = self.fields.iter().find_map(|field_def| {
            let path_as_ident = field_def.path_as_ident();

            match &field_def {
                FieldDef {
                    path: _,
                    value: None,
                    metadata:
                        FieldMetadata {
                            annotation: Annotation { typ: Some(typ), .. },
                            ..
                        },
                    ..
                } => {
                    // If the path is a single identifier, we don't error out right away, because
                    // we might already have found a definition for this field, or might do later
                    // in the loop.
                    if let Some(ident) = path_as_ident {
                        match candidate_fields.entry(ident.ident()) {
                            // If the hashmap is occupied, we've met this field before. Either
                            // there is another definition without annotation, in which case
                            // there's no need to replace it, or there is a `Defined` element,
                            // which means this is false positive that we can ignore. In both cases,
                            // we don't have anytning more to do
                            Entry::Occupied(_) => None,
                            Entry::Vacant(vacant_entry) => {
                                vacant_entry.insert(FieldState::Candidate((
                                    ident.pos.unwrap(),
                                    typ.pos.unwrap(),
                                )));
                                None
                            }
                        }
                    }
                    // We don't do anything smart for composite paths: we raise an error right way
                    else {
                        Some((field_def.pos.unwrap(), typ.pos.unwrap()))
                    }
                }
                field_def => {
                    if let (Some(ident), Some(_)) = (path_as_ident, &field_def.value) {
                        candidate_fields.insert(ident.ident(), FieldState::Defined);
                    }

                    None
                }
            }
        });

        let first_without_def =
            first_without_def.or(candidate_fields.into_iter().find_map(|(_, field_state)| {
                if let FieldState::Candidate(spans) = field_state {
                    Some(spans)
                } else {
                    None
                }
            }));

        if let Some((ident_span, annot_span)) = first_without_def {
            Err(ParseError::TypedFieldWithoutDefinition {
                field_span: ident_span,
                annot_span,
            })
        } else {
            Ok(())
        }
    }

    /// Checks if this record qualifies as a record type. If this function
    /// returns `true`, then [Self::into_type_strict] must succeed.
    pub fn is_record_type(&self) -> bool {
        self.fields.iter().all(|field_def| {
            // Field paths with a depth > 1 are not supported in record types.
            field_def.path.len() == 1
                // Warning: this pattern must stay in sync with the
                // corresponding pattern in `into_type_strict`.
                && matches!(&field_def,
                FieldDef {
                    value: None,
                    metadata:
                        FieldMetadata {
                            doc: None,
                            annotation:
                                Annotation {
                                    typ: Some(_),
                                    contracts,
                                },
                            opt: false,
                            not_exported: false,
                            priority: MergePriority::Neutral,
                        },
                    ..
                } if contracts.is_empty())
        })
    }

    /// Turns this record into a plain record type, uniquely containing fields of the form `fields:
    /// Type`. Currently, this doesn't support the field path syntax: `{foo.bar.baz :
    /// Type}.into_type_strict()` returns an `Err`.
    pub fn into_type_strict(
        self,
        alloc: &'ast AstAlloc,
    ) -> Result<Type<'ast>, InvalidRecordTypeError> {
        fn term_to_record_rows<'ast>(
            alloc: &'ast AstAlloc,
            id: LocIdent,
            field_def: FieldDef<'ast>,
            tail: RecordRows<'ast>,
        ) -> Result<RecordRows<'ast>, InvalidRecordTypeError> {
            match field_def {
                // Warning: this pattern must stay in sync with the corresponding pattern in
                // `is_record_type`.
                FieldDef {
                    path: _,
                    value: None,
                    metadata:
                        FieldMetadata {
                            doc: None,
                            annotation:
                                Annotation {
                                    typ: Some(typ),
                                    contracts: [],
                                },
                            opt: false,
                            not_exported: false,
                            priority: MergePriority::Neutral,
                        },
                    pos: _,
                } => Ok(RecordRows(RecordRowsF::Extend {
                    row: RecordRow {
                        id,
                        typ: alloc.type_data(typ.typ, typ.pos),
                    },
                    tail: alloc.record_rows(tail.0),
                })),
                _ => {
                    Err(InvalidRecordTypeError::InvalidField(
                        // Position of identifiers must always be set at this stage (parsing)
                        id.pos.fuse(field_def.pos).unwrap(),
                    ))
                }
            }
        }

        // An open record (with an ellipsis `..` at the end) can't be translated to a record type.
        // `pos_ellipsis` should be set iff `attrs.open` is true.
        debug_assert!((self.pos_ellipsis == TermPos::None) != self.open);

        if let Some(raw_span) = self.pos_ellipsis.into_opt() {
            return Err(InvalidRecordTypeError::IsOpen(raw_span));
        }

        // Track the field names we've seen, to check for duplicates.
        let mut fields_seen = HashMap::new();

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
                |acc: RecordRows, field_def| {
                    // We don't support compound paths for types, yet.
                    // All positions can be unwrapped because we're still parsing.
                    if field_def.path.len() > 1 {
                        let span = field_def
                            .path
                            .iter()
                            .map(|path_elem| path_elem.pos().unwrap())
                            .reduce(|acc, span| acc.fuse(span).unwrap_or(acc))
                            // We already checked that the path is non-empty.
                            .unwrap();

                        Err(InvalidRecordTypeError::InvalidField(span))
                    } else {
                        let elem = field_def.path.last().unwrap();

                        let id = match elem {
                            FieldPathElem::Ident(id) => *id,
                            FieldPathElem::Expr(expr) => {
                                let pos = expr.pos;
                                let name = expr.node.try_str_chunk_as_static_str().ok_or(
                                    InvalidRecordTypeError::InterpolatedField(
                                        field_def.pos.unwrap(),
                                    ),
                                )?;
                                LocIdent::new_with_pos(name, pos)
                            }
                        };
                        if let Some(prev_id) = fields_seen.insert(id.ident(), id) {
                            return Err(InvalidRecordTypeError::RepeatedField {
                                // Because we're iterating backwards, `id` came first.
                                orig: id.pos.unwrap(),
                                dup: prev_id.pos.unwrap(),
                            });
                        }

                        term_to_record_rows(alloc, id, field_def, acc)
                    }
                },
            )?;
        Ok(Type {
            typ: TypeF::Record(rrows),
            pos: self.pos,
        })
    }

    pub fn with_pos(mut self, pos: TermPos) -> Self {
        self.pos = pos;
        self
    }
}

impl<'ast> TryConvert<'ast, UniRecord<'ast>> for Ast<'ast> {
    type Error = ParseError;

    /// Convert a `UniRecord` to a term. If the `UniRecord` is syntactically a record type or it
    /// has a tail, it is first interpreted as a type and then wrapped in a `Term::Type`. One
    /// exception is the empty record, which behaves the same both as a type and a contract, and
    /// turning an empty record literal to an opaque contract would break everything, so the empty
    /// record is always interpreted as a term directly.
    ///
    /// If the unirecord isn't a record type and doesn't have a tail, it is interpreted as an
    /// equivalent record term. Fail if the `UniRecord` has a tail but isn't syntactically a record
    /// type either. Elaborate field paths `foo.bar = value` to the expanded form `{foo = {bar =
    /// value}}`.
    ///
    /// We also fix the type variables of the type appearing inside annotations (see in-code
    /// documentation of the private symbol `FixTypeVars::fix_type_vars`).
    fn try_convert(alloc: &'ast AstAlloc, ur: UniRecord<'ast>) -> Result<Self, ParseError> {
        let pos = ur.pos;

        // First try to interpret this record as a type.
        if ur.tail.is_some() || (ur.is_record_type() && !ur.fields.is_empty()) {
            let tail_span = ur.tail.as_ref().and_then(|t| t.1.into_opt());
            // We unwrap all positions: at this stage of the parsing, they must all be set
            let typ =
                ur.into_type_strict(alloc)
                    .map_err(|cause| ParseError::InvalidRecordType {
                        tail_span,
                        record_span: pos.unwrap(),
                        cause,
                    })?;

            let typ = typ.fix_type_vars(alloc, pos.unwrap())?;

            Ok(alloc.typ(typ).spanned(pos))
        } else {
            ur.check_typed_field_without_def()?;

            let UniRecord { fields, open, .. } = ur;

            let field_defs_fixed = fields
                .into_iter()
                .map(|field_def| {
                    Ok(FieldDef {
                        metadata: fix_field_types(
                            alloc,
                            field_def.metadata,
                            field_def.pos.unwrap(),
                        )?,
                        ..field_def
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(alloc
                .record(ast::record::Record {
                    field_defs: alloc.alloc_many(field_defs_fixed),
                    open,
                })
                .spanned(pos))
        }
    }
}

/// Try to convert a `UniRecord` to a type. The strict part means that the `UniRecord` must be
impl<'ast> TryConvert<'ast, UniRecord<'ast>> for Type<'ast> {
    type Error = ParseError;

    /// Convert a `UniRecord` to a type. If the `UniRecord` has a tail, it is interpreted strictly
    /// as a type and fail if it isn't a plain record type. Otherwise, we first try to interpret it
    /// as a plain record type, and if that doesn't work, we interpret it as a term and wrap it
    /// back as a user-defined contract.
    fn try_convert(alloc: &'ast AstAlloc, ur: UniRecord<'ast>) -> Result<Self, ParseError> {
        let pos = ur.pos;

        if let Some((_, tail_pos)) = ur.tail {
            ur.into_type_strict(alloc)
                .map_err(|cause| ParseError::InvalidRecordType {
                    tail_span: tail_pos.into_opt(),
                    record_span: pos.unwrap(),
                    cause,
                })
        } else {
            let pos = ur.pos;
            ur.clone().into_type_strict(alloc).or_else(|_| {
                Ast::try_convert(alloc, ur).map(|ast| Type {
                    typ: TypeF::Contract(alloc.alloc(ast)),
                    pos,
                })
            })
        }
    }
}

/// Cell providing shared mutable access to a var_kind. This is used to decide the kind of a
/// variable associated to a forall in the `fix_type_vars` phase.
///
/// This cell provides interior mutability for [`VarKind`]. It makes it possible to mutate the
/// inner data when put in an environment, which only provides immutable references to its values.
#[derive(PartialEq, Eq)]
pub(super) struct VarKindCell(RefCell<Option<VarKind>>);

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
    /// Create a new unset `VarKindCell` at resolution time, this will default to `VarKind::Type`,
    pub(super) fn new() -> Self {
        VarKindCell(RefCell::new(None))
    }

    /// Everywhere a forall variable is used it must be of the same type. If this is the first time
    /// we encounter the variable, we can set it freely. If it has been set, and is of the same
    /// type, we only need to combine `excluded` record row variables. If it has been set to a
    /// different `VarKind`, we return `Err(_)`.
    pub(super) fn try_set(&self, var_kind: VarKind) -> Result<(), VarKindMismatch> {
        match (&mut *self.0.borrow_mut(), var_kind) {
            (s @ None, var_kind) => {
                *s = Some(var_kind);
                Ok(())
            }
            (Some(data), var_kind) if data == &var_kind => Ok(()),
            (
                Some(VarKind::RecordRows {
                    excluded: ref mut ex1,
                }),
                VarKind::RecordRows { excluded: ex2 },
            ) => {
                ex1.extend(ex2);
                Ok(())
            }
            (
                Some(VarKind::EnumRows {
                    excluded: ref mut ex1,
                }),
                VarKind::EnumRows { excluded: ex2 },
            ) => {
                ex1.extend(ex2);
                Ok(())
            }
            _ => Err(VarKindMismatch),
        }
    }

    /// Return a clone of the current var_kind.
    #[allow(dead_code)]
    pub fn var_kind(&self) -> Option<VarKind> {
        self.0.borrow().clone()
    }

    /// Return the inner var_kind, leaving `Nothing` behind in the `VarKindCell`.
    pub fn take_var_kind(&self) -> Option<VarKind> {
        self.0.borrow_mut().take()
    }
}

pub(super) trait FixTypeVars<'ast>
where
    Self: Sized,
{
    /// Post-process a type at the right hand side of an annotation by replacing each unbound type
    /// variable `TypeF::Var(id)` by a term variable with the same identifier seen as a custom
    /// contract `TypeF::Contract(Node::Var(id))`.
    ///
    /// Additionally, this passes determine the kind of a variable introduced by a forall binder.
    ///
    /// # Type variable fixing
    ///
    /// Since parsing is done bottom-up, and given the specification of the uniterm syntax for
    /// variables occurring in types, we often can't know right away if such a variable occurrence
    /// will eventually be a type variable or a term variable seen as a custom contract.
    ///
    /// Take for example `a -> b`. At this stage, `a` and `b` could be both variables referring to
    /// a contract (e.g. in `x | a -> b`) or type variables (e.g. in `x | forall a b. a -> b`),
    /// depending on enclosing `forall`s. To handle both cases, we initially parse all variables
    /// inside types as type variables. When reaching the right-hand side of an annotation, because
    /// `forall`s can only bind locally in a type, we can then decide the actual nature of each
    /// occurrence. We thus recurse into the newly constructed type to change those type variables
    /// that are not actually bound by a `forall` to be term variables. This is the role of
    /// `fix_type_vars()`.
    ///
    /// Since `forall`s only bind type variables locally and cross contract boundaries, we don't
    /// have to recurse into contracts and this pass will only visit each node of the AST at most
    /// once in total (and most probably much less so). In some sense, we just visit the type
    /// layer, or type spine, composed only of type constructors.
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
    /// there is indeed an enclosing forall binding `a`. With unirecords, before fixing type
    /// variables, we have to wait until we eventually convert the unirecord to a term (in which
    /// case we fix all the top-level annotations) or a type (in which case we do nothing: the
    /// enclosing type will trigger the fix once it's fully constructed). Fixing a unirecord prior
    /// to a conversion to a term is done by [`fix_field_types`].
    ///
    /// # Variable kind
    ///
    /// Type variables can have different kind (cf [crate::typ::VarKind]). The kind determines if
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
    /// forall a. [| 'foo, 'bar; a |] -> {foo : Str, bar: Str; a}
    /// ```
    fn fix_type_vars(self, alloc: &'ast AstAlloc, span: RawSpan) -> Result<Self, ParseError> {
        Ok(self
            .fix_type_vars_env(alloc, BoundVarEnv::new(), span)?
            .unwrap_or(self))
    }

    /// Same as [Self::fix_type_vars], but takes `self` as a reference instead, and returns
    /// `Ok(None)` when `self` hasn't been modified by the type fixing phase or
    /// `Ok(Some(new_self))` with a modified, owned `self` upon change.
    fn fix_type_vars_ref(
        &self,
        alloc: &'ast AstAlloc,
        span: RawSpan,
    ) -> Result<Option<Self>, ParseError> {
        self.fix_type_vars_env(alloc, BoundVarEnv::new(), span)
    }

    /// Fix type vars in a given environment of variables bound by foralls enclosing this type. The
    /// environment maps bound variables to a reference to the variable kind of the corresponding
    /// forall.
    ///
    /// # Ownership
    ///
    /// [Self::fix_type_vars_env] might need to be called both on owned data and on immutably
    /// borrowed data (e.g. [`Type`][crate::bytecode::ast::typ::Type] and [`&'ast
    /// Type`][crate::bytecode::ast::typ::Type]). We don't want to duplicate the logic of
    /// [Self::fix_type_vars_env] for both, as we can't write one that is generic enough while
    /// properly avoiding useless allocations.
    ///
    /// The idea of the current API is that even when operating on owned data, `self` is taken by
    /// reference. If `self` isn't modified by the fix type phase, then `None` is returned and the
    /// caller can just reuse the original `self` how they please.
    ///
    /// If `self` has been modified by the fix type phase, then `Some(new_value)` is returned with
    /// a new owned version of `self`. If the caller needed an owned version, the job is done.
    /// Otherwise, the caller can use [the ast allocator `alloc`][crate::bytecode::ast::AstAlloc]
    /// to move the owned data into the allocator and get an `&'ast` reference out of it. The only
    /// cost is that for owned data, we could have reused the original `self` instead of returning
    /// a new one, but this is a detail: in practice, only the top-level call of `fix_type_vars` is
    /// performed on owned data, and the recursive calls are all performed on `&'ast` references.
    /// At worse, we waste the top-level node, which is stack-allocated anyway.
    ///
    /// Because AST nodes are allocated in an arena and are immutable, they won't be reclaimed
    /// until the whole AST is finally transformed to either the mainline AST or (in the future)
    /// compiled to bytecode. We want to avoid building useless copies of existing nodes, which is
    /// the reason behind not using a simpler strategy of just always returning a new value, that
    /// might be identical to the old one if no type variable has been fixed.
    fn fix_type_vars_env(
        &self,
        alloc: &'ast AstAlloc,
        bound_vars: BoundVarEnv,
        span: RawSpan,
    ) -> Result<Option<Self>, ParseError>;
}

impl<'ast> FixTypeVars<'ast> for Type<'ast> {
    fn fix_type_vars_env(
        &self,
        alloc: &'ast AstAlloc,
        mut bound_vars: BoundVarEnv,
        span: RawSpan,
    ) -> Result<Option<Self>, ParseError> {
        use crate::bytecode::ast::typ::TypeUnr;

        let pos = self.pos;

        let build_fixed = |new_type: TypeUnr<'ast>| -> Self { Type { typ: new_type, pos } };

        match self.typ {
            TypeF::Dyn
            | TypeF::Number
            | TypeF::Bool
            | TypeF::String
            | TypeF::ForeignId
            | TypeF::Symbol
            | TypeF::Contract(_)
            // We don't fix type variables inside a dictionary contract. A dictionary contract
            // should not be considered as a static type, but instead work as a contract. In
            // particular we forbid capturing type variables from the enclosing type: see
            // https://github.com/tweag/nickel/issues/1228.
            | TypeF::Dict { flavour: DictTypeFlavour::Contract, ..}
            | TypeF::Wildcard(_) => Ok(None),
            TypeF::Arrow(src, tgt) => {
                let src_result = src.fix_type_vars_env(alloc, bound_vars.clone(), span)?;
                let tgt_result = tgt.fix_type_vars_env(alloc, bound_vars, span)?;

                if src_result.is_some() || tgt_result.is_some() {
                    let src = src_result.map(|ty| alloc.alloc(ty)).unwrap_or(src);
                    let tgt = tgt_result.map(|ty| alloc.alloc(ty)).unwrap_or(tgt);

                    Ok(Some(build_fixed(TypeF::Arrow(src, tgt))))
                }
                else {
                    Ok(None)
                }
            }
            TypeF::Var(sym) => {
                if let Some(cell) = bound_vars.get(&sym) {
                    cell.try_set(VarKind::Type)
                        .map_err(|_| ParseError::TypeVariableKindMismatch {
                            ty_var: LocIdent::from(sym).with_pos(self.pos),
                            span
                        })?;

                    Ok(None)
                } else {
                    let id = LocIdent::from(sym).with_pos(self.pos);

                    Ok(Some(build_fixed(TypeF::Contract(alloc.alloc(Ast {
                        node: Node::Var(id),
                        pos: id.pos,
                    })))))
                }
            }
            TypeF::Forall {
                var,
                var_kind: ref prev_var_kind,
                body,
            } => {
                // We span a new `VarKindCell` and put it in the environment. The recursive calls
                // to `fix_type_vars` will fill this cell with the correct kind, which we get
                // afterwards to set the right value for `var_kind`.
                bound_vars.insert(var.ident(), VarKindCell::new());
                let body_fixed = body.fix_type_vars_env(alloc, bound_vars.clone(), span)?;

                // unwrap(): we just inserted a value for `var` above, and environment can never
                // delete values.
                // take_var_kind(): once we leave the body of this forall, we no longer need
                // access to this VarKindCell in bound_vars. We can avoid a clone by taking
                // the var_kind out. We could also take the whole key value pair out of the
                // `Environment`, but ownership there is trickier.
                let var_kind = bound_vars
                    .get(&var.ident())
                    .unwrap()
                    .take_var_kind()
                    .unwrap_or_default();

                // By default, the parser sets `var_kind` to `Type`. If the `var_kind` turns out to
                // actually be `Type`, and the body hasn' changed, we can avoid any cloning and
                // return `Ok(None)`. Otherwise, we have to build a new `TypeF::Forall`. We still
                // want to defend against callers that wouldn't follow this convention (that
                // `prev_var_kind` is necessarily `Type` before fixing), so we still check it.
                if body_fixed.is_some() || !matches!((&var_kind, &prev_var_kind), (&VarKind::Type, &VarKind::Type)) {
                    let body = body_fixed.map(|body| alloc.alloc(body)).unwrap_or(body);

                    Ok(Some(build_fixed(TypeF::Forall {
                        var,
                        var_kind,
                        body,
                    })))
                } else {
                    Ok(None)
                }
            }
            TypeF::Dict {
                type_fields,
                flavour: flavour @ DictTypeFlavour::Type
            } => {
                Ok(type_fields.fix_type_vars_env(alloc, bound_vars, span)?.map(|ty| {
                    build_fixed(TypeF::Dict {
                        type_fields: alloc.alloc(ty),
                        flavour,
                    })
                }))
            }
            TypeF::Array(ty) => {
                Ok(ty.fix_type_vars_env(alloc, bound_vars, span)?.map(|ty|
                    build_fixed(TypeF::Array(alloc.alloc(ty)))))
            }
            TypeF::Enum(ref erows) => {
                Ok(erows.fix_type_vars_env(alloc, bound_vars, span)?.map(|erows|
                    build_fixed(TypeF::Enum(erows))
                ))
            }
            TypeF::Record(ref rrows) => {
                Ok(rrows.fix_type_vars_env(alloc, bound_vars, span)?.map(|rrows|
                    build_fixed(TypeF::Record(rrows))
                ))
            }
        }
    }
}

impl<'ast> FixTypeVars<'ast> for RecordRows<'ast> {
    fn fix_type_vars_env(
        &self,
        alloc: &'ast AstAlloc,
        bound_vars: BoundVarEnv,
        span: RawSpan,
    ) -> Result<Option<Self>, ParseError> {
        fn do_fix<'ast>(
            rrows: &RecordRows<'ast>,
            alloc: &'ast AstAlloc,
            bound_vars: BoundVarEnv,
            span: RawSpan,
            mut maybe_excluded: HashSet<Ident>,
        ) -> Result<Option<RecordRows<'ast>>, ParseError> {
            match rrows.0 {
                RecordRowsF::Empty | RecordRowsF::TailDyn => Ok(None),
                // We can't have a contract in tail position, so we don't fix `TailVar`. However, we
                // have to set the correct kind for the corresponding forall binder.
                RecordRowsF::TailVar(id) => {
                    if let Some(cell) = bound_vars.get(&id.ident()) {
                        cell.try_set(VarKind::RecordRows {
                            excluded: maybe_excluded,
                        })
                        .map_err(|_| ParseError::TypeVariableKindMismatch { ty_var: id, span })?;
                    }

                    Ok(None)
                }
                RecordRowsF::Extend { ref row, tail } => {
                    maybe_excluded.insert(row.id.ident());

                    let row_fixed = row.fix_type_vars_env(alloc, bound_vars.clone(), span)?;
                    let tail_fixed = do_fix(tail, alloc, bound_vars, span, maybe_excluded)?;

                    if row_fixed.is_some() || tail_fixed.is_some() {
                        let row = row_fixed.unwrap_or_else(|| row.clone());
                        let tail = tail_fixed
                            .map(|tail_fixed| alloc.alloc(tail_fixed))
                            .unwrap_or(tail);

                        Ok(Some(RecordRows(RecordRowsF::Extend { row, tail })))
                    } else {
                        Ok(None)
                    }
                }
            }
        }

        do_fix(self, alloc, bound_vars, span, HashSet::new())
    }
}

impl<'ast> FixTypeVars<'ast> for RecordRow<'ast> {
    fn fix_type_vars_env(
        &self,
        alloc: &'ast AstAlloc,
        bound_vars: BoundVarEnv,
        span: RawSpan,
    ) -> Result<Option<Self>, ParseError> {
        Ok(self
            .typ
            .fix_type_vars_env(alloc, bound_vars, span)?
            .map(|typ| RecordRow {
                id: self.id,
                typ: alloc.alloc(typ),
            }))
    }
}

impl<'ast> FixTypeVars<'ast> for EnumRows<'ast> {
    fn fix_type_vars_env(
        &self,
        alloc: &'ast AstAlloc,
        bound_vars: BoundVarEnv,
        span: RawSpan,
    ) -> Result<Option<Self>, ParseError> {
        fn do_fix<'ast>(
            erows: &EnumRows<'ast>,
            alloc: &'ast AstAlloc,
            bound_vars: BoundVarEnv,
            span: RawSpan,
            mut maybe_excluded: HashSet<Ident>,
        ) -> Result<Option<EnumRows<'ast>>, ParseError> {
            match erows.0 {
                EnumRowsF::Empty => Ok(None),
                // We can't have a contract in tail position, so we don't fix `TailVar` itself.
                // However, we have to set the correct kind for the corresponding forall binder.
                EnumRowsF::TailVar(id) => {
                    if let Some(cell) = bound_vars.get(&id.ident()) {
                        cell.try_set(VarKind::EnumRows {
                            excluded: maybe_excluded,
                        })
                        .map_err(|_| ParseError::TypeVariableKindMismatch { ty_var: id, span })?;
                    }

                    Ok(None)
                }
                EnumRowsF::Extend { ref row, tail } => {
                    // Enum tags (when `typ` is `None`) can't create a conflict, so we ignore them
                    // for constraints. See the documentation of `typecheck::unif::RowConstrs`.
                    if row.typ.is_some() {
                        maybe_excluded.insert(row.id.ident());
                    }

                    let row_fixed = row.fix_type_vars_env(alloc, bound_vars.clone(), span)?;
                    let tail_fixed = do_fix(tail, alloc, bound_vars, span, maybe_excluded)?;

                    if row_fixed.is_some() || tail_fixed.is_some() {
                        let row = row_fixed.unwrap_or_else(|| row.clone());
                        let tail = tail_fixed
                            .map(|tail_fixed| alloc.alloc(tail_fixed))
                            .unwrap_or(tail);

                        Ok(Some(EnumRows(EnumRowsF::Extend { row, tail })))
                    } else {
                        Ok(None)
                    }
                }
            }
        }

        do_fix(self, alloc, bound_vars, span, HashSet::new())
    }
}

impl<'ast> FixTypeVars<'ast> for EnumRow<'ast> {
    fn fix_type_vars_env(
        &self,
        alloc: &'ast AstAlloc,
        bound_vars: BoundVarEnv,
        span: RawSpan,
    ) -> Result<Option<Self>, ParseError> {
        // `maybe_fixed` is `Some(ty)` if and only if this enum rows has an associated
        // type *and* the type has been changed by fixing.
        let maybe_fixed = self
            .typ
            .as_ref()
            .map(|ty| {
                // Enum tags (when `typ` is `None`) can't create a conflict, so we ignore them
                // for constraints. See the documentation of `typecheck::unif::RowConstrs`.
                ty.fix_type_vars_env(alloc, bound_vars.clone(), span)
            })
            .transpose()?
            .flatten();

        Ok(maybe_fixed.map(|typ| EnumRow {
            id: self.id,
            typ: Some(alloc.alloc(typ)),
        }))
    }
}

/// Fix the type variables of types appearing as annotations of record fields. See the in-code
/// documentation of the private symbol `Types::fix_type_vars`.
pub fn fix_field_types<'ast>(
    alloc: &'ast AstAlloc,
    metadata: FieldMetadata<'ast>,
    span: RawSpan,
) -> Result<FieldMetadata<'ast>, ParseError> {
    use std::borrow::Cow;

    let typ = metadata
        .annotation
        .typ
        .map(|typ| typ.fix_type_vars(alloc, span))
        .transpose()?;

    let contracts: Result<Vec<Cow<'ast, _>>, ParseError> = metadata
        .annotation
        .contracts
        .iter()
        .map(|ctr| {
            Ok(ctr
                .fix_type_vars_ref(alloc, span)?
                .map(Cow::Owned)
                .unwrap_or(Cow::Borrowed(ctr)))
        })
        .collect();
    let contracts = contracts?;

    // If none of the contracts have been changed, we can keep the original `[Type]` allocation.
    let contracts = if contracts.iter().all(|cow| matches!(cow, Cow::Borrowed(_))) {
        metadata.annotation.contracts
    } else {
        alloc.alloc_many(contracts.into_iter().map(|cow| cow.into_owned()))
    };

    Ok(FieldMetadata {
        annotation: Annotation { typ, contracts },
        ..metadata
    })
}
