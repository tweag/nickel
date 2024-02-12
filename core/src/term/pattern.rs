//! Pattern matching and destructuring of Nickel values.
use std::collections::{hash_map::Entry, HashMap};

use crate::{
    error::EvalError,
    identifier::LocIdent,
    label::Label,
    mk_app,
    parser::error::ParseError,
    position::{RawSpan, TermPos},
    stdlib::internals,
    term::{
        record::{Field, RecordAttrs, RecordData},
        LabeledType, Term,
    },
    typ::{Type, TypeF},
};

#[derive(Debug, PartialEq, Clone)]
pub enum PatternData {
    /// A simple pattern consisting of an identifier. Match anything and bind the result to the
    /// corresponding identfier.
    Any(LocIdent),
    /// A record pattern as in `{ a = { b, c } }`
    Record(RecordPattern),
    /// An enum pattern as in `'Foo x` or `'Foo`
    Enum(EnumPattern),
}

/// A generic pattern, that can appear in a match expression (not yet implemented) or in a
/// destructuring let-binding.
#[derive(Debug, PartialEq, Clone)]
pub struct Pattern {
    /// The content of this pattern
    pub data: PatternData,
    /// A potential alias for this pattern, capturing the whole matched value. In the source
    /// language, an alias is introduced by `x @ <pattern>`, where `x` is an arbitrary identifier.
    pub alias: Option<LocIdent>,
    /// The span of the pattern in the source.
    pub span: RawSpan,
}

/// An enum pattern, including both an enum tag and an enum variant.
#[derive(Debug, PartialEq, Clone)]
pub struct EnumPattern {
    pub tag: LocIdent,
    pub pattern: Option<Box<Pattern>>,
    pub span: RawSpan,
}

/// A field pattern inside a record pattern. Every field can be annotated with a type, contracts or
/// with a default value.
#[derive(Debug, PartialEq, Clone)]
pub struct FieldPattern {
    /// The name of the matched field. For example, in `{..., foo = {bar, baz}, ...}`, the matched
    /// identifier is `foo`.
    pub matched_id: LocIdent,
    /// Potential extra annotations of this field pattern, such as a type annotation, contract
    /// annotations, or a default value, represented as record field.
    pub extra: Field,
    /// The pattern on the right-hand side of the `=`. A pattern like `{foo, bar}`, without the `=`
    /// sign, is parsed as `{foo=foo, bar=bar}`. In this case, `pattern.data` will be
    /// [PatternData::Any].
    pub pattern: Pattern,
    pub span: RawSpan,
}

/// The last match in a data structure pattern. This can either be a normal match, or an ellipsis
/// which can capture the rest of the data structure. The type parameter `P` is the type of the
/// pattern of the data structure: currently, ellipsis matches are only supported for record, but
/// we'll probably support them for arrays as well.
///
/// This enum is mostly used during parsing.
///
/// # Example
///
/// - In `{foo={}, bar}`, the last match is an normal match.
/// - In `{foo={}, bar, ..}`, the last match is a non-capturing ellipsis.
/// - In `{foo={}, bar, ..rest}`, the last match is a capturing ellipsis.
#[derive(Debug, PartialEq, Clone)]
pub enum LastPattern<P> {
    /// The last field is a normal match. In this case the pattern is "closed" so every record
    /// fields should be matched.
    Normal(Box<P>),
    /// The pattern is "open" `, ..}`. Optionally you can bind a record containing the remaining
    /// fields to an `Identifier` using the syntax `, ..y}`.
    Ellipsis(Option<LocIdent>),
}

/// A record pattern.
#[derive(Debug, PartialEq, Clone)]
pub struct RecordPattern {
    /// The patterns for each field in the record.
    pub patterns: Vec<FieldPattern>,
    /// The tail of the pattern, indicating if the pattern is open, i.e. if it ended with an
    /// ellipsis, capturing the rest or not.
    pub tail: RecordPatternTail,
    pub span: RawSpan,
}

/// The tail of a record pattern which might capture the rest of the record.
#[derive(Debug, PartialEq, Clone)]
pub enum RecordPatternTail {
    /// The pattern is closed, i.e. it doesn't allow more fields. For example, `{foo, bar}`.
    Empty,
    /// The pattern ends with an ellipsis, making it open. For example, `{foo, bar, ..}`.
    Open,
    /// The pattern ends with an ellispis and a variable capturing the rest of the record. For
    /// example, `{foo, bar, ..rest}`.
    Capture(LocIdent),
}

impl RecordPattern {
    /// Check the matches for duplication, and raise an error if any occur.
    ///
    /// Note that for backwards-compatibility reasons this function _only_
    /// checks top-level matches. In Nickel 1.0, this code panicked:
    ///
    /// ```text
    /// let f = fun { x, x, .. } => x in f { x = 1 }
    /// ```
    ///
    /// However this "works", even though the binding to `y` is duplicated.
    ///
    /// ```text
    /// let f =
    ///   fun { x = { y }, z = { y }, .. } => y
    /// in f { x = { y = 1 }, z = { y = 2 } }
    /// # evaluates to 1
    /// ```
    ///
    /// This function aims to raise errors in the first case, but maintain the
    /// behaviour in the second case.
    pub fn check_dup(&self) -> Result<(), ParseError> {
        let mut bindings = HashMap::new();

        for pat in self.patterns.iter() {
            let binding = pat.matched_id;
            let label = binding.label().to_owned();
            match bindings.entry(label) {
                Entry::Occupied(occupied_entry) => {
                    return Err(ParseError::DuplicateIdentInRecordPattern {
                        ident: binding,
                        prev_ident: occupied_entry.remove_entry().1,
                    })
                }
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(binding);
                }
            }
        }

        Ok(())
    }

    /// Check if this record contract is open, meaning that it accepts additional fields to be
    /// present, whether the rest is captured or not.
    pub fn is_open(&self) -> bool {
        matches!(
            self.tail,
            RecordPatternTail::Open | RecordPatternTail::Capture(_)
        )
    }
}

impl FieldPattern {
    /// Convert this field pattern to a record field binding with metadata. Used to generate the
    /// record contract associated to a record pattern.
    pub fn as_record_binding(&self) -> (LocIdent, Field) {
        let mut decoration = self.extra.clone();

        // If the inner pattern gives rise to a contract, add it the to the field decoration.
        decoration
            .metadata
            .annotation
            .contracts
            .extend(self.pattern.elaborate_contract());

        (self.matched_id, decoration)
    }
}

// We don't implement elaborate_contract for `FieldPattern`, which is of a slightly different
// nature (it's a part of a record pattern). Instead, we call to `FieldPattern::as_record_binding`,
// which takes care of elaborating a field pattern to an appropriate record field.
pub trait ElaborateContract {
    /// Elaborate a contract from this pattern. The contract will check both the structure of the
    /// matched value (e.g. the presence of fields in a record) and incoporate user-provided
    /// contracts and annotations, as well as default values.
    ///
    /// Some patterns don't give rise to any contract (e.g. `Any`), in which case this function
    /// returns `None`.
    fn elaborate_contract(&self) -> Option<LabeledType>;
}

impl ElaborateContract for PatternData {
    fn elaborate_contract(&self) -> Option<LabeledType> {
        match self {
            PatternData::Any(_) => None,
            PatternData::Record(pat) => pat.elaborate_contract(),
            PatternData::Enum(pat) => pat.elaborate_contract(),
        }
    }
}

impl ElaborateContract for Pattern {
    fn elaborate_contract(&self) -> Option<LabeledType> {
        self.data.elaborate_contract()
    }
}

impl ElaborateContract for EnumPattern {
    fn elaborate_contract(&self) -> Option<LabeledType> {
        let pos = TermPos::Original(self.span);

        // TODO[adts]: it would be better to simply build a type like `[| 'tag arg |]` or `[| 'tag
        // |]` and to rely on its derived contract. However, for the time being, the contract
        // derived from enum variants isn't implemented yet.
        let contract = if self.pattern.is_some() {
            mk_app!(internals::enum_variant(), Term::Enum(self.tag))
        } else {
            mk_app!(internals::stdlib_contract_equal(), Term::Enum(self.tag))
        };

        let typ = Type {
            typ: TypeF::Flat(contract),
            pos,
        };

        Some(LabeledType {
            typ: typ.clone(),
            label: Label {
                typ: typ.into(),
                span: self.span,
                ..Default::default()
            },
        })
    }
}

impl ElaborateContract for RecordPattern {
    fn elaborate_contract(&self) -> Option<LabeledType> {
        let pos = TermPos::Original(self.span);

        let typ = Type {
            typ: TypeF::Flat(
                Term::Record(RecordData::new(
                    self.patterns
                        .iter()
                        .map(FieldPattern::as_record_binding)
                        .collect(),
                    RecordAttrs {
                        open: self.is_open(),
                        ..Default::default()
                    },
                    None,
                ))
                .into(),
            ),
            pos,
        };

        Some(LabeledType {
            typ: typ.clone(),
            label: Label {
                typ: typ.into(),
                span: self.span,
                ..Default::default()
            },
        })
    }
}

/// Compilation of pattern matching down to pattern-less Nickel code.
pub mod compile {
    use super::*;
    use crate::{
        mk_app,
        term::{make, BinaryOp, MatchData, RecordExtKind, RecordOpKind, RichTerm, Term, UnaryOp},
    };

    // Generate a `%record_insert% "<id>" value_id bindings_id` primop application.
    fn insert_binding(id: LocIdent, value_id: LocIdent, bindings_id: LocIdent) -> RichTerm {
        mk_app!(
            make::op2(
                BinaryOp::DynExtend {
                    ext_kind: RecordExtKind::WithValue,
                    metadata: Default::default(),
                    pending_contracts: Default::default(),
                    op_kind: RecordOpKind::IgnoreEmptyOpt,
                },
                Term::Str(id.label().into()),
                Term::Var(bindings_id)
            ),
            Term::Var(value_id)
        )
    }

    pub trait CompilePart {
        /// Compile part of a broader pattern to a Nickel expression with two free variables (which
        /// is equivalent to a function of two arguments):
        ///
        /// 1. The value being matched on (`value_id`)
        /// 2. A dictionary of the current assignment of pattern variables to sub-expressions of the
        ///    matched expression
        ///
        /// The compiled expression must return either `null` if the pattern doesn't match, or a
        /// dictionary mapping pattern variables to the corresponding sub-expressions of the
        /// matched value if the pattern matched with success.
        ///
        /// Although the `value` and `bindings` could be passed as [crate::term::RichTerm] in all
        /// generality, forcing them to be variable makes it less likely that the compilation
        /// duplicates sub-expressions: because the value and the bindings must always be passed in
        /// a variable, they are free to share without risk of duplicating work.
        fn compile_part(&self, value_id: LocIdent, bindings_id: LocIdent) -> RichTerm;
    }

    impl CompilePart for Pattern {
        // Compilation of the top-level pattern wrapper (code between < and > is Rust code, think
        // of it as a kind of templating):
        //
        // < if let Some(alias) = alias { >
        //   let bindings = %record_insert% <alias> bindings arg in
        // < } >
        // <pattern_data.compile()> arg bindings
        fn compile_part(&self, value_id: LocIdent, bindings_id: LocIdent) -> RichTerm {
            // The last instruction
            // <pattern_data.compile()>
            let continuation = self.data.compile_part(value_id, bindings_id);

            // Either
            //
            // let bindings = %record_insert% <alias> bindings arg in
            // continuation
            //
            // if `alias` is set, or just `continuation` otherwise.
            if let Some(alias) = self.alias {
                make::let_in(
                    bindings_id,
                    insert_binding(alias, value_id, bindings_id),
                    continuation,
                )
            } else {
                continuation
            }
        }
    }

    impl CompilePart for PatternData {
        fn compile_part(&self, value_id: LocIdent, bindings_id: LocIdent) -> RichTerm {
            match self {
                PatternData::Any(id) => {
                    // %record_insert% "<id>" value_id bindings_id
                    insert_binding(*id, value_id, bindings_id)
                }
                PatternData::Record(pat) => pat.compile_part(value_id, bindings_id),
                PatternData::Enum(pat) => pat.compile_part(value_id, bindings_id),
            }
        }
    }

    impl CompilePart for RecordPattern {
        // Compilation of the top-level record pattern wrapper:
        //
        // if %typeof% value_id == 'Record
        //    <fold (field, value) in fields / cont is the accumulator>
        //    if %has_field% field value_id && %is_defined% field value_id then
        //      let value_id = %static_access(field)% value_id in
        //      let new_bindings_id = cont in
        //
        //      if new_bindings_id == null then
        //        null
        //      else
        //        <field.compile_part(new_value_id, new_bindings_id)>
        //    else
        //      null
        fn compile_part(&self, value_id: LocIdent, bindings_id: LocIdent) -> RichTerm {
            // The fold block:
            //
            // <fold (field, value) in fields / cont is the accumulator>
            //    if %field_is_defined% field value_id then
            //      let new_value_id = %static_access(field)% value_id in
            //      let new_bindings_id = cont in
            //
            //      if new_bindings_id == null then
            //        null
            //      else
            //        <field.compile_part(new_value_id, new_bindings_id)>
            //    else
            //      null
            let inner: RichTerm =
                self.patterns
                    .iter()
                    .fold(Term::Var(bindings_id).into(), |cont, field_pat| {
                        let field = field_pat.matched_id;
                        let new_bindings_id = LocIdent::fresh();
                        let new_value_id = LocIdent::fresh();

                        // %field_is_defined% field value_id
                        let has_field = make::op2(
                            BinaryOp::FieldIsDefined(RecordOpKind::ConsiderAllFields),
                            Term::Str(field.label().into()),
                            Term::Var(value_id),
                        );

                        // The innermost if:
                        //
                        // if new_bindings_id == null then
                        //   null
                        // else
                        //   <field.compile_part(new_value_id, new_bindings_id)>
                        let inner_if = make::if_then_else(
                            make::op2(BinaryOp::Eq(), Term::Var(new_bindings_id), Term::Null),
                            Term::Null,
                            field_pat
                                .pattern
                                .compile_part(new_value_id, new_bindings_id),
                        );

                        // let new_bindings_id = cont in <inner_if>
                        let inner_let = make::let_in(new_bindings_id, cont, inner_if);

                        // let new_value_id = %static_access(field)% value_id in <inner_let>
                        let outer_let = make::let_in(
                            new_value_id,
                            make::op1(UnaryOp::StaticAccess(field), Term::Var(value_id)),
                            inner_let,
                        );

                        // if <has_field> then <outer_let> else null
                        make::if_then_else(has_field, outer_let, Term::Null)
                    });

            // %typeof% value_id == 'Record
            let is_record: RichTerm = make::op2(
                BinaryOp::Eq(),
                make::op1(UnaryOp::Typeof(), Term::Var(value_id)),
                Term::Enum("Record".into()),
            );

            // if <is_record> then inner else null
            make::if_then_else(is_record, inner, Term::Null)
        }
    }

    impl CompilePart for EnumPattern {
        fn compile_part(&self, _value_id: LocIdent, _bindings_id: LocIdent) -> RichTerm {
            todo!()
        }
    }

    pub trait Compile {
        /// Compile a match expression to a Nickel expression with the provided `value_id` as a
        /// free variable (representing a placeholder for the matched expression).
        fn compile(self, value: RichTerm, pos: TermPos) -> RichTerm;
    }

    impl Compile for MatchData {
        // Compilation of a full match expression (code between < and > is Rust code, think of it
        // as a kind of templating):
        //
        // let value_id = value in
        //
        // <for (pattern, body) in branches.rev()
        //  - cont is the accumulator
        //  - initial accumulator is the default branch (or error if not default branch)
        // >
        //    let init_bindings_id = {} in
        //    let bindings_id = <pattern.compile()> value_id init_bindings_id in
        //
        //    if bindings_id == null then
        //      cont
        //    else
        //      # this primop evaluates body with an environment extended with bindings_id
        //      %with_env% body bindings_id
        fn compile(self, value: RichTerm, pos: TermPos) -> RichTerm {
            let default_branch = self.default.unwrap_or_else(|| {
                Term::RuntimeError(EvalError::NonExhaustiveMatch {
                    value: value.clone(),
                    pos,
                })
                .into()
            });
            let value_id = LocIdent::fresh();

            // The fold block:
            //
            // <for (pattern, body) in branches.rev()
            //  - cont is the accumulator
            //  - initial accumulator is the default branch (or error if not default branch)
            // >
            //    let init_bindings_id = {} in
            //    let bindings_id = <pattern.compile_part(value_id, init_bindings)> in
            //
            //    if bindings_id == null then
            //      cont
            //    else
            //      # this primop evaluates body with an environment extended with bindings_id
            //      %with_env% body bindings_id
            let fold_block =
                self.branches
                    .into_iter()
                    .rev()
                    .fold(default_branch, |cont, (pat, body)| {
                        let init_bindings_id = LocIdent::fresh();
                        let bindings_id = LocIdent::fresh();

                        // inner if block:
                        //
                        // if bindings_id == null then
                        //   cont
                        // else
                        //   # this primop evaluates body with an environment extended with bindings_id
                        //   %with_env% bindings_id body
                        let inner = make::if_then_else(
                            make::op2(BinaryOp::Eq(), Term::Var(bindings_id), Term::Null),
                            cont,
                            mk_app!(make::op1(UnaryOp::WithEnv(), Term::Var(bindings_id),), body),
                        );

                        // The two initial chained let-bindings:
                        //
                        // let init_bindings_id = {} in
                        // let bindings_id = <pattern.compile_part(value_id, init_bindings)> in
                        // <inner>
                        make::let_in(
                            init_bindings_id,
                            Term::Record(RecordData::empty()),
                            make::let_in(
                                bindings_id,
                                pat.compile_part(value_id, init_bindings_id),
                                inner,
                            ),
                        )
                    });

            // let value_id = value in <fold_block>
            make::let_in(value_id, value, fold_block)
        }
    }
}
