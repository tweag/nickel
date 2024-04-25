//! Compilation of pattern matching down to pattern-less Nickel code.
//!
//! # Algorithm
//!
//! Compiling patterns amounts to generate a decision tree - concretely, a term composed mostly of
//! nested if-then-else - which either succeeds to match a value and returns the bindings of
//! pattern variables, or fails and returns `null`.
//!
//! Compilation of pattern matching is a well-studied problem in the literature, where efficient
//! algorithms try to avoid the duplication of checks by "grouping" them in a smart way. A standard
//! resource on this topic is the paper [_Compiling Pattern Matching to Good Decision
//! Trees_](https://dl.acm.org/doi/10.1145/1411304.1411311) by Luc Maranget.
//!
//! The current version of pattern compilation in Nickel is naive: it simply compiles each pattern
//! to a checking expression and tries them all until one works. We don't expect pattern matching
//! to be relevant for performance anytime soon (allegedly, there are much more impacting aspects
//! to handle before that). We might revisit this in the future if pattern matching turns out to be
//! a bottleneck.
//!
//! Most build blocks are generated programmatically rather than written out as e.g. members of the
//! [internals] stdlib module. While clunkier, this lets more easily change the compilation
//! strategy in the future and is already a more efficient in the current setting (combining
//! building blocks from the standard library would require much more function applications, while
//! we can generate inlined versions on-the-fly here).
use super::*;
use crate::{
    mk_app,
    term::{
        make, record::FieldMetadata, BinaryOp, MatchData, RecordExtKind, RecordOpKind, RichTerm,
        Term, UnaryOp,
    },
};

/// Generate a standard `%record_insert%` primop as generated by the parser.
fn record_insert() -> BinaryOp {
    BinaryOp::DynExtend {
        ext_kind: RecordExtKind::WithValue,
        metadata: Default::default(),
        pending_contracts: Default::default(),
        // We don't really care for optional fields here and we don't need to filter them out
        op_kind: RecordOpKind::ConsiderAllFields,
    }
}

/// Generate a record update

/// Generate a Nickel expression which inserts a new binding in the working dictionary.
///
/// `%record_insert% "<id>" bindings_id value_id`
fn insert_binding(id: LocIdent, value_id: LocIdent, bindings_id: LocIdent) -> RichTerm {
    mk_app!(
        make::op2(
            record_insert(),
            Term::Str(id.label().into()),
            Term::Var(bindings_id)
        ),
        Term::Var(value_id)
    )
}

/// Generate a Nickel expression which update the `REST_FIELD` field of the working bindings by
/// remove the `field` from it.
///
/// ```nickel
/// %record_insert% "<REST_FIELD>"
///   (%record_remove% "<REST_FIELD>" bindings_id)
///   (%record_remove% "<field>"
///     (%static_access(REST_FIELD) bindings_id)
///   )
/// ```
fn remove_from_rest(rest_field: LocIdent, field: LocIdent, bindings_id: LocIdent) -> RichTerm {
    let rest = make::op1(UnaryOp::StaticAccess(rest_field), Term::Var(bindings_id));

    let rest_shrinked = make::op2(
        BinaryOp::DynRemove(RecordOpKind::ConsiderAllFields),
        Term::Str(field.label().into()),
        rest,
    );

    let bindings_shrinked = make::op2(
        BinaryOp::DynRemove(RecordOpKind::ConsiderAllFields),
        Term::Str(rest_field.into()),
        Term::Var(bindings_id),
    );

    mk_app!(
        make::op2(
            record_insert(),
            Term::Str(rest_field.into()),
            bindings_shrinked,
        ),
        rest_shrinked
    )
}

/// Generate a Nickel expression which checks if a field is defined in a record provided as a
/// variable, and if not, insert a default value. Return the result (either the original record
/// unchanged, or the original record with the default value). The resulting record is guaranteed
/// to have the `field` defined. The implementation uses merging to avoid dropping the contracts
/// and other metadata affected to `field`, if the field exists but has no definition.
///
/// More precisely, [with_default_value] generates the following code:
///
/// ```nickel
/// if !(%field_is_defined% "<field>" record_id) then
///   if %has_field% "<field>" record_id then
///     record_id & { "<field>" = default }
///   else
///     # Merging is potentially more costly, and we can just fallback to record insertion here.
///     %record_insert% "<field>" record_id default
/// else
///   record_id
/// ```
pub(crate) fn with_default_value(
    record_id: LocIdent,
    field: LocIdent,
    default: RichTerm,
) -> RichTerm {
    let field_not_defined = make::op1(
        UnaryOp::BoolNot(),
        make::op2(
            BinaryOp::FieldIsDefined(RecordOpKind::ConsiderAllFields),
            Term::Str(field.label().into()),
            Term::Var(record_id),
        ),
    );

    let has_field = make::op2(
        BinaryOp::HasField(RecordOpKind::ConsiderAllFields),
        Term::Str(field.label().into()),
        Term::Var(record_id),
    );

    let insert = mk_app!(
        make::op2(
            record_insert(),
            Term::Str(field.into()),
            make::var(record_id)
        ),
        default.clone()
    );

    let inner_let_if = make::if_then_else(
        has_field,
        update_with_merge(record_id, field, Field::from(default)),
        insert,
    );

    make::if_then_else(field_not_defined, inner_let_if, Term::Var(record_id))
}

/// Update a record field by merging it with a singleton record containing the new value.
///
/// ```nickel
/// record_id & { "<id>" = <field> }
/// ```
fn update_with_merge(record_id: LocIdent, id: LocIdent, field: Field) -> RichTerm {
    use crate::{
        label::{MergeKind, MergeLabel},
        term::IndexMap,
    };

    let annot = field.metadata.annotation.clone();
    let pos_value = field.value.as_ref().and_then(|value| value.pos.into_opt());

    let singleton = Term::Record(RecordData {
        fields: IndexMap::from([(id, field)]),
        ..Default::default()
    });
    // Right now, patterns are compiled on-the-fly during evaluation. We thus need to
    // perform the gen_pending_contract transformation manually, or the contracts will
    // just be ignored. One step suffices, as we create a singleton record that doesn't
    // contain other non-transformed records (the default value, if any, has been
    // transformed normally).
    //
    // unwrap(): typechecking ensures that there are no unbound variables at this point
    let singleton =
        crate::transform::gen_pending_contracts::transform_one(singleton.into()).unwrap();

    let span = annot
        .iter()
        .map(|labeled_ty| labeled_ty.label.span)
        .chain(pos_value)
        // We fuse all the definite spans together.
        // unwrap(): all span should come from the same file
        // unwrap(): we hope that at least one position is defined
        .reduce(|span1, span2| crate::position::RawSpan::fuse(span1, span2).unwrap())
        .unwrap();

    let merge_label = MergeLabel {
        span,
        kind: MergeKind::Standard,
    };

    make::op2(
        BinaryOp::Merge(merge_label),
        Term::Var(record_id),
        singleton,
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
    // a template of some sort):
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
            PatternData::Constant(pat) => pat.compile_part(value_id, bindings_id),
        }
    }
}

impl CompilePart for ConstantPattern {
    fn compile_part(&self, value_id: LocIdent, bindings_id: LocIdent) -> RichTerm {
        self.data.compile_part(value_id, bindings_id)
    }
}

impl CompilePart for ConstantPatternData {
    fn compile_part(&self, value_id: LocIdent, bindings_id: LocIdent) -> RichTerm {
        let compile_constant = |nickel_type: &str, value: Term| {
            // if %typeof% value_id == '<nickel_type> && value_id == <value> then
            //   bindings_id
            // else
            //   null

            // %typeof% value_id == '<nickel_type>
            let type_matches = make::op2(
                BinaryOp::Eq(),
                make::op1(UnaryOp::Typeof(), Term::Var(value_id)),
                Term::Enum(nickel_type.into()),
            );

            // value_id == <value>
            let value_matches = make::op2(BinaryOp::Eq(), Term::Var(value_id), value);

            // <type_matches> && <value_matches>
            let if_condition = mk_app!(make::op1(UnaryOp::BoolAnd(), type_matches), value_matches);

            make::if_then_else(if_condition, Term::Var(bindings_id), Term::Null)
        };

        match self {
            ConstantPatternData::Bool(b) => compile_constant("Bool", Term::Bool(*b)),
            ConstantPatternData::Number(n) => compile_constant("Number", Term::Num(n.clone())),
            ConstantPatternData::String(s) => compile_constant("String", Term::Str(s.clone())),
            ConstantPatternData::Null => compile_constant("Other", Term::Null),
        }
    }
}

impl CompilePart for RecordPattern {
    // Compilation of the top-level record pattern wrapper.
    //
    // To check that the value doesn't contain extra fields, or to capture the rest of the
    // record when using the `..rest` construct, we need to remove matched fields from the
    // original value at each stage and thread this working value in addition to the bindings.
    //
    // We don't have tuples, and to avoid adding an indirection (by storing the current state
    // as `{rest, bindings}` where bindings itself is a record), we store this rest alongside
    // the bindings in a special field which is a freshly generated indentifier. This is an
    // implementation detail which isn't very hard to change, should we have to.
    //
    // if %typeof% value_id == 'Record
    //   let final_bindings_id =
    //     <fold (field, value) in fields
    //      - cont is the accumulator
    //      - initial accumulator is `%record_insert% "<REST_FIELD>" bindings_id value_id`
    //      >
    //
    //     # If there is a default value, we must set it before the %field_is_defined% check below,
    //     # because the default acts like if the original matched value always have this field
    //     # defined
    //     <if field.default.is_some()>
    //       let value_id = <with_default_value value_id field default> in
    //     <end if>
    //
    //      if %field_is_defined% field value_id then
    //         <if !field_pat.annotation.is_empty()>
    //           let value_id = value_id & { "<field>" | <field_pat.annotation> } in
    //         <end if>
    //
    //         let local_bindings_id = cont in
    //
    //         if local_bindings_id == null then
    //           null
    //         else
    //           let local_value_id = %static_access(field)% (%static_access(REST_FIELD)% local_bindings_id)
    //           let local_bindings_id = <remove_from_rest(field, local_bindings_id)> in
    //           <field.compile_part(local_value_id, local_bindings_id)>
    //       else
    //         null
    //     <end fold>
    //   in
    //
    //   if final_bindings_id == null then
    //     null
    //   else
    //     <if self.tail is empty>
    //       # if tail is empty, check that the value doesn't contain extra fields
    //       if (%static_access% <REST_FIELD> final_bindings_id) != {} then
    //         null
    //       else
    //         %record_remove% "<REST>" final_bindings_id
    //     <else if self.tail is capture(rest)>
    //       # move the rest from REST_FIELD to rest, and remove REST_FIELD
    //       %record_remove% "<REST>"
    //         (%record_insert% <rest>
    //           final_bindings_id
    //           (%static_access% <REST_FIELD> final_bindings_id)
    //         )
    //     <else if self.tail is open>
    //       %record_remove% "<REST>" final_bindings_id
    //     <end if>
    // else
    //   null
    fn compile_part(&self, value_id: LocIdent, bindings_id: LocIdent) -> RichTerm {
        let rest_field = LocIdent::fresh();

        // `%record_insert% "<REST>" bindings_id value_id`
        let init_bindings = mk_app!(
            make::op2(
                record_insert(),
                Term::Str(rest_field.into()),
                Term::Var(bindings_id)
            ),
            Term::Var(value_id)
        );

        // The fold block:
        //
        // <fold (field, value) in fields
        //  - cont is the accumulator
        //  - initial accumulator is `%record_insert% "<REST>" bindings_id value_id`
        // >
        //
        // # If there is a default value, we must set it before the %field_is_defined% check below,
        // # because the default acts like if the original matched value always have this field
        // # defined
        // <if field.default.is_some()>
        //   let value_id = <with_default_value value_id field default> in
        // <end if>
        //
        // if %field_is_defined% field value_id then
        //   # If the field is present, we apply the potential contracts coming from user-provided
        //   # annotations before anything else. We just offload the actual work to `&`
        //   <if !field_pat.annotation.is_empty() >
        //     let value_id = value_id & { "<field>" | <field_pat.annotation> } in
        //   <end if>
        //
        //   let local_bindings_id = cont in
        //
        //   if local_bindings_id == null then
        //     null
        //   else
        //     let local_value_id = %static_access(field)% (%static_access(REST_FIELD)% local_bindings_id) in
        //     let local_bindings_id = <remove_from_rest(field, local_bindings_id)> in
        //     <field.compile_part(local_value_id, local_bindings_id)>
        let fold_block: RichTerm = self.patterns.iter().fold(init_bindings, |cont, field_pat| {
            let field = field_pat.matched_id;
            let local_bindings_id = LocIdent::fresh();
            let local_value_id = LocIdent::fresh();

            // let bindings_id = <remove_from_rest(field, local_bindings_id)> in
            // <field.compile_part(local_value_id, local_bindings_id)>
            let updated_bindings_let = make::let_in(
                local_bindings_id,
                remove_from_rest(rest_field, field, local_bindings_id),
                field_pat
                    .pattern
                    .compile_part(local_value_id, local_bindings_id),
            );

            // %static_access(field)% (%static_access(REST_FIELD)% local_bindings_id)
            let extracted_value = make::op1(
                UnaryOp::StaticAccess(field),
                make::op1(
                    UnaryOp::StaticAccess(rest_field),
                    Term::Var(local_bindings_id),
                ),
            );

            // let local_value_id = <extracted_value> in <updated_bindings_let>
            let inner_else_block =
                make::let_in(local_value_id, extracted_value, updated_bindings_let);

            // The innermost if:
            //
            // if local_bindings_id == null then
            //   null
            // else
            //  <inner_else_block>
            let inner_if = make::if_then_else(
                make::op2(BinaryOp::Eq(), Term::Var(local_bindings_id), Term::Null),
                Term::Null,
                inner_else_block,
            );

            // let local_bindings_id = cont in <value_let>
            let binding_cont_let = make::let_in(local_bindings_id, cont, inner_if);

            // <if !field.annotation.is_empty()>
            //   let value_id = <update_with_merge...> in <binding_cont_let>
            // <end if>
            let optional_merge = if !field_pat.annotation.is_empty() {
                make::let_in(
                    value_id,
                    update_with_merge(
                        value_id,
                        field,
                        Field::from(FieldMetadata {
                            annotation: field_pat.annotation.clone(),
                            ..Default::default()
                        }),
                    ),
                    binding_cont_let,
                )
            } else {
                binding_cont_let
            };

            // %field_is_defined% field value_id
            let has_field = make::op2(
                BinaryOp::FieldIsDefined(RecordOpKind::ConsiderAllFields),
                Term::Str(field.label().into()),
                Term::Var(value_id),
            );

            // if <has_field> then <optional_merge> else null
            let enclosing_if = make::if_then_else(has_field, optional_merge, Term::Null);

            // <if field_pat.default.is_some()>
            //   let value_id = <with_default_value value_id field default> in
            // <end if>
            if let Some(default) = field_pat.default.as_ref() {
                make::let_in(
                    value_id,
                    with_default_value(value_id, field, default.clone()),
                    enclosing_if,
                )
            } else {
                enclosing_if
            }
        });

        // %typeof% value_id == 'Record
        let is_record: RichTerm = make::op2(
            BinaryOp::Eq(),
            make::op1(UnaryOp::Typeof(), Term::Var(value_id)),
            Term::Enum("Record".into()),
        );

        let final_bindings_id = LocIdent::fresh();

        // %record_remove% "<REST>" final_bindings_id
        let bindings_without_rest = make::op2(
            BinaryOp::DynRemove(RecordOpKind::ConsiderAllFields),
            Term::Str(rest_field.into()),
            Term::Var(final_bindings_id),
        );

        // the else block which depends on the tail of the record pattern
        let tail_block = match self.tail {
            // if (%static_access% <REST_FIELD> final_bindings_id) != {} then
            //   null
            // else
            //   %record_remove% "<REST>" final_bindings_id
            RecordPatternTail::Empty => make::if_then_else(
                make::op1(
                    UnaryOp::BoolNot(),
                    make::op2(
                        BinaryOp::Eq(),
                        make::op1(
                            UnaryOp::StaticAccess(rest_field),
                            Term::Var(final_bindings_id),
                        ),
                        Term::Record(RecordData::empty()),
                    ),
                ),
                Term::Null,
                bindings_without_rest,
            ),
            // %record_remove% "<REST>"
            //   (%record_insert% <rest>
            //     final_bindings_id
            //     (%static_access% <REST_FIELD> final_bindings_id)
            //   )
            RecordPatternTail::Capture(rest) => make::op2(
                BinaryOp::DynRemove(RecordOpKind::ConsiderAllFields),
                Term::Str(rest_field.into()),
                mk_app!(
                    make::op2(
                        record_insert(),
                        Term::Str(rest.label().into()),
                        Term::Var(final_bindings_id),
                    ),
                    make::op1(
                        UnaryOp::StaticAccess(rest_field),
                        Term::Var(final_bindings_id)
                    )
                ),
            ),
            // %record_remove% "<REST>" final_bindings_id
            RecordPatternTail::Open => bindings_without_rest,
        };

        // the last `final_bindings_id != null` guard:
        //
        // if final_bindings_id == null then
        //   null
        // else
        //   <tail_block>
        let guard_tail_block = make::if_then_else(
            make::op2(BinaryOp::Eq(), Term::Var(final_bindings_id), Term::Null),
            Term::Null,
            tail_block,
        );

        // The let enclosing the fold block and the final block:
        // let final_bindings_id = <fold_block> in <tail_block>
        let outer_let = make::let_in(final_bindings_id, fold_block, guard_tail_block);

        // if <is_record> then <outer_let> else null
        make::if_then_else(is_record, outer_let, Term::Null)
    }
}

impl CompilePart for EnumPattern {
    fn compile_part(&self, value_id: LocIdent, bindings_id: LocIdent) -> RichTerm {
        // %enum_get_tag% value_id == '<self.tag>
        let tag_matches = make::op2(
            BinaryOp::Eq(),
            make::op1(UnaryOp::EnumGetTag(), Term::Var(value_id)),
            Term::Enum(self.tag),
        );

        if let Some(pat) = &self.pattern {
            // if %enum_is_variant% value_id && %enum_get_tag% value_id == '<self.tag> then
            //   let value_id = %enum_unwrap_variant% value_id in
            //   <pattern.compile(value_id, bindings_id)>
            // else
            //   null

            // %enum_is_variant% value_id && <tag_matches>
            let if_condition = mk_app!(
                make::op1(
                    UnaryOp::BoolAnd(),
                    make::op1(UnaryOp::EnumIsVariant(), Term::Var(value_id)),
                ),
                tag_matches
            );

            make::if_then_else(
                if_condition,
                make::let_in(
                    value_id,
                    make::op1(UnaryOp::EnumUnwrapVariant(), Term::Var(value_id)),
                    pat.compile_part(value_id, bindings_id),
                ),
                Term::Null,
            )
        } else {
            // if %typeof% value_id == 'Enum && !(%enum_is_variant% value_id) && <tag_matches> then
            //   bindings_id
            // else
            //   null

            // %typeof% value_id == 'Enum
            let is_enum = make::op2(
                BinaryOp::Eq(),
                make::op1(UnaryOp::Typeof(), Term::Var(value_id)),
                Term::Enum("Enum".into()),
            );

            // !(%enum_is_variant% value_id)
            let is_enum_tag = make::op1(
                UnaryOp::BoolNot(),
                make::op1(UnaryOp::EnumIsVariant(), Term::Var(value_id)),
            );

            // <is_enum> && <is_enum_tag> && <tag_matches>
            let if_condition = mk_app!(
                make::op1(UnaryOp::BoolAnd(), is_enum,),
                mk_app!(make::op1(UnaryOp::BoolAnd(), is_enum_tag,), tag_matches)
            );

            make::if_then_else(if_condition, Term::Var(bindings_id), Term::Null)
        }
    }
}

pub trait Compile {
    /// Compile a match expression to a Nickel expression with the provided `value_id` as a
    /// free variable (representing a placeholder for the matched expression).
    fn compile(self, value: RichTerm, pos: TermPos) -> RichTerm;
}

impl Compile for MatchData {
    // Compilation of a full match expression (code between < and > is Rust code, think of it as a
    // kind of templating). Note that some special cases compile differently as optimizations.
    //
    // let value_id = value in
    //
    // <fold (pattern, body) in branches.rev()
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
    //      %pattern_branch% body bindings_id
    fn compile(self, value: RichTerm, pos: TermPos) -> RichTerm {
        if self.branches.iter().all(|(pat, _)| {
            matches!(
                pat.data,
                PatternData::Enum(EnumPattern { pattern: None, .. })
            )
        }) {
            let tags_only = self.branches.into_iter().map(|(pat, body)| {
                let PatternData::Enum(EnumPattern {tag, ..}) = pat.data else {
                    panic!("match compilation: just tested that all cases are enum tags, but found a non enum tag pattern");
                };

                (tag, body)
            }).collect();

            return TagsOnlyMatch {
                branches: tags_only,
                default: self.default,
            }
            .compile(value, pos);
        }

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
        //      %pattern_branch% body bindings_id
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
                    //   %pattern_branch% bindings_id body
                    let inner = make::if_then_else(
                        make::op2(BinaryOp::Eq(), Term::Var(bindings_id), Term::Null),
                        cont,
                        mk_app!(
                            make::op1(UnaryOp::PatternBranch(), Term::Var(bindings_id),),
                            body
                        ),
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

/// Simple wrapper used to implement specialization of match statements when all of the patterns
/// are enum tags. Instead of a sequence of conditionals (which has linear time complexity), we use
/// a special primops based on a hashmap, which has amortized constant time complexity.
struct TagsOnlyMatch {
    branches: Vec<(LocIdent, RichTerm)>,
    default: Option<RichTerm>,
}

impl Compile for TagsOnlyMatch {
    fn compile(self, value: RichTerm, pos: TermPos) -> RichTerm {
        // We simply use the corresponding specialized primop in that case.
        let match_op = mk_app!(
            make::op1(
                UnaryOp::TagsOnlyMatch {
                    has_default: self.default.is_some()
                },
                value
            )
            .with_pos(pos),
            Term::Record(RecordData::with_field_values(self.branches.into_iter()))
        );

        let match_op = if let Some(default) = self.default {
            mk_app!(match_op, default)
        } else {
            match_op
        };

        match_op.with_pos(pos)
    }
}
