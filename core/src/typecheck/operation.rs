//! Typing of primitive operations.
use super::*;
use crate::{
    ast::{AstAlloc, builder, primop::PrimOp},
    label::{Polarity, TypeVarData},
    typ::TypeF,
};

use crate::{mk_uty_arrow, mk_uty_enum, mk_uty_record};

pub trait PrimOpType {
    fn primop_type<'ast>(
        &self,
        state: &mut State<'ast, '_>,
        var_level: VarLevel,
    ) -> Result<(Vec<UnifType<'ast>>, UnifType<'ast>), TypecheckError>;
}

impl PrimOpType for PrimOp {
    fn primop_type<'ast>(
        &self,
        state: &mut State<'ast, '_>,
        var_level: VarLevel,
    ) -> Result<(Vec<UnifType<'ast>>, UnifType<'ast>), TypecheckError> {
        Ok(match self {
            // Dyn -> [| 'Number, 'Bool, 'String, 'Enum, 'Function, 'Array, 'Record, 'Label,
            // 'ForeignId, 'Type, 'Other |]
            PrimOp::Typeof => (
                vec![mk_uniftype::dynamic()],
                mk_uty_enum!(
                    "Number",
                    "Bool",
                    "String",
                    "Enum",
                    "Function",
                    "CustomContract",
                    "Array",
                    "Record",
                    "Label",
                    "ForeignId",
                    "Type",
                    "Other"
                ),
            ),
            // Dyn -> [|
            //   'Number Number, 'Bool Bool, 'String String, 'Enum Dyn,
            //   'Function (Dyn -> Dyn), 'Array (Array Dyn), 'Record {_: Dyn}, 'Label Dyn,
            //   'ForeignId Dyn, 'Type Dyn, 'Other Dyn
            // |]
            PrimOp::Cast => (
                vec![mk_uniftype::dynamic()],
                mk_uty_enum!(
                    ("Number", TypeF::Number),
                    ("Bool", TypeF::Bool),
                    ("String", TypeF::String),
                    ("Enum", TypeF::Dyn),
                    ("Function", mk_uty_arrow!(TypeF::Dyn, TypeF::Dyn)),
                    ("CustomContract", TypeF::Dyn),
                    ("Array", mk_uniftype::array(TypeF::Dyn)),
                    ("Record", mk_uniftype::dict(TypeF::Dyn)),
                    ("Label", TypeF::Dyn),
                    ("ForeignId", TypeF::Dyn),
                    ("Type", TypeF::Dyn),
                    ("Other", TypeF::Dyn)
                ),
            ),
            // Bool -> Bool -> Bool
            PrimOp::BoolAnd | PrimOp::BoolOr => (
                vec![mk_uniftype::bool()],
                mk_uty_arrow!(TypeF::Bool, TypeF::Bool),
            ),
            // Bool -> Bool
            PrimOp::BoolNot => (vec![mk_uniftype::bool()], mk_uniftype::bool()),
            // forall a. Dyn -> a
            PrimOp::Blame => {
                let res = state.table.fresh_type_uvar(var_level);

                (vec![mk_uniftype::dynamic()], res)
            }
            // Dyn -> Polarity
            PrimOp::LabelPol => (
                vec![mk_uniftype::dynamic()],
                mk_uty_enum!("Positive", "Negative"),
            ),
            // forall rows. [| ; rows |] -> [| id ; rows |]
            PrimOp::EnumEmbed(id) => {
                let row_var_id = state.table.fresh_erows_var_id(var_level);
                let row = UnifEnumRows::UnifVar {
                    id: row_var_id,
                    init_level: var_level,
                };

                let domain = mk_uty_enum!(; row.clone());
                let codomain = mk_uty_enum!(*id; row);

                (vec![domain], codomain)
            }
            // Morally, Label -> Label
            // Dyn -> Dyn
            PrimOp::LabelFlipPol
            | PrimOp::LabelGoDom
            | PrimOp::LabelGoCodom
            | PrimOp::LabelGoArray
            | PrimOp::LabelGoDict => (vec![mk_uniftype::dynamic()], mk_uniftype::dynamic()),
            // forall rows a. { id: a | rows} -> a
            PrimOp::RecordStatAccess(id) => {
                let rows = state.table.fresh_rrows_uvar(var_level);
                let res = state.table.fresh_type_uvar(var_level);

                (vec![mk_uty_record!((*id, res.clone()); rows)], res)
            }
            // forall a b. Array a -> (a -> b) -> Array b
            PrimOp::ArrayMap => {
                let a = state.table.fresh_type_uvar(var_level);
                let b = state.table.fresh_type_uvar(var_level);

                let f_type = mk_uty_arrow!(a.clone(), b.clone());
                (
                    vec![mk_uniftype::array(a)],
                    mk_uty_arrow!(f_type, mk_uniftype::array(b)),
                )
            }
            // forall a. Num -> (Num -> a) -> Array a
            PrimOp::ArrayGen => {
                let a = state.table.fresh_type_uvar(var_level);

                let f_type = mk_uty_arrow!(TypeF::Number, a.clone());
                (
                    vec![mk_uniftype::num()],
                    mk_uty_arrow!(f_type, mk_uniftype::array(a)),
                )
            }
            // forall a b. { _ : a} -> (Str -> a -> b) -> { _ : b }
            PrimOp::RecordMap => {
                // Assuming f has type Str -> a -> b,
                // this has type Dict(a) -> Dict(b)

                let a = state.table.fresh_type_uvar(var_level);
                let b = state.table.fresh_type_uvar(var_level);

                let f_type = mk_uty_arrow!(TypeF::String, a.clone(), b.clone());
                (
                    vec![mk_uniftype::dict(a)],
                    mk_uty_arrow!(f_type, mk_uniftype::dict(b)),
                )
            }
            // forall a b. a -> b -> b
            PrimOp::Seq | PrimOp::DeepSeq => {
                let fst = state.table.fresh_type_uvar(var_level);
                let snd = state.table.fresh_type_uvar(var_level);

                (vec![fst], mk_uty_arrow!(snd.clone(), snd))
            }
            // forall a. Array a -> Num
            PrimOp::ArrayLength => {
                let ty_elt = state.table.fresh_type_uvar(var_level);
                (vec![mk_uniftype::array(ty_elt)], mk_uniftype::num())
            }
            // forall a. { _: a } -> Array Str
            PrimOp::RecordFields(_) => {
                let ty_a = state.table.fresh_type_uvar(var_level);

                (
                    vec![mk_uniftype::dict(ty_a)],
                    mk_uniftype::array(mk_uniftype::str()),
                )
            }
            // forall a. { _: a } -> Array a
            PrimOp::RecordValues => {
                let ty_a = state.table.fresh_type_uvar(var_level);

                (
                    vec![mk_uniftype::dict(ty_a.clone())],
                    mk_uniftype::array(ty_a),
                )
            }
            // Str -> Str
            PrimOp::StringTrim => (vec![mk_uniftype::str()], mk_uniftype::str()),
            // Str -> Array Str
            PrimOp::StringChars => (
                vec![mk_uniftype::str()],
                mk_uniftype::array(mk_uniftype::str()),
            ),
            // Str -> Str
            PrimOp::StringUppercase => (vec![mk_uniftype::str()], mk_uniftype::str()),
            // Str -> Str
            PrimOp::StringLowercase => (vec![mk_uniftype::str()], mk_uniftype::str()),
            // Str -> Num
            PrimOp::StringLength => (vec![mk_uniftype::str()], mk_uniftype::num()),
            // Dyn -> Str
            PrimOp::ToString => (vec![mk_uniftype::dynamic()], mk_uniftype::str()),
            // Str -> Num
            PrimOp::NumberFromString => (vec![mk_uniftype::str()], mk_uniftype::num()),
            // Str -> < | a> for a rigid type variable a
            PrimOp::EnumFromString => (
                vec![mk_uniftype::str()],
                mk_uty_enum!(; state.table.fresh_erows_const(var_level)),
            ),
            // Str -> Str -> Bool
            PrimOp::StringIsMatch => (
                vec![mk_uniftype::str()],
                mk_uty_arrow!(mk_uniftype::str(), mk_uniftype::bool()),
            ),
            // Str -> Str -> {matched: Str, index: Num, groups: Array Str}
            PrimOp::StringFind => (
                vec![mk_uniftype::str()],
                mk_uty_arrow!(
                    mk_uniftype::str(),
                    mk_uty_record!(
                        ("matched", TypeF::String),
                        ("index", TypeF::Number),
                        ("groups", mk_uniftype::array(TypeF::String))
                    )
                ),
            ),
            // String -> String -> Array { matched: String, index: Number, groups: Array String }
            PrimOp::StringFindAll => (
                vec![mk_uniftype::str()],
                mk_uty_arrow!(
                    mk_uniftype::str(),
                    mk_uniftype::array(mk_uty_record!(
                        ("matched", TypeF::String),
                        ("index", TypeF::Number),
                        ("groups", mk_uniftype::array(TypeF::String))
                    ))
                ),
            ),
            // Dyn -> Dyn
            PrimOp::Force { .. } => (vec![mk_uniftype::dynamic()], mk_uniftype::dynamic()),
            PrimOp::RecordEmptyWithTail => (vec![mk_uniftype::dynamic()], mk_uniftype::dynamic()),
            // forall a. { _ : a} -> { _ : a }
            PrimOp::RecordFreeze => {
                let dict = mk_uniftype::dict(state.table.fresh_type_uvar(var_level));
                (vec![dict.clone()], dict)
            }
            // forall a. Str -> a -> a
            PrimOp::Trace => {
                let ty = state.table.fresh_type_uvar(var_level);
                (vec![mk_uniftype::str()], mk_uty_arrow!(ty.clone(), ty))
            }
            // Morally: Lbl -> Lbl
            // Actual: Dyn -> Dyn
            PrimOp::LabelPushDiag => (vec![mk_uniftype::dynamic()], mk_uniftype::dynamic()),
            // Str -> Dyn
            #[cfg(feature = "nix-experimental")]
            PrimOp::EvalNix => (vec![mk_uniftype::str()], mk_uniftype::dynamic()),
            // Because the tag isn't fixed, we can't really provide a proper static type for this
            // primop.
            // This isn't a problem, as this operator is mostly internal and pattern matching should be
            // used to destructure enum variants.
            // Dyn -> Dyn
            PrimOp::EnumGetArg => (vec![mk_uniftype::dynamic()], mk_uniftype::dynamic()),
            // String -> (Dyn -> Dyn)
            PrimOp::EnumMakeVariant => (
                vec![mk_uniftype::str()],
                mk_uniftype::arrow(mk_uniftype::dynamic(), mk_uniftype::dynamic()),
            ),
            // Same as `EnumGetArg` just above.
            // Dyn -> Dyn
            PrimOp::EnumGetTag => (vec![mk_uniftype::dynamic()], mk_uniftype::dynamic()),
            // Note that is_variant breaks parametricity, so it can't get a polymorphic type.
            // Dyn -> Bool
            PrimOp::EnumIsVariant => (vec![mk_uniftype::dynamic()], mk_uniftype::bool()),
            // // [crate::term::PrimOp::PatternBranch] shouldn't appear anywhere in actual code, because its
            // // second argument can't be properly typechecked: it has unbound variables. However, it's
            // // not hard to come up with a vague working type for it, so we do.
            // // forall a. {_ : a} -> Dyn -> Dyn
            // PrimOp::PatternBranch => {
            //     let ty_elt = state.table.fresh_type_uvar(var_level);
            //     (
            //         mk_uniftype::dict(ty_elt),
            //         mk_buty_arrow!(mk_uniftype::dynamic(), mk_uniftype::dynamic()),
            //     )
            // }
            // <custom_contract_type()> -> Dyn
            PrimOp::ContractCustom => (
                vec![custom_contract_type(state.ast_alloc)],
                mk_uniftype::dynamic(),
            ),
            // Number -> Number
            PrimOp::NumberCos
            | PrimOp::NumberSin
            | PrimOp::NumberTan
            | PrimOp::NumberArcCos
            | PrimOp::NumberArcSin
            | PrimOp::NumberArcTan => (vec![mk_uniftype::num()], mk_uniftype::num()),

            // Binary ops

            // Number -> Number -> Number
            PrimOp::Plus | PrimOp::Sub | PrimOp::Mult | PrimOp::Div | PrimOp::Modulo => (
                vec![mk_uniftype::num(), mk_uniftype::num()],
                mk_uniftype::num(),
            ),
            // Sym -> Dyn -> Dyn -> Dyn
            PrimOp::Seal => (
                vec![mk_uniftype::sym(), mk_uniftype::dynamic()],
                mk_uty_arrow!(TypeF::Dyn, TypeF::Dyn),
            ),
            // String -> String -> String
            PrimOp::StringConcat => (
                vec![mk_uniftype::str(), mk_uniftype::str()],
                mk_uniftype::str(),
            ),
            // Ideally: Contract -> Label -> Dyn -> Dyn
            // Currently: Dyn -> Dyn -> (Dyn -> Dyn)
            PrimOp::ContractApply => (
                vec![mk_uniftype::dynamic(), mk_uniftype::dynamic()],
                mk_uty_arrow!(mk_uniftype::dynamic(), mk_uniftype::dynamic()),
            ),
            // Ideally: Contract -> Label -> Dyn -> <custom_contract_ret_type()>
            // Currently: Dyn -> Dyn -> (Dyn -> <custom_contract_ret_type()>)
            PrimOp::ContractCheck => (
                vec![mk_uniftype::dynamic(), mk_uniftype::dynamic()],
                mk_uty_arrow!(
                    mk_uniftype::dynamic(),
                    custom_contract_ret_type(state.ast_alloc)
                ),
            ),
            // Ideally: <error_data_type()> -> Label -> Dyn
            // Currently: <error_data_type()> -> Dyn -> Dyn
            PrimOp::LabelWithErrorData => (
                vec![error_data_type(state.ast_alloc), mk_uniftype::dynamic()],
                mk_uniftype::dynamic(),
            ),
            // Sym -> Dyn -> Dyn -> Dyn
            PrimOp::Unseal => (
                vec![mk_uniftype::sym(), mk_uniftype::dynamic()],
                mk_uty_arrow!(TypeF::Dyn, TypeF::Dyn),
            ),
            // forall a b. a -> b -> Bool
            PrimOp::Eq => (
                vec![
                    state.table.fresh_type_uvar(var_level),
                    state.table.fresh_type_uvar(var_level),
                ],
                mk_uniftype::bool(),
            ),
            // Num -> Num -> Bool
            PrimOp::LessThan | PrimOp::LessOrEq | PrimOp::GreaterThan | PrimOp::GreaterOrEq => (
                vec![mk_uniftype::num(), mk_uniftype::num()],
                mk_uniftype::bool(),
            ),
            // Str -> Dyn -> Dyn
            PrimOp::LabelGoField => (
                vec![mk_uniftype::str(), mk_uniftype::dynamic()],
                mk_uniftype::dynamic(),
            ),
            // forall a. Str -> { _ : a} -> a
            PrimOp::RecordGet => {
                let res = state.table.fresh_type_uvar(var_level);

                (
                    vec![mk_uniftype::str(), mk_uniftype::dict(res.clone())],
                    res,
                )
            }
            // forall a. Str -> {_ : a} -> a -> {_ : a}
            PrimOp::RecordInsert(_) => {
                let res = state.table.fresh_type_uvar(var_level);
                (
                    vec![mk_uniftype::str(), mk_uniftype::dict(res.clone())],
                    mk_uty_arrow!(res.clone(), mk_uniftype::dict(res)),
                )
            }
            // forall a. Str -> { _ : a } -> { _ : a}
            PrimOp::RecordRemove(_) => {
                let res = state.table.fresh_type_uvar(var_level);
                (
                    vec![mk_uniftype::str(), mk_uniftype::dict(res.clone())],
                    mk_uniftype::dict(res),
                )
            }
            // forall a. Str -> {_: a} -> Bool
            PrimOp::RecordHasField(_) => {
                let ty_elt = state.table.fresh_type_uvar(var_level);
                (
                    vec![mk_uniftype::str(), mk_uniftype::dict(ty_elt)],
                    mk_uniftype::bool(),
                )
            }
            // forall a. Str -> {_: a} -> Bool
            PrimOp::RecordFieldIsDefined(_) => {
                let ty_elt = state.table.fresh_type_uvar(var_level);
                (
                    vec![mk_uniftype::str(), mk_uniftype::dict(ty_elt)],
                    mk_uniftype::bool(),
                )
            }
            // forall a. Array a -> Array a -> Array a
            PrimOp::ArrayConcat => {
                let ty_elt = state.table.fresh_type_uvar(var_level);
                let ty_array = mk_uniftype::array(ty_elt);
                (vec![ty_array.clone(), ty_array.clone()], ty_array)
            }
            // forall a. Array a -> Num -> a
            PrimOp::ArrayAt => {
                let ty_elt = state.table.fresh_type_uvar(var_level);
                (
                    vec![mk_uniftype::array(ty_elt.clone()), mk_uniftype::num()],
                    ty_elt,
                )
            }
            // Dyn -> Dyn -> Dyn
            PrimOp::Merge(_) => (
                vec![mk_uniftype::dynamic(), mk_uniftype::dynamic()],
                mk_uniftype::dynamic(),
            ),
            // <Md5, Sha1, Sha256, Sha512> -> Str -> Str
            PrimOp::Hash => (
                vec![
                    mk_uty_enum!("Md5", "Sha1", "Sha256", "Sha512"),
                    mk_uniftype::str(),
                ],
                mk_uniftype::str(),
            ),
            // forall a. <Json, Yaml, Toml> -> a -> Str
            PrimOp::Serialize => {
                let ty_input = state.table.fresh_type_uvar(var_level);
                (
                    vec![mk_uty_enum!("Json", "Yaml", "Toml"), ty_input],
                    mk_uniftype::str(),
                )
            }
            // <Json, Yaml, Toml> -> Str -> Dyn
            PrimOp::Deserialize => (
                vec![mk_uty_enum!("Json", "Yaml", "Toml"), mk_uniftype::str()],
                mk_uniftype::dynamic(),
            ),
            // Num -> Num -> Num
            PrimOp::NumberArcTan2 | PrimOp::NumberLog | PrimOp::Pow => (
                vec![mk_uniftype::num(), mk_uniftype::num()],
                mk_uniftype::num(),
            ),
            // Str -> Str -> Bool
            PrimOp::StringContains => (
                vec![mk_uniftype::str(), mk_uniftype::str()],
                mk_uniftype::bool(),
            ),
            // Str -> Str -> <Lesser, Equal, Greater>
            PrimOp::StringCompare => (
                vec![mk_uniftype::str(), mk_uniftype::str()],
                mk_uty_enum!("Lesser", "Equal", "Greater"),
            ),
            // Str -> Str -> Array Str
            PrimOp::StringSplit => (
                vec![mk_uniftype::str(), mk_uniftype::str()],
                mk_uniftype::array(TypeF::String),
            ),
            // [| 'Standard, 'UrlSafe, 'NoPad, 'UrlSafeNoPad |] -> String -> String
            PrimOp::StringBase64Encode => (
                vec![
                    mk_uty_enum!("Standard", "UrlSafe", "NoPad", "UrlSafeNoPad"),
                    mk_uniftype::str(),
                ],
                mk_uniftype::str(),
            ),
            // [| 'Standard, 'UrlSafe, 'NoPad, 'UrlSafeNoPad |] -> String -> [| 'Ok String, 'Error { message : String } |]
            PrimOp::StringBase64Decode => (
                vec![
                    mk_uty_enum!("Standard", "UrlSafe", "NoPad", "UrlSafeNoPad"),
                    mk_uniftype::str(),
                ],
                mk_uty_enum!(
                    ("Ok", TypeF::String),
                    ("Error", mk_uty_record!(("message", TypeF::String)))
                ),
            ),
            // The first argument is a contract, the second is a label.
            // forall a. Dyn -> Dyn -> Array a -> Array a
            PrimOp::ContractArrayLazyApp => {
                let ty_elt = state.table.fresh_type_uvar(var_level);
                let ty_array = mk_uniftype::array(ty_elt);
                (
                    vec![mk_uniftype::dynamic(), mk_uniftype::dynamic()],
                    mk_uty_arrow!(ty_array.clone(), ty_array),
                )
            }
            // The first argument is a label, the third is a contract.
            // forall a. Dyn -> {_: a} -> Dyn -> {_: a}
            PrimOp::ContractRecordLazyApp => {
                let ty_field = state.table.fresh_type_uvar(var_level);
                let ty_dict = mk_uniftype::dict(ty_field);
                (
                    vec![mk_uniftype::dynamic(), ty_dict.clone()],
                    mk_uty_arrow!(mk_uniftype::dynamic(), ty_dict),
                )
            }
            // Morally: Str -> Lbl -> Lbl
            // Actual: Str -> Dyn -> Dyn
            PrimOp::LabelWithMessage => (
                vec![mk_uniftype::str(), mk_uniftype::dynamic()],
                mk_uniftype::dynamic(),
            ),
            // Morally: Array Str -> Lbl -> Lbl
            // Actual: Array Str -> Dyn -> Dyn
            PrimOp::LabelWithNotes => (
                vec![mk_uniftype::array(TypeF::String), mk_uniftype::dynamic()],
                mk_uniftype::dynamic(),
            ),
            // Morally: Str -> Lbl -> Lbl
            // Actual: Str -> Dyn -> Dyn
            PrimOp::LabelAppendNote => (
                vec![mk_uniftype::str(), mk_uniftype::dynamic()],
                mk_uniftype::dynamic(),
            ),
            // Morally: Sym -> Lbl -> TypeVarData
            // Actual: Sym -> Dyn -> TypeVarData
            PrimOp::LabelLookupTypeVar => (
                vec![mk_uniftype::sym(), mk_uniftype::dynamic()],
                TypeVarData::unif_type(),
            ),
            // {_ : a} -> {_ : a}
            // -> {
            //  left_only: {_ : a},
            //  right_only: {_ : a},
            //  left_center: {_ : a},
            //  right_center: {_ : a},
            // }
            PrimOp::RecordSplitPair => {
                let elt = state.table.fresh_type_uvar(var_level);
                let dict = mk_uniftype::dict(elt.clone());

                let split_result = mk_uty_record!(
                    ("left_only", dict.clone()),
                    ("right_only", dict.clone()),
                    ("left_center", dict.clone()),
                    ("right_center", dict.clone())
                );

                (vec![dict.clone(), dict], split_result)
            }
            // {_ : a} -> {_ : a} -> {_ : a}
            PrimOp::RecordDisjointMerge => {
                let elt = state.table.fresh_type_uvar(var_level);
                let dict = mk_uniftype::dict(elt.clone());

                (vec![dict.clone(), dict.clone()], dict)
            }
            // Str -> Str -> Str -> Str
            PrimOp::StringReplace | PrimOp::StringReplaceRegex => (
                vec![mk_uniftype::str(), mk_uniftype::str(), mk_uniftype::str()],
                mk_uniftype::str(),
            ),
            // Str -> Num -> Num -> Str
            PrimOp::StringSubstr => (
                vec![mk_uniftype::str(), mk_uniftype::num(), mk_uniftype::num()],
                mk_uniftype::str(),
            ),
            // Dyn -> Dyn -> Dyn -> Dyn -> Dyn
            PrimOp::RecordSealTail => (
                vec![
                    mk_uniftype::dynamic(),
                    mk_uniftype::dynamic(),
                    mk_uniftype::dict(mk_uniftype::dynamic()),
                    mk_uniftype::dict(mk_uniftype::dynamic()),
                ],
                mk_uniftype::dynamic(),
            ),
            // Dyn -> Dyn -> Dyn -> Dyn
            PrimOp::RecordUnsealTail => (
                vec![
                    mk_uniftype::dynamic(),
                    mk_uniftype::dynamic(),
                    mk_uniftype::dict(mk_uniftype::dynamic()),
                ],
                mk_uniftype::dynamic(),
            ),
            // Num -> Num -> Array a -> Array a
            PrimOp::ArraySlice => {
                let element_type = state.table.fresh_type_uvar(var_level);

                (
                    vec![
                        mk_uniftype::num(),
                        mk_uniftype::num(),
                        mk_uniftype::array(element_type.clone()),
                    ],
                    mk_uniftype::array(element_type),
                )
            }
            // Morally: Label -> Record -> Record -> Record
            // Actual: Dyn -> Dyn -> Dyn -> Dyn
            PrimOp::MergeContract => (
                vec![
                    mk_uniftype::dynamic(),
                    mk_uniftype::dynamic(),
                    mk_uniftype::dynamic(),
                ],
                mk_uniftype::dynamic(),
            ),
            // Morally: Sym -> Polarity -> Lbl -> Lbl
            // Actual: Sym -> Polarity -> Dyn -> Dyn
            PrimOp::LabelInsertTypeVar => (
                vec![
                    mk_uniftype::sym(),
                    Polarity::unif_type(),
                    mk_uniftype::dynamic(),
                ],
                mk_uniftype::dynamic(),
            ),
        })
    }
}

/// The type of a custom contract. In nickel syntax, the returned type is:
///
/// ```nickel
/// Dyn -> Dyn -> [|
///   'Ok Dyn,
///   'Error { message | String | optional, notes | Array String | optional }
/// |]
/// ```
pub fn custom_contract_type(alloc: &AstAlloc) -> UnifType<'_> {
    mk_uty_arrow!(
        mk_uniftype::dynamic(),
        mk_uniftype::dynamic(),
        custom_contract_ret_type(alloc)
    )
}

/// The return type of a custom contract. See [custom_contract_type].
///
/// ```nickel
/// [|
///   'Ok Dyn,
///   'Error { message | String | optional, notes | Array String | optional }
/// |]
/// ```
pub fn custom_contract_ret_type(alloc: &AstAlloc) -> UnifType<'_> {
    mk_uty_enum!(
        ("Ok", mk_uniftype::dynamic()),
        ("Error", error_data_type(alloc))
    )
}

/// The type of error data that can be returned by a custom contract:
///
/// ```nickel
/// {
///   message
///     | String
///     | optional,
///   notes
///     | Array String
///     | optional
/// }
/// ```
fn error_data_type(alloc: &AstAlloc) -> UnifType<'_> {
    let error_data = builder::Record::new()
        .field("message")
        .optional(true)
        .contract(TypeF::String)
        .no_value(alloc)
        .field("notes")
        .contract(TypeF::Array(alloc.alloc(Type::from(TypeF::String))))
        .optional(true)
        .no_value(alloc)
        .field("blame_location")
        .optional(true)
        .contract(TypeF::Dyn)
        .no_value(alloc);

    UnifType::concrete(TypeF::Contract((
        alloc.alloc(error_data.build(alloc)),
        TermEnv::new(),
    )))
}
