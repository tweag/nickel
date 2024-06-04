//! Typing of primitive operations.
use super::*;
use crate::{
    error::TypecheckError,
    label::{Polarity, TypeVarData},
    term::{BinaryOp, NAryOp, RecordExtKind, UnaryOp},
    typ::TypeF,
};
use crate::{mk_uty_arrow, mk_uty_enum, mk_uty_record};

/// Type of unary operations.
pub fn get_uop_type(
    state: &mut State,
    var_level: VarLevel,
    op: &UnaryOp,
) -> Result<(UnifType, UnifType), TypecheckError> {
    Ok(match op {
        // forall a. bool -> a -> a -> a
        UnaryOp::IfThenElse => {
            let branches = state.table.fresh_type_uvar(var_level);

            (
                mk_uniftype::bool(),
                mk_uty_arrow!(branches.clone(), branches.clone(), branches),
            )
        }
        // Dyn -> [| 'Number, 'Bool, 'String, 'Enum, 'Function, 'Array, 'Record, 'Label, 'Other |]
        UnaryOp::Typeof => (
            mk_uniftype::dynamic(),
            mk_uty_enum!(
                "Number",
                "Bool",
                "String",
                "Enum",
                "Function",
                "Array",
                "Record",
                "Label",
                "ForeignId",
                "Other"
            ),
        ),
        // Bool -> Bool -> Bool
        UnaryOp::BoolAnd | UnaryOp::BoolOr => {
            (mk_uniftype::bool(), mk_uty_arrow!(TypeF::Bool, TypeF::Bool))
        }
        // Bool -> Bool
        UnaryOp::BoolNot => (mk_uniftype::bool(), mk_uniftype::bool()),
        // forall a. Dyn -> a
        UnaryOp::Blame => {
            let res = state.table.fresh_type_uvar(var_level);

            (mk_uniftype::dynamic(), res)
        }
        // Dyn -> Polarity
        UnaryOp::LabelPol => (mk_uniftype::dynamic(), mk_uty_enum!("Positive", "Negative")),
        // forall rows. [| ; rows |] -> [| id ; rows |]
        UnaryOp::EnumEmbed(id) => {
            let row_var_id = state.table.fresh_erows_var_id(var_level);
            let row = UnifEnumRows::UnifVar {
                id: row_var_id,
                init_level: var_level,
            };

            let domain = mk_uty_enum!(; row.clone());
            let codomain = mk_uty_enum!(*id; row);

            (domain, codomain)
        }
        // This should not happen, as a match primop is only produced during evaluation.
        UnaryOp::TagsOnlyMatch { .. } => panic!("cannot typecheck match primop"),
        // Morally, Label -> Label
        // Dyn -> Dyn
        UnaryOp::LabelFlipPol
        | UnaryOp::LabelGoDom
        | UnaryOp::LabelGoCodom
        | UnaryOp::LabelGoArray
        | UnaryOp::LabelGoDict => (mk_uniftype::dynamic(), mk_uniftype::dynamic()),
        // forall rows a. { id: a | rows} -> a
        UnaryOp::RecordAccess(id) => {
            let rows = state.table.fresh_rrows_uvar(var_level);
            let res = state.table.fresh_type_uvar(var_level);

            (mk_uty_record!((*id, res.clone()); rows), res)
        }
        // forall a b. Array a -> (a -> b) -> Array b
        UnaryOp::ArrayMap => {
            let a = state.table.fresh_type_uvar(var_level);
            let b = state.table.fresh_type_uvar(var_level);

            let f_type = mk_uty_arrow!(a.clone(), b.clone());
            (
                mk_uniftype::array(a),
                mk_uty_arrow!(f_type, mk_uniftype::array(b)),
            )
        }
        // forall a. Num -> (Num -> a) -> Array a
        UnaryOp::ArrayGen => {
            let a = state.table.fresh_type_uvar(var_level);

            let f_type = mk_uty_arrow!(TypeF::Number, a.clone());
            (
                mk_uniftype::num(),
                mk_uty_arrow!(f_type, mk_uniftype::array(a)),
            )
        }
        // forall a b. { _ : a} -> (Str -> a -> b) -> { _ : b }
        UnaryOp::RecordMap => {
            // Assuming f has type Str -> a -> b,
            // this has type Dict(a) -> Dict(b)

            let a = state.table.fresh_type_uvar(var_level);
            let b = state.table.fresh_type_uvar(var_level);

            let f_type = mk_uty_arrow!(TypeF::String, a.clone(), b.clone());
            (
                mk_uniftype::dict(a),
                mk_uty_arrow!(f_type, mk_uniftype::dict(b)),
            )
        }
        // forall a b. a -> b -> b
        UnaryOp::Seq | UnaryOp::DeepSeq => {
            let fst = state.table.fresh_type_uvar(var_level);
            let snd = state.table.fresh_type_uvar(var_level);

            (fst, mk_uty_arrow!(snd.clone(), snd))
        }
        // forall a. Array a -> Num
        UnaryOp::ArrayLength => {
            let ty_elt = state.table.fresh_type_uvar(var_level);
            (mk_uniftype::array(ty_elt), mk_uniftype::num())
        }
        // This should not happen, as ChunksConcat() is only produced during evaluation.
        UnaryOp::ChunksConcat => panic!("cannot type ChunksConcat()"),
        // forall a. { _: a } -> Array Str
        UnaryOp::RecordFields(_) => {
            let ty_a = state.table.fresh_type_uvar(var_level);

            (
                mk_uniftype::dict(ty_a),
                mk_uniftype::array(mk_uniftype::str()),
            )
        }
        // forall a. { _: a } -> Array a
        UnaryOp::RecordValues => {
            let ty_a = state.table.fresh_type_uvar(var_level);

            (mk_uniftype::dict(ty_a.clone()), mk_uniftype::array(ty_a))
        }
        // Str -> Str
        UnaryOp::StringTrim => (mk_uniftype::str(), mk_uniftype::str()),
        // Str -> Array Str
        UnaryOp::StringChars => (mk_uniftype::str(), mk_uniftype::array(mk_uniftype::str())),
        // Str -> Str
        UnaryOp::StringUppercase => (mk_uniftype::str(), mk_uniftype::str()),
        // Str -> Str
        UnaryOp::StringLowercase => (mk_uniftype::str(), mk_uniftype::str()),
        // Str -> Num
        UnaryOp::StringLength => (mk_uniftype::str(), mk_uniftype::num()),
        // Dyn -> Str
        UnaryOp::ToString => (mk_uniftype::dynamic(), mk_uniftype::str()),
        // Str -> Num
        UnaryOp::NumberFromString => (mk_uniftype::str(), mk_uniftype::num()),
        // Str -> < | a> for a rigid type variable a
        UnaryOp::EnumFromString => (
            mk_uniftype::str(),
            mk_uty_enum!(; state.table.fresh_erows_const(var_level)),
        ),
        // Str -> Str -> Bool
        UnaryOp::StringIsMatch => (
            mk_uniftype::str(),
            mk_uty_arrow!(mk_uniftype::str(), mk_uniftype::bool()),
        ),
        // Str -> Str -> {matched: Str, index: Num, groups: Array Str}
        UnaryOp::StringFind => (
            mk_uniftype::str(),
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
        UnaryOp::StringFindAll => (
            mk_uniftype::str(),
            mk_uty_arrow!(
                mk_uniftype::str(),
                mk_uniftype::array(mk_uty_record!(
                    ("matched", TypeF::String),
                    ("index", TypeF::Number),
                    ("groups", mk_uniftype::array(TypeF::String))
                ))
            ),
        ),
        // Str -> Bool
        UnaryOp::StringIsMatchCompiled(_) => (mk_uniftype::str(), mk_uniftype::bool()),
        // Str -> {matched: Str, index: Num, groups: Array Str}
        UnaryOp::StringFindCompiled(_) => (
            mk_uniftype::str(),
            mk_uty_record!(
                ("matched", TypeF::String),
                ("index", TypeF::Number),
                ("groups", mk_uniftype::array(TypeF::String))
            ),
        ),
        UnaryOp::StringFindAllCompiled(_) => (
            mk_uniftype::str(),
            mk_uniftype::array(mk_uty_record!(
                ("matched", TypeF::String),
                ("index", TypeF::Number),
                ("groups", mk_uniftype::array(TypeF::String))
            )),
        ),
        // Dyn -> Dyn
        UnaryOp::Force { .. } => (mk_uniftype::dynamic(), mk_uniftype::dynamic()),
        // forall a. a -> a
        UnaryOp::RecDefault => {
            let ty = state.table.fresh_type_uvar(var_level);
            (ty.clone(), ty)
        }
        // forall a. a -> a
        UnaryOp::RecForce => {
            let ty = state.table.fresh_type_uvar(var_level);
            (ty.clone(), ty)
        }
        UnaryOp::RecordEmptyWithTail => (mk_uniftype::dynamic(), mk_uniftype::dynamic()),

        // forall a. Str -> a -> a
        UnaryOp::Trace => {
            let ty = state.table.fresh_type_uvar(var_level);
            (mk_uniftype::str(), mk_uty_arrow!(ty.clone(), ty))
        }
        // Morally: Lbl -> Lbl
        // Actual: Dyn -> Dyn
        UnaryOp::LabelPushDiag => (mk_uniftype::dynamic(), mk_uniftype::dynamic()),
        // Str -> Dyn
        #[cfg(feature = "nix-experimental")]
        UnaryOp::EvalNix => (mk_uniftype::str(), mk_uniftype::dynamic()),
        // Because the tag isn't fixed, we can't really provide a proper static type for this
        // primop.
        // This isn't a problem, as this operator is mostly internal and pattern matching should be
        // used to destructure enum variants.
        // Dyn -> Dyn
        UnaryOp::EnumGetArg => (mk_uniftype::dynamic(), mk_uniftype::dynamic()),
        // String -> (Dyn -> Dyn)
        UnaryOp::EnumMakeVariant => (
            mk_uniftype::str(),
            mk_uniftype::arrow(mk_uniftype::dynamic(), mk_uniftype::dynamic()),
        ),
        // Same as `EnumGetArg` just above.
        // Dyn -> Dyn
        UnaryOp::EnumGetTag => (mk_uniftype::dynamic(), mk_uniftype::dynamic()),
        // Note that is_variant breaks parametricity, so it can't get a polymorphic type.
        // Dyn -> Bool
        UnaryOp::EnumIsVariant => (mk_uniftype::dynamic(), mk_uniftype::bool()),
        // [crate::term::UnaryOp::PatternBranch] shouldn't appear anywhere in actual code, because its
        // second argument can't be properly typechecked: it has unbound variables. However, it's
        // not hard to come up with a vague working type for it, so we do.
        // forall a. {_ : a} -> Dyn -> Dyn
        UnaryOp::PatternBranch => {
            let ty_elt = state.table.fresh_type_uvar(var_level);
            (
                mk_uniftype::dict(ty_elt),
                mk_uty_arrow!(mk_uniftype::dynamic(), mk_uniftype::dynamic()),
            )
        }
    })
}

/// Type of a binary operation.
pub fn get_bop_type(
    state: &mut State,
    var_level: VarLevel,
    op: &BinaryOp,
) -> Result<(UnifType, UnifType, UnifType), TypecheckError> {
    Ok(match op {
        // Num -> Num -> Num
        BinaryOp::Plus | BinaryOp::Sub | BinaryOp::Mult | BinaryOp::Div | BinaryOp::Modulo => {
            (mk_uniftype::num(), mk_uniftype::num(), mk_uniftype::num())
        }
        // Sym -> Dyn -> Dyn -> Dyn
        BinaryOp::Seal => (
            mk_uniftype::sym(),
            mk_uniftype::dynamic(),
            mk_uty_arrow!(TypeF::Dyn, TypeF::Dyn),
        ),
        // Str -> Str -> Str
        BinaryOp::StringConcat => (mk_uniftype::str(), mk_uniftype::str(), mk_uniftype::str()),
        // Ideally: Contract -> Label -> Dyn -> Dyn
        // Currently: Dyn -> Dyn -> (Dyn -> Dyn)
        BinaryOp::ContractApply => (
            mk_uniftype::dynamic(),
            mk_uniftype::dynamic(),
            mk_uty_arrow!(mk_uniftype::dynamic(), mk_uniftype::dynamic()),
        ),
        // Sym -> Dyn -> Dyn -> Dyn
        BinaryOp::Unseal => (
            mk_uniftype::sym(),
            mk_uniftype::dynamic(),
            mk_uty_arrow!(TypeF::Dyn, TypeF::Dyn),
        ),
        // forall a b. a -> b -> Bool
        BinaryOp::Eq => (
            state.table.fresh_type_uvar(var_level),
            state.table.fresh_type_uvar(var_level),
            mk_uniftype::bool(),
        ),
        // Num -> Num -> Bool
        BinaryOp::LessThan | BinaryOp::LessOrEq | BinaryOp::GreaterThan | BinaryOp::GreaterOrEq => {
            (mk_uniftype::num(), mk_uniftype::num(), mk_uniftype::bool())
        }
        // Str -> Dyn -> Dyn
        BinaryOp::LabelGoField => (
            mk_uniftype::str(),
            mk_uniftype::dynamic(),
            mk_uniftype::dynamic(),
        ),
        // forall a. Str -> { _ : a} -> a
        BinaryOp::RecordGet => {
            let res = state.table.fresh_type_uvar(var_level);

            (mk_uniftype::str(), mk_uniftype::dict(res.clone()), res)
        }
        // forall a. Str -> {_ : a} -> a -> {_ : a}
        BinaryOp::RecordInsert {
            ext_kind: RecordExtKind::WithValue,
            ..
        } => {
            let res = state.table.fresh_type_uvar(var_level);
            (
                mk_uniftype::str(),
                mk_uniftype::dict(res.clone()),
                mk_uty_arrow!(res.clone(), mk_uniftype::dict(res)),
            )
        }
        // forall a. Str -> {_ : a} -> {_ : a}
        BinaryOp::RecordInsert {
            ext_kind: RecordExtKind::WithoutValue,
            ..
        } => {
            let res = state.table.fresh_type_uvar(var_level);
            (
                mk_uniftype::str(),
                mk_uniftype::dict(res.clone()),
                mk_uty_arrow!(res.clone(), mk_uniftype::dict(res)),
            )
        }
        // forall a. Str -> { _ : a } -> { _ : a}
        BinaryOp::RecordRemove(_) => {
            let res = state.table.fresh_type_uvar(var_level);
            (
                mk_uniftype::str(),
                mk_uniftype::dict(res.clone()),
                mk_uniftype::dict(res),
            )
        }
        // forall a. Str -> {_: a} -> Bool
        BinaryOp::RecordHasField(_) => {
            let ty_elt = state.table.fresh_type_uvar(var_level);
            (
                mk_uniftype::str(),
                mk_uniftype::dict(ty_elt),
                mk_uniftype::bool(),
            )
        }
        // forall a. Str -> {_: a} -> Bool
        BinaryOp::RecordFieldIsDefined(_) => {
            let ty_elt = state.table.fresh_type_uvar(var_level);
            (
                mk_uniftype::str(),
                mk_uniftype::dict(ty_elt),
                mk_uniftype::bool(),
            )
        }
        // forall a. Array a -> Array a -> Array a
        BinaryOp::ArrayConcat => {
            let ty_elt = state.table.fresh_type_uvar(var_level);
            let ty_array = mk_uniftype::array(ty_elt);
            (ty_array.clone(), ty_array.clone(), ty_array)
        }
        // forall a. Array a -> Num -> a
        BinaryOp::ArrayAt => {
            let ty_elt = state.table.fresh_type_uvar(var_level);
            (
                mk_uniftype::array(ty_elt.clone()),
                mk_uniftype::num(),
                ty_elt,
            )
        }
        // Dyn -> Dyn -> Dyn
        BinaryOp::Merge(_) => (
            mk_uniftype::dynamic(),
            mk_uniftype::dynamic(),
            mk_uniftype::dynamic(),
        ),
        // <Md5, Sha1, Sha256, Sha512> -> Str -> Str
        BinaryOp::Hash => (
            mk_uty_enum!("Md5", "Sha1", "Sha256", "Sha512"),
            mk_uniftype::str(),
            mk_uniftype::str(),
        ),
        // forall a. <Json, Yaml, Toml> -> a -> Str
        BinaryOp::Serialize => {
            let ty_input = state.table.fresh_type_uvar(var_level);
            (
                mk_uty_enum!("Json", "Yaml", "Toml"),
                ty_input,
                mk_uniftype::str(),
            )
        }
        // <Json, Yaml, Toml> -> Str -> Dyn
        BinaryOp::Deserialize => (
            mk_uty_enum!("Json", "Yaml", "Toml"),
            mk_uniftype::str(),
            mk_uniftype::dynamic(),
        ),
        // Num -> Num -> Num
        BinaryOp::Pow => (mk_uniftype::num(), mk_uniftype::num(), mk_uniftype::num()),
        // Str -> Str -> Bool
        BinaryOp::StringContains => (mk_uniftype::str(), mk_uniftype::str(), mk_uniftype::bool()),
        // Str -> Str -> Array Str
        BinaryOp::StringSplit => (
            mk_uniftype::str(),
            mk_uniftype::str(),
            mk_uniftype::array(TypeF::String),
        ),
        // The first argument is a contract, the second is a label.
        // forall a. Dyn -> Dyn -> Array a -> Array a
        BinaryOp::ContractArrayLazyApp => {
            let ty_elt = state.table.fresh_type_uvar(var_level);
            let ty_array = mk_uniftype::array(ty_elt);
            (
                mk_uniftype::dynamic(),
                mk_uniftype::dynamic(),
                mk_uty_arrow!(ty_array.clone(), ty_array),
            )
        }
        // The first argument is a label, the third is a contract.
        // forall a. Dyn -> {_: a} -> Dyn -> {_: a}
        BinaryOp::ContractRecordLazyApp => {
            let ty_field = state.table.fresh_type_uvar(var_level);
            let ty_dict = mk_uniftype::dict(ty_field);
            (
                mk_uniftype::dynamic(),
                ty_dict.clone(),
                mk_uty_arrow!(mk_uniftype::dynamic(), ty_dict),
            )
        }
        // Morally: Str -> Lbl -> Lbl
        // Actual: Str -> Dyn -> Dyn
        BinaryOp::LabelWithMessage => (
            mk_uniftype::str(),
            mk_uniftype::dynamic(),
            mk_uniftype::dynamic(),
        ),
        // Morally: Array Str -> Lbl -> Lbl
        // Actual: Array Str -> Dyn -> Dyn
        BinaryOp::LabelWithNotes => (
            mk_uniftype::array(TypeF::String),
            mk_uniftype::dynamic(),
            mk_uniftype::dynamic(),
        ),
        // Morally: Str -> Lbl -> Lbl
        // Actual: Str -> Dyn -> Dyn
        BinaryOp::LabelAppendNote => (
            mk_uniftype::str(),
            mk_uniftype::dynamic(),
            mk_uniftype::dynamic(),
        ),
        // Morally: Sym -> Lbl -> TypeVarData
        // Actual: Sym -> Dyn -> TypeVarData
        BinaryOp::LabelLookupTypeVar => (
            mk_uniftype::sym(),
            mk_uniftype::dynamic(),
            TypeVarData::unif_type(),
        ),
    })
}

pub fn get_nop_type(
    state: &mut State,
    var_level: VarLevel,
    op: &NAryOp,
) -> Result<(Vec<UnifType>, UnifType), TypecheckError> {
    Ok(match op {
        // Str -> Str -> Str -> Str
        NAryOp::StringReplace | NAryOp::StringReplaceRegex => (
            vec![mk_uniftype::str(), mk_uniftype::str(), mk_uniftype::str()],
            mk_uniftype::str(),
        ),
        // Str -> Num -> Num -> Str
        NAryOp::StringSubstr => (
            vec![mk_uniftype::str(), mk_uniftype::num(), mk_uniftype::num()],
            mk_uniftype::str(),
        ),
        // Dyn -> Dyn -> Dyn -> Dyn -> Dyn
        NAryOp::RecordSealTail => (
            vec![
                mk_uniftype::dynamic(),
                mk_uniftype::dynamic(),
                mk_uniftype::dict(mk_uniftype::dynamic()),
                mk_uniftype::dict(mk_uniftype::dynamic()),
            ],
            mk_uniftype::dynamic(),
        ),
        // Dyn -> Dyn -> Dyn -> Dyn
        NAryOp::RecordUnsealTail => (
            vec![
                mk_uniftype::dynamic(),
                mk_uniftype::dynamic(),
                mk_uniftype::dict(mk_uniftype::dynamic()),
            ],
            mk_uniftype::dynamic(),
        ),
        // Num -> Num -> Array a -> Array a
        NAryOp::ArraySlice => {
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
        // This should not happen, as MergeContract() is only produced during evaluation.
        NAryOp::MergeContract => panic!("cannot typecheck MergeContract()"),
        // Morally: Sym -> Polarity -> Lbl -> Lbl
        // Actual: Sym -> Polarity -> Dyn -> Dyn
        NAryOp::LabelInsertTypeVar => (
            vec![
                mk_uniftype::sym(),
                Polarity::unif_type(),
                mk_uniftype::dynamic(),
            ],
            mk_uniftype::dynamic(),
        ),
    })
}
