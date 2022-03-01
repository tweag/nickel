//! Typing of primitive operations.
use super::*;
use crate::{
    error::TypecheckError,
    term::{BinaryOp, NAryOp, UnaryOp},
    types::AbsType,
};
use crate::{mk_tyw_arrow, mk_tyw_enum, mk_tyw_enum_row, mk_tyw_record, mk_tyw_row};

/// Type of unary operations.
pub fn get_uop_type(
    state: &mut State,
    op: &UnaryOp,
) -> Result<(TypeWrapper, TypeWrapper), TypecheckError> {
    Ok(match op {
        // forall a. bool -> a -> a -> a
        UnaryOp::Ite() => {
            let branches = TypeWrapper::Ptr(state.table.fresh_var());

            (
                mk_typewrapper::bool(),
                mk_tyw_arrow!(branches.clone(), branches.clone(), branches),
            )
        }
        // forall a. a -> Bool
        UnaryOp::IsNum()
        | UnaryOp::IsBool()
        | UnaryOp::IsStr()
        | UnaryOp::IsFun()
        | UnaryOp::IsArray()
        | UnaryOp::IsRecord() => {
            let inp = TypeWrapper::Ptr(state.table.fresh_var());
            (inp, mk_typewrapper::bool())
        }
        // Bool -> Bool -> Bool
        UnaryOp::BoolAnd() | UnaryOp::BoolOr() => (
            mk_typewrapper::bool(),
            mk_tyw_arrow!(AbsType::Bool(), AbsType::Bool()),
        ),
        // Bool -> Bool
        UnaryOp::BoolNot() => (mk_typewrapper::bool(), mk_typewrapper::bool()),
        // forall a. Dyn -> a
        UnaryOp::Blame() => {
            let res = TypeWrapper::Ptr(state.table.fresh_var());

            (mk_typewrapper::dynamic(), res)
        }
        // Dyn -> Bool
        UnaryOp::Pol() => (mk_typewrapper::dynamic(), mk_typewrapper::bool()),
        // forall rows. < | rows> -> <id | rows>
        UnaryOp::Embed(id) => {
            let row = TypeWrapper::Ptr(state.table.fresh_var());
            // Constraining a freshly created variable should never fail.
            constraint(state, row.clone(), id.clone()).unwrap();
            (mk_tyw_enum!(row.clone()), mk_tyw_enum!(id.clone(), row))
        }
        // This should not happen, as Switch() is only produced during evaluation.
        UnaryOp::Switch(_) => panic!("cannot typecheck Switch()"),
        // Dyn -> Dyn
        UnaryOp::ChangePolarity() | UnaryOp::GoDom() | UnaryOp::GoCodom() | UnaryOp::GoArray() => {
            (mk_typewrapper::dynamic(), mk_typewrapper::dynamic())
        }
        // Sym -> Dyn -> Dyn
        UnaryOp::Wrap() => (
            mk_typewrapper::sym(),
            mk_tyw_arrow!(AbsType::Dyn(), AbsType::Dyn()),
        ),
        // forall rows a. { id: a | rows} -> a
        UnaryOp::StaticAccess(id) => {
            let row = TypeWrapper::Ptr(state.table.fresh_var());
            let res = TypeWrapper::Ptr(state.table.fresh_var());

            (mk_tyw_record!((id.clone(), res.clone()); row), res)
        }
        // forall a b. Array a -> (a -> b) -> Array b
        UnaryOp::ArrayMap() => {
            let a = TypeWrapper::Ptr(state.table.fresh_var());
            let b = TypeWrapper::Ptr(state.table.fresh_var());

            let f_type = mk_tyw_arrow!(a.clone(), b.clone());
            (
                mk_typewrapper::array(a),
                mk_tyw_arrow!(f_type, mk_typewrapper::array(b)),
            )
        }
        // forall a. Num -> (Num -> a) -> Array a
        UnaryOp::ArrayGen() => {
            let a = TypeWrapper::Ptr(state.table.fresh_var());

            let f_type = mk_tyw_arrow!(AbsType::Num(), a.clone());
            (
                mk_typewrapper::num(),
                mk_tyw_arrow!(f_type, mk_typewrapper::array(a)),
            )
        }
        // forall a b. { _ : a} -> (Str -> a -> b) -> { _ : b }
        UnaryOp::RecordMap() => {
            // Assuming f has type Str -> a -> b,
            // this has type DynRecord(a) -> DynRecord(b)

            let a = TypeWrapper::Ptr(state.table.fresh_var());
            let b = TypeWrapper::Ptr(state.table.fresh_var());

            let f_type = mk_tyw_arrow!(AbsType::Str(), a.clone(), b.clone());
            (
                mk_typewrapper::dyn_record(a),
                mk_tyw_arrow!(f_type, mk_typewrapper::dyn_record(b)),
            )
        }
        // forall a b. a -> b -> b
        UnaryOp::Seq() | UnaryOp::DeepSeq(_) => {
            let fst = TypeWrapper::Ptr(state.table.fresh_var());
            let snd = TypeWrapper::Ptr(state.table.fresh_var());

            (fst, mk_tyw_arrow!(snd.clone(), snd))
        }
        // forall a. Array a -> a
        UnaryOp::ArrayHead() => {
            let ty_elt = TypeWrapper::Ptr(state.table.fresh_var());
            (mk_typewrapper::array(ty_elt.clone()), ty_elt)
        }
        // forall a. Array a -> Array a
        UnaryOp::ArrayTail() => {
            let ty_elt = TypeWrapper::Ptr(state.table.fresh_var());
            (
                mk_typewrapper::array(ty_elt.clone()),
                mk_typewrapper::array(ty_elt),
            )
        }
        // forall a. Array a -> Num
        UnaryOp::ArrayLength() => {
            let ty_elt = TypeWrapper::Ptr(state.table.fresh_var());
            (mk_typewrapper::array(ty_elt), mk_typewrapper::num())
        }
        // This should not happen, as ChunksConcat() is only produced during evaluation.
        UnaryOp::ChunksConcat() => panic!("cannot type ChunksConcat()"),
        // BEFORE: forall rows. { rows } -> Array
        // Dyn -> Array Str
        UnaryOp::FieldsOf() => (
            mk_typewrapper::dynamic(),
            //mk_tyw_record!(; TypeWrapper::Ptr(state.table.fresh_var())),
            mk_typewrapper::array(AbsType::Str()),
        ),
        // Dyn -> Array Dyn
        UnaryOp::ValuesOf() => (
            mk_typewrapper::dynamic(),
            mk_typewrapper::array(AbsType::Dyn()),
        ),
        // Str -> Str
        UnaryOp::StrTrim() => (mk_typewrapper::str(), mk_typewrapper::str()),
        // Str -> Array Str
        UnaryOp::StrChars() => (
            mk_typewrapper::str(),
            mk_typewrapper::array(mk_typewrapper::str()),
        ),
        // Str -> Num
        UnaryOp::CharCode() => (mk_typewrapper::str(), mk_typewrapper::num()),
        // Num -> Str
        UnaryOp::CharFromCode() => (mk_typewrapper::num(), mk_typewrapper::str()),
        // Str -> Str
        UnaryOp::StrUppercase() => (mk_typewrapper::str(), mk_typewrapper::str()),
        // Str -> Str
        UnaryOp::StrLowercase() => (mk_typewrapper::str(), mk_typewrapper::str()),
        // Str -> Num
        UnaryOp::StrLength() => (mk_typewrapper::str(), mk_typewrapper::num()),
        // Dyn -> Str
        UnaryOp::ToStr() => (mk_typewrapper::dynamic(), mk_typewrapper::num()),
        // Str -> Num
        UnaryOp::NumFromStr() => (mk_typewrapper::str(), mk_typewrapper::num()),
        // Str -> < | Dyn>
        UnaryOp::EnumFromStr() => (
            mk_typewrapper::str(),
            mk_tyw_enum!(mk_typewrapper::dynamic()),
        ),
    })
}

/// Type of a binary operation.
pub fn get_bop_type(
    state: &mut State,
    op: &BinaryOp,
) -> Result<(TypeWrapper, TypeWrapper, TypeWrapper), TypecheckError> {
    Ok(match op {
        // Num -> Num -> Num
        BinaryOp::Plus()
        | BinaryOp::Sub()
        | BinaryOp::Mult()
        | BinaryOp::Div()
        | BinaryOp::Modulo() => (
            mk_typewrapper::num(),
            mk_typewrapper::num(),
            mk_typewrapper::num(),
        ),
        // Str -> Str -> Str
        BinaryOp::StrConcat() => (
            mk_typewrapper::str(),
            mk_typewrapper::str(),
            mk_typewrapper::str(),
        ),
        // Ideally: Contract -> Label -> Dyn -> Dyn
        // Currenty: Dyn -> Dyn -> (Dyn -> Dyn)
        BinaryOp::Assume() => (
            mk_typewrapper::dynamic(),
            mk_typewrapper::dynamic(),
            mk_tyw_arrow!(mk_typewrapper::dynamic(), mk_typewrapper::dynamic()),
        ),
        // Sym -> Dyn -> Dyn -> Dyn
        BinaryOp::Unwrap() => (
            mk_typewrapper::sym(),
            mk_typewrapper::dynamic(),
            mk_tyw_arrow!(AbsType::Dyn(), AbsType::Dyn()),
        ),
        // Str -> Dyn -> Dyn
        BinaryOp::Tag() => (
            mk_typewrapper::str(),
            mk_typewrapper::dynamic(),
            mk_typewrapper::dynamic(),
        ),
        // forall a b. a -> b -> Bool
        BinaryOp::Eq() => (
            TypeWrapper::Ptr(state.table.fresh_var()),
            TypeWrapper::Ptr(state.table.fresh_var()),
            mk_typewrapper::bool(),
        ),
        // Num -> Num -> Bool
        BinaryOp::LessThan()
        | BinaryOp::LessOrEq()
        | BinaryOp::GreaterThan()
        | BinaryOp::GreaterOrEq() => (
            mk_typewrapper::num(),
            mk_typewrapper::num(),
            mk_typewrapper::bool(),
        ),
        // Str -> Dyn -> Dyn
        BinaryOp::GoField() => (
            mk_typewrapper::str(),
            mk_typewrapper::dynamic(),
            mk_typewrapper::dynamic(),
        ),
        // forall a. Str -> { _ : a} -> a
        BinaryOp::DynAccess() => {
            let res = TypeWrapper::Ptr(state.table.fresh_var());

            (
                mk_typewrapper::str(),
                mk_typewrapper::dyn_record(res.clone()),
                res,
            )
        }
        // forall a. Str -> { _ : a } -> a -> { _ : a }
        BinaryOp::DynExtend() => {
            let res = TypeWrapper::Ptr(state.table.fresh_var());
            (
                mk_typewrapper::str(),
                mk_typewrapper::dyn_record(res.clone()),
                mk_tyw_arrow!(res.clone(), mk_typewrapper::dyn_record(res)),
            )
        }
        // forall a. Str -> { _ : a } -> { _ : a}
        BinaryOp::DynRemove() => {
            let res = TypeWrapper::Ptr(state.table.fresh_var());

            (
                mk_typewrapper::str(),
                mk_typewrapper::dyn_record(res.clone()),
                mk_typewrapper::dyn_record(res),
            )
        }
        // Str -> Dyn -> Bool
        BinaryOp::HasField() => (
            mk_typewrapper::str(),
            mk_typewrapper::dynamic(),
            mk_typewrapper::bool(),
        ),
        // forall a. Array a -> Array a -> Array a
        BinaryOp::ArrayConcat() => {
            let ty_elt = TypeWrapper::Ptr(state.table.fresh_var());
            let ty_array = mk_typewrapper::array(ty_elt);
            (ty_array.clone(), ty_array.clone(), ty_array)
        }
        // forall a. Array a -> Num -> a
        BinaryOp::ArrayElemAt() => {
            let ty_elt = TypeWrapper::Ptr(state.table.fresh_var());
            (
                mk_typewrapper::array(ty_elt.clone()),
                mk_typewrapper::num(),
                ty_elt,
            )
        }
        // Dyn -> Dyn -> Dyn
        BinaryOp::Merge() => (
            mk_typewrapper::dynamic(),
            mk_typewrapper::dynamic(),
            mk_typewrapper::dynamic(),
        ),
        // <Md5, Sha1, Sha256, Sha512> -> Str -> Str
        BinaryOp::Hash() => (
            mk_tyw_enum!(
                "Md5",
                "Sha1",
                "Sha256",
                "Sha512",
                mk_typewrapper::row_empty()
            ),
            mk_typewrapper::str(),
            mk_typewrapper::str(),
        ),
        // forall a. <Json, Yaml, Toml> -> a -> Str
        BinaryOp::Serialize() => {
            let ty_input = TypeWrapper::Ptr(state.table.fresh_var());
            (
                mk_tyw_enum!("Json", "Yaml", "Toml", mk_typewrapper::row_empty()),
                ty_input,
                mk_typewrapper::str(),
            )
        }
        // <Json, Yaml, Toml> -> Str -> Dyn
        BinaryOp::Deserialize() => (
            mk_tyw_enum!("Json", "Yaml", "Toml", mk_typewrapper::row_empty()),
            mk_typewrapper::str(),
            mk_typewrapper::dynamic(),
        ),
        // Num -> Num -> Num
        BinaryOp::Pow() => (
            mk_typewrapper::num(),
            mk_typewrapper::num(),
            mk_typewrapper::num(),
        ),
        // Str -> Str -> Bool
        BinaryOp::StrContains() => (
            mk_typewrapper::str(),
            mk_typewrapper::str(),
            mk_typewrapper::bool(),
        ),
        // Str -> Str -> Bool
        BinaryOp::StrIsMatch() => (
            mk_typewrapper::str(),
            mk_typewrapper::str(),
            mk_typewrapper::bool(),
        ),
        // Str -> Str -> {match: Str, index: Num, groups: Array Str}
        BinaryOp::StrMatch() => (
            mk_typewrapper::str(),
            mk_typewrapper::str(),
            mk_tyw_record!(
                ("match", AbsType::Str()),
                ("index", AbsType::Num()),
                ("groups", mk_typewrapper::array(AbsType::Str()))
            ),
        ),
        // Str -> Str -> Array Str
        BinaryOp::StrSplit() => (
            mk_typewrapper::str(),
            mk_typewrapper::str(),
            mk_typewrapper::array(AbsType::Str()),
        ),
    })
}

pub fn get_nop_type(
    _state: &mut State,
    op: &NAryOp,
) -> Result<(Vec<TypeWrapper>, TypeWrapper), TypecheckError> {
    Ok(match op {
        // Str -> Str -> Str -> Str
        NAryOp::StrReplace() | NAryOp::StrReplaceRegex() => (
            vec![
                mk_typewrapper::str(),
                mk_typewrapper::str(),
                mk_typewrapper::str(),
            ],
            mk_typewrapper::str(),
        ),
        // Str -> Num -> Num -> Str
        NAryOp::StrSubstr() => (
            vec![
                mk_typewrapper::str(),
                mk_typewrapper::num(),
                mk_typewrapper::num(),
            ],
            mk_typewrapper::str(),
        ),
        // This should not happen, as Switch() is only produced during evaluation.
        NAryOp::MergeContract() => panic!("cannot typecheck MergeContract()"),
    })
}
