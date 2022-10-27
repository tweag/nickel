//! Special typing analysis utils that should be done when we run the type checker in LSP mode

use crate::{
    cache::ImportResolver,
    term::{Contract, MetaValue, Term},
    types::{AbsType, Types},
};

use super::{replace_wildcards_with_var, ApparentType, Context, Environment, State, TypeWrapper};

/// Similar to [`super::apparent_type`], but it adds support for type inference on record terms.
pub fn apparent_type(
    t: &Term,
    env: Option<&Environment>,
    resolver: Option<&dyn ImportResolver>,
) -> ApparentType {
    match t {
        Term::MetaValue(MetaValue {
            types: Some(Contract { types: ty, .. }),
            ..
        }) => ApparentType::Annotated(ty.clone()),
        // For metavalues, if there's no type annotation, choose the first contract appearing.
        Term::MetaValue(MetaValue { contracts, .. }) if !contracts.is_empty() => {
            ApparentType::Annotated(contracts.get(0).unwrap().types.clone())
        }
        Term::MetaValue(MetaValue { value: Some(v), .. }) => {
            apparent_type(v.as_ref(), env, resolver)
        }
        Term::Num(_) => ApparentType::Inferred(Types(AbsType::Num())),
        Term::Bool(_) => ApparentType::Inferred(Types(AbsType::Bool())),
        Term::SealingKey(_) => ApparentType::Inferred(Types(AbsType::Sym())),
        Term::Str(_) | Term::StrChunks(_) => ApparentType::Inferred(Types(AbsType::Str())),
        Term::Array(..) => {
            ApparentType::Approximated(Types(AbsType::Array(Box::new(Types(AbsType::Dyn())))))
        }
        Term::Var(id) => env
            .and_then(|envs| envs.get(id).cloned())
            .map(ApparentType::FromEnv)
            .unwrap_or(ApparentType::Approximated(Types(AbsType::Dyn()))),
        Term::Record(fields, ..) | Term::RecRecord(fields, ..) => {
            let row = fields
                .iter()
                .fold(Types(AbsType::RowEmpty()), |row, (ident, t)| {
                    let ty = Box::new(Types::from(apparent_type(&t.term, env, resolver)));
                    Types(AbsType::RowExtend(ident.clone(), Some(ty), Box::new(row)))
                });
            let t = Types(AbsType::Record(Box::new(row)));
            ApparentType::Approximated(t)
        }
        Term::ResolvedImport(f) => {
            if let Some(r) = resolver {
                let t = r
                    .get(*f)
                    .expect("Internal error: resolved import not found during typechecking.");
                apparent_type(&t.term, env, Some(r))
            } else {
                ApparentType::Approximated(Types(AbsType::Dyn()))
            }
        }
        _ => ApparentType::Approximated(Types(AbsType::Dyn())),
    }
}

/// The LSP's version of [`super::binding_type`], but it delegates to a local type inference function
/// that allows type inference for record terms.
pub fn binding_type(state: &mut State, t: &Term, ctxt: &Context, strict: bool) -> TypeWrapper {
    let ty_apt = apparent_type(t, Some(&ctxt.type_env), Some(state.resolver));

    match ty_apt {
        ApparentType::Annotated(ty) if strict => {
            replace_wildcards_with_var(state.table, state.wildcard_vars, ty, &ctxt.term_env)
        }
        ApparentType::Approximated(_) if strict => state.table.fresh_unif_var(),
        ty_apt => TypeWrapper::from_apparent_type(ty_apt, &ctxt.term_env),
    }
}
