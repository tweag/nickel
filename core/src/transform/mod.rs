//! Program transformations.
use crate::{
    cache::ImportResolver,
    eval::value::NickelValue,
    position::PosTable,
    traverse::{Traverse, TraverseOrder},
    typ::UnboundTypeVariableError,
};

pub mod desugar_destructuring;
pub mod free_vars;
pub mod gen_pending_contracts;
pub mod import_resolution;
pub mod substitute_wildcards;

/// RFC007: we can't use yet the new `typecheck::Wildcards` type, as they follow the new AST. We
/// temporarily redefine a `Wildcards` type that matches the old definition.
pub(crate) type Wildcards = Vec<crate::typ::Type>;

/// Apply all program transformations, excepted import resolution that is currently performed
/// earlier, as it needs to be done before typechecking.
///
/// Do not perform transformations on the imported files. If needed, either do it yourself using
/// pending imports returned by [`resolve_imports`][import_resolution::strict::resolve_imports] or
/// use the [cache][crate::cache::CacheHub].
pub fn transform(
    pos_table: &mut PosTable,
    mut value: NickelValue,
    wildcards: Option<&Wildcards>,
) -> Result<NickelValue, UnboundTypeVariableError> {
    free_vars::transform(&mut value);
    transform_no_free_vars(pos_table, value, wildcards)
}

/// Same as [`transform`], but doesn't apply the free vars transformation.
pub fn transform_no_free_vars(
    pos_table: &mut PosTable,
    value: NickelValue,
    wildcards: Option<&Wildcards>,
) -> Result<NickelValue, UnboundTypeVariableError> {
    let value = value.traverse(
        &mut |mut value: NickelValue| -> Result<NickelValue, UnboundTypeVariableError> {
            // Start by substituting any wildcard with its inferred type
            if let Some(wildcards) = wildcards {
                value = substitute_wildcards::transform_one(value, wildcards);
            }
            // We desugar destructuring before other transformations, as this step generates new
            // record contracts and terms that must be themselves transformed.
            Ok(desugar_destructuring::transform_one(pos_table, value))
        },
        TraverseOrder::TopDown,
    )?;

    Ok(value
        .traverse(
            &mut |value: NickelValue| -> Result<NickelValue, UnboundTypeVariableError> {
                // `gen_pending_contracts` is applied bottom-up, because it might generate
                // additional terms down the AST (pending contracts pushed down the fields of a
                // record). In a top-down workflow, we would then visit those new duplicated nodes
                // (already visited as part of transforming the metadata), and do that potentially
                // again one level down. This results in a potentially non-linear cost in the size
                // of the AST. This was witnessed on Terraform-Nickel, causing examples using huge
                // auto-generated contracts (several of MBs) to not terminate in reasonable time.
                gen_pending_contracts::transform_one(pos_table, value)
            },
            TraverseOrder::BottomUp,
        )
        .unwrap())
}
