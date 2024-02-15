//! Program transformations.
use crate::{
    cache::ImportResolver,
    term::{RichTerm, Traverse, TraverseOrder},
    typ::UnboundTypeVariableError,
    typecheck::Wildcards,
};

pub mod desugar_destructuring;
pub mod free_vars;
pub mod gen_pending_contracts;
pub mod import_resolution;
pub mod substitute_wildcards;

/// Apply all program transformations, excepted import resolution that is currently performed
/// earlier, as it needs to be done before typechecking.
///
/// Do not perform transformations on the imported files. If needed, either do it yourself using
/// pending imports returned by [`resolve_imports`][import_resolution::strict::resolve_imports] or
/// use the [cache][crate::cache::Cache].
pub fn transform(
    mut rt: RichTerm,
    wildcards: Option<&Wildcards>,
) -> Result<RichTerm, UnboundTypeVariableError> {
    free_vars::transform(&mut rt);
    transform_no_free_vars(rt, wildcards)
}

/// Same as [`transform`], but doesn't apply the free vars transformation.
pub fn transform_no_free_vars(
    rt: RichTerm,
    wildcards: Option<&Wildcards>,
) -> Result<RichTerm, UnboundTypeVariableError> {
    let rt = rt.traverse(
        &mut |mut rt: RichTerm| -> Result<RichTerm, UnboundTypeVariableError> {
            // Start by substituting any wildcard with its inferred type
            if let Some(wildcards) = wildcards {
                rt = substitute_wildcards::transform_one(rt, wildcards);
            }
            // We desugar destructuring before other transformations, as this step generates new
            // record contracts and terms that must be themselves transformed.
            let rt = desugar_destructuring::transform_one(rt);
            Ok(rt)
        },
        TraverseOrder::TopDown,
    )?;

    Ok(rt
        .traverse(
            &mut |rt: RichTerm| -> Result<RichTerm, UnboundTypeVariableError> {
                // `gen_pending_contracts` is applied bottom-up, because it might generate
                // additional terms down the AST (pending contracts pushed down the fields of a
                // record). In a top-down workflow, we would then visit those new duplicated nodes
                // (already visited as part of transforming the metadata), and do that potentially
                // again one level down. This results in a potentially non-linear cost in the size
                // of the AST. This was witnessed on Terraform-Nickel, causing examples using huge
                // auto-generated contracts (several of MBs) to not terminate in reasonable time.
                let rt = gen_pending_contracts::transform_one(rt)?;
                Ok(rt)
            },
            TraverseOrder::BottomUp,
        )
        .unwrap())
}
